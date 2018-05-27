#*******************************#
#******Training functions*******#
#*******************************#

DS <- function(X, Y, eta = 1) {
    #Find the best value to split an attribute on
    findSplit <- function(J, Y) {
        result <- sapply(unique(J), function(s)
            data.frame(
                rss = RSS(
                        vector(mode = 'numeric', length = length(Y[which(J < s)]))
                        + mean(Y[which(J < s)]),
                        Y[which(J < s)]
                    ) +
                    RSS(
                        vector(mode = 'numeric', length = length(Y[which(J >= s)]))
                        + mean(Y[which(J >= s)]),
                        Y[which(J >= s)]
                    )
                , threshold = s
                , y1 = mean(Y[which(J < s)])
                , y2 = mean(Y[which(J >= s)])
            )
        )
        return(as.data.frame(result[, which(result[1,] == min(unlist(result[1,])))]))
    }

    #Find the best attribute to split
    result <- apply(X, 2, function(j) findSplit(j, Y))
    res.mat <- matrix(unlist(result), ncol = 4, byrow = T)
    min <- which(res.mat[, 1] == min(res.mat[, 1]))
    out <- matrix(c(res.mat[min, c(2:4)], names(X)[min], eta = eta), nrow = 1)
    out <- data.frame(value = matrix(c(sapply(out[-4], function(o) as.numeric(o)), out[4])),
                      row.names = c(colnames(out)[-4], colnames(out)[4]))
    return(out)
}

BoostedDS <- function(X, Y, eta, B) {
    r <- Y
    f <- matrix(0, ncol = 5, nrow = B)
    for (b in 1:B) {
        f[b,] <- t(DS(X, r, eta))
        r <- r - predict(X, f[b,])
        # ^ All of the transposing because of silly choices at the start
    }
    return(f)
}

#*******************************#
#******Prediction function*******#
#*******************************#

predict <- function(X, model) {
    pred <- matrix(0, nrow = nrow(X), ncol = 1)
    names <- names(X)
    #Prediction for any given f in the model
    predictStump <- function(f) {
        f2 <- matrix(c(as.numeric(f[1]), as.numeric(f[2]), as.numeric(f[3]), as.numeric(f[4])))
        #Assign those below the threshold
        pred[which(X[which(names == f[5])] < f2[1])] <- f2[2]
        #Assign those below the threshold
        pred[which(X[which(names == f[5])] >= f2[1])] <- f2[3]
        return(pred)
    }
    #Find prediction for each f in the model and sum them * eta
    pred <- rowSums(apply(matrix(t(model), ncol = 5), 1, function(f) predictStump(f)))
    pred <- pred * as.numeric(matrix(t(model), ncol = 5)[1, 4]) #eta
    return(pred)
}


#******************************#
#******Residual Functions******#
#******************************#

RSS <- function(set1, set2) {
    return(sum((set1 - set2) ^ 2))
}

MSE <- function(set1, set2) {
    return((1 / length(set1)) * sum((set1 - set2) ^ 2))
}


#***************************#
#*****Testing Functions*****#
#***************************#

testDS <- function(trainingSet, testSet) {
    model <- DS(trainingSet[, -1], trainingSet[, 1])
    preds <- predict(testSet[, -1], model)
    testMSE <- MSE(testSet[, 1], preds)
    return(list(preds, testSet, testMSE))
}

testBoostedDS <- function(trainingSet, testSet, eta = .01, B = 1000) {
    model <- BoostedDS(trainingSet[, -1], trainingSet[, 1], eta = eta, B = B)
    preds <- predict(testSet[, -1], t(model))
    testMSE <- MSE(testSet[, 1], preds)
    return(list(preds, testSet, testMSE))
}

bostonAnalysis <- function(eta = .01, b = 1000) {
    if (!sum(search() == "package:MASS")) library(MASS) #Don't load MASS if it's already loaded
    set.seed(0701)

    #Get our desired attributes
    boston <- data.frame(medv = Boston$medv, lstat = Boston$lstat, rm = Boston$rm)
    #Assign training and test sets
    train <- sample(1:nrow(boston), nrow(boston) / 2)
    trainingSet <- boston[train,]
    testSet <- boston[-train,]

    #return(testDS(trainingSet, testSet))
    return(testBoostedDS(trainingSet, testSet, eta, b))
}

#For those with silly amounts of time, 
#or who can leave their computer running for a while
sillyAmountsOfTime <- function() {
    #B <- c(10, 100, 500, seq(from = 1000, to = 10000, by = 1000))
    B <- seq(from = 20, to = 2000, by = 20)
    metaResult <- sapply(B, function(b) bostonAnalysis(.01, b))
    return(metaResult)
}