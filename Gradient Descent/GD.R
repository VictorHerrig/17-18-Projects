trainGD <- function(trainingSetValues, Y, eta, n, seed) {
    set.seed(seed)
    #Constants
    #eta <- .5
    #n = 100 #Number of epochs
    listMSE <- matrix(0, nrow = n, ncol = 1)
    listYhat <- matrix(0, ncol = n, nrow = nrow(Y))
    #listb <- matrix(0, ncol = n, nrow = m)
    #listb0 <- matrix(0, ncol = n, nrow = 1)
    #listd <- matrix(0, ncol = n, nrow = nrow(Y))

    #Prepare the nodes and weights
    p <- ncol(trainingSetValues)
    m <- (p * 2) %/% 3
    X0 <- 1
    Z <- matrix(0, nrow = m, ncol = nrow(trainingSetValues))
    Z0 <- 1
    a <- matrix(runif(n = p * m, min = -.7, max = .7), nrow = m, ncol = p)
    a0 <- matrix(runif(n = m, min = -.7, max = .7), nrow = m, ncol = 1)
    b <- matrix(runif(n = m, min = -.7, max = .7), ncol = 1)
    b0 <- matrix(runif(n = 1), ncol = 1)

    for (pass in 1:n) {
        #Forward Pass
        Z <- matrix(0, nrow = nrow(trainingSetValues), ncol = m)
        yhat <- matrix(0, nrow = nrow(trainingSetValues), ncol = 1)
        for (i in 1:nrow(trainingSetValues)) {
            result <- predict(trainingSetValues[i,], a, a0, b, b0)
            Z[i,] <- result[[1]]
            yhat[i,] <- result[[2]]
        }

        #Backward Pass
        d <- matrix(sapply(c(1:nrow(trainingSetValues)), function(i) 2 * (yhat[i] - Y[i, 1])), ncol = 1)
        s <- matrix(
            sapply(c(1:nrow(trainingSetValues)), function(i)
                sapply(c(1:m), function(m)
                    d[i] * b[m] * as.vector(
                    sigmoidP(a0[m] + matrix(a[m,], nrow = 1) %*%
                    t(matrix(unlist(trainingSetValues[i,]), nrow = 1))))
                )
            ), nrow = nrow(trainingSetValues)
        )
        
        #Weight update
        b <- matrix(sapply(c(1:m), function(m) b[m] - (eta / n) * (t(d) %*% Z[, m])), ncol = 1)
        b0 <- matrix(b0 - (eta / n) * sum(d), ncol = 1)
        a <- matrix(sapply(1:m, function(m) sapply(1:p, function(l) a[m, l] - (eta / n) * (t(s[, m]) %*% trainingSetValues[, l]))), nrow = m)
        a0 <- matrix(sapply(1:m, function(m) a0[m,] - (eta / n) * sum(s[, m])), ncol = 1)

        listMSE[pass] <- (MSE(Y, yhat))
        listYhat[, pass] <- yhat
        #listb[, pass] <- b
        #listb0[, pass] <- b0
        #listd[,pass] <- d
    }

    #Return values
    return(list(a, a0, b, b0, listMSE, listYhat))#, listb, listb0, listd))

}

#*******************************#
#*******Utility functions*******#
#*******************************#

#Sigmoid
sigmoid <- function(x) 1 / (1 + exp(-x))

#Sigmoid Derivative
sigmoidP <- function(x) exp(x) / (exp(x) + 1) ^ 2

#*******************************#
#******Prediction fuction*******#
#*******************************#

predict <- function(x, a, a0, b, b0) {
    m <- nrow(b)
    ZValues <- matrix(sapply(1:m, function(m) sigmoid(a0[m,] + t(as.matrix(a[m,])) %*% t(matrix(unlist(x), nrow = 1)))), ncol = m)
    #YValues <- matrix(apply(ZValues, 1, function(z) b0 + t(b) %*% z), ncol = 1)
    YValues <- matrix(as.vector(b0) + t(b) %*% t(ZValues), ncol = 1)
    return(list(ZValues, YValues))
}

#******************************#
#*********MSE Function*********#
#******************************#

MSE <- function(set1, set2) {
    return((1 / nrow(set1)) * sum((set1 - set2)^2))
}

#***************************#
#*****Testing Functions*****#
#***************************#

testGD <- function(trainingSet, testSet, eta, n, seed) {
    trainingSetValues <- trainingSet[, -1]
    labels <- data.frame(high = trainingSet[, 1])
    model <- trainGD(trainingSetValues, labels, eta, n, seed)
    test.predictions <- matrix(0, nrow = nrow(testSet), ncol = 1)
    for (i in 1:nrow(testSet)) {
        test.predictions[i] <- predict(testSet[i,-1], model[[1]], model[[2]], model[[3]], model[[4]])[[2]]
    }

    mse <- MSE(test.predictions, testSet[, 1])
    return(list(mse, test.predictions, model))
}

AutoAnalysis <- function() {
    if (!sum(search() == "package:ISLR")) library(ISLR) #Don't load ISLR if it's already loaded
    set.seed(0701)

    auto <- Auto[, c('mpg', 'horsepower', 'weight', 'year', 'origin')]
    auto$origin <- sapply(auto$origin, function(x) which(unique(auto$origin) == x))
    # ^ Not actually needed since origin is already quantified, but just in case, 
    #since it was part of the assignment description
    auto[, - c(1, ncol(auto))] <- scale(auto[, - ncol(auto)]) #Normalization of numeric attributes
    auto <- data.frame(high = as.numeric(auto$mpg >= 23), auto[, -1]) #Replacing mpg with high

    #Assign training and test sets
    train <- sample(1:nrow(auto), nrow(auto) / 2)
    trainingSet <- auto[train,]
    testSet <- auto[-train,]

    #return(testGD(trainingSet, testSet, .01, 670))

    lapply(c(1:100), function(seed) testGD(trainingSet, testSet, .78, 1000, seed))
}