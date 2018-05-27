KRR <- function(X, Y, x, l, K, p = 1) {
    return(matrix(t(Y) %*% solve(diag(l, nrow(X)) + K(X, t(X), p)) %*% K(X, t(x), p), ncol = 1))
}

PolyK <- function(x, y, d = 1) {
    return(matrix(apply(x, 1, function(r) apply(y, 2, function(s) (1 + r %*% s)^d ) ), nrow = nrow(x), ncol = ncol(y), byrow = T))
}

RadialK <- function(x, y, gamma = 1) {
    return(matrix( exp( -gamma * apply(x, 1, function(r) apply(y, 2, function(s) norm(matrix((r - s), nrow =  1))^2 ))), nrow = nrow(x), ncol = ncol(y), byrow = T ))
}

MSE <- function(set1, set2) {
    return((1 / length(set1)) * sum((set1 - set2) ^ 2))
}

TestKRR <- function(trainingSet, testSet, lambdaList, gList, dList) {
    trainLabels <- trainingSet[, 4]
    testLabels <- testSet[, 4]
    variablesRad <- matrix(sapply(gList, function(g)
                        apply(lambdaList, 1, function(l)
                            matrix(c(l, g), nrow = 1)
                        )
                    ), ncol = 2, byrow = T)
    variablesPoly <- matrix(sapply(dList, function(d)
                        apply(lambdaList, 1, function(l)
                            matrix(c(l, d), nrow = 1)
                        )
                    ), ncol = 2, byrow = T)

    Y <- matrix(trainingSet[, 4], ncol = 1)
    X <- trainingSet[, -4]
    
    polyLabels <- matrix(sapply(dList, function(d)
        sapply(lambdaList, function(l)
                            KRR(X, Y, testSet[, -4], l, PolyK, d)
                            )
                        ), nrow = nrow(testSet)
                    )
    pMSE <- apply(polyLabels, 2, function(p) MSE(p, testSet[, 4]))

    radialLabels <- matrix(sapply(gList, function(g)
        sapply(lambdaList, function(l)
                            KRR(X, Y, testSet[,-4], l, RadialK, g)
                            )
                        ), nrow = nrow(testSet)
                    )
    
    rMSE <- apply(radialLabels, 2, function(r) MSE(r, testSet[,4]))
    return(list(polyLabels, variablesPoly, pMSE, radialLabels, variablesRad, rMSE))
}

AnalyseAds <- function(seed) {
    set.seed(seed)
    advertising <- read.table("Advertising.csv", header = T, sep = ',')
    advertising <- advertising[, -1]
    train <- sample(1:nrow(advertising), (nrow(advertising) / 3) * 2)
    #Find the training mean and st dev for normalisation
    trainSD <- apply(advertising[train,], 2, function(a) sd(a))
    trainMean <- apply(advertising[train,], 2, function(a) mean(a))
    #Normalising data
    advertising <- sapply(i <- 1:ncol(advertising), function(i)(advertising[, i] - trainMean[i]) / trainSD[i])
    #Puhing the (training) data to the first quadrant to help with the intercept
    advertising <- sapply(1:4, function(i) advertising[, i] - min(advertising[train, i]))
    trainingSet <- advertising[train,]
    testSet <- advertising[-train,]
    dList <- matrix(c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), ncol = 1)
    gList <- matrix(c(seq(from = .001, to = 1, length.out = 5), seq(from = 10, to = 100, length.out = 5)), ncol = 1)
    lambdaList <- matrix(c(seq(from = 1, to = 9, length.out = 5), seq(from = 20, to = 100, length.out = 5)), ncol = 1)
    TestKRR(trainingSet, testSet, lambdaList, gList, dList)
}