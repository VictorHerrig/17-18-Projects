#Classifies the value x for given values
classify <- function(x, mu, pi, covar, K) {
    p <- data.frame(p =
            matrix(
                unlist(
                    #For each k
                    lapply(K, FUN = function(k)
                        #do the general function for classification
                        - (.5 * log(det(as.matrix(covar[[k]]))))
                            - (.5 * t(as.matrix(x)) %*% (solve(covar[[k]]) %*% as.matrix(x)))
                            + t(as.matrix(x)) %*% (solve(covar[[k]]) %*% mu[, k])
                            - (.5 * (t(mu[, k]) %*% solve(covar[[k]]) %*% mu[, k]))
                            + log(pi[k])
                    )
                ), nrow = 1
            )
        )
    #Associate the values with the respective classes
    colnames(p) <- K

    #Return the k that maximizes the classifier function
    return(colnames(p)[max.col(p)])
}

#Returns the covariance matrix for the given data
findCovar <- function(trainingSet, mu, pi, K) {
    covar <- (1 / (nrow(trainingSet) - length(unique(trainingSet[, ncol(trainingSet)])))) *
        apply(matrix(c(1:nrow(mu)), nrow = 1), 2, FUN = function(x) #For each x and
            apply(matrix(c(1:nrow(mu)), nrow = 1), 2, FUN = function(y) #For each y,
                Reduce("+", #Sum together (used reduce because lapply returns a list) the
                    lapply(K, FUN = function(k) sum(#Sum for each class in K
                        matrix(trainingSet[which(trainingSet[, ncol(trainingSet)] == as.character(k)), x] - mu[x, k], nrow = 1) %*%
                        #^ The difference between (the values of the x column for the elements of the training set in the kth class) 
                        #and the mu for k and x
                        matrix(trainingSet[which(trainingSet[, ncol(trainingSet)] == as.character(k)), y] - mu[y, k], ncol = 1)
                        #^ The difference between (the values of the y column for the elements of the training set in the kth class) 
                        #and the mu for k and y
                    )
                )
            )
        )
     )
    return(covar)
}

#Returns pi for the given data
findPi <- function(trainingSet, K) {
    pi <- data.frame(matrix(
                           apply(matrix(K, nrow = 1), 2, FUN = function(x)
                                     nrow(trainingSet[which(trainingSet[, ncol(trainingSet)] == as.character(x)),]) /
                                         nrow(trainingSet)
                                         #Rows in the training set which match the unique class 
                                         #i.e. 'red'; divided by all the rows in the set
                                ), nrow = 1
                            )
                    )
    colnames(pi) <- K
    return(pi)
}

#Returns mu for the given data
findMu <- function(trainingSet, K) {
    mu <- apply(matrix(K, nrow = 1), 2, #Iterate over row vector K by column
        FUN = function(x)
            apply(as.matrix(trainingSet[which(trainingSet[, ncol(trainingSet)] == as.character(x)), c(1:ncol(trainingSet) - 1)]), 2,
            #^For each k in K, find the corresponding rows of the training set, then iterate over the columns in those rows
                  FUN = function(x) mean(x))
                  #^And save their mean values to mu
    )
    
    if(typeof(mu) != "list")
        mu <- data.frame(matrix(mu, ncol = length(K)))
    colnames(mu) <- K
    return(mu)
}

#Function that returns the total error rate for two sets; must be of same dimension
ErrorRate <- function(set1, set2) {
    return(1 - length(which(as.vector(set1) == as.vector(set2))) / length(set1))
}

#Function that returns labels produced by LDA
LDA <- function(testSet, trainingSet) {
    K <- as.character(unique(trainingSet[, ncol(trainingSet)]))
    pi <- findPi(trainingSet, K)
    mu <- findMu(trainingSet, K)
    covar <- findCovar(trainingSet, mu, pi, K)
    
    covarLDA <- as.list(setNames(nm = K)) #repeat the covariance matrix for every k value
    covarLDA <- setNames(lapply(K, FUN = function(k) covarLDA[[k]] <- covar), K)
    #so we can use a shared classification function
    
    return(matrix(
                  apply(as.matrix(testSet[, c(1:ncol(testSet)) - 1]), 1, FUN = function(x)
                        classify(x, mu, pi, covarLDA, K)
                  ), ncol = 1
           )
    )
}

#Function that returns labels produced by QDA
QDA <- function(testSet, trainingSet) {
    K <- as.character(unique(trainingSet[, ncol(trainingSet)]))

    pi <- findPi(trainingSet, K)

    mu <- findMu(trainingSet, K)

    covarQDA <- as.list(setNames(nm = K))
    covarQDA <- setNames(lapply(K, FUN = function(k) covarQDA[[k]] <- findCovar(trainingSet, mu, pi, k)), K)

    
    return(matrix(
                  apply(as.matrix(testSet[, c(1:ncol(testSet)) - 1]), 1, FUN = function(x)
                        classify(x, mu, pi, covarQDA, K)
                  ), ncol = 1
           )
    )
}

IrisAnalysis <- function(seed, p1, p2, multipleP = TRUE) {
    set.seed(seed)
    iris <- read.table('iris.txt', sep = ',')
    train <- sample(c(1:nrow(iris)), 70)
    if(multipleP) {
        trainingSet <- iris[train,c(p1,p2,5)]
        testSet <- iris[-train, c(p1, p2, 5)]
    }
    else {
        trainingSet <- iris[train, c(p1, 5)]
        testSet <- iris[-train, c(p1, 5)]
    }
    rownames(trainingSet) <- c(1:nrow(trainingSet))
    rownames(testSet) <- c(1:nrow(testSet))
    resultLDA <- LDA(testSet, trainingSet)
    resultQDA <- QDA(testSet, trainingSet)
    return(data.frame(LDAErr = ErrorRate(resultLDA, testSet[, ncol(testSet)]), QDAErr = ErrorRate(resultQDA, testSet[, ncol(testSet)])))
}

ParkinsonsAnalysis <- function(seed, p1, p2, multipleP = TRUE) {
    set.seed(seed)
    parkinsons <- read.table('parkinsons.data', sep = ',', header = TRUE)
    train <- sample(c(1:nrow(parkinsons)), 120)
    trainingSet <- parkinsons[train,]
    testSet <- parkinsons[-train,]
    if (multipleP) {
        trainingSet <- parkinsons[train, c(p1, p2, 18)]
        testSet <- parkinsons[-train, c(p1, p2, 18)]
    }
    else {
        trainingSet <- parkinsons[train, c(p1, 18)]
        testSet <- parkinsons[-train, c(p1, 18)]
    }
    resultLDA <- LDA(testSet, trainingSet)
    resultQDA <- QDA(testSet, trainingSet)
    return(data.frame(LDAErr = ErrorRate(resultLDA, testSet$status), QDAErr = ErrorRate(resultQDA, testSet$status)))
}