#Function for computing the K Nearest Neighbours algorithm
#Retain Values dictates whether classified values will be added 
#to the training set for future classifications, default is FALSE
KNearestNeighbours <- function(newValues, currentValues, K, retainValues = FALSE) {
    currentValues[, 1] <- sapply(currentValues[, 1], as.character)

    #Function to calculate Euclidian length between two n-dimensional vectors
    calculateLength <- function(x1, x2) {
        return(sqrt(sum((x1 - x2) ^ 2)))
    }

    #Function to find the mode of a character vector
    #In the case of ties, returns the value that appears first
    #Combine with sorting for distance tie-breakers
    calculateMode <- function(x) {
        xset <- as.data.frame(unique(x))
        xset <- cbind(xset, matrix(0, nrow = nrow(xset)))
        xset[, 1] <- sapply(xset[, 1], as.character)

        for (value in x) {
            index <- which(xset[, 1] == value)
            xset[index, 2] <- xset[index, 2] + 1
        }
        return(xset[min(which(xset[, 2] == max(xset[, 2]))), 1])
    }

    #Function that classifies a point using the KNN method
    classify <- function(newValue) {
        threshold <- 1
        class <- "Unknown"
        distance <- vector(length = K) + .Machine$integer.max
        
        nearest <- data.frame(class, distance, row.names = c(1:K))
        nearest[, 1] <- sapply(nearest[, 1], as.character) #Ensures values in first column are strings
        
        #Function that finds distance and updates the nearest values if need be
        checkDistance <- function(currentValue) {
            len <- calculateLength(newValue, currentValue[2:ncol(currentValue)])

            if (len < nearest[threshold, 2]) {
                nearest[threshold, 2] <- len
                nearest[threshold, 1] <- as.character(currentValue[1])
            }
            return(nearest)
        }
        
        for (x in c(1:nrow(currentValues))) {
            nearest <- checkDistance(currentValues[x,])
            threshold <- min(which(nearest[, 2] == max(nearest[, 2])))
        }
        classification <- calculateMode(nearest[, 1])
        return(classification)
    }

    #Main
    classifications <- data.frame(class = character(), matrix(nrow = 0, ncol = ncol(currentValues)))
    for (x in c(1:nrow(newValues))) {
        if (!is.na(currentValues[x, 1])) {
            result <- data.frame(class = as.character(classify(newValues[x,])), newValues[x, 1:ncol(newValues)])
            names(result) <- names(currentValues)
            classifications <- rbind.data.frame(classifications, result)
            classifications[x, 1] <- as.character(classifications[x, 1])
            if (retainValues) {
                currentValues <- rbind.data.frame(currentValues, result)
                rownames(currentValues) <- NULL
            }
        }
    }
    return(classifications)
}

#Function that returns the total error rate for two sets; must be of same dimension
ErrorRate <- function(set1, set2) {
    return( 1 - length(which(as.vector(set1) == as.vector(set2))) / length(set1))
}

#Wrapper function for KNearestNeighbours; takes a vector of values for K to be tested
TestKNN <- function(testSet, trainingSet, K) {
    output <- data.frame(numK = numeric(), ErrRate = numeric(), Time = numeric())
    for (k in K) {
        iniTime <- Sys.time()
        result <- KNearestNeighbours(testSet[, 2:ncol(testSet)], trainingSet, k, retainValues = FALSE)
        endTime <- Sys.time()
        output <- rbind.data.frame(output, data.frame(numK = as.numeric(k), ErrRate = ErrorRate(testSet[, 1], result[, 1]), Time = endTime - iniTime))
    }
    return(output)
}

#*******************************************#
#***Functions for processing the datasets***#
#****************Run these!*****************#
#*******************************************#

#Function for getting Error and Time values vs K for the Iris set
IrisKNN <- function(seed, K){
    set.seed(seed)
    train <- sample(1:nrow(iris), 2 * nrow(iris) / 3)
    twoThirds <- 2 * nrow(iris) / 3
    trainingSet <- data.frame(Species = as.character(iris[train, 5]), iris[train, c(1:4)], row.names = c(1:twoThirds))
    testSet <- data.frame(Species = as.character(iris[-train, 5]), iris[-train, c(1:4)], row.names = c(1:(twoThirds / 2)))
    return(TestKNN(testSet, trainingSet, K))
}

#Function for getting Error and Time values vs K for the Ionosphere set
#Fair warning - takes a while   ~ 1.5 min
IonosphereKNN <- function(K) {
    ionosphere <- read.table('ionosphere.txt', sep = ',')
    trainingSet <- data.frame(good = ionosphere[1:200, ncol(ionosphere)],
                              ionosphere[1:200, 1:ncol(ionosphere) - 1])
    testSet <- data.frame(good = ionosphere[200:nrow(ionosphere), ncol(ionosphere)],
                          ionosphere[200:nrow(ionosphere), 1:ncol(ionosphere) - 1],
                          row.names = c(1:(nrow(ionosphere) - 199)))
    return(TestKNN(testSet, trainingSet, K))
}

#Same as above, but for Iris.txt, and with a fixed training/test set
#Use this for the assignment dataset!
IristxtKNN <- function(K) {
    iristxt <- read.table('iris.txt', sep = ',')
    for (x in 1:100) {
        if (iristxt[x,5] == 1)
            iristxt[x,5] <- as.character('Versicolour')
        else
            iristxt[x,5] <- as.character('Setosa')
    }
    trainingSet <- data.frame(Species = as.character(iristxt[c(1:70), 5]), iristxt[c(1:70), c(1:4)], row.names = c(1:70))
    testSet <- data.frame(Species = as.character(iristxt[c(71:100), 5]), iristxt[c(71:100), c(1:4)], row.names = c(1:30))
    return(TestKNN(testSet, trainingSet, K))
}