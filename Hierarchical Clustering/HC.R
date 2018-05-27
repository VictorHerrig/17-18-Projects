HC <- function(data, method) {
    #In hindsight, this is inefficient
    #I could have calculated the distances at the beginning
    #and stored them in a matrix, then referenced it during the
    #recursive step rather than computing some sub-matrix every
    #time.
    if (length(data) <= 2) {
        data[[1]] <- rbind(data[[1]], data[[2]])
        data[[2]] <- NULL
        print("Pass done!")
        return(list(data))
    }
    differences <- sapply(1:(length(data) - 1), function(i)
                        sapply((i + 1):length(data), function(j)
                            method(data[[i]], data[[j]])
                        )
                    )
    diffMat <- matrix(sapply(1:(length(differences[[1]])), function(i)
                    c(rep(0, i), differences[[i]])), nrow = length(differences[[1]] + 1), byrow = T)
    diffMat[which(diffMat == 0)] <- .Machine$integer.max

    d <- which(min(diffMat) == diffMat, arr.ind = T)[1,]
    data[[d[1]]] <- rbind(data[[d[1]]], data[[d[2]]])
    data[[d[2]]] <- NULL
    return(c(list(data), HC(data, method)))
}

singular <- function(c1, c2) {
    return(min(apply(as.matrix(c1), 1, function(r) apply(as.matrix(c2), 1, function(s) norm(as.matrix(r - s))))))
}

average <- function(c1, c2) {
    return(mean(apply(as.matrix(c1), 1, function(r) apply(as.matrix(c2), 1, function(s) norm(as.matrix(r - s))))))
}

complete <- function(c1, c2) {
    return(max(apply(as.matrix(c1), 1, function(r) apply(as.matrix(c2), 1, function(s) norm(as.matrix(r - s))))))
}

centroid <- function(c1, c2) {
    return(norm(matrix(apply(as.matrix(c1), 2, function(r)
                           mean(as.matrix(r)))
                     - apply(as.matrix(c2), 2, function(r)
                           mean(as.matrix(r))), ncol = ncol(c1))))
}

AnalyseMicroArray <- function() {
    nci <- read.table('nci.data.txt')
    nci <- t(nci)
    #All pcs
    nci.pca <- prcomp(nci)
    #Only the top 2 pcs
    nci.pca.2 <- nci.pca$x[, 1:2]
    nci.pca.list <- split(nci.pca$x, seq(nrow(nci.pca$x)))
    nci.pca.list <- c(sapply(1:length(nci.pca.list), function(i) list(matrix(nci.pca.list[[i]], nrow = 1))))

    result <- list(singular = HC(nci.pca.list, singular),
                   complete = HC(nci.pca.list, complete),
                   average = HC(nci.pca.list, average),
                   centroid = HC(nci.pca.list, centroid))
    return(result)
}
