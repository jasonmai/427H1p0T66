indicatorIndices <- seq(from = 7, to = ncol(part1Data), by = 6)

INDResult <- part1Data[, indicatorIndices]


FILL <- function(weights) {
    indTemp <- INDResult[3: nrow(INDResult), ]
    
    res <- weights * indTemp
    
    res[res >= 0] <- 1
    res[res < 0] <- 0
    
    res
}

ALLRP3 <- function(weights) {
    fills <- FILL(weights)
    
    top <- fills * weights * ROCResult[3:nrow(ROCResult),] 
    bottom <- fills * weights
    bottomCopy <- bottom
    
    bottom[bottom == 0] <- NaN
    
    res <- rowSums(top) / rowSums(abs(bottomCopy))
    
    res[is.nan(res)] <- 0
    
    res
}

sharpeRatioP3Train <- function(b) {
    allRP3 <- ALLRP3(b)
    
    allRP3 <- allRP3[1:600]
    
    avgAllRP3 <- mean(allRP3)
    
    print(avgAllRP3)
    
    -(avgAllRP3 / sd(allRP3))
}
