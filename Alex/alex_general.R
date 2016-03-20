AbsWeightTotal <- function(weights) {
    rowMeans(abs(weights))
}

AbsWeightWeightedTotal <- function(t, weights) function(weights) {
    rowSums(weights)/rowSums(abs(weights))
}

PrintOutput <- function(allRP, weights) {
    first5Cols <- matrix(nrow = length(part1Data[,1]), ncol = 5)
    cumRSaved <- GetCumR(allRP)
    allRP <- c(c(99, 99), allRP)
    

    for (t in 1:nrow(part1Data)) {
        first5Cols[t, 1] <- part1Data[t, 1]
        first5Cols[t, 2] <- allRP[t]
        first5Cols[t, 3] <- cumRSaved[t]
        print("cumr")
        first5Cols[t, 4] <- AbsWeight(t, weights)
        first5Cols[t, 5] <- AbsWeightWeighted(t, weights)
        print(t)
    }
    
    weights <- rbind(rep(99, ncol(weights)), weights)
    weights <- rbind(rep(99, ncol(weights)), weights)
    
    result <- cbind(first5Cols, weights)
    write.table(result, "result_part_2.txt", sep=",")
}

GetCumR <- function(allRP) {
    cumRet <- 1
    cumRSaved <- c(99, 99)
    
    for (i in 1:(nrow(part1Data) - 2)) {
        cumRet <- cumRet * (1 + allRP[i])
        cumRSaved <- c(cumRSaved, log(cumRet))
    }
    
    cumRSaved
}

sharpeRatio <- function(allRP2) {
    avgAllRP2 <- mean(allRP2)
    
    avgAllRP2 / sd(allRP2)
}

sharpeSA <- function(a) {
    allRP2 <- AllRP2(a)
    
    avgAllRP2 <- mean(allRP2)
    
    -(avgAllRP2 / sd(allRP2))
}

sharpeSATrain <- function(a) {
    allRP2 <- AllRP2(a)
    
    allRP2 <- allRP2[1:600]
    
    avgAllRP2 <- mean(allRP2)
    
    print(avgAllRP2)
    
    -(avgAllRP2 / sd(allRP2))
}

sharpeSATest <- function(a) {
    allRP2 <- AllRP2(a)
    
    allRP2 <- allRP2[601:length(allRP2)]
    
    avgAllRP2 <- mean(allRP2)
    
    print(avgAllRP2)
    
    -(avgAllRP2 / sd(allRP2))
}

ALLRPGeneral <- function(weights) {
    #taking ROC from the third day since our weights are only available from then on
    top <- as.matrix(weights * ROCResult[3:nrow(ROCResult),])
    bottom <- as.matrix(abs(weights))
    
    rowSums(top) / rowSums(bottom)
}

sharpeTrainGeneral <- function(a, funWeight, funAllRP) {
    weights <- funWeight(a)
    
    allRP <- funAllRP(weights)
    
    allRP <- allRP[1:600]
    
    avgAllRP <- mean(allRP)
    
    -(avgAllRP / sd(allRP))
}



sharpeTestGeneral <- function(a, funWeight, funAllRP) {
    weights <- funWeight(a)
    
    allRP <- funAllRP(weights)
    
    allRP <- allRP[600:length(allRP)]
    
    avgAllRP <- mean(allRP)
    
    -(avgAllRP / sd(allRP))
}