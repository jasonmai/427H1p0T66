N <- 100

part1Data <- read.csv("sampleData.txt", header = FALSE)
indexOfFirstClosePrice <- 5
colsPerStock <- 6

closePriceIndices <- seq(from = 5, to = ncol(part1Data), by = 6)
openPriceIndices <- seq(from = 2, to = ncol(part1Data), by = 6)

RCCSub1 <- part1Data[1:(length(part1Data[,1])-1), closePriceIndices]
RCCSub2 <- part1Data[2:length(part1Data[,1]), closePriceIndices]
RCCResult <- (RCCSub2 / RCCSub1) - 1

RCCMat <- as.matrix(RCCResult)
RCCMat <- rbind(rep(99, N), RCCMat)
RCCResult <- as.data.frame(RCCMat)

AvrRCCResult <- rowMeans(RCCResult)

weights <- WeightMatrix()

W1 = function(t, j) {
    if (t < 3) {
        return(99)
    }
    
    -(1/N)*(RCCResult[t-1, j] - AvrRCCResult[t-1])
}

RCC <- function(t, j) {
    
    if (t < 2) {
        return(99)
    }
    
    RCCResult[t,j]
}

AvrRCC <- function(t) {
    AvrRCCResult[t]
}

RP1 <- function(t) {
    if (t < 3) {
        return(99)
    }
    
    sum(weights[t,] * RCCResult[t, ]) / sum(abs(weights[t, ]))
}

CumR <- function(t) {
    cumRSaved <- c(99,99)
    
    if (t < 3) {
        return(99)
    }
    
    cumRet <- 1
    
    for (i in 3:t) {
        cumRet <- cumRet * (1 + RP1(i))
        cumRSaved <- c(cumRSaved, log(cumRet))
    }
    
    cumRSaved
}

AbsWeight <- function(t) {
    if (t < 3) {
        return(99)
    }
    
    result <- sum(abs(weights[t,]))
    
    result
}

AbsWeightWeighted <- function(t) {
    if (t < 3) {
        return(99)
    }
    
    sum(weights[t,]) / sum(abs(weights[t,]))
}

WeightMatrix <- function() {
    emptyMatrix <- matrix(nrow = length(part1Data[,1]), ncol = N)
    
    for (t in 1: length(part1Data[,1])) {
        for (j in 1:N) {
            emptyMatrix[t, j] <- W1(t, j)
        }
    }
    
    emptyMatrix
}

generateReturns <- function() {
    first5Cols <- matrix(nrow = length(part1Data[,1]), ncol = 5)
    cumRSaved <- CumR(length(part1Data[,1]))
    
    for (t in 1:length(part1Data[,1])) {
        first5Cols[t, 1] <- part1Data[t, 1]
        first5Cols[t, 2] <- RP1(t)
        first5Cols[t, 3] <- cumRSaved[t]
        first5Cols[t, 4] <- AbsWeight(t)
        first5Cols[t, 5] <- AbsWeightWeighted(t)
    }
    
    result <- cbind(first5Cols, weights)
    write.table(result, "result.txt", sep=",")
}
