#Indices of various columns for all stocks
openPriceIndices <- seq(from = 2, to = ncol(part1Data), by = 6)
intradayHighIndices <- seq(from = 3, to = ncol(part1Data), by = 6)
intradayLowIndices <- seq(from = 4, to = ncol(part1Data), by = 6)
tradingVolumeIndices <- seq(from = 6, to = ncol(part1Data), by = 6)

#RCO Cache
RCOSub1 <- part1Data[1:(length(part1Data[,1])-1), closePriceIndices]
RCOSub2 <- part1Data[2:length(part1Data[,1]), openPriceIndices]

RCOResult <- (RCOSub2 / RCOSub1) - 1

RCOMat <- as.matrix(RCOResult)
RCOMat <- rbind(rep(99, N), RCOMat)
RCOResult <- as.data.frame(RCOMat)

AvrRCOResult <- rowMeans(RCOResult)

#ROC Cache
ROCSub1 <- part1Data[1:(length(part1Data[,1])), openPriceIndices]
ROCSub2 <- part1Data[1:length(part1Data[,1]), closePriceIndices]

ROCResult <- (ROCSub2 / ROCSub1) - 1

AvrROCResult <- rowMeans(ROCResult)

#ROO Cache
ROOSub1 <- part1Data[1:(length(part1Data[,1])-1), openPriceIndices]
ROOSub2 <- part1Data[2:length(part1Data[,1]), openPriceIndices]

ROOResult <- (ROOSub2 / ROOSub1) - 1

ROOMat <- as.matrix(ROOResult)
ROOMat <- rbind(rep(99, N), ROOMat)
ROOResult <- as.data.frame(ROOMat)

AvrROOResult <- rowMeans(ROOResult)

#RVP Cache
RVPSubSH <- part1Data[, intradayHighIndices]
RVPSubSL <- part1Data[, intradayLowIndices]

RVPResult <- as.data.frame((1/(4*log(2)))*(((log(RVPSubSH) - log(RVPSubSL))))^2)
AvrRVPResult <- computeAvrRVP()

#TVL Cache
TVLResult <- part1Data[, tradingVolumeIndices]
AvrTVLResult <- computeAvrTVL()

#W2 Cache
Weights2 <- computeAllWeights(rep(1,12))

W2 <- function(a, t, j) {
    if (t < 3) {
        return(99)
    }
    
    if (length(a) != 12) {
        print("a needs to be a vector of length 12!")
        return(99)
    }
    
    sum(a * c(RCCResult[t - 1, j] - AvrRCCResult[t - 1], 
          ROOResult[t, j] - AvrROOResult[t],
          ROCResult[t - 1, j] - AvrROCResult[t - 1],
          RCOResult[t, j] - AvrRCOResult[t],
          (TVLResult[t - 1, j] / AvrTVLResult[t - 1]) * (RCCResult[t - 1, j] - AvrRCCResult[t - 1]),
          (TVLResult[t - 1, j] / AvrTVLResult[t - 1]) * (ROOResult[t, j] - AvrROOResult[t]),
          (TVLResult[t - 1, j] / AvrTVLResult[t - 1]) * (ROCResult[t - 1, j] - AvrROCResult[t - 1]),
          (TVLResult[t - 1, j] / AvrTVLResult[t - 1]) * (RCOResult[t, j] - AvrRCOResult[t]),
          (RVPResult[t - 1, j] / AvrRVPResult[t - 1, j]) * (RCCResult[t - 1, j] - AvrRCCResult[t - 1]),
          (RVPResult[t - 1, j] / AvrRVPResult[t - 1, j]) * (ROOResult[t, j] - AvrROOResult[t]),
          (RVPResult[t - 1, j] / AvrRVPResult[t - 1, j]) * (ROCResult[t - 1, j] - AvrROCResult[t - 1]),
          (RVPResult[t - 1, j] / AvrRVPResult[t - 1, j]) * (RCOResult[t, j] - AvrRCOResult[t])
          )
    )
    
}

RP2 <- function(t) {
    if (t < 3) {
        return(99)
    }
    
    sum(Weights2[t, ] * ROCResult[t, ]) / sum(abs(Weight2[t, ]))
}

computeAvrTVL <- function() {
    result <- matrix(nrow = nrow(part1Data), ncol = length(tradingVolumeIndices))
    
    
    for (t in 2:(nrow(part1Data) - 1)) {
        start <- max(1, t - 200)
        
        result[t, ] = colMeans(TVLResult[start: (t - 1),])
    }
    
    result
}

computeAvrRVP <- function() {
    result <- matrix(nrow = nrow(part1Data), ncol = N)
    
    
    for (t in 2:(nrow(part1Data) - 1)) {
        start <- max(1, t - 200)
        
        result[t,] = colMeans(RVPResult[start: (t - 1),])
    }
    
    result
}

computeAllWeights <- function(a) {
    emptyMatrix <- matrix(nrow = length(part1Data[,1]), ncol = N)
    
    for (t in 1: length(part1Data[,1])) {
        print(t)
        for (j in 1:N) {
            emptyMatrix[t, j] <- W2(a, t, j)
        }
    }
    
    emptyMatrix
}

sharpeRatio <- function() {
    allRP2 <- c()
    
    for (t in 1:nrow(part1Data)) {
        allRP2 <- c(allRP2, RP2(t))
    }
    
    avgAllRP2 <- mean(allRP2)
    
    avgAllRP2 / sd(allRP2)
}