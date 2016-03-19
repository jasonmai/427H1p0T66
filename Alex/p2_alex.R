#Indices of various columns for all stocks
openPriceIndices <- seq(from = 2, to = ncol(part1Data), by = 6)
intradayHighIndices <- seq(from = 3, to = ncol(part1Data), by = 6)
intradayLowIndices <- seq(from = 4, to = ncol(part1Data), by = 6)
tradingVolumeIndices <- seq(from = 6, to = ncol(part1Data), by = 6)

#RCO Cache
RCOSub1 <- part1Data[1:(length(part1Data[,1])-1), closePriceIndices]
RCOSub2 <- part1Data[2:length(part1Data[,1]), openPriceIndices]

RCOResult <- (RCOSub2 / RCOSub1) - 1

# RCOMat <- as.matrix(RCOResult)
# RCOMat <- rbind(rep(99, N), RCOMat)
# RCOResult <- as.data.frame(RCOMat)

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

# ROOMat <- as.matrix(ROOResult)
# ROOMat <- rbind(rep(99, N), ROOMat)
# ROOResult <- as.data.frame(ROOMat)

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
Weights2 <- W2(rep(1, 12))
#Weights2 <- Weights2

#Q's
# Q1 <- (RCCResult - rep(AvrRCCResult, N)) / N
# Q1 <- Q1[1: (nrow(part1Data) - 2),]
# 
# Q2 <- (ROOResult - rep(AvrROOResult, N)) / N
# Q2 <- Q2[2: (nrow(part1Data) - 1),]
# 
# Q3 <- (ROCResult - rep(AvrROCResult, N)) / N
# Q3 <- Q3[1: (nrow(part1Data) - 2),]
# 
# Q4 <- (RCOResult - rep(AvrRCOResult, N)) / N
# Q4 <- Q4[2: (nrow(part1Data) - 1),]
# 
# Q5 <- (TVLResult[2: (nrow(part1Data) - 1),] / AvrTVLResult[2: (nrow(part1Data) - 1),]) * Q1
# Q6 <- (TVLResult[2: (nrow(part1Data) - 1),] / AvrTVLResult[2: (nrow(part1Data) - 1),]) * Q2
# Q7 <- (TVLResult[2: (nrow(part1Data) - 1),] / AvrTVLResult[2: (nrow(part1Data) - 1),]) * Q3
# Q8 <- (TVLResult[2: (nrow(part1Data) - 1),] / AvrTVLResult[2: (nrow(part1Data) - 1),]) * Q4
# 
# Q9 <- (RVPResult[2: (nrow(part1Data) - 1),] / AvrRVPResult[2: (nrow(part1Data) - 1),]) * Q1
# Q10 <- (RVPResult[2: (nrow(part1Data) - 1),] / AvrRVPResult[2: (nrow(part1Data) - 1),]) * Q2
# Q11 <- (RVPResult[2: (nrow(part1Data) - 1),] / AvrRVPResult[2: (nrow(part1Data) - 1),]) * Q3
# Q12 <- (RVPResult[2: (nrow(part1Data) - 1),] / AvrRVPResult[2: (nrow(part1Data) - 1),]) * Q4
# 

W2 <- function(a) {
#     if (t < 3) {
#         return(99)
#     }
#     
#     if (length(a) != 12) {
#         print("a needs to be a vector of length 12!")
#         return(99)
#     }
#     
#     sum(a * c(RCCResult[t - 1, j] - AvrRCCResult[t - 1], 
#           ROOResult[t, j] - AvrROOResult[t],
#           ROCResult[t - 1, j] - AvrROCResult[t - 1],
#           RCOResult[t, j] - AvrRCOResult[t],
#           (TVLResult[t - 1, j] / AvrTVLResult[t - 1]) * (RCCResult[t - 1, j] - AvrRCCResult[t - 1]),
#           (TVLResult[t - 1, j] / AvrTVLResult[t - 1]) * (ROOResult[t, j] - AvrROOResult[t]),
#           (TVLResult[t - 1, j] / AvrTVLResult[t - 1]) * (ROCResult[t - 1, j] - AvrROCResult[t - 1]),
#           (TVLResult[t - 1, j] / AvrTVLResult[t - 1]) * (RCOResult[t, j] - AvrRCOResult[t]),
#           (RVPResult[t - 1, j] / AvrRVPResult[t - 1, j]) * (RCCResult[t - 1, j] - AvrRCCResult[t - 1]),
#           (RVPResult[t - 1, j] / AvrRVPResult[t - 1, j]) * (ROOResult[t, j] - AvrROOResult[t]),
#           (RVPResult[t - 1, j] / AvrRVPResult[t - 1, j]) * (ROCResult[t - 1, j] - AvrROCResult[t - 1]),
#           (RVPResult[t - 1, j] / AvrRVPResult[t - 1, j]) * (RCOResult[t, j] - AvrRCOResult[t])
#           )
#     )
#     
    Q1 <- (RCCResult - rep(AvrRCCResult, N)) / N
    Q1 <- Q1[1: (nrow(part1Data) - 2),]
    
    Q2 <- (ROOResult - rep(AvrROOResult, N)) / N
    Q2 <- Q2[2: (nrow(part1Data) - 1),]
    
    Q3 <- (ROCResult - rep(AvrROCResult, N)) / N
    Q3 <- Q3[1: (nrow(part1Data) - 2),]
    
    Q4 <- (RCOResult - rep(AvrRCOResult, N)) / N
    Q4 <- Q4[2: (nrow(part1Data) - 1),]
    
    Q5 <- (TVLResult[2: (nrow(part1Data) - 1),] / AvrTVLResult[2: (nrow(part1Data) - 1),]) * Q1
    Q6 <- (TVLResult[2: (nrow(part1Data) - 1),] / AvrTVLResult[2: (nrow(part1Data) - 1),]) * Q2
    Q7 <- (TVLResult[2: (nrow(part1Data) - 1),] / AvrTVLResult[2: (nrow(part1Data) - 1),]) * Q3
    Q8 <- (TVLResult[2: (nrow(part1Data) - 1),] / AvrTVLResult[2: (nrow(part1Data) - 1),]) * Q4
    
    Q9 <- (RVPResult[2: (nrow(part1Data) - 1),] / AvrRVPResult[2: (nrow(part1Data) - 1),]) * Q1
    Q10 <- (RVPResult[2: (nrow(part1Data) - 1),] / AvrRVPResult[2: (nrow(part1Data) - 1),]) * Q2
    Q11 <- (RVPResult[2: (nrow(part1Data) - 1),] / AvrRVPResult[2: (nrow(part1Data) - 1),]) * Q3
    Q12 <- (RVPResult[2: (nrow(part1Data) - 1),] / AvrRVPResult[2: (nrow(part1Data) - 1),]) * Q4
    
    return(a[1]*Q1 + a[2]*Q2 + a[3]*Q3 + a[4]*Q4 + a[5]*Q5 + 
            a[6]*Q6 + a[7]*Q7 + a[8]*Q8 + a[9]*Q9 + a[10]*Q10 + a[11]*Q11 + a[12]*Q12)
}

RP2 <- function(t) {
    #ROC t+2 is to account for the fact 
    sum(Weights2[t, ] * ROCResult[t + 2, ]) / sum(abs(Weights2[t, ]))
}

AllRP2 <- function() {
    top <- as.matrix(Weights2 * ROCResult[3:nrow(ROCResult),])
    bottom <- as.matrix(abs(Weights2))
    
    sumTop <- matrix(rep(1, ncol(top)), nrow = ncol(top), ncol = 1)
    sumTop <- top %*% sumTop
    
    sumBottom <- matrix(rep(1, ncol(bottom)), nrow = ncol(bottom), ncol = 1)
    sumBottom <- bottom %*% sumBottom
    
    sumTop / sumBottom
}

computeAvrTVL <- function() {
    result <- matrix(nrow = nrow(part1Data), ncol = length(tradingVolumeIndices))
    
    
    for (t in 1:(nrow(part1Data) - 1)) {
        start <- max(1, t - 200)
        
        result[t, ] = colMeans(TVLResult[start: (t),])
    }
    
    result
}

computeAvrRVP <- function() {
    result <- matrix(nrow = nrow(part1Data), ncol = N)
    
    
    for (t in 1:(nrow(part1Data) - 1)) {
        start <- max(1, t - 200)
        
        result[t,] = colMeans(RVPResult[start: (t),])
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

sharpeRatio <- function(allRP2) {
#     allRP2 <- c()
#     
#     for (t in 1:nrow(part1Data)) {
#         allRP2 <- c(allRP2, RP2(t))
#     }
    
    avgAllRP2 <- mean(allRP2)
    
    avgAllRP2 / sd(allRP2)
}

printWeights <- function(weights) {
    write.table(weights, "weights.txt", sep=",")
}

