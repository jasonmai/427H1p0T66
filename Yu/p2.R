source('yu_lib.r')
library('numDeriv')

data <- read.table("../in_sample_data.txt", sep=",")
so_indices <- 1 + 6 * 0:99 + 1
sh_indices <- 1 + 6 * 0:99 + 2
sl_indices <- 1 + 6 * 0:99 + 3
sc_indices <- 1 + 6 * 0:99 + 4
tvl_indices <- 1 + 6 * 0:99 + 5
ind_indices <- 1 + 6 * 0:99 + 6


so_mat <- data.matrix(data[,so_indices])
sh_mat <- data.matrix(data[,sh_indices])
sl_mat <- data.matrix(data[,sl_indices])
sc_mat <- data.matrix(data[,sc_indices])
tvl_mat <- data.matrix(data[,tvl_indices])
ind_mat <- data.matrix(data[,ind_indices])

N = 100
num_days <- nrow(sc_mat)

rco <- so_mat[2:num_days,] / sc_mat[1:(num_days - 1),] - 1
roc <- sc_mat[1:num_days,] / so_mat[1:num_days,] - 1
roo <- so_mat[2:num_days,] / so_mat[1:(num_days - 1),] - 1
rcc <- sc_mat[2:num_days,] / sc_mat[1:(num_days - 1),] - 1
rvp <- (1 / (4 * log(2))) * (log(sh_mat) - log(sl_mat))^2

avrRCO <- rowMeans(rco)
avrROC <- rowMeans(roc)
avrROO <- rowMeans(roo)
avrRCC <- rowMeans(rcc)

avrTVL <- matrix(0, nrow = num_days - 1, ncol = N)
avrRVP <- matrix(0, nrow = num_days - 1, ncol = N)

for (t in 1: (num_days - 1)){
  if (t > 200){
    avrTVL[t,] <- colMeans(tvl_mat[(t - 199):t,])
    avrRVP[t,] <- colMeans(rvp[(t - 199):t,])
  }
  else{
    if (t == 1){
      avrTVL[1,] <- tvl_mat[1,]
      avrRVP[1,] <- rvp[1,]
    }
    else{
      avrTVL[t,] <- colMeans(tvl_mat[1:t,])
      avrRVP[t,] <- colMeans(rvp[1:t,])
    }
  }
}

  test <- matrix(avrRCC[1:(num_days - 2)], nrow = num_days - 2,ncol = 100)
  Q1 <- (rcc[1:(num_days - 2),] - test) / N
  
  test <- matrix(avrROO[2:(num_days - 1)], nrow = num_days - 2,ncol = 100)
  Q2 <- (roo[2:(num_days - 1),] - test) / N
  
  test <- matrix(avrROC[2:(num_days - 1)], nrow = num_days - 2,ncol = 100)
  Q3 <- (roc[2:(num_days - 1),] - test) / N
  
  test <- matrix(avrRCO[2:(num_days - 1)], nrow = num_days - 2,ncol = 100)
  Q4 <- (rco[2:(num_days - 1),] - test) / N
  
  Q5 <- Q1 * tvl_mat[2:(num_days - 1),] / avrTVL[2:(num_days - 1),]
  Q6 <- Q2 * tvl_mat[2:(num_days - 1),] / avrTVL[2:(num_days - 1),]
  Q7 <- Q3 * tvl_mat[2:(num_days - 1),] / avrTVL[2:(num_days - 1),]
  Q8 <- Q4 * tvl_mat[2:(num_days - 1),] / avrTVL[2:(num_days - 1),]
  
  Q9 <- Q1 * rvp[2:(num_days - 1),] / avrRVP[2:(num_days - 1),]
  Q10 <- Q2 * rvp[2:(num_days - 1),] / avrRVP[2:(num_days - 1),]
  Q11 <- Q3 * rvp[2:(num_days - 1),] / avrRVP[2:(num_days - 1),]
  Q12 <- Q4 * rvp[2:(num_days - 1),] / avrRVP[2:(num_days - 1),]

#a <- stochastic_gradient_descent(1000, 300)
a <- bee_Algorithm(1000)
#a <- rep(1, 12)
#a <- c(2.0072875, -9.0448937, 9.0826591, -9.2819942, 0.5534035, -2.4395467,  1.2962942, -1.2100140, -2.9944981,  0.9093179,  1.3772564,  0.1244828)
a <- c(-0.03951564, -0.4744389, 0.8760662, -1.874715, 0.166849, -0.3826503, 0.2115474, 0.177571, -0.2179634, -0.03938351, 0.1606311, 0.302414)
for (iter in 1:12){
  a <- rep(0, 12)
  a[2] <- 1
  a[11] <- 1

#a <- c(0.5891798, 1.019876, -0.2498859, -6.685727, 0.6505215, -0.6545521, 0.07183188, 1.107381, -0.7832779, 0.410139, -0.1511953, -0.2990031)
#a <- c(1.149473, 3.129871, 0.2192001, -18.15229, 1.76555, -1.626993, -0.1536617, 1.954675, -1.962249, 0.8841682, -0.5954123, 0.01034994)
#a <- c(0.09715403, 4.715921, -0.3932329, -23.25983, 1.684965, -1.729966, -0.3329639, 2.401324, -1.814161, 0.7962605, -0.3231082, 0.1524329)
#       0.5340869 -3.077797 5.43862 -7.664841 1.093445 -0.4191783 -0.6802841 0.7925298 -1.308893 -0.5640708 1.158036 0.9793952   
  W2 <- getW2(a)
  RP2 <- rowSums(W2 * roc[3:(num_days),]) / rowSums(abs(W2))
  output_mat <- output(data[,1], RP2, W2, paste('2_a',iter,'=1', sep = ''))
}
sharpe <- mean(RP2[600:1001])/sd(RP2[600:1001])
sharpe_total <- mean(RP2)/sd(RP2)
plot(RP2, pch = '.')

