source('yu_lib.r')

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
a <- c(0,0,0,0,1,0,0,0,0,0,0,0)
#a <- rep(1, 12)
W2 <- getW2(a)
RP2 <- rowSums(W2 * roc[3:(num_days),]) / rowSums(abs(W2))
output_mat <- output(data[,1], RP2, W2, 2)

corr <- rep(0, N)
for (s in 1:N){
  t1 <- roc[2:(num_days - 1),s]
  t2 <- roc[3:(num_days),s]
  corr[s] <- cor(t1, t2)
}

