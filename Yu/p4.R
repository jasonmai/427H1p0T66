source('yu_lib4.r')

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

trade_cap_mat <- tvl_mat * so_mat
trade_cap_mat <- trade_cap_mat / matrix(rowSums(trade_cap_mat), nrow = num_days, ncol = N)



roc <- sc_mat[1:num_days,] / so_mat[1:num_days,] - 1
rco <- so_mat[2:num_days,] / sc_mat[1:(num_days - 1),] - 1
rvp <- (1 / (4 * log(2))) * (log(sh_mat) - log(sl_mat))^2
mkt_roc <- rowSums(trade_cap_mat * roc)
mkt_rco <- rowSums(trade_cap_mat[1:(num_days - 1),] * rco)

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

test <- matrix(mkt_roc[1:(num_days - 1)], nrow = num_days - 1,ncol = 100)
Q41 <- (roc[1:(num_days - 1),] - test) / N

test <- matrix(mkt_rco[1:(num_days - 1)], nrow = num_days - 1,ncol = 100)
Q42 <- (rco[1:(num_days - 1),] - test) / N

Q43 <- trade_cap_mat[1:(num_days - 1),] * Q41
Q44 <- trade_cap_mat[1:(num_days - 1),] * Q42
Q45 <- Q41 * rvp[1:(num_days - 1),] / avrRVP[1:(num_days - 1),]
Q46 <- Q42 * rvp[1:(num_days - 1),] / avrRVP[1:(num_days - 1),]

training_day_num = 600
a <- bee_Algorithm(training_day_num)

W4 <- getW4(a)
fill4 <- (W4 * ind_mat[2:num_days,]) >= 0
RP4 <- rowSums(fill4 * W4 * roc[2:(num_days),]) / rowSums(abs(fill4 * W4))
RP4[is.nan(RP4)] = 0

sharpe_train <- mean(RP4[1:600])/sd(RP4[1:600])
sharpe_val <- mean(RP4[600:1001])/sd(RP4[600:1001])
sharpe_total <- mean(RP4)/sd(RP4)

output_mat <- output(data[,1], RP4, fill4 * W4, 4)
pdf_out(RP4)
