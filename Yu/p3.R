source('yu_lib2.r')

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

#b <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
b <- c(0.00000000,  0.00000000,  0.17365863, -1.21662467,  0.00000000,  0.00000000, -0.02856933,  0.17573032,  0.00000000, 0.00000000, -0.02474287,0.13901608)
# system.time({b <- bee_Algorithm(600)})
# b_n <- rep(0, 12)
# b_n[c(3:4, 7:8, 11:12)] <- b
# b <- b_n
# #b <- stochastic_gradient_descent(600, 300)
# b_alt <- b[b != 0]
# W3_alt <- getW2Spe(b_alt)
# 
#b <- rep(1, 12)

W3 <- getW2(b)
fill3 <- (W3 * ind_mat[3:num_days,]) >= 0
RP3 <- rowSums(fill3 * W3 * roc[3:(num_days),]) / rowSums(abs(fill3 * W3))
RP3[is.nan(RP3)] = 0
output_mat <- output(data[,1], RP3, fill3 * W3, 3)
output_params <- output_params(b,3)

sharpe_train <- mean(RP3[1:600])/sd(RP3[1:600])
sharpe_val <- mean(RP3[600:1001])/sd(RP3[600:1001])
sharpe_total <- mean(RP3)/sd(RP3)

pdf_out(RP3)
