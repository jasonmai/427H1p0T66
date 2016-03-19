source('yu_lib.r')

data <- read.table("../in_sample_data.txt", sep=",")
sc_indices <- 1 + 6 * 0:99 + 4
sc_mat <- data[,sc_indices]
N = 100
num_days <- nrow(sc_mat)
rcc <- sc_mat[2:num_days,] / sc_mat[1:(num_days - 1),] - 1
avrRCC <- rowMeans(rcc)
test <- matrix(avrRCC[1:(num_days - 2)], nrow = num_days - 2,ncol = 100)
W1 <- -1 / N * (rcc[1:(num_days - 2),] - test)
RP1 <- rowSums(W1 * rcc[2:(num_days-1),]) / rowSums(abs(W1))
output_mat <- output(data[,1], RP1, W1, 1)
