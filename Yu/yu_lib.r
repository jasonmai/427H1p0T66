output <- function(dates, RP, W, Qnum){
  filename <- paste('question ', Qnum,".csv", sep = '')
  N <- ncol(W)
  stock_label <- 1:N
  header <- c('date', 'return', 'cumulative return', 'mean absolute weight', 'mean weight')
  stock_weight_header <- paste('weight of stock ', as.character(1:N))
  header <- c(header, stock_weight_header)
  n_rows <- length(dates)
  n_rows_weights <- nrow(W)
  diff <- n_rows - n_rows_weights
  
  output_mat <- matrix(99, nrow = n_rows, ncol = 5 + N)
  output_mat[,1] <- dates
  output_mat[(diff + 1) : n_rows,2] <- RP
  output_mat[(diff + 1) : n_rows,3] <- cumsum(log(1 + RP))
  output_mat[(diff + 1) : n_rows,4] <- rowSums(abs(W)) / N
  output_mat[(diff + 1) : n_rows,5] <- rowSums(W) / rowSums(abs(W))
  output_mat[(diff + 1) : n_rows,6:(5+N)] <- data.matrix(W)
  output_mat <- round(output_mat, digits = 7)
  write(header, file = filename, ncolumns = 5 + N, sep = ",")
  write(t(output_mat), file = filename, ncolumns = 5 + N, sep = ",", append = TRUE)
  return(output_mat)
}

getW2 <- function(a){
  
  if (length(a) != 12){
    cat('a does not have 12 elements! \n')
    return
  }
  a1 = a[1]
  a2 = a[2]
  a3 = a[3]
  a4 = a[4]
  a5 = a[5]
  a6 = a[6]
  a7 = a[7]
  a8 = a[8]
  a9 = a[9]
  a10 = a[10]
  a11 = a[11]
  a12 = a[12]
  
  test <- matrix(avrRCC[1:(num_days - 2)], nrow = num_days - 2,ncol = 100)
  Q1 <- (rcc[1:(num_days - 2),] - test) / N
  
  test <- matrix(avrROO[2:(num_days - 1)], nrow = num_days - 2,ncol = 100)
  Q2 <- (roo[2:(num_days - 1),] - test) / N
  
  test <- matrix(avrROC[1:(num_days - 2)], nrow = num_days - 2,ncol = 100)
  Q3 <- (roc[1:(num_days - 2),] - test) / N
  
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
  
  return(a1 * Q1 + a2 * Q2 + a3 * Q3 + a4 * Q4 + a5 * Q5 + a6 * Q6 + 
           a7 * Q7 + a8 * Q8 + a9 * Q9 + a10 * Q10 + a11 * Q11 + a12 * Q12)
  
}