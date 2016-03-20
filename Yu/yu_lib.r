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

getW2 <- function(a, days = num_days){
  
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
  
  return(a1 * Q1 + a2 * Q2 + a3 * Q3 + a4 * Q4 + a5 * Q5 + a6 * Q6 + 
           a7 * Q7 + a8 * Q8 + a9 * Q9 + a10 * Q10 + a11 * Q11 + a12 * Q12)
  
}

getSharpe_train <- function(a, training_day_num, random_indices){
  weight_penalty <- 0.0001
  W2 <- getW2(a, training_day_num)
  RP2 <- rowSums(W2[1:(training_day_num - 2),] * roc[3:(training_day_num),]) / rowSums(abs(W2[1:(training_day_num - 2),]))
  return (mean(RP2[random_indices])/sd(RP2[random_indices]) - weight_penalty * abs(sum(a) - 12))
}

stochastic_gradient_descent <- function(training_day_num, batch_size){
  a <- rep(0, 12)
  a[5] <- 1
  initial_step_size <- 5
  inf_step_size = 0.1
  max_iter <- 100000
  threshold <- 0.00001
  momentum = rep(0, 12)
  momentum_factor = 0.9
  for (i in 1:max_iter){
    old_a <- a
    random_indices <- sample(1:(training_day_num - 2), batch_size)
    gradient <- grad(function(a){ return (getSharpe_train(a, training_day_num, random_indices))}, a)
    
    momentum <- momentum_factor * momentum + (inf_step_size + (initial_step_size - inf_step_size) * exp(-i / 200)) * gradient
    a <- a + momentum
    if (i %% 5 == 0){
      cat('iteration ', i, '\n')
      cat(a, '\n')
      cat('sharpe ratio: ', getSharpe_train(a, training_day_num, 1:(training_day_num - 2)), '\n')
    }
    if (sum(abs(gradient)) / (sum(abs(a))) < threshold){
      break
    }
  }
  return (a)
  
}

bee_Algorithm <- function(training_day_num){
  
}
