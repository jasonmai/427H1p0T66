output <- function(dates, RP, W, Qnum){
  filename <- paste('data_part ', Qnum,".team32.csv", sep = '')
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

output_params <- function(params, Qnum){
  filename <- paste('coeff_part ', Qnum,".team32.csv", sep = '')
  if (Qnum == 2){
    header <- paste('a', c(1:12), sep = '')
  }
  else{
    header <- paste('b', c(1:12), sep = '')
  }
  write(header, file = filename, ncolumns = 12, sep = ",")
  write(t(params), file = filename, ncolumns = 12, sep = ",", append = TRUE)
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

getW2Spe <- function(a, days = num_days){
  if (length(a) != 6){
    cat('a does not have 6 elements! \n')
    return
  }
  a3 = a[1]
  a4 = a[2]
  a7 = a[3]
  a8 = a[4]
  a11 = a[5]
  a12 = a[6]
  
  return(a3 * Q3 + a4 * Q4 + 
           a7 * Q7 + a8 * Q8 + a11 * Q11 + a12 * Q12)
}

getSharpe_train <- function(b, training_day_num, random_indices){
  weight_penalty <- 0.0001
  W3 <- getW2(b, training_day_num)
  fill3 <- (W3 * ind_mat[3:num_days,]) >= 0
  RP3 <- rowSums(fill3[3:(training_day_num),] * W3[1:(training_day_num - 2),] * roc[3:(training_day_num),]) / rowSums(abs(fill3[3:(training_day_num),] * W3[1:(training_day_num - 2),]))
  RP3[is.nan(RP3)] = 0
  #return (mean(RP3[random_indices])/sd(RP3[random_indices]) - weight_penalty * abs(sum(b) - 12))
  return (mean(RP3[random_indices])/sd(RP3[random_indices]))
}

getSharpe_BA <- function(b, training_day_num){
  W3 <- getW2Spe(b, training_day_num)
  fill3 <- (W3 * ind_mat[3:num_days,]) >= 0
  RP3 <- rowSums(fill3[1:(training_day_num - 2),] * W3[1:(training_day_num - 2),] * roc[3:(training_day_num),]) / rowSums(abs(fill3[1:(training_day_num - 2),] * W3[1:(training_day_num - 2),]))
  RP3[is.nan(RP3)] = 0
  return(mean(RP3)/sd(RP3))
}

stochastic_gradient_descent <- function(a, training_day_num, batch_size){
  initial_step_size <- 0.03
  inf_step_size = 0.03
  max_iter <- 1000
  threshold <- 0.00001
  momentum = rep(0, 12)
  momentum_factor = 0.7
  for (i in 1:max_iter){
    old_a <- a
    random_indices <- sample(1:(training_day_num - 2), batch_size)
    gradient <- grad(function(a){ return (getSharpe_train(a, training_day_num, random_indices))}, a)
    
    momentum <- momentum_factor * momentum + (inf_step_size + (initial_step_size - inf_step_size) * exp(-i / 10)) * gradient
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
  initial_population <- 1000
  best_patch <- 3
  elite_patch <- 5
  nep <- 100
  nsp <- 20
  ngh <- 0.1
  num_a <- 6
  max_iteration <- 1000
  min_iteration <- 30
  error <- 0.00001
  bees <- matrix(0, nrow = initial_population, ncol = num_a + 1)
  bees[,2:(num_a + 1)] <- matrix(runif(initial_population * num_a, min = -1, max = 1), nrow = initial_population, ncol = num_a)
  best_sharpe <- -100
  best_sharpe2 <- -100
  min_bound <- -1
  max_bound <- 1
  
  for (i in 1:max_iteration){
    min_bound <- min(c(min(bees[,2:(num_a + 1)]), min_bound))
    max_bound <- max(c(max(bees[,2:(num_a + 1)]), max_bound))
    if (i %% 2 == 0){
      cat('iteration ', i, '\n')
      cat(bees[1,2:(num_a + 1)], '\n')
      cat('sharpe ratio: ', getSharpe_BA(bees[1,2:(num_a + 1)], training_day_num), '\n')
    }
    for(b in 1:initial_population){
      bees[b,1] <- getSharpe_BA(bees[b,2:(num_a + 1)], training_day_num)
    }
    bees <- bees[order(bees[,1], decreasing = TRUE),]
    if (bees[1,1] - best_sharpe < error && best_sharpe - best_sharpe2 < error && i > min_iteration){
      return (bees[1,2:(num_a + 1)])
    }
    else{
      best_sharpe2 <- best_sharpe
      best_sharpe <- bees[1,1]
      new_bees <- matrix(0, nrow = initial_population, ncol = (num_a + 1))
      best_bees <- bees[1:best_patch,2:(num_a + 1)]
      dim(best_bees) <- c(best_patch, (num_a))
      elite_bees <- bees[(best_patch + 1):(best_patch + elite_patch), 2:(num_a + 1)]
      for (b in 1:best_patch){
        new_patch <- matrix(0, nrow = nep, ncol = (num_a))
        new_patch[1,] <- best_bees[b,]
        for (j in 1:(num_a)){
          new_patch[2:nep,j] <- runif(nep - 1, min = best_bees[b,j]-ngh, max = best_bees[b,j] + ngh)
        }
        new_bees[((b-1) * nep + 1):(b*nep),2:(num_a + 1)] <- new_patch
      }
      start_index <- best_patch * nep
      for (b in 1:elite_patch){
        new_patch <- matrix(0, nrow = nsp, ncol = (num_a))
        for (j in 1:(num_a)){
          new_patch[1:nsp,j] <- runif(nsp, min = elite_bees[b,j]-ngh, max = elite_bees[b,j] + ngh)
        }
        new_bees[(start_index + ((b-1)*nsp + 1)):(start_index + (b)*nsp),2:(num_a + 1)] <- new_patch
      }
      start_index <- best_patch * nep + elite_patch * nsp
      new_bees[(start_index + 1):initial_population,2:(num_a + 1)] <- matrix(runif((initial_population - start_index) * (num_a), min = min_bound, max = max_bound), 
                                                                             nrow = initial_population - start_index, ncol = (num_a))
      bees <- new_bees
    }
  }
  return(bees[1,2:(num_a + 1)])
}

pdf_out <- function(RP1){
  cum_log_return <- cumsum(log(1 + RP1))
  cat('average daily log return:', mean(log(1 + RP1)), '\n')
  cat('std of daily log return:', sd(log(1 + RP1)), '\n')
  cat('annualized SR: ', sqrt(252) * mean(RP1)/sd(RP1), '\n')
  cat('skewness:', skewness(RP1), '\n')
  cat('kurtosis:', kurtosis(RP1), '\n')
  cat('max draw down: ', exp(min(cum_log_return)), '\n')
  t <- which.min(cum_log_return)
  t0 <- which.max(cum_log_return[1:t])
  cat('length of max drawdown period:', t - t0, '\n')
  cat('cumulative return during draw down: ', exp(min(cum_log_return)) - exp(cum_log_return[t0]), '\n')
  W_eq <- matrix(1, nrow = num_days - 2, ncol = N)
  RP_eq <- rowSums(W_eq * rcc[2:(num_days-1),]) / rowSums(abs(W_eq))
  
  cat('correlation with equally weighted long portfolio:', cor(RP_eq, RP1), '\n')
}