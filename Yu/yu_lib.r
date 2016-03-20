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

getSharpe_BA <- function(a, training_day_num){
  W2 <- getW2(a, training_day_num)
  RP2 <- rowSums(W2[1:(training_day_num - 2),] * roc[3:(training_day_num),]) / rowSums(abs(W2[1:(training_day_num - 2),]))
  return(mean(RP2)/sd(RP2))
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
  initial_population <- 500
  best_patch <- 1
  elite_patch <- 10
  nep <- 50
  nsp <- 20
  ngh <- 0.1
  
  max_iteration <- 2000
  error <- 0.00001
  bees <- matrix(0, nrow = initial_population, ncol = 13)
  bees[,2:13] <- matrix(runif(initial_population * 12, min = -1, max = 1), nrow = initial_population, ncol = 12)
  best_sharpe <- -100
  for (i in 1:max_iteration){
    if (i %% 2 == 0){
      cat('iteration ', i, '\n')
      cat(bees[1,2:13], '\n')
      cat('sharpe ratio: ', getSharpe_BA(bees[1,2:13], training_day_num), '\n')
    }
    for(b in 1:initial_population){
      bees[b,1] <- getSharpe_BA(bees[b,2:13], training_day_num)
    }
    bees <- bees[order(bees[,1], decreasing = TRUE),]
    if (bees[1,1] - best_sharpe < error){
      return (bees[1,2:13])
    }
    else{
      best_sharpe <- bees[1,1]
      new_bees <- matrix(0, nrow = initial_population, ncol = 13)
      best_bees <- bees[1:best_patch,2:13]
      dim(best_bees) <- c(best_patch, 12)
      elite_bees <- bees[(best_patch + 1):(best_patch + elite_patch), 2:13]
      for (b in 1:best_patch){
        new_patch <- matrix(0, nrow = nep, ncol = 12)
        new_patch[1,] <- best_bees[b,]
        for (j in 1:12){
          new_patch[2:nep,j] <- runif(nep - 1, min = best_bees[b,j]-ngh, max = best_bees[b,j] + ngh)
        }
        new_bees[((b-1) * nep + 1):(b*nep),2:13] <- new_patch
      }
      start_index <- best_patch * nep
      for (b in 1:elite_patch){
        new_patch <- matrix(0, nrow = nsp, ncol = 12)
        for (j in 1:12){
          new_patch[1:nsp,j] <- runif(nsp, min = elite_bees[b,j]-ngh, max = elite_bees[b,j] + ngh)
        }
        new_bees[(start_index + ((b-1)*nsp + 1)):(start_index + (b)*nsp),2:13] <- new_patch
      }
      start_index <- best_patch * nep + elite_patch * nsp
      new_bees[(start_index + 1):initial_population,2:13] <- matrix(runif((initial_population - start_index) * 12, min = -1, max = 1), 
                                                                nrow = initial_population - start_index, ncol = 12)
      bees <- new_bees
    }
  }
  return(bees[1,2:13])
}
