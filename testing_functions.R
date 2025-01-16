rank_test <- function(means, vars, alpha=0.2, verbose=FALSE, return_p_vals=TRUE) {
  ranking <- rev(order(means))
  X <- means[ranking]
  S <- vars[ranking]
  if (verbose) {
    print(round(X, 3))
    print(round(sqrt(S), 3))
  }
  d <- length(X)
  num_verified <- 0
  all_p_vals <- c()
  while(num_verified < d-1) {
    x1 <- X[num_verified+1]; s1 <- S[num_verified+1]; 
    p_vals <- sapply((num_verified+2):d, function(j) {
      xj <- X[j]; sj <- S[j]
      s1j <- s1**2/(s1+sj)
      mu1j <- (x1*sj + xj*s1)/(s1+sj)
      num <- 1-pnorm(x1, mean=mu1j, sd=sqrt(s1j))
      highest_unseen <- ifelse(j==num_verified+2 & num_verified+2<d, X[num_verified+3], X[num_verified+2])
      starting_point <- max(highest_unseen, mu1j)
      denom <- 1-pnorm(starting_point, mean=mu1j, sd=sqrt(s1j))
      p_val <- ifelse(denom==0, 0, num/denom)
    })
    cur_p_val <- max(p_vals)
    all_p_vals <- c(all_p_vals, cur_p_val)
    if (verbose) {
      print(paste0("P-value: ", round(cur_p_val, 3)))
    }
    if (cur_p_val < alpha) {
      num_verified <- num_verified + 1
    } else {
      break
    }
  }
  if (num_verified==d-1) {
    num_verified <- num_verified+1
  }
  if (return_p_vals) {
    # results <- as.character(c(num_verified, round(max(p_vals),4)))
    # names(results) <- c("K", "max p-val")
    results <- list(num_verified, round(all_p_vals, 4))
    names(results) <- c("K", "p-vals")
    return(results)
  }
  return(num_verified)
}


test_for_lowest <- function(means, vars, alpha=0.2, verbose=FALSE, return_p_val=TRUE) {
  ranking <- rev(order(means))
  X <- means[ranking]
  S <- vars[ranking]
  if (verbose) {
    print(round(X, 3))
    print(round(sqrt(S), 3))
  }
  d <- length(X)
  num_verified <- 0
  all_p_vals <- c()
  while(num_verified < d-1) {
    x1 <- X[d-num_verified]; s1 <- S[d-num_verified]; 
    p_vals <- sapply(1:(d-num_verified-1), function(j) {
      xj <- X[j]; sj <- S[j]
      s1j <- s1**2/(s1+sj)
      mu1j <- (x1*sj + xj*s1)/(s1+sj)
      num <- pnorm(x1, mean=mu1j, sd=sqrt(s1j))
      
      # Lowest unseen is index before runner-up if comparing against runner-up; otherwise it's the runner-up
      lowest_unseen <- ifelse(j==d-num_verified-1 & j>1, X[j-1], X[d-num_verified-1])
      starting_point <- min(lowest_unseen, mu1j)
      denom <- pnorm(starting_point, mean=mu1j, sd=sqrt(s1j))
      p_val <- ifelse(denom==0, 0, num/denom)
    })
    cur_p_val <- max(p_vals)
    all_p_vals <- c(all_p_vals, cur_p_val)
    if (cur_p_val < alpha) {
      num_verified <- num_verified + 1
    } else {
      if (verbose) {
        print(paste0("P-value: ", round(cur_p_val, 3)))
      }
      break
    }
  }
  if (num_verified==d-1) {
    num_verified <- num_verified+1
  }
  if (return_p_val) {
    results <- list(num_verified, round(all_p_vals, 4))
    names(results) <- c("K", "p-vals")
    return(results)
  }
  return(num_verified)
}




set_test <- function(means, vars, K, alpha=0.2, verbose=FALSE, return_p_val=TRUE) {
  ranking <- rev(order(means))
  X <- means[ranking]
  S <- vars[ranking]
  if (verbose) {
    print(round(X, 3))
    print(round(sqrt(S), 3))
  }
  
  d <- length(X)
  num_verified <- 0
  max_of_all <- -1
  reject <- TRUE
  for (k in 1:K) {
    idx_to_test <- K-k+1 # Start with hardest
    x1 <- X[idx_to_test]; s1 <- S[idx_to_test]; 
    p_vals <- sapply((K+1):d, function(j) {
      xj <- X[j]; sj <- S[j]
      s1j <- s1**2/(s1+sj)
      mu1j <- (x1*sj + xj*s1)/(s1+sj)
      num <- 1-pnorm(x1, mean=mu1j, sd=sqrt(s1j))
      highest_unseen <- ifelse(j==(K+1) & j<d, X[K+2], X[K+1])
      starting_point <- max(highest_unseen, mu1j)
      denom <- 1-pnorm(starting_point, mean=mu1j, sd=sqrt(s1j))
      p_val <- ifelse(denom==0, 0, num/denom)
    })
    union_p_value <- max(p_vals)
    if (union_p_value > alpha) {
      reject <- FALSE
    }
    max_of_all <- max(max_of_all, union_p_value)
  }
  outcome <- ifelse(reject, "reject", "fail to reject")
  if (verbose) {
    print(round(max_of_all, 5))
  }
  if (return_p_val) {
    results <- as.character(c(outcome, round(max_of_all, 4)))
    names(results) <- c("K", "max p-val")
    return(results)
  }
  return(outcome)
}

verify_winner <- function(means, vars, alpha=0.2, verbose=FALSE, return_p_vals=FALSE) {
  ranking <- rev(order(means))
  X <- means[ranking]
  S <- vars[ranking]
  if (verbose) {
    print(round(X, 3))
    print(round(sqrt(S), 3))
  }
  d <- length(X)

  x1 <- X[1]; s1 <- S[1]; 
  p_vals <- sapply(2:d, function(j) {
    xj <- X[j]; sj <- S[j]
    s1j <- s1**2/(s1+sj)
    mu1j <- (x1*sj + xj*s1)/(s1+sj)
    num <- 1-pnorm(x1, mean=mu1j, sd=sqrt(s1j))
    highest_unseen <- ifelse(j==2 & d>2, X[3], X[2])
    starting_point <- max(highest_unseen, mu1j)
    denom <- 1-pnorm(starting_point, mean=mu1j, sd=sqrt(s1j))
    p_val <- ifelse(denom==0, 0, num/denom)
  })
  max_p_val <- max(p_vals)
  if (verbose) {
    print(paste0("P-value: ", round(max_p_val, 3)))
  }
  if (return_p_vals) {
    return(p_vals)
  }
  outcome <- ifelse(max_p_val < alpha, "reject", "fail to reject")
  return(outcome)
}
