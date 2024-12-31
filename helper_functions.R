

rank_test <- function(means, vars, alpha=0.2, verbose=TRUE) {
  ranking <- rev(order(means))
  X <- means[ranking]
  S <- vars[ranking]
  if (verbose) {
    print(round(X, 3))
    print(round(sqrt(S), 3))
  }
  d <- length(X)
  num_verified <- 0
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
      p_val <- num/denom
    })
    if (max(p_vals) < alpha) {
      num_verified <- num_verified + 1
      if (verbose) {
        print(paste0("P-value: ", round(max(p_vals), 3)))
      }
    } else {
      if (verbose) {
        print(paste0("P-value: ", round(max(p_vals), 3)))
      }
      break
    }
  }
  if (num_verified==d-1) {
    num_verified <- num_verified+1
  }
  return(num_verified)
}


test_for_lowest <- function(means, vars, alpha=0.2, verbose=TRUE) {
  ranking <- rev(order(means))
  X <- means[ranking]
  S <- vars[ranking]
  if (verbose) {
    print(round(X, 3))
    print(round(sqrt(S), 3))
  }
  d <- length(X)
  num_verified <- 0
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
      p_val <- num/denom
    })
    if (max(p_vals) < alpha) {
      num_verified <- num_verified + 1
      if (verbose) {
        print(paste0("P-value: ", round(max(p_vals), 3)))
      }
    } else {
      if (verbose) {
        print(paste0("P-value: ", round(max(p_vals), 3)))
      }
      break
    }
  }
  if (num_verified==d-1) {
    num_verified <- num_verified+1
  }
  return(num_verified)
}




set_test <- function(means, vars, K, alpha=0.2, verbose=TRUE) {
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
      p_val <- num/denom
    })
    union_p_value <- max(p_vals)
    if (union_p_value > alpha) {
      reject <- FALSE
    }
    max_of_all <- max(max_of_all, union_p_value)
  }
  outcome <- ifelse(reject, "reject", "fail to reject")
  if (verbose) {# & max_of_all > 0
    print(round(max_of_all, 5))
  }
  return(outcome)
}
