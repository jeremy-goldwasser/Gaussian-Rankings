setwd("/Users/jeremygoldwasser/Desktop/Gaussian Rankings")
source(file.path(here::here(), "testing_functions.R"))
library(tidyverse)
library(patchwork)

d <- 5
means <- d:1
alpha <- 0.05


compute_type_i_error <- function(K, modify_var_rank, SDs, replacement_SDs, n_iters=10000) {
  vars <- SDs**2
  means <- append(4:1, 4-K+1, K)
  replacement_vars <- replacement_SDs**2
  type_i_errors_ranks <- c()
  type_i_errors_set <- c()
  for (replacement_var in replacement_vars) {
    vars[modify_var_rank] <- replacement_var
    vals_all <- mapply(rnorm, n = n_iters, mean = means, sd = sqrt(vars))
    
    n_mistankely_verify_ranks <- 0
    n_mistankely_verify_set <- 0
    for (i in 1:n_iters) {
      vals <- vals_all[i,]
      top_K_ranks <- order(vals, decreasing=TRUE)[1:K]
      top_K_set <- sort(top_K_ranks)
      
      n_verified_ranks <- rank_test(vals,vars, alpha=alpha)$K
      verify_set <- set_test(vals,vars, K=K, alpha=alpha, return_p_val=FALSE)
      
      if (n_verified_ranks >= K) {
        # Verifies too few. Not an error per se, but counts for our experiments
        n_mistankely_verify_ranks <- n_mistankely_verify_ranks +  1
      }
      
      if (verify_set == "reject") {
        n_mistankely_verify_set <- n_mistankely_verify_set +  1
      }
    }

    type_i_error_ranks <- n_mistankely_verify_ranks/n_iters
    type_i_error_set <- n_mistankely_verify_set/n_iters

    type_i_errors_ranks <- c(type_i_errors_ranks,type_i_error_ranks)
    type_i_errors_set <- c(type_i_errors_set,type_i_error_set)

  }
  error_rates <- list(
    type_i_errors_ranks,
    type_i_errors_set
  )
  names(error_rates) <- c("rank-I", "set-I")
  return(error_rates)
}



compute_type_ii_error <- function(K, modify_var_rank, SDs, replacement_SDs, n_iters=10000) {
  vars <- SDs**2
  replacement_vars <- replacement_SDs**2
  type_ii_errors_ranks <- c()
  type_ii_errors_set <- c()
  for (replacement_var in replacement_vars) {
    vars[modify_var_rank] <- replacement_var
    vals_all <- mapply(rnorm, n = n_iters, mean = means, sd = sqrt(vars))
    
    n_correct_order <- 0
    n_fail_to_verify_ranks <- 0

    n_correct_set <- 0
    n_fail_to_verify_set <- 0
    for (i in 1:n_iters) {
      vals <- vals_all[i,]
      top_K_ranks <- order(vals, decreasing=TRUE)[1:K]
      top_K_set <- sort(top_K_ranks)
      
      n_verified_ranks <- rank_test(vals,vars, alpha=alpha)$K
      verify_set <- set_test(vals,vars, K=K, alpha=alpha, return_p_val=FALSE)
      
      # RANK
      if (identical(top_K_ranks, 1:K)) {
        # Top K is in order; hope rank test verifies
        # Go into type II error rate (test should reject but doesn't)
        n_correct_order <- n_correct_order+1
        if (n_verified_ranks < K) {
          # Verifies too few. Not an error per se, but counts for our experiments
          n_fail_to_verify_ranks <- n_fail_to_verify_ranks +  1
        }
      }
      
      # SET
      if (identical(top_K_set, 1:K)) {
        # Top K set is correct; hope set test verifies
        # Go into type II error rate (test should reject but doesn't)
        n_correct_set <- n_correct_set+1
        if (verify_set != "reject") {
          n_fail_to_verify_set <- n_fail_to_verify_set +  1
        }
      }
    }
    n_incorrect_order <- n_iters-n_correct_order
    type_ii_error_ranks <- n_fail_to_verify_ranks/n_correct_order
    
    n_incorrect_set <- n_iters-n_correct_set
    type_ii_error_set <- n_fail_to_verify_set/n_correct_set
    
    type_ii_errors_ranks <- c(type_ii_errors_ranks,type_ii_error_ranks)
    type_ii_errors_set <- c(type_ii_errors_set,type_ii_error_set)
    
  }
  error_rates <- list(
    type_ii_errors_ranks,
    type_ii_errors_set
  )
  names(error_rates) <- c("rank-II", "set-II")
  return(error_rates)
}

########## Choose SD s.t. top-K method has power around 0.9 ##########
# K=1 (rank & set same): 0.219
# K=3, set: 0.219
# K=3, rank: 0.187

# default_SD <- 0.218
# SDs <- rep(default_SD,d)
# compute_type_ii_error(1,2, SDs, default_SD, n_iters=10000)
# compute_type_ii_error(3,4, SDs, default_SD, n_iters=10000)
# 
# default_SD <- 0.187
# SDs <- rep(default_SD,d)
# compute_type_ii_error(3,2, SDs, default_SD, n_iters=10000)
# compute_type_ii_error(3,4, SDs, default_SD, n_iters=10000)

#################### Experimental parameters ####################

default_SD_a <- 0.219
default_SD_b <- 0.187
SDs_a <- rep(default_SD_a, d)
SDs_b <- rep(default_SD_b, d)
n_iters <- 10000

max_exp <- 6
xreplacement_SDs_a <- default_SD_a*(2**(0:max_exp))
xreplacement_SDs_b <- default_SD_b*(2**(0:max_exp))

num_vars <- 10
max_multiple <- 3
replacement_SDs_a <- seq(0,default_SD_a*max_multiple, length.out=num_vars)
replacement_SDs_b <- seq(0,default_SD_b*max_multiple, length.out=num_vars)

#################### Simulate data (only do 1x) ####################

########## Data for Type I Error ##########

# # Focus on small values but let variance blow up
# t1results12 <- compute_type_i_error(1,2, SDs_a, xreplacement_SDs_a, n_iters=n_iters)
# t1results14 <- compute_type_i_error(1,4, SDs_a, xreplacement_SDs_a, n_iters=n_iters)
# t1results32set <- compute_type_i_error(3,2, SDs_a, xreplacement_SDs_a, n_iters=n_iters)
# t1results34set <- compute_type_i_error(3,4, SDs_a, xreplacement_SDs_a, n_iters=n_iters)
# t1results32rank <- compute_type_i_error(3,2, SDs_b, xreplacement_SDs_b, n_iters=n_iters)
# t1results34rank <- compute_type_i_error(3,4, SDs_b, xreplacement_SDs_b, n_iters=n_iters)
# 
# ########## Data for Type II Error ##########
# 
# t2results12 <- compute_type_ii_error(1,2, SDs_a, replacement_SDs_a, n_iters=n_iters)
# t2results14 <- compute_type_ii_error(1,4, SDs_a, replacement_SDs_a, n_iters=n_iters)
# t2results32set <- compute_type_ii_error(3,2, SDs_a, replacement_SDs_a, n_iters=n_iters)
# t2results34set <- compute_type_ii_error(3,4, SDs_a, replacement_SDs_a, n_iters=n_iters)
# t2results32rank <- compute_type_ii_error(3,2, SDs_b, replacement_SDs_b, n_iters=n_iters)
# t2results34rank <- compute_type_ii_error(3,4, SDs_b, replacement_SDs_b, n_iters=n_iters)
# 
# 
# # Create a list of all result objects
# all_results <- list(
#   t1results12      = t1results12,
#   t1results14      = t1results14,
#   t1results32set   = t1results32set,
#   t1results34set   = t1results34set,
#   t1results32rank  = t1results32rank,
#   t1results34rank  = t1results34rank,
#   t2results12       = t2results12,
#   t2results14       = t2results14,
#   t2results32set    = t2results32set,
#   t2results34set    = t2results34set,
#   t2results32rank   = t2results32rank,
#   t2results34rank   = t2results34rank
# )
# # Save to an RData file
# save(all_results, file = file.path(here::here(), "simulated_error_rates.RData"))

#################### Load results ####################

load(file.path(here::here(), "simulated_error_rates.RData"))

# Unpack the list into individual variables
list2env(all_results, envir = .GlobalEnv)


#################### Make plots for type I errors ####################


df_set_i <- tibble(
  prop = (0:max_exp),
  `1v2` = t1results12$`set-I`,
  `1v4` = t1results14$`set-I`,
  `3v2` = t1results32set$`set-I`,
  `3v4` = t1results34set$`set-I`
)

df_long_set_i <- df_set_i %>%
  pivot_longer(cols = -prop, names_to = "comparison", values_to = "error_rate") %>%
  mutate(
    K = ifelse(grepl("^1", comparison), "1", "3"),
    j = ifelse(grepl("2$", comparison), "2", "4")
  )

p_i_set <- ggplot(df_long_set_i, aes(x = prop, y = error_rate, linetype = j, color = K)) +
  geom_line(size = 1) +
  labs(
    title = "Type I Errors (Set)",
    x = "Proportion of SD, Log 2",
    y = "Type I Error Rate",
    color = "K",
    linetype = "j" # "Var Index"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "black") +
  scale_y_continuous(
    limits = c(0, 0.055),
    breaks = seq(0, 0.06, 0.01),
    labels = function(x) format(x, nsmall = 2, trim = TRUE)
  )

p_i_set

##########



df_rank_i <- tibble(
  prop = (0:max_exp),
  `1v2` = t1results12$`rank-I`,
  `1v4` = t1results14$`rank-I`,
  `3v2` = t1results32rank$`rank-I`,
  `3v4` = t1results34rank$`rank-I`
)

df_long_rank_i <- df_rank_i %>%
  pivot_longer(cols = -prop, names_to = "comparison", values_to = "error_rate") %>%
  mutate(
    K = ifelse(grepl("^1", comparison), "1", "3"),
    j = ifelse(grepl("2$", comparison), "2", "4")
  )

p_i_rank <- ggplot(df_long_rank_i, aes(x = prop, y = error_rate, linetype = j, color = K)) +
  geom_line(size = 1) +
  labs(
    title = "Type I Errors (Rank)",
    x = "Proportion of SD, Log 2",
    y = "Type I Error Rate",
    color = "K",
    linetype = "j" # "Var Index"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "black") +
  # ylim(0, 0.06) 
  scale_y_continuous(
    limits = c(0, 0.055),
    breaks = seq(0, 0.06, 0.01),
    labels = function(x) format(x, nsmall = 2, trim = TRUE)
  )

p_i_rank


#################### Make plots for type II errors ####################
# Combine the data into a dataframe
df_set_ii <- tibble(
  prop = seq(0, max_multiple, length.out = num_vars),
  `1v2` = t2results12$`set-II`,
  `1v4` = t2results14$`set-II`,
  `3v2` = t2results32set$`set-II`,
  `3v4` = t2results34set$`set-II`
)

# Convert to long format and add K and j
df_long_set_ii <- df_set_ii %>%
  pivot_longer(cols = -prop, names_to = "comparison", values_to = "error_rate") %>%
  mutate(
    K = ifelse(grepl("^1", comparison), "1", "3"),
    j = ifelse(grepl("2$", comparison), "2", "4")
  )

# Create the plot
p_ii_set <- ggplot(df_long_set_ii, aes(x = prop, y = error_rate, linetype = j, color = K)) +
  geom_line(size = 1) +
  labs(
    title = "Type II Errors (Set)",
    x = "Proportion of SD",
    y = "Type II Error Rate",
    color = "K",
    linetype = "j" # "Var Index"
  ) +
  theme_minimal()

p_ii_set

###############################

df_rank_ii <- tibble(
  prop = seq(0, max_multiple, length.out = num_vars),
  `1v2` = t2results12$`rank-II`,
  `1v4` = t2results14$`rank-II`,
  `3v2` = t2results32rank$`rank-II`,
  `3v4` = t2results34rank$`rank-II`
)

df_long_rank_ii <- df_rank_ii %>%
  pivot_longer(cols = -prop, names_to = "comparison", values_to = "error_rate") %>%
  mutate(
    K = ifelse(grepl("^1", comparison), "1", "3"),
    j = ifelse(grepl("2$", comparison), "2", "4")
  )

p_ii_rank <- ggplot(df_long_rank_ii, aes(x = prop, y = error_rate, linetype = j, color = K)) +
  geom_line(size = 1) +
  labs(
    title = "Type II Errors (Rank)",
    x = "Proportion of SD",
    y = "Type II Error Rate",
    color = "K",
    linetype = "j" # "Var Index"
  ) +
  theme_minimal()



########## 4X4 plot grid for Type I vs II error, rank vs set ##########


set_legend_order <- function(p) {
  p + guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  )
}
p_i_rank <- set_legend_order(p_i_rank)
p_i_set  <- set_legend_order(p_i_set)
p_ii_rank <- set_legend_order(p_ii_rank)
p_ii_set  <- set_legend_order(p_ii_set)
final_plot <- (
  (p_i_rank | p_i_set | p_ii_rank | p_ii_set)
) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Error Rates Across Methods",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  ) & 
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 14)
  )

final_plot

ggsave(
  filename = file.path(here::here(), "Figures", "error_rates_grid.png"),
  plot = final_plot,
  width = 12,
  height = 4,
  dpi = 300,
  units = "in"
)
