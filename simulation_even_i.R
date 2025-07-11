setwd("/Users/jeremygoldwasser/Desktop/Gaussian Rankings")
source(file.path(here::here(), "testing_functions.R"))
library(tidyverse)
library(patchwork)

d <- 5
means <- d:1
alpha <- 0.05


compute_error_rates <- function(K, modify_var_rank, SDs, replacement_SDs, n_iters=10000) {
  vars <- SDs**2
  replacement_vars <- replacement_SDs**2
  type_i_errors_ranks <- c()
  type_ii_errors_ranks <- c()
  type_i_errors_set <- c()
  type_ii_errors_set <- c()
  for (replacement_var in replacement_vars) {
    # print(replacement_var)
    vars[modify_var_rank] <- replacement_var
    vals_all <- mapply(rnorm, n = n_iters, mean = means, sd = sqrt(vars))
    
    n_correct_order <- 0
    n_fail_to_verify_ranks <- 0
    n_mistankely_verify_ranks <- 0
    
    n_correct_set <- 0
    n_fail_to_verify_set <- 0
    n_mistankely_verify_set <- 0
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
      } else {
        # Top K is NOT in order
        # Go into Type I error rate (test mistakenly rejects/ verifies top K)
        if (n_verified_ranks >= K) {
          n_mistankely_verify_ranks <- n_mistankely_verify_ranks +  1
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
      } else {
        # Top K set is NOT correct
        # Go into Type I error rate (test mistakenly rejects/ verifies top K)
        if (verify_set == "reject") {
          n_mistankely_verify_set <- n_mistankely_verify_set +  1
        }
      }
      
    }
    n_incorrect_order <- n_iters-n_correct_order
    type_i_error_ranks <- n_mistankely_verify_ranks/n_incorrect_order
    type_ii_error_ranks <- n_fail_to_verify_ranks/n_correct_order
    
    n_incorrect_set <- n_iters-n_correct_set
    # print(n_incorrect_set)
    type_i_error_set <- n_mistankely_verify_set/n_incorrect_set
    type_ii_error_set <- n_fail_to_verify_set/n_correct_set
    
    type_i_errors_ranks <- c(type_i_errors_ranks,type_i_error_ranks)
    type_ii_errors_ranks <- c(type_ii_errors_ranks,type_ii_error_ranks)
    type_i_errors_set <- c(type_i_errors_set,type_i_error_set)
    type_ii_errors_set <- c(type_ii_errors_set,type_ii_error_set)
    
  }
  error_rates <- list(
    type_i_errors_ranks,
    type_ii_errors_ranks,
    type_i_errors_set,
    type_ii_errors_set
  )
  names(error_rates) <- c("rank-I", "rank-II", "set-I", "set-II")
  return(error_rates)
}


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

#################### Simulate data ####################

# Focus on small values but let variance blow up
xresults12 <- compute_error_rates(1,2, SDs_a, xreplacement_SDs_a, n_iters=n_iters)
xresults14 <- compute_error_rates(1,4, SDs_a, xreplacement_SDs_a, n_iters=n_iters)
xresults32set <- compute_error_rates(3,2, SDs_a, xreplacement_SDs_a, n_iters=n_iters)
xresults34set <- compute_error_rates(3,4, SDs_a, xreplacement_SDs_a, n_iters=n_iters)
xresults32rank <- compute_error_rates(3,2, SDs_b, xreplacement_SDs_b, n_iters=n_iters)
xresults34rank <- compute_error_rates(3,4, SDs_b, xreplacement_SDs_b, n_iters=n_iters)



#################### Make plots for type I errors ####################


df_set_i <- tibble(
  prop = (0:max_exp),
  `1v2` = xresults12$`set-I`,
  `1v4` = xresults14$`set-I`,
  `3v2` = xresults32set$`set-I`,
  `3v4` = xresults34set$`set-I`
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
  ylim(0, 0.05) 

p_i_set

##########



df_rank_i <- tibble(
  prop = (0:max_exp),
  `1v2` = xresults12$`rank-I`,
  `1v4` = xresults14$`rank-I`,
  `3v2` = xresults32rank$`rank-I`,
  `3v4` = xresults34rank$`rank-I`
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
  ylim(0, 0.05) 

p_i_rank


############# 
set_legend_order <- function(p) {
  p + guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  )
}
p_i_rank <- set_legend_order(p_i_rank)
p_i_set  <- set_legend_order(p_i_set)
final_plot <- (
  (p_i_rank | p_i_set)
) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 14)
  )

final_plot


ggsave(
  filename = file.path(here::here(), "Figures", "error_rates_even_i.png"),
  plot = final_plot,
  width = 6.5,
  height = 4,
  dpi = 300,
  units = "in"
)
