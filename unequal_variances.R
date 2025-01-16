# Load necessary library
library(ggplot2)
library(pracma)
source(file.path(here::here(), "testing_functions.R"))

# Create a sequence of x values
x <- seq(-4, 4, by = 0.01)*3

# Define the means and standard deviations
means <- c(-2, -1, 0, 1, 2)
sds <- c(rep(.1, 4), 5)

# Create data frames for plotting
df1 <- data.frame(x = x, y = dnorm(x, mean = means[1], sd = sds[1]), group = "Gaussian 1")
df2 <- data.frame(x = x, y = dnorm(x, mean = means[2], sd = sds[2]), group = "Gaussian 2")
df3 <- data.frame(x = x, y = dnorm(x, mean = means[3], sd = sds[3]), group = "Gaussian 3")
df4 <- data.frame(x = x, y = dnorm(x, mean = means[4], sd = sds[4]), group = "Gaussian 4")
df5 <- data.frame(x = x, y = dnorm(x, mean = means[5], sd = sds[5]), group = "Gaussian 5")
trans <- 1/max(df5$y)*max(df4$y)/2
df5$y <- df5$y*trans

# Combine the data frames
df <- rbind(df1, df2, df3, df4, df5)

# "Observed" data
set.seed(123)
Xs <- sapply(1:5, function(i) {rnorm(1, means[i], sds[i])})
Xs[5] <- -3 # Red dot
p_vals <- verify_winner(Xs,sds**2, alpha=0.05, return_p_vals=TRUE)

dots_df <- data.frame(
  x = Xs,
  y = dnorm(Xs, mean = rep(means, each = 1), sd = rep(sds, each = 1)),
  group = factor(1:5)
)
dots_df[5, 2] <- dots_df[5, 2] * trans

# Ensure df$group has proper group labels
df$group <- rep(c("Gaussian 1", "Gaussian 2", "Gaussian 3", "Gaussian 4", "Gaussian 5"), 
                each = nrow(df) / 5)

# Ensure consistent group labels in dots_df
dots_df$group <- factor(paste0("Gaussian ", dots_df$group), levels = unique(df$group))

# Updated plot
p <- ggplot(df, aes(x = x, y = y, color = group)) +
  geom_line(size = 1.0) +
  labs(title = "Comparison of Gaussian Distributions, Unequal Variances",
       x = "Value",
       y = "Density") +
  scale_color_manual(values = c("blue", "green", "violet", "brown", "red")) + 
  scale_x_continuous(breaks = means) + 
  # geom_vline(xintercept = -0.5, linetype = "dashed") + 
  # geom_segment(aes(x = -0.5, xend = -0.5, y = 0, yend = 4), linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "none") + 
  annotate("text", x = -3.6, y = dots_df[5, 2] + 0.3, label = "p=0.42", color = "black", size = 4) +
  annotate("text", x = 0, y = 4.2, label = "p=2e-9", color = "black", size = 4) +
  geom_point(data = dots_df, aes(x = x, y = y, color = group), size = 4)

plot(p)

# Save the plot
ggsave(file.path(here::here(), "Figures", "gaussian_unequal_vars.pdf"),
       plot = p, device = "pdf", width = 10, height = 3)

#################### Bound Type I error rate ####################
# Type I error = winner is not best, yet test against runner-up rejects
# P(type I error) ≥ P(best is neither winner nor runner-up; test between them rejects)
#                 ≥ P(best is neither winner nor runner-up; 2nd beats 3rd; test between them rejects)
#                 = 34.4% by MC integration

mu1 <- means[5]; mu2 <- means[4]; mu3 <- means[3]
sd1 <- sds[5]; sd2 <- sds[4]; sd3 <- sds[3]

alpha <- 0.05
Z_crit <- qnorm(1-alpha)
sd23 <- sqrt(sd2**2+sd3**2)


f_2_minus_3 <- function(x1, x3, q) {
  # Density unscaled by selection event
  f23 <- dnorm(q, mean = mu2-mu3, sd = sd23)
  f1 <- dnorm(x1, mean = mu1, sd = sd1)
  f3 <- dnorm(x3, mean = mu3, sd = sd3)
  f23*f1*f3
}

# Set up grid points for z, y, and x
n_points_per_integral <- 100
x1_min <- mu1-4*sd1
x3_vals <- seq(mu3-4*sd3, mu3+4*sd3, length.out = n_points_per_integral)
q_to_reject <- Z_crit*sd23 # always positive, so integrating X2 > X3
q_vals <- seq(q_to_reject, (mu2-mu3)+4*sd23, length.out = n_points_per_integral)

# Integrate region where X1 loses and X2 significantly above X3
outer_integral <- sapply(x3_vals, function(x3) {
  middle_integral <- sapply(q_vals, function(q) {
    x1_vals <- seq(x1_min, x3, length.out = n_points_per_integral)
    trapz(x1_vals, f_2_minus_3(x1_vals, x3, q))
  })
  trapz(q_vals, middle_integral)
})

lower_bound_on_type_I_error <- trapz(x3_vals, outer_integral)
lower_bound_on_type_I_error