# Load necessary library
library(ggplot2)

# Create a sequence of x values
x <- seq(-4, 4, by = 0.01)*3

# Define the means and standard deviations
means <- c(-2, -1, 0, 1, 2)
sds <- c(rep(.1, 4), 5)
# means <- c(-1.5, -1.0, -0.5, 0, 0.5)*2
# sds <- c(0.03, 0.03, 0.03, 0.03, 2)*2  # Extremely small SDs for the first two, larger for the third

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

# Data for the red dot
red_dot <- data.frame(x = -3, y = dnorm(-3, mean = 2, sd = 5))

# Plot the data with x-ticks at mean values
p <- ggplot(df, aes(x = x, y = y, color = group)) +
  geom_line(size = 1.0) +
  labs(title = "Comparison of Gaussian Distributions, Unequal Variances",
       x = "Value",
       y = "Density") +
  scale_color_manual(values = c("blue", "green", "violet", "brown", "red")) +
  scale_x_continuous(breaks = means) +  # Set x-ticks at the mean values
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  theme_minimal() +
  theme(legend.position="none") + 
  geom_point(data = red_dot, aes(x = x, y = y*trans), color = "red", size = 4)
plot(p)
# Save the plot
ggsave(file.path(here::here(), "Desktop", "Gaussian Rankings", "gaussian_unequal_vars.pdf"),
       plot = p, device = "pdf", width = 10, height = 3)
