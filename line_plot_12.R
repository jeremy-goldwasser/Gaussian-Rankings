png(file.path(here::here(), "Figures", "line_segments_12.png"), 
    width = 2000, height = 1500, res = 300)

# Define parameters
x3 <- 2      # Vertical line at X_3
S1 <- 6      # Coefficient for X_1 in the slope line equation
S2 <- 4      # Coefficient for X_2 in the slope line equation
u <- 5       # Constant for the slope line equation

# Calculate intersection points
x_intersect_green <- S1 * u / (S1 + S2)  # Intersection of green line with X_1 = X_2
x_intersect_x3 <- x3                     # Intersection of X_1 = X_2 with X_3

# Ensure the green line intersects after the vertical X_3 line
if (x_intersect_green <= x3) {
  stop("Adjust u or coefficients to ensure green line intersects to the right of x3.")
}

# Set up the plot range
xlim <- c(0, 7)
ylim <- c(0, 6)

# Create the plot
plot(
  1, type = "n", xlim = xlim, ylim = ylim,
  xlab = expression(X[1]), ylab = expression(X[2]),
  main = expression(H[0]: ~ mu[1] < mu[2])
)

# Draw X_1 = X_2 line
segments(0, 0, x_intersect_x3, x_intersect_x3, col = "blue", lty = 2)  # Dashed before X_3
segments(x_intersect_x3, x_intersect_x3, xlim[2], xlim[2], col = "blue", lty = 1)  # Solid after X_3

# Draw vertical line X_3
segments(x3, 0, x3, x_intersect_x3, col = "red", lty = 1)   # Solid before X_1 = X_2
segments(x3, x_intersect_x3, x3, ylim[2], col = "red", lty = 2)   # Dashed after X_1 = X_2

# Shade the area between the red and blue lines
x_shade <- seq(x3, xlim[2], length.out = 100)
y_shade <- pmax(x_shade, x3)
polygon(
  c(x_shade, rev(x_shade)),
  c(rep(0, length(x_shade)), rev(y_shade)),
  col = rgb(0.9, 0.9, 0.9, 0.4),
  border = NA
)

# Draw the sloped line: X_1/S1 + X_2/S2 = u
curve(
  u - (x / S1) * S2,
  from = x_intersect_green, to = xlim[2],
  add = TRUE, col = "green", lty = 1
)

# Endpoint where x = xlim[2]
arrow_x_end <- xlim[2]
arrow_y_end <- u - (arrow_x_end / S1) * S2

# Slope of green line
slope <- -S2 / S1

# Direction vector (dx, dy) = (1, slope)
# Scale it so the arrow segment is a manageable length (e.g., 0.5 units)
dir_x <- 1
dir_y <- slope
mag <- sqrt(dir_x^2 + dir_y^2)
arrow_length <- 0.5
scale <- arrow_length / mag

# Draw the arrow in the direction of the line
arrows(
  x0 = arrow_x_end - scale * dir_x,
  y0 = arrow_y_end - scale * dir_y,
  x1 = arrow_x_end,
  y1 = arrow_y_end,
  length = 0.1,    # size of the arrowhead
  angle = 20,      # angle of the arrowhead
  col = "green"
)

# Add a point at observed x1 on the green line
x1 <- 5.5
y_value <- u - (x1 / S1) * S2
points(x1, y_value, col = "green", pch = 16)
text(x1, 0, expression(X[1]), pos = 1, col = "green")

# Add "A" above the green line at x = 4
x_A <- (x_intersect_green+x1)/2
y_A <- u - (x_A / S1) * S2
text(x_A, y_A + 0.3, "A", col = "black")

# Add "B" above the green line at x = 6
x_B <- (x1 + xlim[2])/2
y_B <- u - (x_B / S1) * S2
text(x_B, y_B + 0.3, "B", col = "black")

# Change "A1" to subscript notation A[1]
text(6.5, 5.5, expression(A[1]), col = "black", cex=1.5)

# Another label at x1, above the green line
y1 <- u - (x1 / S1) * S2
text(x1, y1 + 0.3, expression(x[1]), col = "black")

# Highlight intersection points
points(x3, x3, col = "purple", pch = 16)               # Intersection X_3 with X_1=X_2
points(x_intersect_green, x_intersect_green, 
       col = "darkgreen", pch = 16)                    # Intersection X_1=X_2 with the green line

# Legend with less top space and updated sigma notation
legend(
  "topleft",
  legend = c(
    expression(X[1] == X[2]),
    expression(X[1] == x[3]),
    expression(frac(X[1], sigma[1]^2) + frac(X[2], sigma[2]^2) == u)
  ),
  col = c("blue", "red", "green"),
  lty = c(1, 1, 1),
  bty = "n",            # No box around the legend
  cex = 0.9,            # Slightly smaller text
  y.intersp = 0.8       # Reduce vertical spacing
)

dev.off()