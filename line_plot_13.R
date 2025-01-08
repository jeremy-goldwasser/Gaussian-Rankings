png(
  file.path(here::here(), "Desktop", "Gaussian Rankings", "Figures", "line_segments_13.png"), 
  width = 2000, height = 1500, res = 300#1200
)

#### 1. Define parameters
x2 <- 4  # Vertical line at X_1 = x2
S1 <- 6  # Coefficient for X_1
S3 <- 5  # Coefficient for X_3  (formerly 'X_2')
u  <- 1.3  # Constant in X_1/s1 + X_3/s2 = u

#### 2. Intersection of green line with x = x2
#    The green line is: X_1/s1 + X_3/s2 = u
#    Setting X_1 = x2  =>  x2/s1 + X_3/s2 = u  =>  X_3 = s2*(u - x2/s1)
y_intersect_green <- S3 * (u - x2 / S1)
y_intersect_green

#### 3. Set up the plot range
xlim <- c(0, 7)
ylim <- c(0, 6)

plot(
  1, type = "n", xlim = xlim, ylim = ylim,
  xlab = expression(X[1]),
  ylab = expression(X[3]),             # <-- relabeled so the y-axis is now X[3]
  main = expression(H[0]: ~ mu[1] < mu[3])
)

#### 4. Draw the diagonal line X_1 = X_3
#    (Previously X_1 = X_2; we now label it X_1 = X_3.)
#    We show it dashed up to x2, then solid after.
segments(0, 0, x2, x2, col = "blue", lty = 2)  # Dashed portion
segments(x2, x2, xlim[2], xlim[2], col = "blue", lty = 1)  # Solid portion

#### 5. Draw the vertical line at X_1 = x2
#    Solid below the diagonal, dashed above.
segments(x2, 0, x2, x2, col = "red", lty = 1)  # Solid portion
segments(x2, x2, x2, ylim[2], col = "red", lty = 2)  # Dashed portion

#### 6. Shade the area between the red and blue lines (optional)
#    For example, shading the region to the right of x2 and above X_1 = X_3
x_shade <- seq(x2, xlim[2], length.out = 100)
y_shade <- pmax(x_shade, x2)  # Just an example fill
polygon(
  c(x_shade, rev(x_shade)),
  c(rep(0, length(x_shade)), rev(y_shade)),
  col = rgb(0.9, 0.9, 0.9, 0.4),
  border = NA
)

#### 7. Draw the sloped (green) line: X_1/s1 + X_3/s2 = u
#    We start at x = x2 (where it intersects the vertical line)
#    and go to x = xlim[2].
curve(
  S3 * (u - x / S1),    # y = s2*(u - (x/s1))
  from = x2,
  to   = xlim[2],
  add  = TRUE,
  col  = "green", lty = 1
)

# Endpoint at x = xlim[2], for drawing an arrow
arrow_x_end <- xlim[2]
arrow_y_end <- S3 * (u - arrow_x_end / S1)

# Slope of the green line
slope <- -S3 / S1

# Direction vector (dx, dy) = (1, slope), scale for arrow length
dir_x <- 1
dir_y <- slope
mag <- sqrt(dir_x^2 + dir_y^2)
arrow_length <- 0.5
scale <- arrow_length / mag

# Draw the arrowhead
arrows(
  x0 = arrow_x_end - scale * dir_x,
  y0 = arrow_y_end - scale * dir_y,
  x1 = arrow_x_end,
  y1 = arrow_y_end,
  length = 0.1,
  angle  = 20,
  col    = "green"
)

#### 8. Add a point for x1 on the green line
x1 <- 5.5
y1 <- S3 * (u - x1 / S1)
points(x1, y1, col = "green", pch = 16)
# text(x1, 0, expression(X[1]), pos = 1, col = "green") # Doesn't do anything

#### 9. Label a couple of points (A, B) on the green line
x_A <- (x2+x1)/2
y_A <- S3 * (u - x_A / S1)
text(x_A, y_A + 0.3, "A", col = "black")

x_B <- (x1+xlim[2])/2
y_B <- S3 * (u - x_B / S1)
text(x_B, y_B + 0.3, "B", col = "black")

#### 10. Example of subscript notation and label near x=5
text(6.5, 5.5, expression(A[1]), col = "black", cex=1.5)
text(x1, y1 + 0.3, expression(x[1]), col = "black")

#### 11. Highlight key intersection points
#     Intersection of the vertical line x2 with the diagonal X_1 = X_3
points(x2, x2, col = "purple", pch = 16)

#     Intersection of the green line with x2
points(x2, y_intersect_green, col = "darkgreen", pch = 16)

#### 12. Legend (updated to reflect X[3] instead of X[2])
legend(
  "topleft",
  legend = c(
    expression(X[1] == X[3]),
    expression(X[1] == x[2]),
    bquote(frac(X[1], sigma[1]^2) + frac(X[3], sigma[2]^2) == u)#.(u)
  ),
  col = c("blue", "red", "green"),
  lty = c(1, 1, 1),
  bty = "n",
  cex = 0.9,
  y.intersp = 0.8
)

dev.off()