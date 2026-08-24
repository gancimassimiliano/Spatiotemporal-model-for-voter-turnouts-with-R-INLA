# 1. Define the Gamma parameters (change these to automatically update the whole plot)
shape_param <- 50
rate_param <- 5

# 2. Calculate Laplace Approximation parameters mathematically
# Mode (x_0) = (shape - 1) / rate
mode_val <- (shape_param - 1) / rate_param         

# Variance (1/c) = (shape - 1) / rate^2
variance <- (shape_param - 1) / (rate_param^2)     
sd_val <- sqrt(variance)                           

# 3. Generate the sequence of x values (parameter space)
x_max <- qgamma(0.999, shape = shape_param, rate = rate_param)
x <- seq(0, x_max, length.out = 500)

# 4. Calculate the true and approximated densities
y_true <- dgamma(x, shape = shape_param, rate = rate_param)
y_laplace <- dnorm(x, mean = mode_val, sd = sd_val)

# 5. Create dynamic strings for the legend
legend_true <- sprintf("True Posterior: Gamma(%g, %g)", shape_param, rate_param)
legend_laplace <- sprintf("Laplace Approx: Normal(%g, %g)", mode_val, variance)

# 6. Build the classical base R plot
# Adjust margins to give space for axis labels (bottom, left, top, right)
par(mar = c(5, 5, 2, 2))

# Draw the True Posterior line and set up the plot window
plot(x, y_true, type = "l", col = "black", lty = 1, lwd = 1.8,
     xlab = "x", ylab = "Density",
     xlim = c(0, x_max), ylim = c(0, max(y_true, y_laplace) * 1.05),
     bty = "l",        # "l" forces a clean L-shaped axis without a top/right box
     cex.lab = 1.1,    # Slightly larger axis labels
     las = 1)          # Rotate y-axis numbers to be horizontal

# Overlay the Laplace Approximation
lines(x, y_laplace, col = "red", lty = 2, lwd = 1.8)

# Add the dotted vertical line for the mode
abline(v = mode_val, col = "black", lty = 3, lwd = 1.8)

# Dynamically add the mathematical annotation for the mode
# We place it slightly to the right of the mode line, near the bottom
text(x = mode_val + (x_max * 0.02), y = max(y_true) * 0.1, 
     labels = bquote(x[0] == .(round(mode_val, 2)) ~ "(Mode)"), 
     adj = 0, col = "black", cex = 1)

# Add the dynamic legend in the top right (perfect for right-skewed distributions)
legend("topleft", 
       legend = c(legend_true, legend_laplace),
       col = c("black", "red"), 
       lty = c(1, 1), 
       lwd = 1.5, 
       bty = "n",      # "n" removes the box around the legend
       cex = 0.9)     # Slightly scale down legend text

######################################################################################################à

# Load required library
library(ggplot2)

# 1. Define the true posterior mode and covariance for the bivariate distribution
mu <- c(0, 0) # Centered at 0 for abstract representation
Sigma <- matrix(c(1, 0.7, 0.7, 1), nrow = 2) # Covariance matrix with correlation

# Create a background grid for the density contour lines (the joint distribution)
x_val <- seq(-3, 3, length.out = 100)
y_val <- seq(-3, 3, length.out = 100)
grid_bg <- expand.grid(psi1 = x_val, psi2 = y_val)

# Function to calculate 2D Gaussian density
dmvnorm_2d <- function(x, y, mu, Sigma) {
  diff <- cbind(x - mu[1], y - mu[2])
  invSig <- solve(Sigma)
  quad_form <- rowSums((diff %*% invSig) * diff)
  exp(-0.5 * quad_form) / (2 * pi * sqrt(det(Sigma)))
}

grid_bg$density <- dmvnorm_2d(grid_bg$psi1, grid_bg$psi2, mu, Sigma)
mode_density <- dmvnorm_2d(mu[1], mu[2], mu, Sigma)

# 2. Replicate the INLA Grid Exploration Mechanism
eigen_decomp <- eigen(Sigma)
V <- eigen_decomp$vectors
Lambda_sqrt <- diag(sqrt(eigen_decomp$values))

# Generate points in the standard orthogonal space (z-space)
z_steps <- seq(-3, 3, by = 0.75)
z_grid <- expand.grid(z1 = z_steps, z2 = z_steps)

# Transform z-grid back to the hyperparameter space (psi)
psi_grid <- t(mu + V %*% Lambda_sqrt %*% t(as.matrix(z_grid)))
z_grid$psi1 <- psi_grid[, 1]
z_grid$psi2 <- psi_grid[, 2]

# Calculate log-density difference for the truncation rule
z_grid$density <- dmvnorm_2d(z_grid$psi1, z_grid$psi2, mu, Sigma)
z_grid$log_dens_diff <- log(mode_density) - log(z_grid$density)

# Keep ONLY the points that INLA would retain for integration (Delta <= 2.5)
integration_points <- subset(z_grid, log_dens_diff <= 2.5)

# 3. Build the Plot
ggplot() +
  # Bivariate joint distribution contours
  geom_contour(data = grid_bg, aes(x = psi1, y = psi2, z = density), color = "gray80", bins = 8, linewidth = 0.6) +
  
  # Integration points detected with the grid
  geom_point(data = integration_points, aes(x = psi1, y = psi2), color = "grey60", size = 2, shape = 16) +
  
  # The Global Mode
  geom_point(aes(x = mu[1], y = mu[2]), color = "black", size = 5, shape = 18) +
  annotate("text", x = mu[1] + 0.2, y = mu[2] - 0.2, label = expression(psi^"*"), color = "black", size = 6) +
  
  # Formatting
  labs(
    x = expression(psi[1]),
    y = expression(psi[2])
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )
