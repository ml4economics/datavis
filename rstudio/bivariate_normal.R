library(mvtnorm)
library(ggplot2)
library(plotly)

# Define the mean vector and covariance matrix
mu <- c(0, 0)
sigma <- matrix(c(1, 0, 0, 1), nrow = 2)

# Create a grid of values
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
grid <- expand.grid(x = x, y = y)

# Compute the bivariate normal density
grid$z <- dmvnorm(grid, mean = mu, sigma = sigma)

# Convert grid to a matrix for the z-values
z_matrix <- matrix(grid$z, nrow = length(x), ncol = length(y))

# Create the 3D plot
plot_ly(x = ~x, y = ~y, z = ~z_matrix) %>%
  add_surface() %>%
  layout(
    title = "Bivariate Normal Distribution",
    scene = list(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "Density")
    )
  )
