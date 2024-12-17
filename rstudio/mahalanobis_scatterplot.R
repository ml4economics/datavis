library(ggplot2)
library(MASS)

# Mean vector
mean_vector <- c(0, 0)

# Covariance matrix with off-diagonal elements
cov_matrix <- matrix(c(1, 0.8, 0.8, 6), nrow = 2)
# Generate sample data
sample_data <- mvrnorm(n = 100, mu = mean_vector, Sigma = cov_matrix)
data <- as.data.frame(sample_data)
colnames(data) <- c("x", "y")

# Calculate the Mahalanobis distance
cov_matrix <- cov(data)
mean_vector <- colMeans(data)
mahalanobis_dist <- mahalanobis(data, center = mean_vector, cov = cov_matrix)

# Add Mahalanobis distance to the data frame
data$mahalanobis_dist <- mahalanobis_dist

# Generate ellipse coordinates
ellipse_coords <- ellipse(cov_matrix, centre = mean_vector, level = 0.9)
ellipse_df <- as.data.frame(ellipse_coords)

# Create a ggplot2 plot
p <- ggplot(data, aes(x = x, y = y, color = mahalanobis_dist)) +
  geom_point() +
  geom_path(data = ellipse_df, aes(x = x, y = y), color = "red") +
  scale_color_gradient(low = "blue", high = "red") +
  ggtitle("Scatter Plot with Mahalanobis Distance") +
  xlab("X-Axis") +
  ylab("Y-Axis") +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

# Display the plot
print(p)
