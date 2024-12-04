library(ggplot2)
set.seed(123)  # For reproducibility
sample_data <- rnorm(100)  # Generate 100 random normal data points
normal_quantiles <- qnorm(seq(0.05, 0.95, by = 0.05))

# Create a data frame
data <- data.frame(sample_data)

# Generate the Q-Q plot
qq_plot <- ggplot(data, aes(sample = sample_data)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  ggtitle("Q-Q Plot with Vertical Lines at Normal Quantiles") +
  theme_minimal()

# Add vertical lines at the normal distribution quantiles
for (q in normal_quantiles) {
  qq_plot <- qq_plot + geom_vline(xintercept = q, linetype = "dashed", color = "blue")
}

# Display the plot
print(qq_plot)

