# Load necessary library
library(stats)

# Generate example data (change this with your dataset)
set.seed(123)
data <- rexp(100, rate = 1)  # Exponential distribution with rate = 1

# Compute theoretical quantiles for the Exponential distribution
n <- length(data)
theoretical_quantiles <- qexp(seq(0, 1, length.out = n))

# Create QQ plot
qqplot(theoretical_quantiles, data, main = "QQ Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
abline(0, 1, col = "red")  # Add a reference line
