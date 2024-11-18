# Generate a sequence of values
x <- seq(-3, 3, length=100)

# Compute the CDF for a standard normal distribution
cdf <- pnorm(x)

# Plot the CDF
plot(x, cdf, type="l", col="blue", main="CDF of Standard Normal Distribution",
     xlab="Value", ylab="Cumulative Probability")

# Generate a sequence of probabilities
p <- seq(0, 1, length=100)

# Compute the quantiles for a standard normal distribution
quantiles <- qnorm(p)

# Plot the Quantile Function
plot(p, quantiles, type="l", col="red", main="Quantile Function of Standard Normal Distribution",
     xlab="Cumulative Probability", ylab="Value")
