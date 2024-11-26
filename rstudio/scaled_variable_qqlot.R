# Generate a sample of normally distributed data
set.seed(123)  # For reproducibility
data <- rnorm(100)  # Generate 100 random samples from a standard normal distribution

data <- 2*data + 5  # Transform the data to have mean 5 and standard deviation 2
# Create a Q-Q plot
qqnorm(data, main = "Q-Q Plot of Scaled Normally Distributed Data")
qqline(data, col = "red")  # Add a reference line
