library(ggplot2)
library(qqplotr)

# Read the CSV file
data <- read.csv("c:/users/karst/Desktop/psa.csv", header = TRUE)
data <- subset(data, Age >= 54 & Age <= 59 & PSA < 6)
#data$PSA <- log(data$PSA)
                      
# Generate the histogram
histogram_plot <- ggplot(data, aes(x = PSA)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Histogram of Values",
       x = "Value",
       y = "Frequency") +
  theme_minimal()

# Display the histogram
print(histogram_plot)

# Generate the Q-Q plot for the log-transformed data
qqplot <- ggplot(data, aes(sample = PSA)) +
  stat_qq_point(size = 2) +
  stat_qq_line(color = "red") +
  coord_cartesian(xlim = c(-3,3)) +
  labs(title = "Q-Q Plot of Log-Transformed Values",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Display the Q-Q plot
print(qqplot)
