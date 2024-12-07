x <- seq(-10, 10, length.out = 100)
mu <- 0  
sd_values <- c(0.5, 1, 2) 

# Create a data frame with densities
plot_data <- data.frame(
  x = rep(x, times = length(sd_values)),
  y = c(dnorm(x, mean = mu, sd = sd_values[1]),
        dnorm(x, mean = mu, sd = sd_values[2]),
        dnorm(x, mean = mu, sd = sd_values[3])),
  sd = factor(rep(sd_values, each = length(x)))
)
ggplot(plot_data, aes(x = x, y = y, color = sd)) +
  geom_line(size = 1) +
  ggtitle("Normalverteilung mit verschiedenen Standardabweichungen") +
  xlab("Wert") +
  ylab("Dichte") +
  theme_minimal() +
  scale_color_discrete(name = "Standardabweichung")

plot_data <- data.frame(
  x = rep(x, times = length(sd_values)),
  y = c(dnorm(x, mean = mu, sd = sd_values[1]),
        dnorm(x, mean = mu, sd = sd_values[2]),
        dnorm(x, mean = mu, sd = sd_values[3])),
  sd = factor(rep(sd_values, each = length(x)))
)
ggplot(plot_data, aes(x = x, y = y, color = sd)) +
  geom_line(size = 1) +
  ggtitle("Normal Distributions with Different Standard Deviations") +
  xlab("Value") +
  ylab("Density") +
  theme_minimal() +
  scale_color_discrete(name = "Standard Deviation")


x <- seq(0, 1, length.out = 100)
ref = qnorm(x, mean = mu, sd = sd_values[2])
plot_data <- data.frame(
  x = rep(ref, times = length(sd_values)),
  y = c(qnorm(x, mean = mu, sd = sd_values[1]),
        ref,
        qnorm(x, mean = mu, sd = sd_values[3])),
  sd = factor(rep(sd_values, each = length(x)))
)

ggplot(plot_data, aes(x = x, y = y, color = sd)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  ggtitle("Empirical CDF vs. Theoretical CDF") +
  xlab("Theoretical CDF") +
  ylab("Empirical CDF") +
  theme_minimal()
