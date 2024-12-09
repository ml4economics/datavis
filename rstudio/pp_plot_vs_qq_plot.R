library(ggplot2)
library(patchwork)

mu <- 0  
sd_values <- c(0.5, 1, 2) 

# Dichtefunktionen
x <- seq(-6, 6, length.out = 100)
plot_data <- data.frame(
  x = rep(x, times = length(sd_values)),
  y = c(dnorm(x, mean = mu, sd = sd_values[1]),
        dnorm(x, mean = mu, sd = sd_values[2]),
        dnorm(x, mean = mu, sd = sd_values[3])),
  sd = factor(rep(sd_values, each = length(x)))
)
p1 <- ggplot(plot_data, aes(x = x, y = y, color = sd)) +
  geom_line(size = 1) +
  ggtitle("Normalverteilungen") +
  xlab("Wert") +
  ylab("Dichte") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +  
  scale_color_discrete(name = "Standardabweichung")

# PP Plot
x <- seq(-10, 10, length.out = 100)
ref = pnorm(x, mean = mu, sd = sd_values[2])
plot_data <- data.frame(
  x = rep(ref, times = length(sd_values)),
  y = c(pnorm(x, mean = mu, sd = sd_values[1]),
        ref,
        pnorm(x, mean = mu, sd = sd_values[3])),
  sd = factor(rep(sd_values, each = length(x)))
)
p2 <- ggplot(plot_data, aes(x = x, y = y, color = sd)) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  ggtitle("PP Plot") +
  xlab("Wert 1") +
  ylab("Wert 2") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_color_discrete(name = "Standardabweichung")

# QQ Plot
x <- seq(0, 1, length.out = 100)
ref = qnorm(x, mean = mu, sd = sd_values[2])
plot_data <- data.frame(
  x = rep(ref, times = length(sd_values)),
  y = c(qnorm(x, mean = mu, sd = sd_values[1]),
        ref,
        qnorm(x, mean = mu, sd = sd_values[3])),
  sd = factor(rep(sd_values, each = length(x)))
)

p3 <- ggplot(plot_data, aes(x = x, y = y, color = sd)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  ggtitle("QQ Plot") +
  xlab("Standardnormalverteilung") +
  ylab("Skalierte Normalverteilung") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_color_discrete(name = "Standardabweichung")

p1 / (p2 + p3)
