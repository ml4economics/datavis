library(fpp3)
library(forecast)
library(patchwork)
library(ggplot2)
library(qqplotr)

us_change %>% autoplot(Unemployment)
data <- us_change
fit_us <- data |> model(ARIMA(Unemployment))
report(fit_us)

residuals <- residuals(fit_us)

ts_plot <- ggplot(residuals, aes(x = Quarter, y = .resid)) +
       geom_line(color = "red") +
       ggtitle("Residuals Time Series Plot") +
       xlab("Date") +
       ylab("Residuals") +
       theme_minimal()

acf_plot <- ggAcf(residuals$.resid, lag.max = 10) +
       ggtitle("ACF of Residuals") +
       theme_minimal() +
       theme(plot.title = element_text(color = "blue"))

hist_plot <- ggplot(residuals, aes(x = .resid)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Residuals") +
  xlab("Residuals") +
  ylab("Frequency") +
  theme_minimal()

qqplot <- ggplot(residuals, aes(sample = .resid)) +
      stat_qq_band(bandType = "ell", mapping = aes(fill = "red", alpha = 0.5), show.legend = FALSE) +
       stat_qq_point() +
       stat_qq_line() +
       geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
       ggtitle("Q-Q Plot of Residuals") +
       xlab("Theoretical Quantiles") +
       ylab("Sample Quantiles") +
       theme_minimal()

combined <- (ts_plot | acf_plot) / (hist_plot | qqplot)
print(combined)
