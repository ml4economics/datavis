library(forecast)
library(quantmod)
library(patchwork)
library(ggplot2)
library(qqplotr)

# Get daily stock data for Apple Inc. (AAPL)
getSymbols("AAPL", src = "yahoo", from = "2010-01-01", to = "2023-12-31")

# Extract the closing prices
aapl_data <- Cl(AAPL)

# Convert to xts object
aapl_ts <- xts(aapl_data, order.by = index(aapl_data))

# Plot the time series
plot(aapl_ts, main = "AAPL Daily Closing Prices", xlab = "Date", ylab = "Closing Price", col = "blue")

# Fit ARIMA model
arima_model <- auto.arima(aapl_ts)

summary(arima_model)

# Extract residuals from the fitted model
residuals <- residuals(arima_model)

residuals_df <- data.frame(Date = as.Date(index(aapl_ts)), 
                           Residuals = as.numeric(residuals))

# Create Time Series Plot of Residuals
ts_plot <- ggplot(residuals_df, aes(x = Date, y = Residuals)) +
  geom_line(color = "red") +
  ggtitle("Residuals Time Series Plot") +
  xlab("Date") +
  ylab("Residuals") +
  #scale_x_date(date_breaks = "5 years", date_labels = "%b %Y") +
  theme_minimal()

# Create ACF Plot of Residuals
acf_plot <- ggAcf(residuals, lag.max = 10) +
  ggtitle("ACF of Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(color = "blue"))

# Create Histogram of Residuals
hist_plot <- ggplot(residuals_df, aes(x = Residuals)) +
  geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 0.5, fill = "green", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, color = "blue") +
  ggtitle("Histogram of Residuals") +
  xlab("Residuals") +
  ylab("Frequency") +
  theme_minimal()

# Perform Ljung-Box Test and Print Summary
#ljung_box_test <- Box.test(residuals, lag = 20, type = "Ljung-Box")
#print(ljung_box_test)

# Create a Q-Q plot of the residuals
qqplot <- ggplot(residuals_df, aes(sample = Residuals)) +
  stat_qq_band(bandType = "ell", mapping = aes(fill = "red", alpha = 0.5), show.legend = FALSE) +
  stat_qq_point() +
  stat_qq_line() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  ggtitle("Q-Q Plot of Residuals") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()

# Display the Q-Q plot
# Display all plots
combined <- (ts_plot | acf_plot) / (hist_plot | qqplot)
print(combined)

