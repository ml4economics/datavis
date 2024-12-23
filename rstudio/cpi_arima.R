library(quantmod)
library(forecast)
library(patchwork)
library(ggplot2)
library(qqplotr)

# Retrieve CPI inflation data from FRED
cpi_data <- getSymbols("CPIAUCNS", src = "FRED", from='1971-01', to='2016-12', auto.assign = FALSE)

# Calculate the inflation rate
inflation_rate <- diff(log(cpi_data))[-1] * 400
quarterly_inflation_rate <- apply.quarterly(inflation_rate, mean)

# Fit an ARMA(1,1) model
arima_model <- Arima(quarterly_inflation_rate, method = "ML", order = c(1, 0, 1))
summary(arima_model)

# Extract residuals from the fitted model
residuals <- residuals(arima_model)

residuals_df <- data.frame(Date = as.Date(index(quarterly_inflation_rate)), 
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
  geom_histogram(binwidth = 0.5, fill = "green", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Residuals") +
  xlab("Residuals") +
  ylab("Frequency") +
  theme_minimal()

# Perform Ljung-Box Test and Print Summary
#ljung_box_test <- Box.test(residuals, lag = 20, type = "Ljung-Box")
#print(ljung_box_test)

# Create a Q-Q plot of the residuals
qqplot <- ggplot(residuals_df, aes(sample = Residuals)) +
  stat_qq_band(mapping = aes(fill = "red", alpha = 0.5), show.legend = FALSE) +
  stat_qq_point() +
  stat_qq_line() +
  ggtitle("Q-Q Plot of Residuals") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()

# Display the Q-Q plot
# Display all plots
combined <- (ts_plot | acf_plot) / (hist_plot | qqplot)
print(combined)


