library(ggplot2)
set.seed(123) 
sample_data <- rnorm(1000) 

ecdf_data <- ecdf(sample_data) 
empirical_cdf_values <- ecdf_data(sample_data)
theoretical_cdf_values <- pnorm(sample_data/2.0)

plot_data <- data.frame(
  theoretical_cdf = theoretical_cdf_values,
  empirical_cdf = empirical_cdf_values
)

ggplot(plot_data, aes(x = theoretical_cdf, y = empirical_cdf)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  ggtitle("Empirical CDF vs. Theoretical CDF") +
  xlab("Theoretical CDF") +
  ylab("Empirical CDF") +
  theme_minimal()

