source("../jupyter/util.R")
library(patchwork)

create_plot <- function(data, xlab, ylab, color, title) {
  ggplot(data, 
         aes(x = x, y = y)) + 
         geom_line(color = color) +
         xlab(xlab) + 
         ylab(ylab) + 
         ggtitle(title) +
         rstudio_theme()
}

illustrate_qqplot <- function(x_low, x_high,
                              ref_distribution, ref_dfunc, ref_qfunc,
                              test_distribution, test_dfunc, test_qfunc) {
  samples <- seq(x_low, x_high, length.out = 100)
  ref_prob  <- ref_dfunc(q = samples)
  test_prob <- test_dfunc(q = samples)
  
  ref_data  <- data.frame(x = samples,   y = ref_prob)
  test_data <- data.frame(x = test_prob, y = samples)
  
  ref_quantiles <- ref_qfunc(ref_prob)
  test_quantiles <- test_qfunc(ref_prob)
  first_index <- which(test_quantiles > x_low)[1]
  below_x_high <- which(test_quantiles < x_high)
  last_index <- below_x_high[length(below_x_high)]
  qq_data <- data.frame(x = ref_quantiles[first_index:last_index], y = test_quantiles[first_index:last_index])
  
  test_plot <- create_plot(test_data, "p", "x", "blue", test_distribution)
  qq_plot <- create_plot(qq_data, NULL, NULL, "black", "QQ-Plot")
  qq_plot <- qq_plot + geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")
  ref_plot <- create_plot(ref_data, "x", "p", "green", ref_distribution)
  
  (test_plot | qq_plot) / (empty_plot() | ref_plot) +
    plot_layout(heights = c(1, 1))
}

illustrate_qqnorm <- function(test_distribution, test_dfunc, test_qfunc) {
  x_low = -3
  x_high = 3
  ref_distribution = "Standardnormalverteilung"
  ref_dfunc = pnorm
  ref_qfunc = qnorm
  illustrate_qqplot(x_low, x_high,
                    ref_distribution, ref_dfunc, ref_qfunc,
                    test_distribution, test_dfunc, test_qfunc)
}

qqnorm_t_distribution <- function(df) {
  test_distribution = sprintf("t-Verteilung df=%d",df)
  test_dfunc = partial_func(pt, df=df)
  test_qfunc = partial_func(qt, df=df)
  
  illustrate_qqnorm(test_distribution, test_dfunc, test_qfunc)
}

gg <- qqnorm_t_distribution(1)
print(gg)

qq_plot_t_distribution(1)
qq_plot_t_distribution(10)
