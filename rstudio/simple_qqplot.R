library(ggplot2)
library(patchwork)

partial_func <- function(func, ...) {
  fixed_args <- list(...)
  function(...) {
    do.call(func, c(fixed_args, list(...)))
  }
}

empty_plot <- function() {
  ggplot() + theme_void() + theme(aspect.ratio = 1)
}

create_plot <- function(data, xlab, ylab, color, title) {
  ggplot(data, aes(x = x, y = y)) + 
    geom_line(color = color) +
    xlab(xlab) + 
    ylab(ylab) + 
    ggtitle(title) + 
    theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5))
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
  qq_data <- data.frame(x = ref_quantiles, y = test_quantiles)
  
  # Create four example plots
  plot1 <- create_plot(test_data, "p", "x", "blue", test_distribution)
  plot2 <- create_plot(qq_data, NULL, NULL, "red", "QQ-Plot")
  plot3 <- create_plot(ref_data, "x", "p", "green", ref_distribution)
  
  (plot1 | plot2) / (empty_plot() | plot3) +
    plot_layout(heights = c(1, 1))
}

qq_plot_t_distribution <- function(df) {
  x_low = -3
  x_high = 3
  ref_distribution = "Standardnormalverteilung"
  ref_dfunc = pnorm
  ref_qfunc = qnorm
  test_distribution = sprintf("t-Verteilung df=%d",df)
  test_dfunc = partial_func(pt, df=df)
  test_qfunc = partial_func(qt, df=df)
  
  gg <- illustrate_qqplot(x_low, x_high,
                          ref_distribution, ref_dfunc, ref_qfunc,
                          test_distribution, test_dfunc, test_qfunc)
  print(gg)
}

qq_plot_t_distribution(1)
qq_plot_t_distribution(10)
