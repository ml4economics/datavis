library(ggplot2)
library(patchwork)
suppressMessages({library(qqplotr)})

partial_func <- function(func, ...) {
  fixed_args <- list(...)
  function(...) {
    do.call(func, c(fixed_args, list(...)))
  }
}

simple_ggplot <- function(data, xlab, ylab, color, title) {
  ggplot(data, 
         aes(x = x, y = y)) + 
         geom_line(color = color) +
         xlab(xlab) + 
         ylab(ylab) + 
         ggtitle(title)
}

empty_plot <- function() {
  ggplot() + theme_void() + theme(aspect.ratio = 1)
}

theme_squared <- function() {
  theme(aspect.ratio = 1)
}

theme_centered_title <- function(size=30) {
  theme(plot.title = element_text(size=size, , color = "black", hjust = 0.5))
}

theme_text <- function(axis_title=25, legend_title=30) {
  theme(axis.title = element_text(size = axis_title, color = "black"),        
        axis.text = element_text(size = axis_title-5, color = "black"),
        legend.title = element_text(size = legend_title, color = "black"),
        legend.text = element_text(size = legend_title-5, color = "black"))
}

theme_jupyter <- function() {
  theme_bw() +
  theme_centered_title() +
  theme_text()
}

cdf_with_quantile_plot <- function(pfunc, qfunc, 
                                   x_low, x_high, 
                                   p, 
                                   distribution = "Verteilungsfunktion",
                                   cdf_color = "blue",
                                   quantile_color = "red",
                                   label_size = 4) {
  x <- seq(x_low, x_high, length.out = 100)
  cdf <- pfunc(x)
  quantile <- qfunc(p)
  df <- data.frame(x = x, prob = cdf)
  
  plot <- ggplot(df, aes(x = x, y = prob)) + geom_line(color = cdf_color, linewidth=1) + 
    geom_segment(x = quantile, y = 0, xend = quantile, yend = p, color = quantile_color, linewidth=1) + 
    geom_segment(x = x_low, y = p, xend = quantile, yend = p, color = quantile_color, linewidth=1) +
    annotate("text", 
             x = quantile, y = -0.05, 
             label = sprintf("Q(%.1f)=%.2f", p, quantile), 
             color = quantile_color, vjust = 1, size = label_size) + 
    annotate("text", 
             x = x_low, y = p, 
             label = sprintf("%.1f", p), 
             color = quantile_color, hjust = 1, size = label_size) +
    xlab("x") + ylab("CDF")
  
  if ( ! is.null(distribution) ) {
    plot <- plot + labs(title = sprintf("%.1f-Quantil einer %s", p, distribution))
  }
  plot
}