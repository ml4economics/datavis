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