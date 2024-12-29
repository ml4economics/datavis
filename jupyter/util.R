library(ggplot2)
library(patchwork)
suppressMessages({
    library(qqplotr)
})

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

rstudio_theme <- function() {
  theme(plot.title = element_text(hjust = 0.5))
}

jupyter_theme <- function() {
  theme_bw() +
  theme(axis.title = element_text(size = 20, color = "black"),        
        axis.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 30, color = "black"),
        legend.text = element_text(size = 25, color = "black"),
        plot.title = element_text(size=30, color = "black", hjust = 0.5))
}