library(ggplot2)
library(patchwork)

single_plot <- function(data, color, title, aspect_ratio=1) {
  gg <- ggplot(data, aes(x = x, y = y)) +
    geom_line(color = color) +
    labs(x = NULL, y = NULL) +   
    ggtitle(title) +
    theme_bw() +    
    theme(plot.title = element_text(size=20, color = "black", hjust = 0.5), 
          axis.text = element_text(size = 12, color = "black"),
          aspect.ratio=aspect_ratio)
}

combined_plot <- function(x, dfunc, pfunc, qfunc, distribution_name) {
  pdf_data  <- data.frame(x = x, y = dfunc(x))
  pdf_graph <- single_plot(pdf_data, "blue", "Dichte")
  
  cdf_data  <- data.frame(x = x, y = pfunc(x))
  cdf_graph <- single_plot(cdf_data, "green", "Verteilung")
  
  p <- seq(0, 1, length=100)
  quantiles  <- data.frame(x = p, y = qfunc(p))
  quantile_graph <- single_plot(quantiles, "red", "Quantilplot")
  
  (pdf_graph | cdf_graph | quantile_graph) +
    plot_annotation( title = distribution_name, theme = theme(plot.title = element_text(size = 30, hjust = 0.5) ) )
}
x <- seq(-3, 3, length=100)
combined_plot(x, dnorm, pnorm, qnorm, "Normalverteilung")

