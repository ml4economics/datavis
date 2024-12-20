library(ggplot2)
library(patchwork)

x_low = -3
x_high = 3
df = 10

samples <- seq(x_low, x_high, length.out = 100)
ref_prob  <- pnorm(q = samples)
test_prob <- pt(q = samples, df = df)

ref_data  <- data.frame(x = samples,   y = ref_prob)
test_data <- data.frame(x = test_prob, y = samples)

ref_quantiles <- qnorm(ref_prob)
test_quantiles <- qt(ref_prob, df = df)
qq_data <- data.frame(x = ref_quantiles, y = test_quantiles)

create_plot <- function(data, xlab, ylab, color, title) {
  ggplot(data, aes(x = x, y = y)) + 
    geom_line(color = color) +
    xlab(xlab) + 
    ylab(ylab) + 
    ggtitle(title) + 
    theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5))
}

empty_plot <- function() {
  ggplot() + theme_void() + theme(aspect.ratio = 1)
}

# Create four example plots
plot1 <- create_plot(test_data, "p", "x", "black", "Test-Verteilung")
plot2 <- create_plot(qq_data, NULL, NULL, "blue", "QQ-Plot")
plot3 <- create_plot(ref_data, "x", "p", "green", "Referenzverteilung")

combined_plot <- (plot1 | plot2) / (empty_plot() | plot3) +
  plot_layout(heights = c(1, 1))

# Display the combined plot
print(combined_plot)

