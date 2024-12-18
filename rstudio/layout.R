library(ggplot2)
library(patchwork)

# Example data
data <- data.frame(x = rnorm(100), y = rnorm(100))

# Create four example plots
plot1 <- ggplot(data, aes(x = x, y = y)) + geom_point() + ggtitle("Plot 1") + theme(aspect.ratio = 1)
plot2 <- ggplot(data, aes(x = x, y = y)) + geom_point(color = "blue") + ggtitle("Plot 2")  + theme(aspect.ratio = 1)
plot3 <- ggplot(data, aes(x = x, y = y)) + geom_point(color = "green") + ggtitle("Plot 3") + theme(aspect.ratio = 1)
plot4 <- ggplot(data, aes(x = x, y = y)) + geom_point(color = "red") + ggtitle("Plot 4") + theme(aspect.ratio = 1)

# Combine the plots with an empty plot in the lower left quadrant
empty_plot <- ggplot() + theme_void() + theme(aspect.ratio = 1)

combined_plot <- (plot1 | plot2) / (empty_plot | plot3) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(title = "Combined Plots Layout")

# Display the combined plot
print(combined_plot)

