library(qqplotr)

set.seed(0)
distribution <- function(n) {
  rchisq(df=10, n=n)
}
dname <- "chisq"
dparams <- list(df = 10)

#distribution <- rnorm
#dname <- "norm"
#dparams <- list()


smp <- data.frame(samples = distribution(100))

gg <- ggplot(data = smp, mapping = aes(sample = samples)) +
  stat_qq_band(distribution=dname, dparams = dparams, fill = "lightblue", alpha = 0.5) +
  stat_qq_line(distribution=dname, dparams = dparams) +
  stat_qq_point(distribution=dname, dparams = dparams) +
  ggtitle("QQ Plot with Chi-Squared Distribution (df = 10)") +
  labs(x = "Theoretische Quantile", y = "Stichprobenquantile") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )  

gg

