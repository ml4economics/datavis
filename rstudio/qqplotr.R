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

add_qq_band <- function(gg, distribution, dname, dparams, conf, alpha = 0.5) {
  gg + stat_qq_band(distribution=dname, dparams = dparams, conf=conf, mapping = aes(fill = as.character(conf)), alpha = alpha)
}

create_ggplot <- function(data, dname, dparams, title, conflevels = c(0.99, 0.95, 0.90)) {
  gg <- ggplot(data = data, mapping = aes(sample = samples))
  for (conf in conflevels) {
    gg <- add_qq_band(gg, distribution, dname, dparams, conf)
  }  
  gg <- gg + stat_qq_line(distribution=dname, dparams = dparams) 
  gg <- gg + stat_qq_point(distribution=dname, dparams = dparams)
  gg <- gg + scale_fill_discrete("Confidence Level")
  gg <- gg + 
         labs(x = "Theoretische Quantile", y = "Stichprobenquantile") +
         ggtitle(title) +
         theme(plot.title = element_text(hjust = 0.5))
  gg
}

gg <- create_ggplot(smp, 
                    dname, 
                    dparams, 
                    "QQ Plot with Chi-Squared Distribution (df = 10)") 

gg

