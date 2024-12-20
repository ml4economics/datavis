library(qqplotr)

partial_func <- function(func, ...) {
  fixed_args <- list(...)
  function(...) {
    do.call(func, c(fixed_args, list(...)))
  }
}

add_qq_band <- function(gg, dname, dparams, band_type="ell", conf=0.95, alpha = 0.5) {
  gg + stat_qq_band(distribution=dname, 
                    dparams = dparams,
                    bandType = band_type,
                    conf=conf, 
                    mapping = aes(fill = as.character(conf)), alpha = alpha)
}

create_ggplot <- function(title, data, dname, dparams = list(), 
                          band_type="ell", 
                          conflevels = c(0.95), 
                          show_scale = FALSE,
                          show_labs = FALSE) {
  gg <- ggplot(data = data, mapping = aes(sample = samples))
  for (conf in conflevels) {
    gg <- add_qq_band(gg, dname, dparams, band_type, conf)
  }  
  gg <- gg + stat_qq_line(distribution=dname, dparams = dparams) 
  gg <- gg + stat_qq_point(distribution=dname, dparams = dparams)
  if ( show_scale ) {
    gg <- gg + scale_fill_discrete("Konfidenzniveau")
  }
  else {
    gg <- gg + theme(legend.position = "none")
  }
  if ( show_labs) {
    gg <- gg + labs(x = "Theoretische Quantile", y = "Stichprobenquantile") 
  }
  else {
    gg <- gg + labs(x = NULL, y = NULL)
  }
  if ( ! is.null(title)) {
    gg <- gg + ggtitle(title)
  }
  
  gg <- gg + theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5))
  gg
}

create_ggplot_from_sample_func <- function(title, sample_func, ref_func_name, ref_func_params = list(), band_type="ell", conflevels = c(0.95)) {
  sample <- data.frame(samples = sample_func(100))
  create_ggplot(title, sample, ref_func_name, ref_func_params, band_type, conflevels, FALSE)
}

#set.seed(42)
#plot1 <- create_ggplot_from_sample_func("QQ Plot Chi-Quadrat-Verteilung (df = 10)", 
#                                   partial_func(rchisq, df=10), 
#                                   "chisq", 
#                                   list(df=10))


create_norm_ggplot <- function(sample_func, conflevels = c(0.95) ) {
  create_ggplot_from_sample_func(NULL, sample_func, "norm", list(mean = 0, sd = 1), band_type="ell", conflevels=conflevels)
}

create_norm_ggplots <- function(sample_func, conflevels = c(0.95), count = 6) {
  for ( i in 1:count) {
    if ( i == 1) {
      multi_sample_plot <- create_norm_ggplot(sample_func)
    }
    else {
      multi_sample_plot <- multi_sample_plot + create_norm_ggplot(sample_func)
    }
  }
  multi_sample_plot
}

composite_plot <- create_norm_ggplots(partial_func(rt, df = 3))

print(composite_plot)
