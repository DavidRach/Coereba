Utility_QQplots <- function(data, var, myfactor){
  library(ggplot2); library(tidyr); library(broom); library(purrr); library(ggpubr); library(Rita); library(moments)
  theme_set(theme_bw())

  plot <- ggqqplot(Final, x = var, facet.by = myfactor) +
    labs(title = "QQ Plot - Normality Checking", x = "Theoretical Quantiles", y = "Sample Quantiles")

  return(plot)
}
