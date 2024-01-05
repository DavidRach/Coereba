Utility_GeomDensity <- function(data, var, myfactor){
  library(ggplot2); library(tidyr); library(broom); library(purrr); library(ggpubr); library(Rita); library(moments)
  theme_set(theme_bw())

  plot <- ggplot(data, aes(x =.data[[var]], fill = .data[[myfactor]])) +
    geom_density(alpha = 0.5) +
    labs(title = var, x = NULL, y = "Frequency")

  return(plot)
}
