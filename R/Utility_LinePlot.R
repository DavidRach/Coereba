Utility_LinePlot <- function(data, varX, varY, myfactor, color_palette){
  library(ggplot2); library(tidyr); library(broom); library(purrr); library(ggpubr); library(ggbeeswarm); library(Rita); library(moments)
  theme_set(theme_bw())
  set.seed(1989)

  plot <- ggplot(data, aes(x =.data[[varX]], y = .data[[varY]], color = .data[[myfactor]])) + geom_point(aes(fill = .data[[myfactor]]), size = 3) + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1) + stat_cor(method = "spearman", aes(group = NULL)) + labs(title = NULL, x = varX, y = varY) + scale_color_manual(values = color_palette)

  return(plot)
}
