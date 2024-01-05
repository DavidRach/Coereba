Utility_BoxPlot <- function(data, var, myfactor, shape_palette, fill_palette){
  library(ggplot2); library(tidyr); library(broom); library(purrr); library(ggpubr); library(ggbeeswarm); library(Rita); library(moments)
  theme_set(theme_bw())
  set.seed(1989)

  plot <- ggplot(data, aes(x =.data[[myfactor]], y = .data[[var]])) + geom_boxplot(show.legend = FALSE) + stat_summary(fun = median, show.legend = FALSE, geom = "crossbar", width = 0.75) + geom_beeswarm(show.legend = FALSE, aes(shape = .data[[myfactor]], fill = .data[[myfactor]]), method = "center", side = 0, priority = "density", cex = 3, size = 4) + scale_shape_manual(values = shape_palette) +
    scale_fill_manual(values = fill_palette) + labs(title = var, x = NULL, y = NULL) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))

  return(plot)
}
