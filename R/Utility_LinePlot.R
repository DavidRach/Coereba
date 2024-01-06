#' Generate a line plot
#'
#' @param data
#' @param varX
#' @param varY
#' @param myfactor
#' @param color_palette
#' @import ggplot2,
#'         tidyr,
#'         broom,
#'         purrr,
#'         ggpubr,
#'         ggbeeswarm,
#' @return NULL
#' @export
#'
#' @examples NULL
Utility_LinePlot <- function(data, varX, varY, myfactor, color_palette){
  theme_set(theme_bw())

  plot <- ggplot(data, aes(x =.data[[varX]], y = .data[[varY]], color = .data[[myfactor]])) + geom_point(aes(fill = .data[[myfactor]]), size = 3) + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1) + stat_cor(method = "spearman", aes(group = NULL)) + labs(title = NULL, x = varX, y = varY) + scale_color_manual(values = color_palette)

  return(plot)
}
