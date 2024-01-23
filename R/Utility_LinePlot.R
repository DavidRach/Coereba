#' Generate a line plot
#'
#' @param data A data.frame object
#' @param varX The column to plot on X axis
#' @param varY The column to plot on Y axis
#' @param myfactor The column denoting the factor of interest to compare
#' @param color_palette The color palette to use for the factor variable
#'
#' @import ggplot2
#'
#' @return NULL
#' @export
#'
#' @examples NULL
Utility_LinePlot <- function(data, varX, varY, myfactor, color_palette){

  plot <- ggplot(data, aes(x =.data[[varX]], y = .data[[varY]], color = .data[[myfactor]])) +
    geom_point(aes(fill = .data[[myfactor]]), size = 3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1) +
    stat_cor(method = "spearman", aes(group = NULL)) +
    labs(title = NULL, x = varX, y = varY) + theme_bw()
    scale_color_manual(values = color_palette)

  return(plot)
}
