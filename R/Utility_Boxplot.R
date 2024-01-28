#' Plot boxplot/beeswarm
#'
#' @param data A data.frame of your data
#' @param var A column of data interest
#' @param myfactor A column containing the factor you want to compare between
#' @param shape_palette palette corresponding to the factor, specifying the shape
#' @param fill_palette palette corresponding to the factor, specifying the fill
#'
#' @import ggplot2
#' @importFrom ggbeeswarm geom_beeswarm
#'
#' @return NULL
#' @export
#'
#' @examples NULL
Utility_BoxPlot <- function(data, var, myfactor, shape_palette, fill_palette){

  plot <- ggplot(data, aes(x =.data[[myfactor]], y = .data[[var]])) +
    geom_boxplot(show.legend = FALSE) +
    stat_summary(fun = median, show.legend = FALSE, geom = "crossbar",
                 width = 0.75) +
    geom_beeswarm(show.legend = FALSE, aes(shape = .data[[myfactor]],
                                           fill = .data[[myfactor]]),
                  method = "center", side = 0, priority = "density",
                  cex = 3, size = 4) +
    scale_shape_manual(values = shape_palette) +
    scale_fill_manual(values = fill_palette) +
    theme_bw() + labs(title = var, x = NULL, y = NULL) +
    theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))

  return(plot)
}
