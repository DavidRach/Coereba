#' Returns distribution overlay of population of interest
#'
#' @param data A data.frame object containing your data
#' @param var Column in the data.frame of specific data interested in
#' @param myfactor Column containing the factor you want to compare between.
#'
#' @import ggplot2
#' @import tidyr
#' @import broom
#' @import ggpubr
#'
#'
#' @return NULL
#' @export
#'
#' @examples NULL
Utility_GeomDensity <- function(data, var, myfactor){
  theme_set(theme_bw())

  plot <- ggplot(data, aes(x =.data[[var]], fill = .data[[myfactor]])) +
    geom_density(alpha = 0.5) +
    labs(title = var, x = NULL, y = "Frequency")

  return(plot)
}
