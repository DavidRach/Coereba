#' Generate QQ plots to assess normality
#'
#' @param data A data.frame object
#' @param var The column containing your values
#' @param myfactor The column containing the factor designations that
#' you want to compare.
#'
#' @importFrom ggpubr ggqqplot
#' @import ggplot2
#'
#' @return NULL
#' @export
#'
#' @examples NULL
Utility_QQplots <- function(data, var, myfactor){

  plot <- ggqqplot(Final, x = var, facet.by = myfactor) + theme_bw() +
    labs(title = "QQ Plot - Normality Checking", x = "Theoretical Quantiles",
         y = "Sample Quantiles")

  return(plot)
}
