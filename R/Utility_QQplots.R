#' Generate QQ plots to assess normality
#'
#' @param data A data.frame object
#' @param var The column containing your values
#' @param myfactor The column containing the factor designations that you want to compare.
#'
#' @return NULL
#' @export
#'
#' @examples NULL
Utility_QQplots <- function(data, var, myfactor){
  theme_set(theme_bw())

  plot <- ggqqplot(Final, x = var, facet.by = myfactor) +
    labs(title = "QQ Plot - Normality Checking", x = "Theoretical Quantiles", y = "Sample Quantiles")

  return(plot)
}
