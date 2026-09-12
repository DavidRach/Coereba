#' Does Something
#' 
MedianIQR <- function(data, var, myfactor) {
  Tada <- data %>% group_by(.data[[myfactor]]) %>%
    summarize(Median = median(.data[[var]], na.rm = TRUE),
      IQR = IQR(.data[[var]], na.rm = TRUE),
      P25 = quantile(.data[[var]], 0.25, na.rm = TRUE),
      P75 = quantile(.data[[var]], 0.75, na.rm = TRUE),
      .groups = "drop")
  return(Tada)
}