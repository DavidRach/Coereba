#' Does Something
#' 
MeanSD <- function(data, var, myfactor) {
  Tada <- data %>% group_by(.data[[myfactor]]) %>%
    summarize(Mean = mean(.data[[var]], na.rm = TRUE),
      SD = sd(.data[[var]], na.rm = TRUE), .groups = "drop")
  return(Tada)
}
