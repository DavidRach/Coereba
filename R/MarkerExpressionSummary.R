
#' Flattens Coereba_MarkerExpressions into averaged measurement for Heatmap
#' 
#' @param x The MarkerExpressions data.frame
#' @param stats Default median, alternate is mean
#' @param TheName Character value to give the Population
#' 
#' @importFrom dplyr select
#' @importFrom tidyselect where
#' @importFrom dplyr summarise
#' @importFrom dplyr across
#' @importFrom tidyselect everything
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' 
#' @return A data.frame row of summarized data
#' 
#' @noRd
MarkerExpressionSummary <- function(x, stats="median", TheName){
  data <- x |> select(where(is.numeric))

  if (stats == "median"){
    ReturnValues <- data |>
      summarise(across(everything(), median, na.rm = TRUE)) |>
      round(digits=2) |> mutate(Population=TheName[[1]]) |>
      relocate(Population, .before=1)
  } else if (stats == "mean"){
    ReturnValues <- data |>
      summarise(across(everything(), median, na.rm = TRUE)) |>
      round(digits=2) |> mutate(Population=TheName[[1]]) |>
      relocate(Population, .before=1)
  } else {stop("Select for stats argument either mean or median")}

  return(ReturnValues)
}
