#' Rearrange FlowJo .csv files for nested questions
#'
#' @param x the data.table
#' @param pattern recognizable string
#' @param target Level of nesting
#'
#' @importFrom stringr str_extract_all
#'
#' @return Some additional value to edit
#' @export
#'
#' @examples NULL
Utility_Rearrange <- function(x, pattern, target){

  x <- x
  x <- data.frame(x, check.names = FALSE)
  Names <- colnames(x)
  pattern <- pattern
  target <- target

  selected_columns <- character(0)

  for (i in Names){
    count <- sum(lengths(str_extract_all(i, pattern, simplify = FALSE)))
    if (count == target) {
      selected_columns <- c(selected_columns, i)}
  }

  selected_data <- x[, selected_columns]
  selected_data <- data.frame(selected_data, check.names = FALSE)

  return(selected_data)

}
