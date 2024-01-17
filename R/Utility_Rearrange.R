#' Rearrange FlowJo .csv files for nested questions
#'
#' @param x the data.table
#' @param pattern recognizable string
#' @param target Level of nesting
#'
#' @return NULL
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

  #selected_columns
  #common_prefix <- sapply(strsplit(selected_columns, " "), function(x) paste(x[-length(x)], collapse = " "))
  #common_prefix
  #ordered_col_names <- col_names[order(common_prefix)]

  #selected_data <- x[, ..ordered_col_names]
  #selected_data <- data.frame(selected_data, check.names = FALSE)
  #colnames(selected_data)
  return(selected_data)

}
