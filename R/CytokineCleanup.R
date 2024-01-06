#' Combine FlowJo Q1 Q2 columns
#'
#' @param x A data.frame object
#' @param variable A column name
#' @param first Return name first column
#' @param second Return name second column
#'
#' @return NULL
#' @export
#'
#' @examples NULL
CytokineCleanup <- function(x, variable, first, second){
  x <- x
  Targets <- colnames(x)[str_detect(colnames(x), variable)]
  for(i in Targets) {SayMyName <- str_extract(i, paste0("^(.*?)(?=", variable, ")"))
  column_index <- which(colnames(x) == i)
  FocusData <- x %>% select(all_of(c(column_index - 1, column_index, column_index + 1)))
  colnames(FocusData) <- c("A", "B", "C")
  Deja <- FocusData %>% mutate({{first}} := A + B, {{second}} := B + C) %>% select({{first}}, {{second}}) %>% rename_with(~paste(SayMyName, ., sep = ""))
  x <- bind_cols(x, Deja)
  }
  return(x)
}
