#' Take FlowJo .csv files and convert values to designated root.
#'
#' @param data A data.frame object
#' @param column A given column of the data.frame
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rowwise
#' @importFrom dplyr c_across
#'
#' @return Some additional value to edit
#' @export
#'
#' @examples NULL
Utility_Rooted <- function(data, column){
  input <- column
  interest <- sub(".*/", "", input)
  root <- sub("/.*", "", input)

  NewName <- paste(interest, '(% of', root, ' cells)')

  phrase <- input
  gates <- list()
  while ("/" %in% strsplit(phrase, NULL)[[1]]) {
    remainder <- sub("/[^/]*$", "", phrase)
    gates <- c(gates, remainder)
    phrase <- remainder
  }

  MyColumns <- c(rev(gates), input)
  SelectColumns <- which(colnames(data) %in% MyColumns)
  Mini <- data[, SelectColumns]

  Mini <- Mini/100 #Return Everything to Decimal
  result <- rowwise(Mini) %>% mutate(NewColumn = prod(
    c_across(everything()))) #Multiply all by each other
  result <- mutate(result, NewColumn = NewColumn*100) #Return to decimal
  result$NewColumn <- as.numeric(result$NewColumn)
  result1 <- result %>% select(NewColumn) %>% as.data.frame()

  colnames(result1)[colnames(result1) == "NewColumn"] <- NewName
  result1
}
