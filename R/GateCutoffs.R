#' Rough estimate of the location of the dichotomized gates.
#'
#' @param x A GatingSet object (ex. gs or gs[[1]])
#' @param columns Columns of interest, numeric.
#' @param sample.name Keyword for sample names (ex. "GUID")
#'
#' @return NULL
#' @export
#'
#' @examples NULL
GateCutoffs <- function(x, columns, sample.name){
  library(tidyr)
  name <- keyword(x, sample.name)
  df <- flowCore::exprs(x[,columns]) #specify a colunm vector in the future
  dsf <- data.frame(df, check.names = FALSE)
  Vector <- colnames(dsf)
  #Fluorophore <- Vector[1]
  #i <- Vector[1]
  #x <- dsf

  TheEstimate <- function(x, i) {Fluorophore <- i
  max_value <- quantile(x[[i]], 0.95)
  min_value <- quantile(x[[i]], 0.05)
  if(min_value < 0) {MyRange <- abs(-max_value + min_value)} else {MyRange <- max_value - min_value}
  DivisionPoint <- max_value-(MyRange/2)
  MyBin <- x[[i]][x[[i]] < DivisionPoint]
  Value <- quantile(MyBin, 0.95)
  #Value <- DivisionPoint
  ColumnValue <- round(Value, 2)
  newest <- cbind(Fluorophore, ColumnValue)
  newest <- data.frame(newest, row.names = NULL)
  }

  Meowdy <- lapply(Vector, FUN = TheEstimate, x = dsf) %>% bind_rows()

  Rough <- pivot_wider(Meowdy, names_from = Fluorophore, values_from = ColumnValue)
  Rough <- Rough %>% mutate_if(is.character, as.numeric)
  Data <- cbind(name, Rough)
  Data

  #No Hyphens or Spaces Have been removed at this step.
}
