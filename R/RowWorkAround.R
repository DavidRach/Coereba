


#' Internal Utility_Coereba, work around for nrow function
#' 
#' @param x The flowframe object
#' 
#' @importFrom BiocGenerics nrow
#' 
#' @return The number of rows. 
#' 
#' @noRd
RowWorkAround <- function(x){
  TheNumberRows <- nrow(x)[[1]] # For Ratio
  return(TheNumberRows)
}