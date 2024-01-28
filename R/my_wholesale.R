#' Downsamples a population of interest
#'
#' @param x A GatingSet object (ex. gs or gs[[1]])
#' @param sample Keyword denoting sample name (ex. "GUID")
#' @param cellNumber Desired number of cells to downsample to.
#'
#' @importFrom flowWorkspace keyword
#' @importFrom flowCore exprs
#' @importFrom dplyr slice_sample
#'
#' @return Some additional value to edit
#' @export
#'
#' @examples NULL
my_wholesale <- function(x, sample, cellNumber){
  name <- keyword(x, sample)
  df <- exprs(x)[,11:39]
  df <- data.frame(df, check.names = FALSE)
  fd <- slice_sample(df, n = cellNumber, replace = FALSE)
  oye <- data.frame(fd, check.names = FALSE)
  oye$Samples <- as.factor(name)
  #Some Kind of Assign Oye the Sample Name
  return(oye)
}
