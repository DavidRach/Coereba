#' Downsample for cell population or marker of choice
#'
#' @param x A GatingSet object (ex. gs or gs[[1]])
#' @param sample A keyword denoting sample name (ex. "GUID")
#' @param channel A list of columns of interest to retrieve data from, numeric.
#' @param cellNumber Total number of cells to downsample to.
#'
#' @return NULL
#' @export
#'
#' @examples NULL
my_modified_utility <- function(x, sample, channel, cellNumber){
  name <- keyword(x, sample)
  df <- flowCore::exprs(x[, channel])
  dsf <- data.frame(df, check.names = FALSE)
  oye <- slice_sample(dsf, n = cellNumber, replace = FALSE)
  oye$Samples <- as.factor(name)
  return(oye)
}
