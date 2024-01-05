my_modified_utility <- function(x, sample, channel, cellNumber){
  name <- keyword(x, sample)
  df <- flowCore::exprs(x[, channel])
  dsf <- data.frame(df, check.names = FALSE)
  oye <- slice_sample(dsf, n = cellNumber, replace = FALSE)
  oye$Samples <- as.factor(name)
  return(oye)
}
