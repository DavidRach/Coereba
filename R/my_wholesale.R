my_wholesale <- function(x, sample, cellNumber){
  name <- keyword(x, sample)
  df <- flowCore::exprs(x)[,11:39]
  df <- data.frame(df, check.names = FALSE)
  fd <- slice_sample(df, n = cellNumber, replace = FALSE)
  oye <- data.frame(fd, check.names = FALSE)
  oye$Samples <- as.factor(name)
  #Some Kind of Assign Oye the Sample Name
  return(oye)
}
