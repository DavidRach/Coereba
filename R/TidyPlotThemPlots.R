TidyPlotThemPlots <- function(x, subsets, column, sample.name, experiment = NULL, experiment.name = NULL, condition = NULL, condition.name = NULL, bins, clearance, outpath, sourcelocation){
  library(flowCore); library(dplyr)
  x <- x
  name <- keyword(x, sample.name)
  if(is.null(experiment)){experiment <- keyword(x, experiment.name)
  experiment <- gsub("DTR_2023_", "", experiment) #Not Tidy Data Manipulation
  experiment <- gsub("^(.*?\\d{2}).*", "\\1", experiment) #Not Tidy Data Manipulation;
  } else {experiment = experiment}
  if(is.null(condition)) {condition <- keyword(x, condition.name)
  } else {condition = condition}
  AggregateName <- paste(name, experiment, condition, sep = "_")
  StorageLocation <- paste(outpath, AggregateName, sep = "/", collapse = NULL)

  ff <- gs_pop_get_data(x, subsets)
  df <- flowCore::exprs(ff[[1]][,column]) #Is the one necessary in this case? Unclear how works with lapply...
  TheDF <- data.frame(df, check.names = FALSE)
  source(sourcelocation, local = TRUE)
}
