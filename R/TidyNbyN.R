TidyNbyN <- function(x, subsets, level, column, starter, sample.name, experiment = NULL, experiment.name = NULL, condition = NULL, condition.name = NULL, bins, clearance, outpath, sourcelocation, reference){
  library(flowCore); library(dplyr)
  x <- x
  yD <- subsets
  yX <- starter
  outpath <- outpath
  name <- keyword(x, sample.name)
  new.name <- name
  new.name <- gsub(".fcs$", "", gsub("IFN", "INF", gsub("-M0 ", "_", fixed = TRUE, new.name)))
  if(is.null(experiment)){experiment <- keyword(x, experiment.name)
  experiment <- gsub("DTR_2023_", "", experiment) #Not Tidy Data Manipulation
  experiment <- gsub("^(.*?\\d{2}).*", "\\1", experiment) #Not Tidy Data Manipulation;
  } else {experiment = experiment}
  if(is.null(condition)) {condition <- keyword(x, condition.name)
  } else {condition = condition}
  AggregateName <- paste(new.name, experiment, condition, sep = "_")
  StorageLocation <- paste(outpath, AggregateName, sep = "/", collapse = NULL)

  ff <- gs_pop_get_data(x, level) #What surrounding for comparison gating you are using.
  df <- flowCore::exprs(ff[[1]][,column])
  TheDF <- data.frame(df, check.names = FALSE)
  pdf_locations <- list()
  source(sourcelocation, local = TRUE)
}
