#' Early version Luciernaga's single color plot functionality
#'
#' @param x A GatingSet object (ex. gs or gs[[1]])
#' @param subsets Your population of interest (ex. "root")
#' @param column A list of columns (superceeded)
#' @param sample.name Keyword for which samples name is stored (ex. "GUID")
#' @param experiment Directly assign experiment name (ex. "JAN2024")
#' @param experiment.name Keyword for which experiment name is stored (ex. "GROUPNAME")
#' @param condition Directly assign condition name (ex. "Control")
#' @param condition.name Keyword for which condition name is stored (ex. "TUBENAME")
#' @param bins Granularity of the plots by how many bins the cells are divided into
#' @param clearance A number by which the quantile value is multiplied to provide a margin
#' @param outpath Location to which to store the generated plots
#' @param sourcelocation Location to find the plot file.
#'
#' @importFrom flowWorkspace keyword
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom flowCore exprs
#'
#' @return NULL
#' @export
#'
#' @examples NULL
TidyPlotThemPlots <- function(x, subsets, column, sample.name, experiment = NULL, experiment.name = NULL, condition = NULL, condition.name = NULL, bins, clearance, outpath, sourcelocation){

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
