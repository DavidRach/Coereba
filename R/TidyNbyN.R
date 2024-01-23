#' An early version of Luciernaga's NbyN plot.
#'
#' @param x A GatingSet object (ex. gs or gs[[1]])
#' @param subsets A Gating Hierarchy node containing cells of interest.
#' @param level unclear
#' @param column A list of columns for exprs, superceeded
#' @param starter The y-column fluor to start with. (ex. "BV711-A")
#' @param sample.name Keyword that contains the sample names (ex. "GUID")
#' @param experiment Directly assign experiment (ex. "JAN2024")
#' @param experiment.name Keyword that contains the experiment name (ex. "GROUPNAME")
#' @param condition Directly assign condition (ex. "Control")
#' @param condition.name Keyword that contains the condition name (ex. "TUBENAME")
#' @param bins For plotting resolution, how many bins to divide the cells into
#' @param clearance Multiplied by quantile value to dictate the margin of the plots
#' @param outpath Location to send the .pdf
#' @param sourcelocation Location of the plotting file
#' @param reference A data.frame containing the location of the dividing lines per specimen.
#'
#' @importFrom flowWorkspace keyword
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom flowCore exprs
#'
#' @return NULL
#' @export
#'
#' @examples NULL
TidyNbyN <- function(x, subsets, level, column, starter, sample.name, experiment = NULL, experiment.name = NULL, condition = NULL, condition.name = NULL, bins, clearance, outpath, sourcelocation, reference){

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
