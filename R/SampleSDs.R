#' Returns Standard Deviations for Normalized Detectors
#'
#' @param x A GatingSet object (ex. gs or gs[[1]])
#' @param subsets A subset of interest, (ex. "lymph)
#' @param sample Keyword denoting the sample name (ex. "GUID")
#' @param experiment Keyword denoting the experiment name (ex. "GROUPNAME")
#' @param condition Keyword denoting the condition name (ex. "TUBENAME")
#'
#' @return NULL
#' @export
#'
#' @examples NULL
SampleSDs <- function(x, subsets, sample, experiment, condition){
  gc()
  x <- x
  bid <- keyword(x, sample)
  experiment <- keyword(x, experiment)
  experiment <- gsub("DTR_2023_", "", experiment)
  experiment <- gsub("_Antibodies", "", experiment)
  condition <- keyword(x, condition)
  condition <- gsub("_Unstained", "", fixed = TRUE, condition)

  ff <- gs_pop_get_data(x, subsets)
  df <- flowCore::exprs(ff[[1]])
  DF <- as.data.frame(df, check.names = FALSE)
  CleanedDF <- DF[,-grep("Time|FS|SC|SS|Original", names(DF))]
  BackupNames <- colnames(CleanedDF)
  n <- CleanedDF

  colnames(n) <- gsub("-A", "", colnames(n))
  n[n < 0] <- 0
  A <- do.call(pmax, n)
  Normalized <- n/A
  Normalized <- round(Normalized, 1)
  colnames(Normalized) <- BackupNames
  #View(Normalized)

  system.time({Stash <- sapply(Normalized, sd)})
  SD <- round(data.frame(t(Stash)), 2)
  colnames(SD) <- gsub(".A", "-A", fixed = TRUE, colnames(SD))
  #View(SD)
  return(SD)
}
