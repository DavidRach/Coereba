#' Update a gate cutoff csv with the Coereba_App edits
#'
#' @param Clicks A path to a .csv file or a folder containing only the Click .csv files.
#' @param Old  File path to the CSV containing the original Estimated Gate Cutoffs
#' @param export Whether to export the updated .csv, default is TRUE
#' @param outpath File path to save new .csv file at.
#' @param fileName Desired name for the updated .csv file
#'
#' @importFrom utils read.csv
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @importFrom dplyr pull
#' @importFrom utils write.csv
#'
#' @return An updated .csv file to the new location, and a data.frame
#' @export
#'
#' @examples
#'
#' File_Location <- system.file("extdata", package = "Coereba")
#' TheOldCSV <- file.path(File_Location, "GateCutoffsForNKs.csv")
#' TheClickInfo <- file.path(File_Location, "ClickDataExample.csv")
#' 
#' UpdatedCSV <- Coereba_UpdateGates(Clicks=TheClickInfo, Old=TheOldCSV,
#'  export=FALSE, outpath=NULL, fileName="UpdatedCSV")
#'
Coereba_UpdateGates <- function(Clicks, Old, export=TRUE, outpath, fileName){

  if (!grepl("\\.csv$", Clicks)){
  TheFiles <- list.files(Clicks, pattern=".csv", full.names = TRUE)
  } else {TheFiles <- Clicks}

  TheOld <- read.csv(Old, check.names = FALSE)

  Retained <- map(.x=TheFiles, .f=InternalGateUpdate) %>% bind_rows()

  TheModified <- TheOld

  for (i in 1:nrow(Retained)) {
    TheSpecimen <- Retained[i,1] %>% pull()
    TheFluorophore <- Retained[i,2] %>% pull()
    TheValue <- Retained[i,3] %>% pull()

    row_index <- which(TheModified$specimen == TheSpecimen)
    col_index <- which(names(TheModified) == TheFluorophore)
    TheModified[row_index, col_index] <- TheValue
  }

  if(export==TRUE){
  TheName <- gsub(".csv", "", fileName)
  StorageLocation <- file.path(outpath, TheName)
  write.csv(TheModified, file=paste0(StorageLocation, ".csv"), row.names = FALSE)
  } else {return(TheModified)}
  }


