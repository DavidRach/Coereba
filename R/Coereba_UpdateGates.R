#' Update a gate cutoff csv with the Coereba_App edits
#'
#' @param Clicks A path to a .csv file or a folder containing only the Click .csv files.
#' @param Old  File path to the CSV containing the original Estimated Gate Cutoffs
#' @param fileName Desired name for the updated .csv file
#' @param outpath File path to save new .csv file at.
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
#' TheUpdateClickInfo <- file.path(File_Location, "UpdateClickData.csv")
#'
Coereba_UpdateGates <- function(Clicks, Old, fileName, outpath){
  TheFiles <- list.files(Clicks, pattern=".csv", full.names = TRUE)
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

  TheName <- gsub(".csv", "", fileName)
  StorageLocation <- file.path(outpath, TheName)
  write.csv(TheModified, file=paste0(StorageLocation, ".csv"), row.names = FALSE)

  return(TheModified)
}



#' Internal for UpdateGates
#'
#' @param x A path to a .csv file
#'
#' @importFrom utils read.csv
#' @importFrom dplyr rename
#' @importFrom lubridate ymd_hms
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#'
#' @noRd
InternalGateUpdate <- function(x){
  TheClicks <- read.csv(x, check.names = FALSE)
  TheClicks <- TheClicks %>% rename(specimen = Plot_Name)
  TheClicks$Time <- ymd_hms(TheClicks$Time)
  Retained <- TheClicks %>% group_by(specimen, X_Label) %>%
    arrange(desc(Time)) %>% slice(1) %>% ungroup()
  Retained <- Retained %>% select(-Time)
  return(Retained)
}
