#' Update a gate cutoff csv with the CoerebaApp edits
#'
#' @param Clicks File path to CSV output from CoerebaApp containing new gate cutoff locations
#' @param Old  File path to the CSV containing the original Estimated Gate Cutoffs
#' @param fileName Desired name for the updated .csv file
#' @param outpath File path to save new .csv file at.
#'
#' @importFrom utils read.csv
#' @importFrom dplyr rename
#' @importFrom lubridate ymd_hms
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr slice
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr pull
#'
#' @return An updated .csv file to the new location, and a data.frame
#' @export
#'
#' @examples Not at this time

UpdateGates <- function(Clicks, Old, fileName, outpath){
  TheClicks <- read.csv(Clicks, check.names = FALSE)
  TheOld <- read.csv(Old, check.names = FALSE)

  TheClicks <- TheClicks %>% rename(specimen = Plot_Name)
  TheClicks$Time <- hms(TheClicks$Time)
  Retained <- TheClicks %>% group_by(specimen, X_Label) %>%
    arrange(desc(Time)) %>% slice(1) %>% ungroup()
  Retained <- Retained %>% select(-Time)

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
