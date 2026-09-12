
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
