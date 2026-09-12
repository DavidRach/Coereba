
#' Internal for Coereba_GateCutoffs
#'
#' @param splitpoint The data.frame of Fluorophore and Cutoff columns
#' @param data The sample data in a data.frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr pull
#' @importFrom purrr map
#' @importFrom tidyselect all_of
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr filter
#' @importFrom dplyr row_number

#' @importFrom dplyr lag
#'
#' @noRd
GateChecks <- function(splitpoint, data, returnPlots=FALSE) {
  TheMarkers <- splitpoint %>% pull(Fluorophore)
  #x <- TheMarkers[7]

  CorrectedData <- map(.x=TheMarkers, .f=FunctionStandin,
                       splitpoint=splitpoint, data=data,
                       returnPlots=returnPlots)

  if (returnPlots== TRUE){return(CorrectedData)}
}