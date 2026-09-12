
#' Internal for Coereba_GateCutoffs
#'
#' @param x The Fluorophore being iterated on
#' @param splitpoint The data.frame of fluorophore cutoff splitpoints
#' @param data The data.frame of exprs for the fluorophores
#' @param returnPlots Default False, returns plots for troubleshooting
#'
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr row_number
#'
#' @noRd
FunctionStandin <- function(x, splitpoint, data, returnPlots=FALSE){
  Fluorophore <- x
  TheSplitpoint <- splitpoint %>% dplyr::filter(Fluorophore %in% x) %>%
    pull(Cutoff) %>% round(., 0)
  TheData <- data %>% select(all_of(x)) %>% round(., 0)
  colnames(TheData) <- "xVal"
  freq_table <- data.frame(table(TheData)) %>% rename(yVal = Freq)

  freq_table <- freq_table %>%
    mutate(OriginalX = as.numeric(as.character(xVal))) %>%
    relocate(OriginalX, .before = xVal)

  freq_table$xVal <- as.numeric(freq_table$xVal)
  freq_table$yVal <- as.numeric(freq_table$yVal)

  # Filtering out random dots that might throw off dimensions
  freq_table <- freq_table %>% dplyr::filter(yVal > 3)

  # TheMode <- TheMax
  TheMode <- freq_table %>% dplyr::filter(yVal == max(yVal)) %>%
    pull(OriginalX)
  if (length(TheMode) > 1){TheMode <- TheMode[[1]]}
  # TheModeCount <- TheYMax
  TheModeCount <- freq_table %>% dplyr::filter(yVal == max(yVal)) %>%
    pull(yVal)
  if (length(TheModeCount) > 1){TheModeCount <- TheModeCount[1]}

  # TheMin <- TheLow
  TheMin <- freq_table %>% dplyr::filter(row_number() == 1) %>% pull(OriginalX)
  # TheMax <- TheHigh
  TheMax <- freq_table %>% dplyr::filter(row_number() == nrow(freq_table)) %>%
    pull(OriginalX)
  TheRange <- TheMax-TheMin # Caution Here With Negative Values
  Middle <- (TheRange/2)+TheMin

  freqX <- freq_table %>% pull(OriginalX)
  freqY <- freq_table %>% pull(yVal)

  Minima <- LocalMinima(theX = freqX, theY = freqY)
  TheMinima <- Minima %>% pull(x)

  if (returnPlots == TRUE){

  MyData <- data.frame(MFI=freqX, Count=freqY)

  Plot <- MinimaPlot(data=MyData, TheMin=TheMin, TheMax=TheMax,
             Middle=Middle, TheSplitpoint=TheSplitpoint,
             LocalMinima=TheMinima, name=Fluorophore)

  return(Plot)
  }

  if (length(TheMinima) == 0){message("No minima present")}

  Options <- c(TheSplitpoint, TheMinima)

  if (TheMode <= Middle){orientation <- "left"
  } else {orientation <- "right"}

  if (all(Options <= TheMode)){shape <- "lesser"
  } else if (all(Options > TheMode)){shape <- "greater"
  } else {shape <- "sandwhich"}

  Summons <- paste(Fluorophore, shape, orientation, sep=" ")
  return(Summons)
}