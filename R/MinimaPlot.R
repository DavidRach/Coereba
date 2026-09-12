
#' Internal for Coereba_GateCutoffs
#'
#' @param data The passed dataframe
#' @param TheMin The mininum MFI for the dataset with a count of 3
#' @param TheMax The maxinum MFI with a count of 3
#' @param TheMiddle The estimated middle point between TheMin and TheMax
#' @param TheSplitpoint The splitpoint retrieved by GateRetrieval
#' @param name The Fluorophore name for the plot
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#'
#' @noRd
MinimaPlot <- function(data, TheMin, TheMax, Middle, TheSplitpoint,
                       LocalMinima, name=Fluorophore){

    Plot <- ggplot(data, aes(x=MFI, y=Count)) + geom_line() +
      geom_vline(aes(xintercept = TheMax), color = "red", linewidth = 0.5) +
      geom_vline(aes(xintercept = TheMin), color = "red", linewidth = 0.5) +
      geom_vline(aes(xintercept = Middle), color = "red", linewidth = 0.5) +
      geom_vline(aes(xintercept = TheSplitpoint), color = "blue", linewidth = 1) +
      geom_vline(xintercept = LocalMinima, color = "black", linewidth = 1) +
      labs(title=name)+
      theme_bw()

  return(Plot)
}