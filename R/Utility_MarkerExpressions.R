#' Generate overall marker expressions across Coereba clusters,
#'
#' @param BinaryFile The Coereba file with 0 and 1 for Marker expression by cluster
#' @param OriginalData The data.frame with the individual specimen info
#' @param myfactor The factor you want the points to resemble
#' @param shape_palette Shape of your points by factor
#' @param fill_palette Fill of your points by factor
#' @param panel A .csv or data.frame of your Fluorophores and Markers
#' @param scalefactor Not implemented yet
#' @param scalefactorlabel Not implemented yet
#' @param label Plot Label
#' @param plot Whether to return a plot, FALSE returns data.frame
#' @param savePlot Whether to also save the plot
#' @param filename What file name to save it as (location not yet implemented)
#' @param cex How cramped to make the beeswarm columns.
#' @param XAxisLevels provide a list of final names to rearrange the x axis data.
#'
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tidyselect starts_with
#' @importFrom purrr map
#' @importFrom dplyr left_join
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom ggplot2 ggsave
#' @importFrom tidyr as_tibble
#' @importFrom dplyr c_across
#' @importFrom dplyr mutate
#' @importFrom dplyr rowwise
#'
#' @return returns the data.frame or a ggplot plot.
#' @export
#'
#' @examples Not at this time

Utility_MarkerExpressions <- function(BinaryFile, OriginalData, myfactor, starter, shape_palette, fill_palette,
                                      panel, scalefactor, scalefactorlabel, label, plot, savePlot, filename = NULL, cex, XAxisLevels){

  if (!is.data.frame(panel)) {MyPanel <- read.csv(panel, check.names = FALSE)} else {MyPanel <- panel}

  Metadata <- OriginalData %>% select(!starts_with(starter))

  AllMarkers <- BinaryFile %>% select(!starts_with("Identity")) %>% colnames()

  SwampPuppy <- map(.x=AllMarkers, .f=.Internal_Aggregate, panel=panel, BinaryFile=BinaryFile,
                    OriginalData=OriginalData, MyPanel=MyPanel, Metadata=Metadata, myfactor=myfactor, normality=normality,
                    shape_palette=shape_palette, fill_palette=fill_palette, scalefactor=scalefactor,
                    scalefactorlabel=scalefactorlabel...) %>% bind_cols()

  SwampFluors <- SwampPuppy %>% colnames()
  SwampFluors <- data.frame(SwampFluors) %>% rename(Fluorophore = SwampFluors)
  RetainedFluors <- left_join(SwampFluors, MyPanel, by = "Fluorophore")
  NewNames <- RetainedFluors$Marker
  colnames(SwampPuppy) <- NewNames
  #SwampPuppy

  MarkerExpressions <- cbind(Metadata, SwampPuppy)

  if (plot == TRUE){
    MetaColumns <- colnames(Metadata)
    DataColumns <- length(SwampPuppy)

    df_melted <- gather(MarkerExpressions, key = "Marker", value = "Value", -all_of(MetaColumns))

    df_melted$Marker <- factor(df_melted$Marker, levels = XAxisLevels)

    TheggPlot <- ggplot(df_melted, aes(x = Marker, y = Value)) + geom_boxplot(show.legend = FALSE) +
      stat_summary(fun = median, show.legend = FALSE, geom = "crossbar", width = 0.75) +
      geom_beeswarm(show.legend = FALSE, aes(shape = .data[[myfactor]], fill = .data[[myfactor]]),
                    method = "center", side = 0, priority = "density", cex = cex, size = 4) +
      scale_shape_manual(values = shape_palette) + scale_fill_manual(values = fill_palette) +
      labs(title = label, x = NULL, y = NULL) + theme_bw() + theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, size = 8))

    if (savePlot == TRUE){ggsave(filename, TheggPlot,dpi = 600, units = "in", width = 6, height = 4)
      } else {return(TheggPlot)}
  } else {return(MarkerExpressions)}
}

.Internal_Aggregate <- function(x, panel, BinaryFile, OriginalData, Metadata, myfactor, normality, shape_palette,
                                fill_palette, MyPanel=MyPanel, scalefactor, scalefactorlabel, ...){
  Column <- x

  #Positive Marker Expressions
  Fluorophore <- Column
  Fluorophore <- data.frame(Fluorophore)
  Attempt1 <- left_join(Fluorophore, MyPanel, by = "Fluorophore")
  ThePlotName <- Attempt1$Marker
  #ThePlotName <- paste0(ThePlotName, "+", sep = "")

  Positive <- BinaryFile %>% dplyr::filter(.data[[Column]] == 1)

  if(nrow(Positive) >0){
    TheInternalBypass <- Positive$Identity
    TheInternalBypass <- gsub("_", "", TheInternalBypass)
    TheInternalBypass <- gsub("-", "", TheInternalBypass)
    InternalData <- OriginalData[, names(OriginalData) %in%
                                   TheInternalBypass] #Input from HeatMap filtered.
    InternalData <- as_tibble(InternalData)
    Subsetted <- InternalData %>% rowwise() %>% mutate(
      aggregate = sum(c_across(everything()), na.rm = TRUE))
    InternalFinal <- Subsetted %>% select(aggregate)
    colnames(InternalFinal)[1] <- Column
    InternalFinal
  } else {Negative <- BinaryFile %>% dplyr::filter(.data[[Column]] == 0)
  TheInternalBypass <- Positive$Identity
  TheInternalBypass <- gsub("_", "", TheInternalBypass)
  TheInternalBypass <- gsub("-", "", TheInternalBypass)
  InternalData <- OriginalData[, names(OriginalData) %in%
                                 TheInternalBypass] #Input from HeatMap filtered.
  InternalData <- as_tibble(InternalData)
  Subsetted <- InternalData %>% rowwise() %>% mutate(
    aggregate = sum(c_across(everything()), na.rm = TRUE))
  InternalFinal <- Subsetted %>% select(aggregate) %>% mutate_all(~0) # Since No Positive Columns
  colnames(InternalFinal)[1] <- Column
  InternalFinal
  }

  return(InternalFinal)
}
