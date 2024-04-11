#' Combine clusters and check
#'
#' @param BinaryFile  The Heatmap file
#' @param OriginalData The Ready file
#' @param myfactor The metadata factor in Original data you wish to compare
#' @param starter First parameter Coereba split by, used to identify columns that aren't metadata
#' @param normality  Choice of normality test, "dagostino" or "shapiro"
#' @param shape_palette Desired shape for the points by factor
#' @param fill_palette Desired fill for the points by factor
#' @param panel A .csv or dataframe containing the Fluorophores and their Marker names
#' @param scalefactor Desired adjust to the axis
#' @param scalefactorlabel Desired name for the axis
#' @param label Replaces the title with the following string
#'
#' @return NA
#' @export
#'
#' @examples NA
Utility_Reaggregate <- function(BinaryFile, OriginalData, myfactor, starter, normality, shape_palette, fill_palette, panel, scalefactor, scalefactorlabel, label, ...){

  TheClusters <- BinaryFile %>% select(Identity)
  TheClusters$Identity <- gsub("_", "", fixed = TRUE, TheClusters$Identity)
  TheClusters$Identity <- gsub("-", "", fixed = TRUE, TheClusters$Identity)
  TheClustersList <- TheClusters$Identity
  #View(Ready)

  Metadata <- OriginalData %>% select(-starts_with(starter))
  Subsetted <- OriginalData %>% select(any_of(TheClustersList))
  Subsetted <- Subsetted %>% rowwise() %>% mutate(aggregate = sum(c_across(everything()), na.rm = TRUE))
  ReaggregatedData <- Subsetted %>% select(aggregate) %>% cbind(Metadata, .)
  #View(ReaggregatedData) #Column Eleven Is The Aggregate Column

  table(ReaggregatedData$ptype)

  NewPlot <- Utility_Behemoth(data = ReaggregatedData, var = "aggregate", myfactor = myfactor,
                              normality = normality, shape_palette = shape_ptype, fill_palette = fill_ptype,
                              panel = panel, scalefactor = scalefactor, scalefactorlabel = scalefactorlabel, ...)

  ptypeggplot <- NewPlot
  ptypeggplot + labs(title = label)


}
