#' Combine clusters and check
#'
#' @param BinaryFile  NA
#' @param OriginalData NA
#' @param myfactor NA
#' @param starter First parameter Coereba split by, used to identify columns that aren't metadata
#' @param normality  NA
#' @param shape_palette NA
#' @param fill_palette NA
#' @param panel NA
#' @param scalefactor NA
#' @param scalefactorlabel NA
#' @param label NA
#'
#' @return NA
#' @export
#'
#' @examples NA
Utility_Reaggregate <- function(BinaryFile, OriginalData, myfactor, starter, normality, shape_palette, fill_palette, panel, scalefactor, scalefactorlabel, label){

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

  NewPlot <- Utility_Behemoth(data = ReaggregatedData, var = "aggregate", myfactor = "ptype", normality = "dagostino", shape_palette = shape_ptype, fill_palette = fill_ptype, panel = panel, scalefactor = scalefactor, scalefactorlabel = scalefactorlabel)

  ptypeggplot <- NewPlot
  ptypeggplot + labs(title = label)


}
