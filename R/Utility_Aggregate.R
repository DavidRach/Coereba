#' Returns aggregate plots comparing factor of interest for given markers
#'
#' @param BinaryFile The Heatmap file
#' @param OriginalData The Ready file
#' @param markers NULL or a list of Fluorophores desire to aggregate
#' @param myfactor The metadata factor in Original data you wish to compare
#' @param starter The starting name for the Coereba cuts, used to find the columns
#' @param shape_palette Desired shape for the points by factor
#' @param fill_palette Desired fill for the points by factor
#' @param panel A .csv or dataframe containing the Fluorophores and their Marker names
#' @param scalefactor Desired adjust to the axis
#' @param scalefactorlabel Desired name for the axis
#' @param label If a single plot, replaces the title with the following string
#' @param cex Desired spacing for the beeswarm
#' @param size Desired size for the points
#' @param ... Additional arguments, feeds to Internal_Aggregate and Utility_Behemoth
#'
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom tidyselect starts_with
#' @importFrom purrr map
#'
#' @return The generated ggplots.
#' @export
#'
#' @examples Not at this time

Utility_Aggregate <- function(BinaryFile, OriginalData, markers, myfactor, starter, shape_palette,
                              fill_palette, panel, scalefactor, scalefactorlabel, label, cex, size, ...) {

  if (!is.data.frame(panel)) {MyPanel <- read.csv(panel, check.names = FALSE)} else {MyPanel <- panel}

  Metadata <- OriginalData %>% select(!starts_with(starter))

  AllMarkers <- BinaryFile %>% select(!starts_with("Identity")) %>% colnames()

  if(!is.null(markers)){AllMarkers <- AllMarkers[AllMarkers %in% markers]}

  SwampPuppy <- map(.x=AllMarkers, .f=.Internal_Aggregate, panel=panel, BinaryFile=BinaryFile,
                    OriginalData=OriginalData, MyPanel=MyPanel, Metadata=Metadata,
                    myfactor=myfactor, normality=normality,shape_palette=shape_palette,
                    fill_palette=fill_palette, scalefactor=scalefactor,
                    scalefactorlabel=scalefactorlabel...) %>% bind_cols()

  SwampSums <- colSums(SwampPuppy)
  ZeroPuppy <- colnames(SwampPuppy[, SwampSums == 0])

  SwampFluors <- SwampPuppy %>% colnames()
  TheSwampFluors <- SwampFluors[!SwampFluors %in% ZeroPuppy]

  TheSwampPuppy <- SwampPuppy %>% select(-(all_of(ZeroPuppy)))

  TheSwampFluors <- data.frame(TheSwampFluors) %>% rename(Fluorophore = TheSwampFluors)

  TheMyPanel <- MyPanel %>% filter(!Fluorophore %in% ZeroPuppy)

  RetainedFluors <- left_join(TheSwampFluors, TheMyPanel, by = "Fluorophore")
  NewNames <- RetainedFluors$Marker
  colnames(TheSwampPuppy) <- NewNames
  #SwampPuppy

  MarkerExpressions <- cbind(Metadata, TheSwampPuppy)

  TheFigures <- map(colnames(TheSwampPuppy), ~ Utility_Behemoth(
    data = MarkerExpressions, var = .x, myfactor = myfactor,
    normality = normality, shape_palette = shape_palette,
    fill_palette = fill_palette,
    scalefactor = scalefactor, scalefactorlabel = scalefactorlabel,
    Override = Override, cex = cex, size = size, correction = correction, ...))

  if(length(TheFigures) == 1) {
    if(!is.null(label)){TheFigures <- TheFigures[[1]] + ggtitle(label)}
  }

  return(TheFigures)
}
