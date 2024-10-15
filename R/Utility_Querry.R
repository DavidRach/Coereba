#' Detect changes a heatmap level below your target
#'
#' @param BinaryFile The Heatmap file
#' @param OriginalData UThe Ready file
#' @param filename Name to give the results
#' @param outfolder Folder to which to output the results
#' @param panel A .csv or dataframe containing the Fluorophores and their Marker names
#' @param starter First parameter Coereba split by, used to identify columns that aren't metadata
#' @param myfactor Column containing factor designation that you want to
#'  differentially compare
#' @param normality Normality test to be used, ex. "dagostino" or "sharpwilks"
#' @param shape_palette palette that list your factors of interest
#' @param fill_palette palette that list your factor of interest
#' @param scalefactor Number you are multiplying the ratio by to make more
#' interpretable (ex. 1000)
#' @param scalefactorlabel The cells of interest, ex. "Vd2 T cells"
#' @param Override parametric or non-parametric
#' @param cex The width of the ggbeeswarm bin
#' @param size Size for the ggbeeswarm circles.
#' @param correction What parameter to apply for ANOVA/Kruskal
#' @param ... Additional arguments
#'
#' @importFrom dplyr select
#' @importFrom tidyr starts_with
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom reshape2 melt
#' @importFrom tidyr as_tibble
#' @importFrom dplyr rowwise
#' @importFrom dplyr c_across
#' @importFrom dplyr filter
#' @importFrom purrr flatten
#' @importFrom viridis scale_fill_viridis
#'
#' @return Some additional value to edit
#' @export
#'
#' @examples NULL

Utility_Querry <- function(BinaryFile, OriginalData, filename, outfolder,
        panel, starter, myfactor, normality, shape_palette, fill_palette,
        scalefactor, scalefactorlabel, Override, cex, size, correction, ...){

  Metadata <- OriginalData %>% select(!starts_with(starter))

  AllMarkers <- BinaryFile %>% select(!starts_with("Identity")) %>% colnames()

  theplots <- map(AllMarkers, .f=Internal_Querry, panel = panel, BinaryFile = BinaryFile,
                  OriginalData = OriginalData, Metadata = Metadata, myfactor = myfactor, normality = normality,
                  shape_palette = shape_palette, fill_palette = fill_palette,
                  scalefactor = scalefactor, scalefactorlabel = scalefactorlabel, Override = Override,
                  cex = cex, size = size, correction = correction, ...)

  #theplots
  theflattenedplots <- flatten(theplots)
  theflattenedplots <- Filter(Negate(is.null), theflattenedplots)
  theflattestplots <- flatten(theflattenedplots)

  Utility_Patchwork(x = theflattestplots, filename = filename,
                    outfolder = outfolder, thecolumns = 2, therows = 2)
}

Internal_Querry <- function(x, panel, BinaryFile, OriginalData, Metadata, myfactor, normality, shape_palette, fill_palette,
                            scalefactor, scalefactorlabel, ...){
  Column <- x

  MyPanel <- read.csv(panel, check.names = FALSE)

  #Positive
  Fluorophore <- Column
  Fluorophore <- data.frame(Fluorophore)
  Attempt1 <- left_join(Fluorophore, MyPanel, by = "Fluorophore")
  ThePlotName <- Attempt1$Marker
  ThePlotName <- paste0(ThePlotName, "+", sep = "")

  Positive <- BinaryFile %>% filter(.data[[Column]] == 1)
  if(nrow(Positive) >0){
    TheInternalBypass <- Positive$Identity
    TheInternalBypass <- gsub("_", "", TheInternalBypass)
    TheInternalBypass <- gsub("-", "", TheInternalBypass)
    InternalData <- OriginalData[, names(OriginalData) %in%
                                   TheInternalBypass] #Input from HeatMap filtered.
    InternalData <- as_tibble(InternalData)
    Subsetted <- InternalData %>% rowwise() %>% mutate(
      aggregate = sum(c_across(everything()), na.rm = TRUE))
    InternalFinal <- Subsetted %>% select(aggregate) %>% cbind(Metadata, .)
    #StarterValue <- ncol(Metadata) + 1
    EndValue <- ncol(InternalFinal)

    Behemoth <- map(names(InternalFinal)[EndValue], ~ Utility_Behemoth(
      data = InternalFinal, var = .x, myfactor = myfactor,
      normality = normality, shape_palette = shape_palette,
      fill_palette = fill_palette,
      scalefactor = scalefactor, scalefactorlabel = scalefactorlabel,
      Override = Override, cex = cex, size = size, correction = correction, ...))

    FinalPosBehemoth <- Behemoth[[1]] + ggtitle(ThePlotName)

    Heatmap <- Internal_Heatmap(filteredCells = Positive, panel = MyPanel, ...)
    FinalPosHeatmap <- Heatmap #+ ggtitle(ThePlotName)

    thePosplots <- list(FinalPosBehemoth, FinalPosHeatmap)
  } else(thePosplots <- NULL)

  ### Negative
  Fluorophore <- Column
  Fluorophore <- data.frame(Fluorophore)
  Attempt1 <- left_join(Fluorophore, MyPanel, by = "Fluorophore")
  ThePlotName <- Attempt1$Marker
  ThePlotName <- paste0(ThePlotName, "-", sep = "")

  Negative <- BinaryFile %>% filter(.data[[Column]] == 0)
  if(nrow(Negative) > 0){
    TheInternalBypass <- Negative$Identity
    TheInternalBypass <- gsub("_", "", TheInternalBypass)
    TheInternalBypass <- gsub("-", "", TheInternalBypass)
    InternalData <- OriginalData[, names(OriginalData) %in%
                                   TheInternalBypass] #Input from HeatMap filtered.
    InternalData <- as_tibble(InternalData)
    Subsetted <- InternalData %>% rowwise() %>% mutate(
      aggregate = sum(c_across(everything()), na.rm = TRUE))
    InternalFinal <- Subsetted %>% select(aggregate) %>% cbind(Metadata, .)
    #StarterValue <- ncol(Metadata) + 1
    EndValue <- ncol(InternalFinal)

    Behemoth <- map(names(InternalFinal)[EndValue], ~ Utility_Behemoth(
      data = InternalFinal, var = .x, myfactor = myfactor,
      normality = normality, shape_palette = shape_palette,
      fill_palette = fill_palette,
      scalefactor = scalefactor, scalefactorlabel = scalefactorlabel,
      Override = Override, cex = cex, size = size, correction = correction))
    FinalNegBehemoth <- Behemoth[[1]] + ggtitle(ThePlotName)

    Heatmap <- Internal_Heatmap(filteredCells = Negative, panel = MyPanel, ...)
    FinalNegHeatmap <- Heatmap #+ ggtitle(ThePlotName)

    theNegplots <- list(FinalNegBehemoth, FinalNegHeatmap)
  } else(theNegplots <- NULL)

  theplots <- list(thePosplots, theNegplots)


  return(theplots)
}
