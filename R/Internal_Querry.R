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
