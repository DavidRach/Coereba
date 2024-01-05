Utility_Querry <- function(BinaryFile, OriginalData, filename, outfolder, panel, starter, myFactor, normality, shape_palette, fill_palette, scalefactor, scalefactorlabel){

  Metadata <- OriginalData %>% select(!starts_with(starter))

  MyPanel <- read.csv(panel)

  Internal_Heatmap <- function(filteredCells, panel){
    MyPanel <- read.csv(panel)
    Fluorophore <- filteredCells %>% select(2:length(filteredCells)) %>% colnames()
    Fluorophore <- data.frame(Fluorophore)
    Attempt2 <- left_join(Fluorophore, MyPanel, by = "Fluorophore")
    Names <- Attempt2$Marker
    colnames(filteredCells)[2:length(filteredCells)] <- Names
    #filteredCells

    theCells <- filteredCells %>% mutate(Cluster = row_number()) %>% relocate(Cluster, .after = Identity)
    retained <- colnames(theCells[,1:2])
    MeltedCells <- reshape2::melt(theCells, id = retained)
    MeltedCells$Cluster <- factor(MeltedCells$Cluster)
    MeltedCells$variable <- factor(MeltedCells$variable)

    MyHeatmap <- ggplot(MeltedCells, aes(Cluster, variable, fill = value)) + geom_tile() + scale_fill_viridis(option = "cividis", discrete=FALSE) + theme_classic() + theme(legend.position = "none", axis.text.x = element_text(size = 5, angle = 300)) + labs(y = NULL)
    MyHeatmap
  }

  AllMarkers <- BinaryFile %>% select(!starts_with("Identity")) %>% colnames()

  for(i in AllMarkers){

    Column <- i

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
      InternalData <- OriginalData[, names(OriginalData) %in% TheInternalBypass] #Input from HeatMap filtered.
      InternalData <- as_tibble(InternalData)
      Subsetted <- InternalData %>% rowwise() %>% mutate(aggregate = sum(c_across(everything()), na.rm = TRUE))
      InternalFinal <- Subsetted %>% select(aggregate) %>% cbind(Metadata, .)
      #StarterValue <- ncol(Metadata) + 1
      EndValue <- ncol(InternalFinal)

      Behemoth <- map(names(InternalFinal)[EndValue], ~ Utility_Behemoth(data = InternalFinal, var = .x, myfactor = myFactor, normality = normality, shape_palette = shape_palette, fill_palette = fill_palette, panel = MyPanel, scalefactor = scalefactor, scalefactorlabel = scalefactorlabel))
      FinalPosBehemoth <- Behemoth[[1]] + ggtitle(ThePlotName)

      Heatmap <- Internal_Heatmap(filteredCells = Positive, panel = panel)
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
      InternalData <- OriginalData[, names(OriginalData) %in% TheInternalBypass] #Input from HeatMap filtered.
      InternalData <- as_tibble(InternalData)
      Subsetted <- InternalData %>% rowwise() %>% mutate(aggregate = sum(c_across(everything()), na.rm = TRUE))
      InternalFinal <- Subsetted %>% select(aggregate) %>% cbind(Metadata, .)
      #StarterValue <- ncol(Metadata) + 1
      EndValue <- ncol(InternalFinal)

      Behemoth <- map(names(InternalFinal)[EndValue], ~ Utility_Behemoth(data = InternalFinal, var = .x, myfactor = myFactor, normality = normality, shape_palette = shape_palette, fill_palette = fill_palette, panel = MyPanel, scalefactor = scalefactor, scalefactorlabel = scalefactorlabel))
      FinalNegBehemoth <- Behemoth[[1]] + ggtitle(ThePlotName)

      Heatmap <- Internal_Heatmap(filteredCells = Negative, panel = panel)
      FinalNegHeatmap <- Heatmap #+ ggtitle(ThePlotName)

      theNegplots <- list(FinalNegBehemoth, FinalNegHeatmap)
    } else(theNegplots <- NULL)

    theplots[[i]] <- list(thePosplots, theNegplots)
  }

  #theplots
  theflattenedplots <- purrr::flatten(theplots)
  theflattenedplots <- Filter(Negate(is.null), theflattenedplots)
  theflattestplots <- purrr::flatten(theflattenedplots)

  Utility_Patchwork(x = theflattestplots, filename = filename, outfolder = outfolder, thecolumns = 2, therows = 2)
}
