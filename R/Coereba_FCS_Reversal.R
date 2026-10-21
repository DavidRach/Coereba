
#' Handles Description Digging for Dictionary Reversal going FCS to data
#' 
#' @param Coereba Either a flow/cytoframe with embedded Keywords. Alternately, the file.path to the .fcs file
#'  with the same embedded keywords
#' @param keywordConvert Default NULL, bypass to switch '_Key' to 'Coereba_' values
#' @param specimenBypass Default NULL, handles scenarios where both specimen (from Coereba) and Specimen column
#'  and keywords are present
#' 
#' @importFrom flowCore exprs keyword read.FCS keyword<-
#' @importFrom dplyr select across mutate bind_cols
#' @importFrom purrr map
#' @importFrom tidyselect starts_with everything
#' 
#' @return A data.frame containing the reversed columns to be passed on to SummarizedExperiment
#' 
#' @export
#' 
#' @examples A <- 2 + 2
Coereba_FCS_Reversal <- function(Coereba, keywordConvert=NULL, specimenBypass=FALSE){
  
  if (!class(Coereba) %in% c("flowFrame", "cytoframe")){
    if (class(Coereba) %in% "character"){
      Coereba <- load_cytoframe_from_fcs(Coereba, transformation=FALSE, truncate_max_range = FALSE)
      #Coereba <- flowCore::read.FCS(Coereba, transformation=FALSE, truncate_max_range = FALSE)
    } else {stop("Please provide either a flowframe, cytoframe or the path to an .fcs file")}
  }

  TheKeywords <- keyword(Coereba)
  KeywordNames <- names(TheKeywords)

  if (specimenBypass == TRUE){
    TheseKeys <- grepl("pecimen", KeywordNames)
    # TheKeywords[TheseKeys]
    Lens <- sapply(TheKeywords[TheseKeys], function(x) length(strsplit(x, "\\s+")[[1]]))
    CommonLen <- as.numeric(names(sort(table(Lens), decreasing = TRUE))[1])
    Subset <- TheKeywords[TheseKeys][Lens == CommonLen]
    DropNames <- names(Subset)[!(names(TheKeywords[TheseKeys]) %in% names(Subset))]
    DropNames <- names(Lens)[Lens != CommonLen]
    TheKeywords[DropNames] <- NULL

    KeywordNames <- names(TheKeywords)
    TheseKeys <- grepl("pecimen", KeywordNames)
    # TheKeywords[TheseKeys]
    keyword(Coereba) <- TheKeywords

    #NonKeyName <- names(TheKeywords[TheseKeys])[!grepl("_Key$", names(TheKeywords[TheseKeys]))]
    #KeyName <- names(TheKeywords[TheseKeys])[grepl("_Key$", names(TheKeywords[TheseKeys]))]
    #names(TheKeywords)[names(TheKeywords) == NonKeyName] <- sub("_Key$", "", KeyName)
  }

  if (!is.null(keywordConvert)){
    # additionalMetadata <- c("Batch", "Specimen", "Study", "ptype", "Timepoint", "name")
    TheKeywords <- keyword(Coereba)
    KeywordNames <- names(TheKeywords)
    TheseKeys <- grepl("_Key$", KeywordNames)
    names(TheKeywords)[TheseKeys] <- paste0("Coereba_", sub("_Key$", "", KeywordNames[TheseKeys]))
    keyword(Coereba) <- TheKeywords
  }

    Data <- exprs(Coereba)
    Data <- data.frame(Data, check.names=FALSE)
    Data <- Data |> mutate(across(starts_with("Coereba_"), ~ as.integer(round(.x))))
  
  if (!is.null(keywordConvert)){
    TheseColumns <- names(Data) %in% keywordConvert
    names(Data)[TheseColumns] <- paste0("Coereba_", names(Data)[TheseColumns])
  }
    
    Original <- Data |> select(!starts_with("Coereba"))
    Data <- Data |> select(starts_with("Coereba"))
    Data <- Data |> mutate(across(everything(), as.character))
    # View(Data)
    These <- colnames(Data)
  
    YesThese <- These[These %in% KeywordNames]
    NotThese <- These[!These %in% KeywordNames]
  
     if (specimenBypass == TRUE){
    Data$Coereba_specimen <- NULL
    Data <- Data |> rename("Coereba_specimen"="Coereba_Specimen")
    # colnames(Data)
    
    }
  
    # x <- YesThese[1]
    # data <- Data
    Reverted <- map(.f=MetadataRetrieval, .x=YesThese, data=Data, Coereba=Coereba)
    # View(Reverted[[1]])
    # map(.x=Reverted, .f=dim)
    Reverted <- Reverted |> bind_cols()
    Assembled <- cbind(Original, Reverted)
    # View(Assembled)
  
    return(Assembled)
}