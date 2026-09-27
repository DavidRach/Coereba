
#' Handles Description Digging for Dictionary Reversal going FCS to data
#' 
#' @param Coereba Either a flow/cytoframe with embedded Keywords. Alternately, the file.path to the .fcs file
#'  with the same embedded keywords
#' @param additionalMetadata Default NULL, bypass to switch '_Key' to 'Coereba_' values
#' @param specimenBypass Default NULL, handles scenarios where both specimen (from Coereba) and Specimen column
#'  and keywords are present
#' 
#' @importFrom flowCore exprs keyword
#' @importFrom dplyr select across mutate bind_cols
#' @importFrom purrr map
#' @importFrom tidyselect starts_with everything
#' 
#' @return A data.frame containing the reversed columns to be passed on to SummarizedExperiment
#' 
#' @export
#' 
#' @examples A <- 2 + 2
Coereba_FCS_Reversal <- function(Coereba, additionalMetadata=NULL, specimenBypass=FALSE){
  
  if (!class(Coereba) %in% c("flowFrame", "cytoframe")){
    if (class(Coereba) %in% "character"){
      Coereba <- load_cytoframe_from_fcs(Coereba, transformation=FALSE, truncate_max_range = FALSE)
    } else {stop("Please provide either a flowframe, cytoframe or the path to an .fcs file")}
  }

  if (specimenBypass == TRUE){
    TheKeywords <- keyword(Coereba)
    KeywordNames <- names(TheKeywords)
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
    

    NonKeyName <- names(TheKeywords[TheseKeys])[!grepl("_Key$", names(TheKeywords[TheseKeys]))]
    KeyName <- names(TheKeywords[TheseKeys])[grepl("_Key$", names(TheKeywords[TheseKeys]))]
    names(TheKeywords)[names(TheKeywords) == NonKeyName] <- sub("_Key$", "", KeyName)
    keyword(Coereba) <- TheKeywords
  }

  if (!is.null(additionalMetadata)){
    # additionalMetadata <- c("Batch", "Specimen", "Study", "ptype", "Timepoint", "name")
    TheKeywords <- keyword(Coereba)
    KeywordNames <- names(TheKeywords)
    TheseKeys <- grepl("_Key$", KeywordNames)
    names(TheKeywords)[TheseKeys] <- paste0("Coereba_", sub("_Key$", "", KeywordNames[TheseKeys]))
    keyword(Coereba) <- TheKeywords
  }

    Data <- exprs(Coereba)
    Data <- data.frame(Data, check.names=FALSE)
  
  if (!is.null(additionalMetadata)){
    TheseColumns <- names(Data) %in% additionalMetadata
    names(Data)[TheseColumns] <- paste0("Coereba_", names(Data)[TheseColumns])
  }
    
    Original <- Data |> select(!starts_with("Coereba"))
    Data <- Data |> select(starts_with("Coereba"))
    Data <- Data |> mutate(across(everything(), as.character))
    These <- colnames(Data)

    if (specimenBypass == TRUE) {
      Data$Coereba_specimen <- NULL
      These <- colnames(Data)
      #Data$Coereba_specimen |> unique()
    }

    # x <- These[3]
    # data <- Data
    Reverted <- map(.f=MetadataRetrieval, .x=These, data=Data, Coereba=Coereba)
    # map(.x=Reverted, .f=dim)
    Reverted <- Reverted |> bind_cols()
    Assembled <- cbind(Original, Reverted)
  
    return(Assembled)
}