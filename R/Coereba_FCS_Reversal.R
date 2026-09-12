
#' Handles Description Digging for Dictionary Reversal going FCS to data
#' 
#' @param Coereba Either a flow/cytoframe with embedded Keywords. Alternately, the file.path to the .fcs file
#'  with the same embedded keywords
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
Coereba_FCS_Reversal <- function(Coereba){
  
  if (!class(Coereba) %in% c("flowFrame", "cytoframe")){
    if (class(Coereba) %in% "character"){
      Coereba <- load_cytoframe_from_fcs(Coereba, transformation=FALSE, truncate_max_range = FALSE)
    } else {stop("Please provide either a flowframe, cytoframe or the path to an .fcs file")}
  }

    Data <- exprs(Coereba)
    Data <- data.frame(Data, check.names=FALSE)
    Original <- Data |> select(!starts_with("Coereba"))
    Data <- Data |> select(starts_with("Coereba"))
    Data <- Data |> mutate(across(everything(), as.character))
    These <- colnames(Data)
    These <- These[These != "Coereba_Specimen"] # Ugh...

    # x <- These[2]
    # data <- Data
    Reverted <- map(.f=MetadataRetrieval, .x=These, data=Data, Coereba=Coereba)
    Reverted <- Reverted |> bind_cols()
    Assembled <- cbind(Original, Reverted)
  
    return(Assembled)
}