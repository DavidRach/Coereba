
#' Internal for Coereba_Concatenate
#' 
#' @param x The iterated in cytoframe
#' @param metadata_cols The columns from pData to keep
#' @param inverse.transform Whether to reverse transformation present
#' 
#' @importFrom dplyr select across mutate bind_cols cross_join
#' @importFrom tidyselect all_of starts_with everything
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom flowCore exprs
#' @importFrom Biobase pData
#' @importFrom purrr map
#' 
#' @return A back-translated keyword column for Coereba
#' 
#' @noRd
Coereba_SingleFrame_Reversal <- function(x, metadata_cols,
   inverse.transform){

  InternalPD <- pData(x)
  TheMetadata <- InternalPD |> select(all_of(metadata_cols))
  row.names(TheMetadata) <- NULL
  CoerebaCS <- gs_pop_get_data(x, inverse.transform=inverse.transform)
  CoerebaCF <- CoerebaCS[[1]]

  if (class(CoerebaCF) %in% "cytoframe"){
    Data <- exprs(CoerebaCF)
    Data <- data.frame(Data, check.names=FALSE)
    Original <- Data |> select(!starts_with("Coereba"))
    Data <- Data |> select(starts_with("Coereba"))
    Data <- Data |> mutate(
      across(c(Coereba_Cluster, Coereba_specimen), round))

    Data <- Data |> mutate(across(everything(), as.character))
    These <- colnames(Data)
    # x <- These[1]
    Reverted <- map(.f=MetadataRetrieval, .x=These,
       data=Data, Coereba=CoerebaCF) |> bind_cols()
    Assembled <- cbind(Original, Reverted)
  
    Completed <- Assembled |> cross_join(TheMetadata)
    return(Completed)

  } else {stop("Not a Cytoframe")}
}
