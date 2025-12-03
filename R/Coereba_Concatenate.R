#' Processes multiple Coereba flowframes, retrieves contents according to our Concatenate criteria, then cumulatively processes
#' 
#' @param Set The flow/cyto set we are working with
#' @param metadata_cols The columns from pData to retain. 
#' @param outpath Where to store the .fcs file
#' @param filename What to call the .fcs file
#' 
#' @export
#' 
#' @examples A <- 2+2
Coereba_Concatenate <- function(Set, metadata_cols, outpath=NULL, filename="CombinedCoerebaFile"){ 

  AllData <- map(.x=Set, .f=Coereba_SingleFrame_Reversal, metadata_cols=metadata_cols) |> bind_rows()

  if (is.null(outpath)){outpath <- getwd()}
  
  FCSFile <- Coereba_FCSExport(data=AllData, gs=Set[1],
        returnType="fcs", outpath=outpath, filename=filename,
        nameAppend="", Aggregate=FALSE, coerebaCombine =TRUE)
}

Coereba_SingleFrame_Reversal <- function(x, metadata_cols){

  InternalPD <- pData(x)
  TheMetadata <- InternalPD |> select(all_of(metadata_cols))
  row.names(TheMetadata) <- NULL
  CoerebaCS <- gs_pop_get_data(x)
  CoerebaCF <- CoerebaCS[[1]]

  if (class(CoerebaCF) %in% "cytoframe"){
    Data <- exprs(CoerebaCF)
    Data <- data.frame(Data, check.names=FALSE)
    Original <- Data |> select(!starts_with("Coereba"))
    Data <- Data |> select(starts_with("Coereba"))
    Data <- Data |> mutate(across(everything(), as.character))
    These <- colnames(Data)
    Reverted <- map(.f=MetadataRetrieval, .x=These, data=Data, Coereba=CoerebaCF) |> bind_cols()
    Assembled <- cbind(Original, Reverted)
  
    Completed <- Assembled |> cross_join(TheMetadata)
    return(Completed)

  } else {stop("Not a Cytoframe")}
}


