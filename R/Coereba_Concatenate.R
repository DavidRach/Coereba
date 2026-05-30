#' Processes multiple Coereba flowframes, retrieves contents according to our Concatenate criteria, then cumulatively processes
#' 
#' @param Set The flow/cyto set we are working with
#' @param metadata_cols The columns from pData to retain.
#' @param inverse.transform Whether to reverse transform
#'  the exprs matrix, default is TRUE. 
#' @param outpath Where to store the .fcs file
#' @param filename What to call the .fcs file
#' @param returnType Whether to return "fcs", "data" or "flowframe"
#' 
#' @export
#' 
#' @examples A <- 2+2
Coereba_Concatenate <- function(Set, metadata_cols,
  inverse.transform=TRUE, outpath=NULL,
  filename="CombinedCoerebaFile", returnType="fcs"){ 

  # x <- Set[1]
  AllData <- map(.x=Set, .f=Coereba_SingleFrame_Reversal,
     metadata_cols=metadata_cols,
     inverse.transform=inverse.transform) |> bind_rows()

  if (returnType=="fcs"){
    if (is.null(outpath)){outpath <- getwd()}
    
    FCSFile <- Coereba_FCSExport(data=AllData, gs=Set[1],
          returnType="fcs", outpath=outpath, filename=filename,
          nameAppend="", Aggregate=FALSE, coerebaCombine =TRUE)
  } else if (returnType == "data"){
    return(AllData)
  } else {
    flowFrame <- Coereba_FCSExport(data=AllData, gs=Set[1],
          returnType="flowframe", outpath=outpath, filename=filename,
          nameAppend="", Aggregate=FALSE, coerebaCombine =TRUE)
    return(flowFrame)
    }
}






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


