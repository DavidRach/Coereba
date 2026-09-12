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







