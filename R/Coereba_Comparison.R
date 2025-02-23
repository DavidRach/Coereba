#' Compares two Coereba populations to find difference,
#' returning either data or a plot of the marker expressions
#' 
#' @param gs The GatingSet
#' @param gs2 Default NULL, if comparing a different GatingSet
#' @param Arg1 First Population Name
#' @param Arg2 Second Population Name, can be identical if different GatingSet
#' @param panel A data.frame or a file.path to the Panel .csv
#' @param returnType Whether to return "plot" or "data". Data default.
#' 
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' 
#' @return Either data or a ggplot2 object comparing the two populations of interest
#' 
#' @examples
#' 
#' library(Coereba)
Coereba_Comparison <- function(gs, gs2=NULL, Arg1, Arg2, panel, returnType){
  Arg1Pop <- gs_pop_get_data(gs, Arg1)
  Arg1Pop <- Arg1Pop[[1, returnType = "flowFrame"]]
  CoerebaOne <- Coereba:::Coereba_FCS_Reversal(Coereba=Arg1Pop)
  CoerebaOneSE <- Coereba_Processing(x=CoerebaOne, panel=panel)
  CoerebaOneData <- Coereba_MarkerExpressions2(x=CoerebaOneSE)
  CoerebaOneData <- CoerebaOneData |> mutate(Comparison=Arg1) |>
    relocate(Comparison, .before=1)

  if(!is.null(gs2)){Arg2Pop <- gs_pop_get_data(gs2, Arg2)
  } else {Arg2Pop <- gs_pop_get_data(gs, Arg2)}

  Arg2Pop <- Arg2Pop[[1, returnType = "flowFrame"]]
  CoerebaTwo <- Coereba:::Coereba_FCS_Reversal(Coereba=Arg2Pop)
  CoerebaTwoSE <- Coereba_Processing(x=CoerebaTwo, panel=panel)
  CoerebaTwoData <- Coereba_MarkerExpressions2(x=CoerebaTwoSE)
  CoerebaTwoData <- CoerebaTwoData |> mutate(Comparison=Arg2) |>
    relocate(Comparison, .before=1)

  Data <- rbind(CoerebaOneData, CoerebaTwoData)
  
  if (returnType == "plot"){
    ThePlot <- Utility_MarkerPlots(data=Data, panel=panel,
    myfactor="Comparison", shape_palette = NULL, fill_palette = NULL)
    return(ThePlot)
  } else {return(Data)}
}