
#' Internal for Coereba_GateCutoffs
#'
#' @param x The particular Fluorophore to be gated
#' @param data The data.frame containing the openCyto gating template
#' @param gs The iterated on GatingSet object
#' @param GateOverwrite Whether when an existing gate is present, to delete and recreate it. 
#'
#' @importFrom dplyr filter
#' @importFrom flowWorkspace gs_get_pop_paths gs_pop_remove
#' @importFrom stringr str_detect
#' @importFrom openCyto gs_add_gating_method
#'
#' @noRd
GateExecution <- function(x, data, gs, GateOverwrite){
  Data <- data %>% dplyr::filter(alias %in% x)
  if (nrow(Data) != 1){stop("Too many rows at GateFilter")}

  ExistingGates <- gs_get_pop_paths(gs, path="auto")
  #ExistingGates <- gs_get_leaf_nodes(gs)

  # if (Data[[1,6]] == ""){message("No gating_args")}

  if(!any(str_detect(ExistingGates, x))){

    suppressMessages(
      gs_add_gating_method(gs, alias = Data[[1,1]], pop = "+", parent = Data[[1,3]], 
                           dims = Data[[1,4]], gating_method = "gate_mindensity",
                           gating_args=Data[[1,6]])
    )
  } else {
    if (GateOverwrite == TRUE){
    gs_pop_remove(gs, x, recompute = TRUE, recursive = TRUE)      
    suppressMessages(
      gs_add_gating_method(gs, alias = Data[[1,1]], pop = "+", parent = Data[[1,3]], 
                           dims = Data[[1,4]], gating_method = "gate_mindensity",
                           gating_args=Data[[1,6]])
    )      
    } else { Harry <- "Youre a Wizard"}
  }
}