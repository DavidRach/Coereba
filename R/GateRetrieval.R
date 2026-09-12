
#' Internal for Coereba_GateCutoffs
#'
#' @param x The passed gate corresponding fluorophore splitpoint
#' @param gs The iterated on GatingSet object
#'
#' @importFrom flowWorkspace gs_pop_get_gate
#'
#' @noRd
GateRetrieval <- function(x, gs){
  ReturnValue <- gs_pop_get_gate(gs, x)
  Cutoff <- ReturnValue[[1]]@min
  Cutoff <- data.frame(Cutoff, check.names=FALSE)
}
