#' Internal for Coereba_GateCutoffs
#'
#' @param x A fluorophore name
#' @param data The data.frame row of to-be-modified gating template
#'
#' @noRd
GateTemplateAssembly <- function(x, data){
  data[1,1] <- x
  data[1,4] <- x
  return(data)
}