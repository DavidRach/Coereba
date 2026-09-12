#' A bloody internal function that isn't worth all these fing issues it keeps causing
#' 
#' @param x A value to be rounder
#' @param showClose A work around to showing values between 0.05 vs 0.1 instead of ns
#' 
#' @return A rounded p-value
#' 
#' @noRd
pval_mold <- function(x, showClose=TRUE){

  if (showClose==TRUE){
  if (x > 0.10){"n.s"} else if (x > 0.01) {round(x, 2)} else if (x > 0.001) {
    round(x, 3)} else if (x > 0.0001) {round(x, 4)} else if (x > 0.00001) {
      round(x, 5)} else if (x > 0.000001) {round(x, 6)} else{return(x)}
  } else {
  if (x > 0.05){"n.s"} else if (x > 0.01) {round(x, 2)} else if (x > 0.001) {
      round(x, 3)} else if (x > 0.0001) {round(x, 4)} else if (x > 0.00001) {
      round(x, 5)} else if (x > 0.000001) {round(x, 6)} else{return(x)}
  } 
}