#' Plot for Pearson/Spearman
#'
#' @param data A data.frame object containing your data
#' @param varX The column of variables to plot on the X axis
#' @param varY The column of variables to plot on the y axis
#' @param myfactor The column containing factor you want to compare
#' @param mymethod Choice of "pearson" or "spearman"
#'
#' @return NULL
#' @export
#'
#' @examples NULL
Utility_AssociationNotAutomaticallyCausation <- function(data, varX, varY, myfactor, mymethod){
  result <- data %>% group_by(.data[[myfactor]]) %>% do(tidy(cor.test(.[[varX]], .[[varY]], method = mymethod))) %>% ungroup()
  Group <- result %>% select(ptype)
  p.value <- result$p.value
  method <- result$method
  theresults <- cbind(varX, varY, Group, p.value, method)
}
