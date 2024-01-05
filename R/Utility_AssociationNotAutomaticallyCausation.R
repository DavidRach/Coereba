Utility_AssociationNotAutomaticallyCausation <- function(data, varX, varY, myfactor, mymethod){
  result <- data %>% group_by(.data[[myfactor]]) %>% do(tidy(cor.test(.[[varX]], .[[varY]], method = mymethod))) %>% ungroup()
  Group <- result %>% select(ptype)
  p.value <- result$p.value
  method <- result$method
  theresults <- cbind(varX, varY, Group, p.value, method)
}
