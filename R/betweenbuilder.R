betweenbuilder <- function(x, Limit1, Limit2){
  #Credit for solving: #StackOverflow #61435048
  cells_body(columns = !!sym(x), rows = !!sym(x) >= Limit1 & !!sym(x) < Limit2)
}