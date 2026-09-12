revbuilder <- function(x, Limit){
  #Credit for solving: #StackOverflow #61435048
  cells_body(columns = !!sym(x), rows = !!sym(x) < Limit)
}