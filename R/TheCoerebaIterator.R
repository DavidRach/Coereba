
#' Internal Utility_Coereba, processes negative from positive rows
#' 
#' @param x The individual fluorophore being iterated on
#' @param data The processed exprs data 
#' @param reference The processed reference splitpoint object
#' @param sample.name Used to identify individual (row)
#' @param name Used to identify fluorophore (column)
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' 
#' @return A column of splitpoint designations to be later combined to form ClusterID
#' 
#' @noRd
TheCoerebaIterator <- function(x, data, reference, sample.name, name){
  Internal <- data |> mutate(Cluster = case_when(
    data[[x]] < reference[reference[[sample.name]] == name, x] ~
      paste(x, "neg", sep=""),
    data[[x]] >= reference[reference[[sample.name]] == name, x] ~
      paste(x, "pos", sep="")))
  Internal <- Internal |> select(Cluster)
  colnames(Internal) <- paste("Cluster", x, sep="_")
  return(Internal)
}
