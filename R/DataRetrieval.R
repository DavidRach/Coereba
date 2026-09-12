#' Internal for CombinatorialAggregate Coereba_MarkerExpressions
#'
#' @param x The two fluorophores to quadrant by
#' @param data The data.frame of clusters vs individual specimens, with the ratio values
#' @param binary The data.frame of markers vs clusters, with 0 and 1 values
#' @param Column The label name for the respective quadrant
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom dplyr c_across
#' @importFrom tidyselect everything
#' @importFrom dplyr select
#' @importFrom dplyr mutate_all
#'
#' @noRd
DataRetrieval <- function(x, data, binary, Column){
  Positive <- x
    #Retrieve corresponding Clusters from data
    TheInternalBypass <- Positive$Identity
    #TheInternalBypass <- gsub("_", "", TheInternalBypass)
    #TheInternalBypass <- gsub("-", "", TheInternalBypass)
    InternalData <- data[, names(data) %in% TheInternalBypass]
    InternalData <- as_tibble(InternalData)

    if(nrow(Positive) >0){
      #Aggregate the Ratio Values
      Subsetted <- InternalData |> rowwise() |> mutate(
        aggregate = sum(c_across(everything()), na.rm = TRUE))
      InternalFinal <- Subsetted |> select(aggregate)

    } else {
      #Aggregate the Ratio Values
      Subsetted <- InternalData |> rowwise() |> mutate(
        aggregate = sum(c_across(everything()), na.rm = TRUE))
      InternalFinal <- Subsetted |> select(aggregate) |>
        mutate_all(~0) # Since No Positive Columns
    }

    colnames(InternalFinal)[1] <- Column
  return(InternalFinal)
}