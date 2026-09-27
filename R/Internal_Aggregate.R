
#' Internal for Coereba_MarkerExpressions
#'
#' @param x Iterated Marker
#' @param data The data.frame of clusters vs individual specimens, with the ratio values
#' @param binary The data.frame of markers vs clusters, with 0 and 1 values
#'
#' @importFrom dplyr filter pull select rowwise mutate c_across  mutate_all 
#' @importFrom tidyselect all_of
#' @importFrom tibble as_tibble
#' @importFrom tidyselect everything
#'
#' @noRd
Internal_Aggregate <- function(x, data, binary){
  Column <- x # The column fluorophore
  Positive <- binary |> filter(.data[[Column]] == 1) |> pull(Identity)
  TotalColumns <- length(Positive)

  #InternalData <- data |> select(all_of(Positive))
  data <- data[, !(is.na(names(data)) | names(data) == "")]

  InternalData <- data |> select(all_of(Positive))

  if (length(Positive) != ncol(InternalData)){stop("We have a column number mismatch!")}

  if(length(Positive) > 0){
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

  summary(InternalFinal)

  #Change Name
  colnames(InternalFinal)[1] <- Column

  return(InternalFinal)
}
