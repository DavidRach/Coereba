
#' Internal for Coereba_MarkerExpressions
#'
#' @param x The two fluorophores to generate quadrants from
#' @param data The data.frame of clusters vs individual specimens, with the ratio values
#' @param binary The data.frame of markers vs clusters, with 0 and 1 values
#' @param panel The passed Panel data.frame
#'
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#'
#' @noRd
CombinatorialAggregate <- function(x, data, binary, panel){
  First <- x[1]
  Second <- x[2]

  SwampFluors <- rbind(First, Second)
  SwampFluors <- data.frame(SwampFluors) |> rename(Fluorophore = SwampFluors)
  RetainedFluors <- left_join(SwampFluors, panel, by = "Fluorophore")
  NewNames <- RetainedFluors$Marker

  Q1 <- binary |> dplyr::filter(.data[[First]] == 0 & .data[[Second]] == 1)
  Q1_Label <- paste0(NewNames[1], "-", NewNames[2], "+")
  Q1_Assembly <- DataRetrieval(x=Q1, data=data, binary=binary, Column=Q1_Label)

  Q2 <- binary |> dplyr::filter(.data[[First]] == 1 & .data[[Second]] == 1)
  Q2_Label <- paste0(NewNames[1], "+", NewNames[2], "+")
  Q2_Assembly <- DataRetrieval(x=Q2, data=data, binary=binary, Column=Q2_Label)

  Q3 <- binary |> dplyr::filter(.data[[First]] == 1 & .data[[Second]] == 0)
  Q3_Label <- paste0(NewNames[1], "+", NewNames[2], "-")
  Q3_Assembly <- DataRetrieval(x=Q3, data=data, binary=binary, Column=Q3_Label)

  Q4 <- binary |> dplyr::filter(.data[[First]] == 0 & .data[[Second]] == 0)
  Q4_Label <- paste0(NewNames[1], "-", NewNames[2], "-")
  Q4_Assembly <- DataRetrieval(x=Q4, data=data, binary=binary, Column=Q4_Label)

  SwampPuppy <- cbind(Q1_Assembly, Q2_Assembly, Q3_Assembly, Q4_Assembly)
}