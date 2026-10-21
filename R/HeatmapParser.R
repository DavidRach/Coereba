#' Parses a list of .csv returns from Coereba_MarkerExpressions
#' 
#' @param x A file path to the .csv file
#' @param y The desired name
#' 
#' @importFrom utils read.csv
#' @importFrom dplyr select mutate relocate
#' 
#' 
HeatmapParserInternal <- function(x, y){
  Data <- read.csv(x, check.names=FALSE)
  Data <- Data |> select(where(is.numeric)) |>
    summarise(across(everything(), \(v) median(v, na.rm = TRUE))) |> 
    mutate(Cluster=as.character(y)) |> relocate(Cluster, .before=1)
  return(Data)
}


#' Parses a list of .csv returns from Coereba_MarkerExpressions
#' 
#' @param x A file path to the .csv file
#' @param y The desired name
#' @param RemoveMarkers Names of columns to remove
#' @param MarkerOrder Column names in order of desired order. 
#' 
#' @importFrom purrr map2
#' @importFrom dplyr bind_rows
#' 
#' @export
HeatmapParser <- function(x, y, RemoveMarkers, MarkerOrder){
  Dataset <- map2(.x=x, .y=y, .f=HeatmapParserInternal) |> bind_rows()
  Table <- Coereba:::Coereba_Heatmap(data=Dataset, RemoveMarkers=RemoveMarkers, MarkerOrder=MarkerOrder)
  return(Table)
}