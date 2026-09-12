
#' Generate overall marker expressions across Coereba clusters,
#'
#' @param data The data.frame of clusters vs individual specimens, with the ratio values
#' @param binary The data.frame of markers vs clusters, with 0 and 1 values
#' @param panel A .csv or data.frame containing Fluorophore and Marker columns of your panel markers.
#' @param starter A string containing the starting characters for all Coereba cluster names
#' @param returnType Either "All" or "Combinatorial". Default is ALL.
#' @param CombinatorialArgs When returnType Combinatorial, the two fluorophores to create quadrants for.
#'
#' @importFrom utils read.csv
#' @importFrom dplyr select
#' @importFrom tidyselect starts_with
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom Luciernaga NameCleanUp
#'
#' @return returns a data.frame
#' @noRd
#'
#' @examples
#'
#' File_Location <- system.file("extdata", package = "Coereba")
#' panelPath <- file.path(File_Location, "ILTPanelTetramer.csv")
#' panelData <- read.csv(panelPath, check.names=FALSE)
#' binaryPath <- file.path(File_Location, "HeatmapExample.csv")
#' binaryData <- read.csv(binaryPath, check.names=FALSE)
#' dataPath <- file.path(File_Location, "ReadyFileExample.csv")
#' dataData <- read.csv(dataPath, check.names=FALSE)
#'
#' All <- Coereba_MarkerExpressions(data=dataData, binary=binaryData,
#'  panel=panelData, starter="SparkBlue550")
#'
#' Memory <- Coereba_MarkerExpressions(data=dataData, binary=binaryData,
#'  panel=panelData, starter="SparkBlue550", returnType = "Combinatorial",
#'  CombinatorialArgs=c("BV510", "APC-Fire 750"))
#'
Coereba_MarkerExpressionsDeprecated <- function(data, binary, panel, starter, returnType="All",
  CombinatorialArgs=NULL){

  if (!is.data.frame(panel)) {MyPanel <- read.csv(panel, check.names = FALSE)
  } else {MyPanel <- panel}

  Metadata <- data %>% select(!starts_with(starter))

  AllMarkers <- binary %>% select(!starts_with("Identity")) %>% colnames()
  #x <- AllMarkers[1]

  if (returnType == "Combinatorial" && !is.null(CombinatorialArgs)){
    #CombinatorialArgs <- c("BV510", "APC-Fire 750")
    internalstrings <- c(" ", "-", "_", ".")
    TheCombinatorialArgs <- NameCleanUp(CombinatorialArgs, removestrings=internalstrings)
    if (length(TheCombinatorialArgs) != 2){
      stop("Combinatorial Args should be a list of two fluorophores to generate quadrants from")
    }

    SwampPuppy <-CombinatorialAggregate(x=TheCombinatorialArgs, data=data, binary=binary, panel=MyPanel)
  }

  if (returnType == "All"){
  # Return Marker Expressions for All Markers
  SwampPuppy <- map(.x=AllMarkers, .f=Internal_Aggregate, data=data,
    binary=binary) %>% bind_cols()

  # Swap out Fluorophore for Marker Names
  SwampFluors <- SwampPuppy %>% colnames()
  SwampFluors <- data.frame(SwampFluors) %>% rename(Fluorophore = SwampFluors)
  RetainedFluors <- left_join(SwampFluors, MyPanel, by = "Fluorophore")
  NewNames <- RetainedFluors$Marker
  colnames(SwampPuppy) <- NewNames
  }

  #Reattach Metadata to Summarized Marker Expressions
  MarkerExpressions <- cbind(Metadata, SwampPuppy)

  return(MarkerExpressions)
  }