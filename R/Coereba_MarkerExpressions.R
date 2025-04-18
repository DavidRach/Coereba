#' Generate overall marker expressions across Coereba clusters,
#'
#' @param x The Coereba Summarized Experiment object from Coereba_Processing
#' @param TheAssay Default is "ratios"
#' @param returnType Either "All" or "Combinatorial". Default is ALL.
#' @param CombinatorialArgs When returnType Combinatorial, the two fluorophores to create quadrants for.
#'
#' @importFrom S4Vectors metadata 
#' @importFrom SummarizedExperiment colData
#' @importFrom SummarizedExperiment rowData
#' @importFrom SummarizedExperiment assay 
#' @importFrom Luciernaga NameCleanUp
#' @importFrom dplyr select
#' @importFrom tidyselect starts_with
#' @importFrom dplyr pull
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' 
#' @return returns a data.frame
#' @export
#'
#' @examples
#'
#' File_Location <- system.file("extdata", package = "Coereba")
#' panelPath <- file.path(File_Location, "ILTPanelTetramer.csv")
#' 
#' files <- gs_pop_get_data(gs, "CD4")
#' MySE <- Coereba_Processing(x=files[[1]], panel=panelPath)
#'
#' All <- Coereba_MarkerExpressions2(x=MySE)
#'
#' Memory <- Coereba_MarkerExpressions2(x=MySE, returnType = "Combinatorial",
#'  CombinatorialArgs=c("BV510", "APC-Fire 750"))
#'
Coereba_MarkerExpressions2 <- function(x, theassay="ratios", 
returnType="All",CombinatorialArgs=NULL){
  
  panel <- metadata(x)$panel
  internalstrings <- c(" ", "-", "_", ".")
  Cleaned <- NameCleanUp(panel$Fluorophore, removestrings=internalstrings)
  MyPanel <- data.frame(Fluorophore=Cleaned, Marker=panel[,2],
     check.names=FALSE)

  Metadata <- colData(x)
  Metadata <- as.data.frame(Metadata@listData)
  
  binary <- rowData(x)
  binary <- as.data.frame(binary@listData)
  TheCluster <- binary |> dplyr::pull(Identity)
  AllMarkers <- binary |> select(!starts_with("Identity")) |> colnames()

  data <- assay(x, theassay)
  tdata1 <- t(data)
  colnames(tdata1) <- TheCluster
  tdata1 <- data.frame(tdata1, check.names=FALSE)
  Dataset <- cbind(Metadata, tdata1)

  if (returnType == "Combinatorial" && !is.null(CombinatorialArgs)){
    #CombinatorialArgs <- c("BV510", "APC-Fire 750")
    internalstrings <- c(" ", "-", "_", ".")
    TheCombinatorialArgs <- NameCleanUp(CombinatorialArgs, removestrings=internalstrings)
    if (length(TheCombinatorialArgs) != 2){
      stop("Combinatorial Args should be a list of two fluorophores to generate quadrants from")
    }

    SwampPuppy <- CombinatorialAggregate(x=TheCombinatorialArgs,
       data=Dataset, binary=binary, panel=MyPanel)
  }

  if (returnType == "All"){
  # Return Marker Expressions for All Markers
  SwampPuppy <- map(.x=AllMarkers, .f=.Internal_Aggregate, data=Dataset,
    binary=binary) |> bind_cols()

  # Swap out Fluorophore for Marker Names
  SwampFluors <- SwampPuppy |> colnames()
  SwampFluors <- data.frame(SwampFluors) |> rename(Fluorophore = SwampFluors)
    
  RetainedFluors <- left_join(SwampFluors, MyPanel, by = "Fluorophore")
  
  if (nrow(SwampFluors) != nrow(RetainedFluors)){
    warning("Mismatch between number of external panel and internal panel markers")
  }
    
  NewNames <- RetainedFluors$Marker
  colnames(SwampPuppy) <- NewNames
  }

  #Reattach Metadata to Summarized Marker Expressions
  MarkerExpressions <- cbind(Metadata, SwampPuppy)

  return(MarkerExpressions)
}

#' Flattens Coereba_MarkerExpressions into averaged measurement for Heatmap
#' 
#' @param x The MarkerExpressions data.frame
#' @param stats Default median, alternate is mean
#' @param TheName Character value to give the Population
#' 
#' @importFrom dplyr select
#' @importFrom tidyselect where
#' @importFrom dplyr summarise
#' @importFrom dplyr across
#' @importFrom tidyselect everything
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' 
#' @return A data.frame row of summarized data
#' 
#' @noRd
MarkerExpressionSummary <- function(x, stats="median", TheName){
  data <- x |> select(where(is.numeric))

  if (stats == "median"){
    ReturnValues <- data |>
      summarise(across(everything(), median, na.rm = TRUE)) |>
      round(digits=2) |> mutate(Population=TheName[[1]]) |>
      relocate(Population, .before=1)
  } else if (stats == "mean"){
    ReturnValues <- data |>
      summarise(across(everything(), median, na.rm = TRUE)) |>
      round(digits=2) |> mutate(Population=TheName[[1]]) |>
      relocate(Population, .before=1)
  } else {stop("Select for stats argument either mean or median")}

  return(ReturnValues)
}




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
#' @export
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
Coereba_MarkerExpressions <- function(data, binary, panel, starter, returnType="All",
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
  SwampPuppy <- map(.x=AllMarkers, .f=.Internal_Aggregate, data=data,
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

#' Internal for CombinatorialAggregate Coereba_MarkerExpressions
#'
#' @param x The two fluorophores to quadrant by
#' @param data The data.frame of clusters vs individual specimens, with the ratio values
#' @param binary The data.frame of markers vs clusters, with 0 and 1 values
#' @param Column The label name for the respective quadrant
#'
#' @importFrom tidyr as_tibble
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

#' Internal for Coereba_MarkerExpressions
#'
#' @param x Iterated Marker
#' @param data The data.frame of clusters vs individual specimens, with the ratio values
#' @param binary The data.frame of markers vs clusters, with 0 and 1 values
#'
#' @importFrom dplyr filter
#' @importFrom tidyr as_tibble
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom dplyr c_across
#' @importFrom tidyselect everything
#' @importFrom dplyr select
#' @importFrom dplyr mutate_all
#'
#' @noRd
.Internal_Aggregate <- function(x, data, binary){
  # x=AllMarkers[20]
  Column <- x

  # Retrieve Clusters that are Positive for Iterated Marker
  Positive <- binary |> dplyr::filter(.data[[Column]] == 1)

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

  #Change Name
  colnames(InternalFinal)[1] <- Column

  return(InternalFinal)
}

