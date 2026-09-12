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
Coereba_MarkerExpressions <- function(x, theassay="ratios", 
returnType="All", CombinatorialArgs=NULL){
  
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
  #data %>% select(where(is.numeric)) %>% purrr::map_dbl(sum, na.rm = TRUE)

  tdata1 <- t(data)
  colnames(tdata1) <- TheCluster #Likely issue spot
  tdata1 <- data.frame(tdata1, check.names=FALSE)
  Dataset <- cbind(Metadata, tdata1)
  # Dataset |> mutate(RowSum = rowSums(across(where(is.numeric))))


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
  # x <- AllMarkers[1]
  SwampPuppy <- map(.x=AllMarkers, .f=Internal_Aggregate, data=Dataset,
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