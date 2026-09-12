#' Runs Coereba for Dichotomized Gating Annotation
#'
#' @param gs A Gating Set object
#' @param subsets The desired GatingHierarchy subset
#' @param sample.name Keyword for sample name
#' @param subsample An optional downsample, doesn't work with inverse.transform=TRUE
#' @param columns An optional way to select columns to keep.
#' @param notcolumns An optional way to remove select columns.
#' @param reference The external data.frame or path to the .csv with the
#' specified gate split points by specimen and marker.
#' @param starter The column name to start the splits with
#' @param inverse.transform Whether to reverse the data transform after Coereba cluster
#'  is calculated, Default is set to TRUE to allow for .fcs export.
#' @param returnType Whether to return data, flowframe or fcs
#' @param Individual Default FALSE, when TRUE, returns individual fcs instead of grouped. 
#' @param outpath Default NULL, file.path for fcs file storage
#' @param filename Default NULL, sets name for aggregated flowframe or fcs
#' @param nameAppend For flowframe and fcs returnType, what gets appended before .fcs 
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows slice_head arrange desc
#'
#' @return Either "data", "flowframe" or "fcs", individually or concatinated
#' @export
#'
#' @examples
#'
#' library(flowCore)
#' library(flowWorkspace)
#' library(openCyto)
#' library(data.table)
#'
#' File_Location <- system.file("extdata", package = "Coereba")
#' TheCSV <- file.path(File_Location, "GateCutoffsForNKs.csv")
#'
#' FCS_Files <- list.files(path = File_Location, pattern = ".fcs", full.names = TRUE)
#' UnmixedFCSFiles <- FCS_Files[1]
#' UnmixedCytoSet <- load_cytoset_from_fcs(UnmixedFCSFiles,
#'  truncate_max_range = FALSE, transformation = FALSE)
#' UnmixedGatingSet <- GatingSet(UnmixedCytoSet)
#' Markers <- colnames(UnmixedCytoSet[[1]])
#' KeptMarkers <- Markers[-grep("Time|FS|SC|SS|Original|-W$|-H$|AF", Markers)]
#' biex_transform <- flowjo_biexp_trans(channelRange = 256, maxValue = 1000000,
#'  pos = 4.5, neg = 0, widthBasis = -1000)
#' TransformList <- transformerList(KeptMarkers, biex_transform)
#' flowWorkspace::transform(UnmixedGatingSet, TransformList)
#' UnmixedGates <- fread(file.path(path = File_Location,
#'  pattern = 'GatesUnmixed.csv'))
#' UnmixedGating <- gatingTemplate(UnmixedGates)
#' gt_gating(UnmixedGating, UnmixedGatingSet)
#'
#' CoerebaIDs <- Utility_Coereba(gs=UnmixedGatingSet[1], subsets="live",
#'  sample.name="GROUPNAME", reference=TheCSV, starter="Spark Blue 550-A",
#'  returnType="flowframe", Individual=TRUE)
#'
Utility_Coereba <- function(gs, subsets, sample.name, subsample = NULL, columns=NULL,
  notcolumns=NULL, reference, starter, inverse.transform = TRUE, returnType,
  Individual=FALSE, outpath=NULL, filename=NULL, nameAppend=NULL){
  
  # x <- gs[1]
  Data <- map(.x=gs, .f=Internal_Coereba, subsets=subsets,
     sample.name=sample.name, subsample=subsample, columns=columns,
    notcolumns=notcolumns, reference=reference, starter=starter,
    inverse.transform = inverse.transform, returnType=returnType,
    Individual=Individual)
  
  if (returnType == "data" && Individual == TRUE){return(Data)}

  if (returnType == "flowframe" && Individual == TRUE){return(Data)}

  if (length(Data) > 1){Data1 <- bind_rows(Data)
    } else {Data1 <- Data}
  
  data.frame(table(Data1$Cluster)) |> arrange(desc(Freq)) |> slice_head(n=3)
  
  if (returnType == "data" && Individual == FALSE){return(Data1)}

  if (returnType == "flowframe" && Individual == FALSE){
    message("Returning aggregated flowframe")}
  
    FlowFrame <- Coereba_FCSExport(data=Data1, gs=gs[1],
      returnType=returnType, outpath=outpath, filename=filename,
      nameAppend=nameAppend, Aggregate=TRUE)
    return(FlowFrame)
  
  if (returnType == "fcs" && Individual == FALSE){
      message("Returning aggregated fcs file")
    
    FCSFile <- Coereba_FCSExport(data=Data1, gs=gs[1],
      returnType=returnType, outpath=outpath, filename=filename,
      nameAppend=nameAppend, Aggregate=TRUE)
    
    message("FCS file exported")
  }

}

  