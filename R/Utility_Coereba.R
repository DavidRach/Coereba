#' Runs Coereba for Dichotomized Gating Annotation
#'
#' @param x A Gating Set object
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
#'
#' @importFrom utils read.csv
#' @importFrom Luciernaga NameCleanUp
#' @importFrom flowCore keyword
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom flowCore exprs
#' @importFrom dplyr slice_sample
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom dplyr case_when
#' @importFrom dplyr left_join
#' @importFrom dplyr relocate
#'
#' @return A data.frame
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
#' CoerebaIDs <- Utility_Coereba(x=UnmixedGatingSet[1], subsets="live",
#'  sample.name="GROUPNAME", reference=TheCSV, starter="Spark Blue 550-A")
#'
#'
Utility_Coereba <- function(x, subsets, sample.name, subsample = NULL, columns=NULL,
                            notcolumns=NULL, reference, starter,
                            inverse.transform = TRUE){

  if (!is.data.frame(reference)){
    ReferenceLines <- read.csv(reference, check.names = FALSE)
    } else {ReferenceLines <- reference}

  internalstrings <- c("Comp-", "-A", "-", " ", ".")
  colnames(ReferenceLines) <- NameCleanUp(colnames(ReferenceLines),
                                          removestrings = internalstrings)
  colnames(ReferenceLines)[1] <- sample.name
  New <- ReferenceLines

  name <- keyword(x, sample.name)
  # Needs to match the reference file naming convention for matching

  ff <- gs_pop_get_data(x, subsets, inverse.transform = FALSE)
  inversed_ff <- gs_pop_get_data(x, subsets, inverse.transform = TRUE)

  startingcells <- RowWorkAround(ff)
  DF <- as.data.frame(exprs(ff[[1]]), check.names = FALSE)

  if(!is.null(subsample)){DF <- slice_sample(DF, n = subsample,
                                             replace = FALSE)
    startingcells <- nrow(DF)
  } else{DF <- DF}

  # Saving Columns for future column reordering
  OriginalColumnsVector <- colnames(DF)
  OriginalColumns <- colnames(DF)
  OriginalColumns <- data.frame(OriginalColumns)
  OriginalColumnsIndex <- OriginalColumns %>% mutate(IndexLocation = 1:nrow(.))

  # Adding Backups for future row reordering
  Backups <- DF %>% mutate(Backups = 1:nrow(DF)) %>% select(Backups)

  #Stashing Away Time FSC SSC For Later Use
  StashedDF <- DF[,grep("Time|FS|SC|SS|Original|W$|H$", names(DF))]
  StashedDF <- cbind(Backups, StashedDF)

  #Consolidating Columns Going Forward
  CleanedDF <- DF[,-grep("Time|FS|SC|SS|Original|W$|H$", names(DF))]
  BackupNames <- colnames(CleanedDF)

  # If external columns interest specified
  if (!is.null(columns) && !is.null(notcolumns)){
    stop("Please select either columns or not columns, not both")
    }

  if (!is.null(columns)){dsf <- CleanedDF %>% select(all_of(columns))
  } else {dsf <- CleanedDF}

  if (!is.null(notcolumns)){dsf <- CleanedDF %>% select(-all_of(columns))
  } else {dsf <- dsf}

  NamingColBackup <- colnames(dsf)
  colnames(dsf) <- NameCleanUp(colnames(dsf), removestrings = internalstrings)
  starter <- NameCleanUp(starter, removestrings = internalstrings)

  decimal_places <- nchar(sub("\\d+\\.", "", as.character(startingcells)))

  My.Data <- dsf
  Columns <- colnames(My.Data)
  Columns <- Columns[!Columns == starter]
  Columns <- Columns[!Columns == "AF"]

  New1 <- New %>% dplyr::filter(.data[[sample.name]] %in% name)

  # Starting the Cluster Name
  MyNewestData <- My.Data %>% mutate(Cluster = case_when(
    My.Data[[starter]] < New1[New1[[sample.name]] == name, starter] ~ paste(
      starter, "neg", sep = "", collapse = NULL),
    My.Data[[starter]] > New1[New1[[sample.name]] == name, starter] ~ paste(
      starter, "pos", sep = "", collapse = NULL)))

  # Expanding the Cluster Name
  for(i in Columns) {MyNewestData <- MyNewestData %>%
    mutate(Cluster = case_when(
      MyNewestData[[i]] < New1[New1[[sample.name]] == name, i] ~
        paste(MyNewestData$Cluster, i, "neg", sep = ""),
      MyNewestData[[i]] > New1[New1[[sample.name]] == name, i] ~
        paste(MyNewestData$Cluster, i, "pos", sep = "")
    ))
  }

  if (inverse.transform == FALSE){
  Reordering <- MyNewestData
  NamingColBackup <- c(NamingColBackup, "Cluster")
  colnames(Reordering) <- NamingColBackup
  Reordering <- cbind(Backups, Reordering)
  Reintegrated <- left_join(Reordering, StashedDF, by = "Backups")
  BackupsCol <- "Backups"
  DesiredOrder <- c("Backups", OriginalColumnsVector, "Cluster")
  Reintegrated1 <- Reintegrated %>% relocate(all_of(DesiredOrder)) %>% select(-Backups)
  Reintegrated1 <- Reintegrated1 %>% mutate(specimen = name)
  }

  if (inverse.transform == TRUE && is.null(subsample)) {
    Cluster <- MyNewestData %>% select(Cluster)
    DF <- as.data.frame(exprs(inversed_ff[[1]]), check.names = FALSE)
    Reintegrated1 <- cbind(DF, Cluster)
    Reintegrated1 <- Reintegrated1 %>% mutate(specimen = name)
  }

  return(Reintegrated1)
}



#' @noRd
#' @importFrom BiocGenerics nrow

RowWorkAround <- function(x){
  TheNumberRows <- nrow(x)[[1]] # For Ratio
  return(TheNumberRows)
}
