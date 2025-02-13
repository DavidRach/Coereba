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
  
  # Bringing in splitpoint and clean column names
  if (!is.data.frame(reference)){
    ReferenceLines <- read.csv(reference, check.names = FALSE)
    } else {ReferenceLines <- reference}
  
  internalstrings <- c("Comp-", "-A", "-", " ", ".")
  colnames(ReferenceLines) <- NameCleanUp(colnames(ReferenceLines),
                                          removestrings = internalstrings)
  
  # Checking for specimen identifier consistency
  if (sample.name != colnames(ReferenceLines)[[1]]){
    message("sample.name does not match the specimen identifier name found in reference,
     converting over")
    colnames(ReferenceLines)[1] <- sample.name
  }

  # Internal switch in naming convention
  New <- ReferenceLines
  
  # Retrieving a name, needs to match that in reference file naming convention.
  name <- keyword(x, sample.name)

  Specimens <- ReferenceLines %>% pull(.data[[sample.name]])

  if (!name[[1]] %in% Specimens){
    message("sample.name ", name, " not recognized among identification names found in reference file,
    returning NULL instead of a data.frame, if iterating with map use compact instead of bind_rows to remove")
    Reintegrated1 <- NULL
    return(Reintegrated1)
  }

  # Preparing if sending back to .fcs
  if (inverse.transform == TRUE){
  inversed_ff <- gs_pop_get_data(x, subsets, inverse.transform = TRUE)
  flippedDF <- as.data.frame(exprs(inversed_ff[[1]]), check.names = FALSE)
  flippedDF <- flippedDF |> mutate(Backups = 1:nrow(DF)) |> 
    relocate(Backups, .before=1)
  }

  # Retrieving Data for Coereba assignment
  ff <- gs_pop_get_data(x, subsets, inverse.transform = FALSE)

  # Extracting Data
  startingcells <- RowWorkAround(ff)
  DF <- as.data.frame(exprs(ff[[1]]), check.names = FALSE)

  # Saving Columns for future column reordering
  OriginalColumnsVector <- colnames(DF)

  # Adding Backups for future row reordering
  DF <- DF |> mutate(Backups = 1:nrow(DF)) 

  if (inverse.transform == TRUE){
    if (nrow(ff)[[1]] != nrow(inversed_ff)[[1]]){
      stop("Mismatched number of rows, contact Maintainer")
    }
  }

  # Optional Downsampling
  if(!is.null(subsample)){DF <- slice_sample(DF, n = subsample,
                                             replace = FALSE)
    startingcells <- nrow(DF)
  } else{DF <- DF}

  # Identifying retained cells
  Backups <- DF |> select(Backups)

  #Stashing Away FSC SSC For Later
  StashedDF <- DF[,grep("Time|FS|SC|SS|Original|W$|H$", names(DF))]
  StashedDF <- StashedDF |> relocate(Backups, .before=1)

  #Consolidating Columns Going Forward
  CleanedDF <- DF[,-grep("Time|FS|SC|SS|Original|W$|H$", names(DF))]
  BackupNames <- colnames(CleanedDF)
  CleanedDF <- CleanedDF |> select(-Backups)

  # If external columns interest specified
  if (!is.null(columns) && !is.null(notcolumns)){
    stop("Please select either columns (to keep) or notcolumns (to exclude).
    Leave the other agument as NULL")
    }

  if (!is.null(columns)){dsf <- CleanedDF %>% select(all_of(columns))
  } else {dsf <- CleanedDF}

  if (!is.null(notcolumns)){dsf <- CleanedDF %>% select(-all_of(columns))
  } else {dsf <- dsf}

  NamingColBackup <- colnames(dsf)

  # Final Column Name Clean Up On Data Side
  colnames(dsf) <- NameCleanUp(colnames(dsf), removestrings = internalstrings)
  starter <- NameCleanUp(starter, removestrings = internalstrings)

  # Name Swap and getting column vector ready.
  My.Data <- dsf
  Columns <- colnames(My.Data)
  Columns <- Columns[!Columns == starter]
  Columns <- Columns[!Columns == "AF"]

  New1 <- New |> dplyr::filter(.data[[sample.name]] %in% name)

  if (nrow(New1) != 1){
    warning("Multiple rows being iterated on for ", name, ", check sample.name and reference to ensure 
  that the naming convention matches only a single specimen")
  }

  # Starting the Cluster Name
  MyNewestData <- My.Data |> mutate(Cluster = case_when(
    My.Data[[starter]] < New1[New1[[sample.name]] == name, starter] ~ paste(
      starter, "neg", sep = "", collapse = NULL),
    My.Data[[starter]] > New1[New1[[sample.name]] == name, starter] ~ paste(
      starter, "pos", sep = "", collapse = NULL)))

  # Expanding the Cluster Name
  for(i in Columns) {MyNewestData <- MyNewestData |>
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
  Reintegrated1 <- Reintegrated |> relocate(all_of(DesiredOrder)) |> select(-Backups)
  Reintegrated1 <- Reintegrated1 |> mutate(specimen = name)
  }

  if (inverse.transform == TRUE && is.null(subsample)) {
    Cluster <- MyNewestData |> select(Cluster)
    DF <- as.data.frame(exprs(inversed_ff[[1]]), check.names = FALSE)
    Reintegrated1 <- cbind(DF, Cluster)
    Reintegrated1 <- Reintegrated1 |> mutate(specimen = name)
  }

  return(Reintegrated1)
}



#' @noRd
#' @importFrom BiocGenerics nrow

RowWorkAround <- function(x){
  TheNumberRows <- nrow(x)[[1]] # For Ratio
  return(TheNumberRows)
}
