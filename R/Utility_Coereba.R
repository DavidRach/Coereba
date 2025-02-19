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
#' @importFrom dplyr bind_rows
#'
#' @return Either data, flowframe or fcs, individually or concatinated
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
  
  Data <- map(.x=gs, .f=Internal_Coereba, subsets=subsets,
     sample.name=sample.name, subsample=subsample, columns=columns,
    notcolumns=notcolumns, reference=reference, starter=starter,
    inverse.transform = inverse.transform, returnType=returnType,
    Individual=Individual)
  
  if (returnType == "data" && Individual == TRUE){return(Data)}

  if (returnType == "flowframe" && Individual == TRUE){return(Data)}

  if (length(Data) > 1){Data1 <- bind_rows(Data)
    } else {Data1 <- Data}
  
  if (returnType == "data" && Individual == FALSE){return(Data1)}

  if (returnType == "flowframe" && Individual == FALSE){
    message("Returning aggregated flowframe")}
  
  if (returnType == "fcs" && Individual == FALSE){
      message("Returning aggregated fcs file")
    #if(is.null(outpath)){outpath <- getwd()}
  }

}


#' Internal for Utility_Coereba, orchestrates individual file processing
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
#' @param returnType Whether to return data, flowframe or fcs
#' @param outpath Default NULL, file.path to location to store fcs file
#' @param filename Default NULL, filename to save the aggregated flowframe/fcs file.
#' @param nameAppend For flowframe and fcs returnType, what gets appended before .fcs 
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
#' @return Either data, flowframe or to the outfolder an .fcs
#'
#' @noRd
Internal_Coereba <- function(x, subsets, sample.name, subsample = NULL, columns=NULL,
                            notcolumns=NULL, reference, starter,
                            inverse.transform = TRUE, returnType, Individual,
                            outpath=NULL, filename=NULL, nameAppend=NULL){
  
  if (!is.data.frame(reference)){
    ReferenceLines <- read.csv(reference, check.names = FALSE)
    } else {ReferenceLines <- reference}
  
  internalstrings <- c("Comp-", "-A", "-", " ", ".")
  colnames(ReferenceLines) <- NameCleanUp(colnames(ReferenceLines),
                                          removestrings = internalstrings)
  
  if (sample.name != colnames(ReferenceLines)[[1]]){
    message("sample.name does not match the specimen identifier name found in reference,
     converting over")
    colnames(ReferenceLines)[1] <- sample.name
  }

  New <- ReferenceLines
  
  name <- keyword(x, sample.name)

  Specimens <- ReferenceLines %>% pull(.data[[sample.name]])

  if (!name[[1]] %in% Specimens){
    message("sample.name ", name, " not recognized among identification names found in reference file,
    returning NULL instead of a data.frame, if iterating with map use compact instead of bind_rows to remove")
    Reintegrated1 <- NULL
    return(Reintegrated1)
  }

  if (inverse.transform == TRUE){
  inversed_ff <- gs_pop_get_data(x, subsets, inverse.transform = TRUE)
  flippedDF <- as.data.frame(exprs(inversed_ff[[1]]), check.names = FALSE)
  flippedDF <- flippedDF |> mutate(Backups = 1:nrow(flippedDF)) |> 
    relocate(Backups, .before=1)
  }

  ff <- gs_pop_get_data(x, subsets, inverse.transform = FALSE)

  startingcells <- RowWorkAround(ff)
  OriginalDF <- as.data.frame(exprs(ff[[1]]), check.names = FALSE)

  OriginalColumnsVector <- colnames(OriginalDF)

  OriginalDF <- OriginalDF |> mutate(Backups = 1:nrow(OriginalDF)) 

  if (inverse.transform == TRUE){
    if (nrow(ff)[[1]] != nrow(inversed_ff)[[1]]){
      stop("Mismatched number of rows, contact Maintainer")
    }
  }

  if(!is.null(subsample)){DF <- slice_sample(OriginalDF, n = subsample,
                                             replace = FALSE)
    startingcells <- nrow(DF)
  } else{DF <- OriginalDF}

  TheBackups <- DF |> dplyr::select(Backups)

  StashedDF <- DF[,grep("Time|FS|SC|SS|Original|W$|H$", names(DF))]
  StashedDF <- cbind(TheBackups, StashedDF)

  CleanedDF <- DF[,-grep("Time|FS|SC|SS|Original|W$|H$", names(DF))]
  BackupNames <- colnames(CleanedDF)
  CleanedDF <- CleanedDF |> dplyr::select(-Backups)

  if (!is.null(columns) && !is.null(notcolumns)){
    stop("Please select either columns (to keep) or notcolumns (to exclude).
    Leave the other agument as NULL")
    }

  if (!is.null(columns)){
    dsf <- CleanedDF %>% select(all_of(c(columns, starter)))
  } else {dsf <- CleanedDF
  }

  if (!is.null(notcolumns)){
    if (starter %in% notcolumns){stop("The fluorophore ", starter, "shouldn't be included in the notcolumns list")}
    dsf <- CleanedDF %>% select(-all_of(notcolumns))
  } else {dsf <- dsf
  }

  NamingColBackup <- colnames(dsf)

  colnames(dsf) <- NameCleanUp(colnames(dsf), removestrings = internalstrings)
  starter <- NameCleanUp(starter, removestrings = internalstrings)

  MyData <- dsf
  Columns <- colnames(MyData)
  Columns <- Columns[!Columns == starter]
  Columns <- Columns[!Columns == "AF"]
  Columns <- c(starter, Columns)

  New1 <- New |> dplyr::filter(.data[[sample.name]] %in% name)

  if (nrow(New1) != 1){
    warning("Multiple rows being iterated on for ", name, ", check sample.name and reference to ensure 
  that the naming convention matches only a single specimen")
  }

  # Generating Coereba Cluster Name
  MyDataPieces <- map(.x=Columns, .f=TheCoerebaIterator, data=MyData,
    reference=New1, sample.name=sample.name, name=name) |> bind_cols()
  
  Combined <- apply(MyDataPieces, 1, paste, collapse = "")
  Cluster <- data.frame(Cluster=Combined)
  MyNewestData <- cbind(MyData, Cluster)

  if (inverse.transform == FALSE && returnType %in% c("fcs", "flowframe")){
    message("For flowframe or fcs export, we inverse.transform to avoid distorting the original
    fcs files. We are now internally setting inverse.transform to TRUE")
    inverse.transform <- TRUE
  }

  # Left-joining to original data and adding ClusterID Specimen
  if (inverse.transform == TRUE) {
    Subsetted <- left_join(TheBackups,  flippedDF, by = "Backups")
    Cluster <- MyNewestData |> select(Cluster)
    Reintegrated1 <- cbind(Subsetted, Cluster) 
    Reintegrated1 <- Reintegrated1 |> select(-Backups) |>
      mutate(specimen = name[[1]])
  } else {
    Subsetted <- left_join(TheBackups,  OriginalDF, by = "Backups")
    Cluster <- MyNewestData |> select(Cluster)
    Reintegrated1 <- cbind(Subsetted, Cluster) 
    Reintegrated1 <- Reintegrated1 |> select(-Backups) |>
      mutate(specimen = name[[1]])
  }

  if (returnType == "flowframe" && Individual == TRUE){
    message("returning individual flowframe")
    FlowFrame <- Coereba_FCSExport(data=Reintegrated1, gs=x,
       returnType=returnType, outpath=outpath, filename=name,
      nameAppend=nameAppend)
    return(FlowFrame)
  } else if (returnType == "fcs" && Individual == TRUE){
    FCSFile <- Coereba_FCSExport(data=Reintegrated1, gs=x,
      returnType=returnType, outpath=outpath, filename=name,
      nameAppend=nameAppend)
  } else {return(Reintegrated1)}
}


#' Internal Utility_Coereba, processes negative from positive rows
#' 
#' @param x The individual fluorophore being iterated on
#' @param data The processed exprs data 
#' @param reference The processed reference splitpoint object
#' @param sample.name Used to identify individual (row)
#' @param name Used to identify fluorophore (column)
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' 
#' @return A column of splitpoint designations to be later combined to form ClusterID
#' 
#' @noRd
TheCoerebaIterator <- function(x, data, reference, sample.name, name){
  Internal <- data |> mutate(Cluster = case_when(
    data[[x]] < reference[reference[[sample.name]] == name, x] ~
      paste(x, "neg", sep=""),
    data[[x]] >= reference[reference[[sample.name]] == name, x] ~
      paste(x, "pos", sep="")))
  Internal <- Internal |> select(Cluster)
  colnames(Internal) <- paste("Cluster", x, sep="_")
  return(Internal)
}

#' Internal Utility_Coereba, work around for nrow function
#' 
#' @param x The flowframe object
#' 
#' @importFrom BiocGenerics nrow
#' 
#' @return The number of rows. 
#' 
#' @noRd
RowWorkAround <- function(x){
  TheNumberRows <- nrow(x)[[1]] # For Ratio
  return(TheNumberRows)
}
  