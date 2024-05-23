#' Run Coereba and return as a data.frame
#'
#' @param x A Gating Set object
#' @param subsets The desired GatingHierarchy subset
#' @param sample.name Keyword for sample name
#' @param subsample If desired, downsample to a certain number
#' @param columns Columns to Keep.
#' @param notcolumns Columns to Remove
#' @param reference The external data.frame with the specified gate cutoffs
#' @param starter The column name to start the splits with
#' @param ReturnType Whether to return transformed (default) or inverse.transformed data ("inversed")
#'
#' @importFrom flowWorkspace keyword
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom flowCore exprs
#' @importFrom flowCore write.FCS
#' @importFrom dplyr slice_sample
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr relocate
#'
#' @return A data.frame
#' @export
#'
#' @examples NULL

Utility_Coereba <- function(x, subsets, sample.name, subsample = NULL, columns, notcolumns, reference, starter,
                            ReturnType = "data"){

  #Clean up Gate Cutoff colnames to match samples.
  New <- reference
  colnames(New) <- gsub("Comp-", "", colnames(New), fixed = TRUE)
  colnames(New) <- gsub("-A", "", colnames(New), fixed = TRUE)
  colnames(New) <- gsub("-", "", colnames(New), fixed = TRUE)
  colnames(New) <- gsub(" ", "", colnames(New), fixed = TRUE)
  colnames(New) <- gsub(".", "", colnames(New), fixed = TRUE)
  colnames(New)[1] <- sample.name

  #Retrieve metadata
  name <- keyword(x, sample.name)

  #Retrieve the cells of interest
  ff <- gs_pop_get_data(x, subsets, inverse.transform = FALSE)
  inversed_ff <- gs_pop_get_data(x, subsets, inverse.transform = TRUE)
  #newff <- realize_view(ff)
  startingcells <- RowWorkAround(x)


  df <- exprs(ff[[1]])
  DF <- as.data.frame(df, check.names = FALSE)

  # If down-sampling is specified
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
  if (!is.null(columns)){CleanedDF1 <- CleanedDF %>% select(all_of(columns))
  } else {CleanedDF1 <- CleanedDF}

  if (!is.null(notcolumns)){CleanedDF1 <- CleanedDF1 %>% select(-all_of(columns))
  } else {CleanedDF1 <- CleanedDF1}

  dsf <- CleanedDF1

  #Clean up data's colnames.
  NamingColBackup <- colnames(dsf)
  colnames(dsf) <- gsub("Comp-", "", colnames(dsf), fixed = TRUE)
  colnames(dsf) <- gsub("-A", "", colnames(dsf), fixed = TRUE)
  colnames(dsf) <- gsub("-", "", colnames(dsf), fixed = TRUE)
  colnames(dsf) <- gsub(" ", "", colnames(dsf), fixed = TRUE)
  colnames(dsf) <- gsub(".", "", colnames(dsf), fixed = TRUE)

  starter <- gsub("Comp-", "", starter, fixed = TRUE)
  starter <- gsub("-A", "", starter, fixed = TRUE)
  starter <- gsub("-", "", starter, fixed = TRUE)
  starter <- gsub(" ", "", starter, fixed = TRUE)
  starter <- gsub(".", "", starter, fixed = TRUE)

  #StartingCount
  decimal_places <- nchar(sub("\\d+\\.", "", as.character(startingcells)))

  My.Data <- dsf
  Columns <- colnames(My.Data)
  Columns <- Columns[ !Columns == starter]
  Columns <- Columns[!Columns == "AF"]

  MyNewestData <- My.Data %>% mutate(Cluster = case_when(
    My.Data[[starter]] < New[New[[sample.name]] == name, starter] ~ paste(
      starter, "neg", sep = "", collapse = NULL),
    My.Data[[starter]] > New[New[[sample.name]] == name, starter] ~ paste(
      starter, "pos", sep = "", collapse = NULL)))

  for(i in Columns) {MyNewestData <- MyNewestData %>%
    mutate(Cluster = case_when(
      MyNewestData[[i]] < New[New[[sample.name]] == name, i] ~
        paste(MyNewestData$Cluster, i, "neg", sep = ""),
      MyNewestData[[i]] > New[New[[sample.name]] == name, i] ~
        paste(MyNewestData$Cluster, i, "pos", sep = "")
    ))
  }

  if(ReturnType == "data"){
  NamingColBackup <- c(NamingColBackup, "Cluster")
  Reordering <- MyNewestData
  colnames(Reordering) <- NamingColBackup
  Reordering <- cbind(Backups, Reordering)
  Reintegrated <- left_join(Reordering, StashedDF, by = "Backups")
  BackupsCol <- "Backups"
  DesiredOrder <- c("Backups", OriginalColumnsVector, "Cluster")
  Reintegrated1 <- Reintegrated %>% relocate(all_of(DesiredOrder)) %>% select(-Backups)
  Reintegrated1 <- Reintegrated1 %>% mutate(specimen = name)
  } else {
    Cluster <- MyNewestData %>% select(Cluster)
    if(!is.null(subsample)){error("There is a mismatch we haven't fixed yet")}
    df <- exprs(inversed_ff[[1]])
    DF <- as.data.frame(df, check.names = FALSE)
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
