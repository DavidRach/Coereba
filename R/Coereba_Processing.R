#' Takes Utility_Coereba output, plus additional metadata,and returns as a
#'  Bioconductor Summarized Experiment object
#' 
#' @param x An object with Coereba gating and metadata. Acccepted arguments
#'  include  a file.path to an .fcs, a flowFrame or cytoFrame, and a data.frame. 
#' @param themetadata A data.frame or file.path to a metadata.csv file
#' @param metadata_columns A vector of column names from the metadata want to incorporate.
#' @param metadataTemplate When metadata is NULL, returns a template data.frame containing specimen list. 
#' @param outpath Default NULL, alternate a file.path to send metadataTemplate output to. 
#' @param panel A data.frame or file.path to a .csv containing Fluorophore and Marker columns
#' @param SpecimenVariable Column designating specimen in the metadata, default "specimen"
#' @param ClusterVariable Column designating specimen in the metadata, default "Cluster"
#' 
#' @importFrom flowWorkspace load_cytoframe_from_fcs
#' @importFrom dplyr select group_by summarise n ungroup left_join mutate rename
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider unite unnest separate_wider_delim
#' @importFrom utils read.csv
#' @importFrom SummarizedExperiment SummarizedExperiment
#' 
#' @return A summarized experiment object
#' 
#' @export
#' 
#' @examples
#' 
#' library(Coereba)
#' 
Coereba_Processing <- function(x, themetadata=NULL, metadata_columns=NULL,
  metadataTemplate=FALSE, outpath=NULL, panel, SpecimenVariable=NULL, ClusterVariable="Cluster"){
  
  # Converging on a data.frame
  if (class(x) == "character"){
    Coereba <- load_cytoframe_from_fcs(x, truncate_max_range = FALSE, 
    transformation = FALSE)
  } else if (class(x) == "flowFrame"){Coereba <- x
  } else if (class(x) == "cytoframe"){Coereba <- x
  } else if (is.data.frame(x)){Coereba <- x
  } else {message("Argument x is a class ", class(x), " object, which is not recognized")}

  if (class(Coereba) %in% c("flowFrame", "cytoframe")){
    CoerebaOne <- Coereba_FCS_Reversal(Coereba=Coereba)
  } else if (is.data.frame(Coereba)){
    CoerebaOne <- Coereba
  } else {message("Class not recognized at reversal step")}

  # Reading in the panel
  if (!is.data.frame(panel)){panelData <- read.csv(panel, check.names=FALSE)
    } else {panelData <- panel}
  panelData$Fluorophore <- gsub("-", "", panelData$Fluorophore)

  # Reading in the metadata
  if (!is.null(themetadata)){
    if (!is.data.frame(themetadata)){
      themetadata <- read.csv(themetadata, check.names = FALSE)
    } else {themetadata <- themetadata}
  }

  if (is.null(SpecimenVariable) & !is.null(metadata_columns)){
    # print("Aggregate")
    themetadata <- themetadata |> select(all_of(metadata_columns))
    themetadata <- themetadata |> 
      unite("TheSpecimen", all_of(metadata_columns), sep = "_", remove = FALSE)

    CoerebaOne <- CoerebaOne |> 
      unite("TheSpecimen", all_of(metadata_columns), sep = "_", remove = FALSE) 
    SpecimenVariable <- "TheSpecimen"

  } else if (is.null(SpecimenVariable) & is.null(metadata_columns)){
    # print("Provide Stand-In")
    SpecimenVariable <- "specimen"
  } else if (!is.null(SpecimenVariable) & is.null(metadata_columns)){
    # print("As you wish")
    SpecimenVariable <- SpecimenVariable
  } else if (!is.null(SpecimenVariable) & !is.null(metadata_columns)){
    # print("Dueling Banjos")
        themetadata <- themetadata |> select(all_of(metadata_columns))
    themetadata <- themetadata |> 
      unite("TheSpecimen", all_of(metadata_columns), sep = "_", remove = FALSE)

    CoerebaOne <- CoerebaOne |> 
      unite("TheSpecimen", all_of(metadata_columns), sep = "_", remove = FALSE) 
    SpecimenVariable <- "TheSpecimen"
  } else {
    stop("Provide arguments to SpecimenVariable or metadata_columns")
  }

  # Deriving ratio cluster*specimen/specimen
  TheSpecimens <- CoerebaOne |> group_by(.data[[SpecimenVariable]]) |>
    summarise(SpecimenCount = n(), .groups = "drop") 
  TheClusters <- CoerebaOne |> group_by(.data[[SpecimenVariable]], .data[[ClusterVariable]]) |>
    summarise(Count = n(), .groups = "drop")
  Merging <- left_join(TheClusters, TheSpecimens, by = SpecimenVariable)
  Data <- Merging |> mutate(Ratio = Count / SpecimenCount)

  Data <- Data |> filter(!.data[[ClusterVariable]] %in% "NA")

  Counts <- Data |> select(!all_of(c("Ratio", "SpecimenCount"))) |>
    pivot_wider(names_from = all_of(SpecimenVariable), values_from = "Count")
  Counts1 <- Counts |> select(-all_of(ClusterVariable))
  Counts1[is.na(Counts1)] <- 0

  Ratio <- Data |> select(!all_of(c("Count", "SpecimenCount"))) |>
    pivot_wider(names_from = all_of(SpecimenVariable), values_from = "Ratio")
  Ratio1 <- Ratio |> select(-all_of(ClusterVariable))
  Ratio1[is.na(Ratio1)] <- 0
  ClusterNameOrder <- Ratio |> pull(ClusterVariable)

  # Returning a metadata template when none is provided

  if (metadataTemplate==TRUE){
    Names <- colnames(Counts)
    Names <- Names[-grep(ClusterVariable, Names)]
    Metadata <- data.frame(specimen=Names) # Specimen List
    if (!is.null(outpath)){Location <- outpath} else {Location <- getwd()}
    CSVName <- "Coereba_metadataTemplate.csv"
    StorageLocation <- file.path(Location, CSVName)
    write.csv(Metadata, StorageLocation, row.names=FALSE)
    return(StorageLocation)
  }

  # Assembling rowNames (Cluster by Marker Status)
  TheDataset <- Data |> group_by(.data[[ClusterVariable]]) |>
    summarise(Total_Counts = sum(Count)) |> 
    dplyr::rename(Identity = all_of(ClusterVariable)) |> select(Identity)
  TheDataset$Identity <- gsub("pos", "pos_", TheDataset$Identity)
  TheDataset$Identity <- gsub("neg", "neg-", TheDataset$Identity)
  A <- TheDataset |> mutate(to = strsplit(Identity, "_")) |> unnest(to)
  B <- A |> mutate(to = strsplit(to, "-")) |> unnest(to)
  B$to <- gsub("pos", "_pos", gsub("neg", "_neg", B$to))
  C <- B |> separate_wider_delim(to, delim = "_", names = c("Fluorophore", "Value")) |>
      pivot_wider(names_from = Fluorophore, values_from = Value)
  C[C == "pos"] <- "1"
  C[C == "neg"] <- "0"
  ColsC <- ncol(C)
  C[,2:ColsC] <- lapply(C[,2:ColsC], as.numeric)
  C$Identity <- gsub("_", "", gsub("-", "", C$Identity))

  # Rearranging to match Ratio order of clusters
  C$Identity <- factor(C$Identity, levels = ClusterNameOrder)
  C <- C[order(C$Identity), ]

  # Metadata <- themetadata |> rename(specimen = all_of(SpecimenVariable))
  # Metadata <- Metadata |> unique()
  Metadata <- theetadata |> unique()
  
  #Assembling Summarized Experiment 
  MySE <- SummarizedExperiment(assays=list(ratios=Ratio1, count=Counts1),
  rowData = C,
  colData=Metadata,
  metadata = list(panel=panelData))
  return(MySE)
}
