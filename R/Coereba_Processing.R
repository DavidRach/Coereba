#' Takes Utility_Coereba output, plus additional metadata,and returns as a
#'  Bioconductor Summarized Experiment object
#' 
#' @param x An object with Coereba gating and metadata. Acccepted arguments
#'  include  a file.path to an .fcs, a flowFrame or cytoFrame, and a data.frame. 
#' @param metadata A data.frame or file.path to a metadata.csv file
#' @param metadata_columns A vector of column names from the metadata want to incorporate.
#' @param Identity Column designating specimen in the metadata
#' @param metadataTemplate When metadata is NULL, returns a template data.frame containing specimen list. 
#' @param outpath Default NULL, alternate a file.path to send metadataTemplate output to. 
#' 
#' @importFrom flowWorkspace load_cytoframe_from_fcs
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_wider
#' @importFrom utils read.csv
#' @importFrom dplyr rename
#' @importFrom tidyr unnest
#' @importFrom tidyr separate_wider_delim
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
Coereba_Processing <- function(x, metadata=NULL, metadata_columns=NULL,
  Identity="specimen", metadataTemplate, outpath=NULL){

  if (class(x) == "character"){
    Coereba <- load_cytoframe_from_fcs(x, truncate_max_range = FALSE, 
    transformation = FALSE)
  } else if (class(x) == "flowFrame"){Coereba <- x
  } else if (class(x) == "cytoframe"){Coereba <- x
  } else if (is.data.frame(x)){Coereba <- x
  } else {message("Argument x is a class ", class(x), " object, which is not recognized")}

  if (class(Coereba) %in% c("flowFrame", "cytoframe")){
    CoerebaOne <- Coereba_FCS_Reversal(Coereba=Coereba)
  } else if (is.data.frame(Coereba)){KnownColumns <- c("Cluster", "specimen")
    CoerebaOne <- Coereba |> select(all_of(KnownColumns))
  } else {message("Class not recognized at reversal step")}

    # Derriving Count and Ratio
    TheClusters <- CoerebaOne |> group_by(specimen, Cluster) |>
      summarise(Count = n()) |> ungroup() #Counts clusters per specimen
    TheSpecimens <- CoerebaOne |> group_by(specimen) |>
      summarise(SpecimenCount = n()) |> ungroup() #Counts per Specimen
    Merging <- left_join(TheClusters, TheSpecimens, by = "specimen")
    Data <- Merging |> mutate(Ratio = Count / SpecimenCount)
  
    Ratio <- Data |> select(-Count, -SpecimenCount) |>
      pivot_wider(names_from = specimen, values_from = Ratio)
    Ratio[is.na(Ratio)] <- 0

    Counts <- Data |> dplyr::select(-"Ratio", -"SpecimenCount") |>
      pivot_wider(names_from = specimen, values_from = "Count")
    Counts[is.na(Counts)] <- 0
  
  # Assembling Metadata
  if (is.null(metadata)){
    Names <- colnames(Counts)
    Names <- Names[-grep("Cluster", Names)]
    Metadata <- data.frame(specimen=Names)
  } else {
    if (!is.data.frame(metadata)){
      Metadata <- read.csv(metadata, check.names = FALSE)
    } else {Metadata <- metadata}
  
    if (!is.null(metadata_columns)){
      if (!Identity %in% metadata_columns){
        TheseColumns <- c(Identity, metadata_columns)
      } else {TheseColumns <- metadata_columns}

      Metadata <- Metadata |> select(all_of(TheseColumns))
    }

  Metadata <- Metadata %>% rename(specimen = .data[[Identity]])
  Metadata <- Metadata %>% unique()
  }

  if (is.null(metadata) && metadataTemplate==TRUE){
    if (!is.null(outpath)){
      Location <- outpath
    } else {Location <- getwd()}
    CSVName <- "Coereba_metadataTemplate.csv"
    StorageLocation <- file.path(Location, CSVName)
    write.csv(Metadata, StorageLocation, row.names=FALSE)
  }

    # Assembling rowNames
    TheDataset <- Data %>% group_by(Cluster) %>%
      summarise(Total_Counts = sum(Count)) |> rename(Identity = Cluster) |> 
      select(Identity)
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
    C <- C |> rename(Cluster=Identity)
    
    #Assembling Summarized Experiment
    Ratio1 <- Ratio |> select(-Cluster)
    Counts1 <- Counts |> select(-Cluster)
    
    MySE <- SummarizedExperiment(assays=list(ratios=Ratio1, count=Counts1),
     rowData = C,
     colData=Metadata)

    return(MySE)
}