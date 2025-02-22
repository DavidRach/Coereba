#' Takes Utility_Coereba output, plus additional metadata,and returns as a
#'  Bioconductor Summarized Experiment object
#' 
#' @param x An object with Coereba gating and metadata. Acccepted arguments
#'  include  a file.path to an .fcs, a flowFrame or cytoFrame, and a data.frame. 
#' @param metadata A data.frame or file.path to a metadata.csv file
#' @param metadata_columns A vector of column names from the metadata want to incorporate.
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
#' 
#' 
#' 
#' 
#'
#' 
#' @return A summarized experiment object
#' 
#' @noRd
Coereba_Processing <- function(x, metadata=NULL, metadata_columns=NULL, 
  
Identity){

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
  
    if (!is.null(thecolumns)){
      Metadata <- Metadata |> select(all_of(metadata_columns))
    }

  #Metadata <- Metadata %>% rename(specimen = all_of(Identity))
  #Metadata <- Metadata %>% rename(specimen = .data[[Identity]])
  #Metadata <- Metadata %>% unique()
  }
  

  # Assembling rowNames

  Filtered <- x
  TheWeightedDataset <- Filtered %>% group_by(Clusters) %>%
    summarise(Total_Counts = sum(Count)) %>% arrange((Total_Counts))
  TheDataset <- TheWeightedDataset %>% rename(Identity = Clusters) %>%
    arrange(desc(Total_Counts)) %>% select(Identity)
  TheDataset$Identity <- gsub("pos", "pos_", TheDataset$Identity)
  TheDataset$Identity <- gsub("neg", "neg-", TheDataset$Identity)
  Upload <- TheDataset

  #strsplit divides by character. unnest then changes downward into rows.
  A <- Upload %>% mutate(to = strsplit(Identity, "_")) %>% unnest(to)
  B <- A %>% mutate(to = strsplit(to, "-")) %>% unnest(to)
  B$to <- gsub("pos", "_pos", gsub("neg", "_neg", B$to))

  #B <- B %>% filter(!to %in% c("NABV605_neg", "NABV711_pos", "NAZombieNIR_neg"))

  C <- B %>% separate_wider_delim(to, delim = "_", names = c("Fluorophore", "Value")) %>%
    pivot_wider(names_from = Fluorophore, values_from = Value)
  C[C == "pos"] <- "1"
  C[C == "neg"] <- "0"
  ColsC <- ncol(C)
  C[,2:ColsC] <- lapply(C[,2:ColsC], as.numeric)
  return(C)


}





#' Processes concatinated file to pivot_longer data.frame with specimen, Cluster, Ratio. 
#'
#' @param x A Coereba data.frame containing specimen and Cluster columns.
#' @param TheDictionary Default NULL, alternatively a file.path
#' @param ReplaceCharacters Default NULL, alternative a character string to append
#' in order to reverse the dictionary change.
#'
#' @importFrom Luciernaga NameCleanUp
#' @importFrom utils read.csv
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#'
#' @return An intermediate
#'
#' @noRd
Coereba_Enumeration <- function(x, TheDictionary=NULL, Sprint="%03d", ReplaceCharacters=NULL){

  TheIsolatedData <- data.frame(x, check.names = FALSE)
  TheIsolatedData <- TheIsolatedData[,-grep("Time|FS|SC|SS|Original|W$|H$|AF",
                                            names(TheIsolatedData))]
  
  #internalstrings <- c("FJComp-", "Comp-", "-A")
  #internalstrings <- c("FJComp-", "Comp-", "-A", "-", "_", " ")
  #colnames(TheIsolatedData) <- NameCleanUp(colnames(TheIsolatedData),
  #                                         removestrings = internalstrings)

  if(!is.null(TheDictionary)){

    if(!is.data.frame(TheDictionary)){
      Dictionary <- read.csv(TheDictionary, check.names=FALSE)
    } else {Dictionary <- TheDictionary}

    Conjoined <- left_join(TheIsolatedData, Dictionary, by="ClusterNumber")
    Conjoined <- Conjoined %>% select(-all_of(c("Freq", "ClusterNumber")))

    TheIsolatedData <- Conjoined
  }

  if(!is.null(ReplaceCharacters)){

    if (!is.null(Sprint)){
      TheIsolatedData$specimen <- sprintf(Sprint, TheIsolatedData$specimen)
    }
    
    TheIsolatedData$specimen <- paste0(ReplaceCharacters, TheIsolatedData$specimen)
  }

  TheIsolatedData <- TheIsolatedData %>% select(specimen, Cluster)
  #TheIsolatedData <- TheIsolatedData %>% select(.data[[specimen]], Cluster)
  #colnames(TheIsolatedData)[[1]] <- "specimen"

  TheClusters <- TheIsolatedData %>% group_by(specimen, Cluster) %>%
    summarise(ClusterCount = n()) %>% ungroup() #Counts clusters per specimen
  TheSpecimens <- TheIsolatedData %>% group_by(specimen) %>%
    summarise(SpecimenCount = n()) %>% ungroup() #Counts per Specimen
  Merging <- left_join(TheClusters, TheSpecimens, by = "specimen")
  Data <- Merging %>% mutate(Ratio = ClusterCount / SpecimenCount) %>%
    select(-ClusterCount, -SpecimenCount) # Derive cluster ratio
  return(Data)
}



#' The second intermediate, this one study specific, references in metadata
#' A lot of work will be needed to generalize it
#'
#' @param x The Coereba Enumeration output data.frame
#' @param Metadata A path to the metadata
#' @param Identity The column name corresponding to identity
#' @param thecolumns The columns corresponding to the metadata
#'
#' @importFrom utils read.csv
#' @importFrom stringr str_pad
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom dplyr relocate
#'
#' @return An intermediate dataframe
#'
#' @noRd
HEUAnnotation <- function(x, Metadata, Identity, thecolumns){
  if(!is.data.frame(Metadata)){Metadata <- read.csv(Metadata, check.names = FALSE)
  } else {Metadata <- Metadata}

  # My specific non-generalizable edits #Needs to be External.
  Metadata[[Identity]] <- gsub("INF-", "", Metadata[[Identity]])
  Metadata[[Identity]] <- sub("^0+", "", Metadata[[Identity]])
  Metadata[[Identity]] <- sub("-[0-9]{1}", "", Metadata[[Identity]])
  Metadata[[Identity]] <- str_pad(Metadata[[Identity]], 3, pad = "0")
  Metadata <- Metadata %>% mutate(
    bid = ifelse(grepl("^[0-9]", bid), paste0("INF", bid), bid))
  
  
  Metadata <- Metadata %>% select(all_of(thecolumns))
  Metadata <- Metadata %>% rename(specimen = all_of(Identity))
  Metadata <- Metadata %>% unique() # Handling Issues With Repeated Rows.

  Merged <- merge(x, Metadata, by= "specimen") #Swap left-join
  
  # Provide as Factor Levels Arguments (List?)
  Merged$specimen <- factor(Merged$specimen)
  Merged$ptype <- factor(Merged$ptype, levels = c("HU", "HEU-lo", "HEU-hi"))
  Merged$infant_sex <- factor(Merged$infant_sex, levels = c("Female", "Male"))
  Merged$Ratio <- as.numeric(Merged$Ratio)

  AlteredFinal <- Merged %>% mutate(artexposure = ptype) %>%
    relocate(artexposure, .after = ptype)
  AlteredFinal$artexposure <- gsub("HEU-hi", "Limited",
                                   gsub("HU", "Limited",
                                        gsub("HEU-lo", "Prolongued",
                                             AlteredFinal$artexposure)))
  AlteredFinal$artexposure <- factor(AlteredFinal$artexposure,
                                     levels = c("Limited", "Prolongued"))

  AlteredFinal <- AlteredFinal %>% mutate(Viral = ptype) %>%
    relocate(Viral, .after = ptype)
  AlteredFinal$Viral <- gsub("HEU-lo", "NoViral",
                             gsub("HU", "NoViral",
                                  gsub("HEU-hi", "Viral", AlteredFinal$Viral)))
  AlteredFinal$Viral <- factor(AlteredFinal$Viral, levels = c("NoViral", "Viral"))

  AlteredFinal <- AlteredFinal %>% mutate(ART = ptype) %>%
    relocate(ART, .after = ptype)
  AlteredFinal$ART <- gsub("HEU-lo", "HEU", gsub("HEU-hi", "HEU", AlteredFinal$ART))
  AlteredFinal$ART <- factor(AlteredFinal$ART, levels = c("HU", "HEU"))

  return(AlteredFinal)
}

#' The next step in the sequence
#'
#' @param x The passed data.frame intermediate
#'
#' @importFrom dplyr rename
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr n_distinct
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom dplyr semi_join
#'
#' @return An intermediate dataframe
#'
#' @noRd
Coereba_Processing2 <- function(x){

  colnames(x)[colnames(x) == "Cluster"] <- "Clusters"
  colnames(x)[colnames(x) == "Ratio"] <- "Count"
  Merged <- x %>% rename(bid = specimen)

  NotUnique <- Merged %>% group_by(Clusters) %>%
    summarise(levels_of_bid = n_distinct(bid)) %>%
    filter(levels_of_bid >= 0) %>% select(Clusters) %>% ungroup()
  Filtered <- Merged %>% semi_join(NotUnique, by = "Clusters")
  Filtered <- Filtered %>% distinct()
  return(Filtered)
}

#' The next piece in the cog
#'
#' @param x The intermediate data.frame
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyr pivot_wider
#'
#' @return An intermediate
#'
#' @noRd
Coereba_Processing3 <- function(x){
  Filtered <- x
  TheWeightedDataset <- Filtered %>% group_by(Clusters) %>%
    summarise(Total_Counts = sum(Count)) %>% arrange((Total_Counts))
  TheDataset <- TheWeightedDataset %>% rename(Identity = Clusters) %>%
    arrange(desc(Total_Counts)) %>% select(Identity)
  TheDataset$Identity <- gsub("pos", "pos_", TheDataset$Identity)
  TheDataset$Identity <- gsub("neg", "neg-", TheDataset$Identity)
  Upload <- TheDataset

  #strsplit divides by character. unnest then changes downward into rows.
  A <- Upload %>% mutate(to = strsplit(Identity, "_")) %>% unnest(to)
  B <- A %>% mutate(to = strsplit(to, "-")) %>% unnest(to)
  B$to <- gsub("pos", "_pos", gsub("neg", "_neg", B$to))

  #B <- B %>% filter(!to %in% c("NABV605_neg", "NABV711_pos", "NAZombieNIR_neg"))

  C <- B %>% separate_wider_delim(to, delim = "_", names = c("Fluorophore", "Value")) %>%
    pivot_wider(names_from = Fluorophore, values_from = Value)
  C[C == "pos"] <- "1"
  C[C == "neg"] <- "0"
  ColsC <- ncol(C)
  C[,2:ColsC] <- lapply(C[,2:ColsC], as.numeric)
  return(C)
}

#' The Assembly Function for all the above
#'
#' @param x A list of data.frames
#' @param y A list of names for respective data.frames in argument x
#' @param DictionaryPath Path to dictionary to convert
#' @param ReplaceCharacters Char to revert
#' @param Metadata Path to metadata csv
#' @param MetadataCols Desired csv columns
#' @param IdentityColName Colname corresponding to identity
#' @param summarizeMedians Default is TRUE, returns median
#' @param panel Path to the panel
#' @param starter Character string corresponding to Coereba start clusterid
#'
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr across
#' @importFrom tidyselect everything
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#'
#' @return Either data.frame with specimens by marker, or a summarized version
#'
#' @noRd
Coereba_LocalMarkerExpressions <- function(x, y, DictionaryPath,
                                           ReplaceCharacters, Metadata, MetadataCols,
                                           IdentityColName, summarizeMedians=TRUE,
                                           panel, starter, returnType="All",
                                           CombinatorialArgs=NULL){
  TheX <- x
  TheName <- y

  Here <- Coereba_Enumeration(x=TheX, TheDictionary = DictionaryPath,
                                        ReplaceCharacters=ReplaceCharacters)

  Hmm2 <- HEUAnnotation(x=Here, Metadata=Metadata,
                                  Identity=IdentityColName, thecolumns=MetadataCols)

  Filtered <- Coereba_Processing2(x=Hmm2)

  C <- Coereba_Processing3(x=Filtered)

  Ready <- Filtered %>% pivot_wider(names_from = Clusters, values_from = Count)
  Ready[is.na(Ready)] <- 0

  Aggregated <- Coereba_MarkerExpressions(data=Ready, binary=C,
                                          panel=panel, starter=starter,
                                          returnType=returnType,
                                          CombinatorialArgs = CombinatorialArgs)

  if(summarizeMedians==TRUE){

    TheMarkerMedians <- Aggregated %>% select(-c(1:8)) %>%
      summarise(across(everything(), median, na.rm = TRUE)) %>%
      round(., digits=2) %>% mutate(Population=TheName[[1]]) %>%
      relocate(Population, .before=1)

  } else {return(Aggregated)}

}

