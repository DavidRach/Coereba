#' Preliminary Function, processes colnames, reverts from dictionary if needed,
#'  derives Ratio.
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
Coereba_Enumeration <- function(x, TheDictionary=NULL, ReplaceCharacters=NULL){

  TheIsolatedData <- data.frame(x, check.names = FALSE)
  TheIsolatedData <- TheIsolatedData[,-grep("Time|FS|SC|SS|Original|W$|H$|AF",
                                            names(TheIsolatedData))]
  internalstrings <- c("FJComp-", "Comp-", "-A", "-", "_", " ")
  colnames(TheIsolatedData) <- NameCleanUp(colnames(TheIsolatedData),
                                           removestrings = internalstrings)

  if(!is.null(TheDictionary)){

    if(!is.data.frame(TheDictionary)){
      Dictionary <- read.csv(TheDictionary, check.names=FALSE)
    } else {Dictionary <- TheDictionary}

    Conjoined <- left_join(TheIsolatedData, Dictionary, by="ClusterNumber")
    Conjoined <- Conjoined %>% select(-all_of(c("Freq", "ClusterNumber")))

    if(!is.null(ReplaceCharacters)){
      Conjoined$specimen <- sprintf("%03d", Conjoined$specimen) #Not Generalizable
      Conjoined$specimen <- paste0(ReplaceCharacters, Conjoined$specimen)
    }

    TheIsolatedData <- Conjoined
  }

  TheIsolatedData <- TheIsolatedData %>% select(specimen, Cluster)

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

  # My specific non-generalizable edits
  Metadata[[Identity]] <- gsub("INF-", "", Metadata[[Identity]])
  Metadata[[Identity]] <- sub("^0+", "", Metadata[[Identity]])
  Metadata[[Identity]] <- sub("-[0-9]{1}", "", Metadata[[Identity]])
  Metadata[[Identity]] <- str_pad(Metadata[[Identity]], 3, pad = "0")
  Metadata <- Metadata %>% mutate(
    bid = ifelse(grepl("^[0-9]", bid), paste0("INF", bid), bid))
  Metadata <- Metadata %>% select(all_of(thecolumns))
  Metadata <- Metadata %>% rename(specimen = all_of(Identity))
  Metadata <- Metadata %>% unique() # Handling Issues With Repeated Rows.

  Merged <- merge(x, Metadata, by= "specimen")

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
                                           panel, starter){
  TheX <- x
  TheName <- y

  Here <- Coereba:::Coereba_Enumeration(x=TheX, TheDictionary = DictionaryPath,
                                        ReplaceCharacters=ReplaceCharacters)

  Hmm2 <- Coereba:::HEUAnnotation(x=Here, Metadata=Metadata,
                                  Identity=IdentityColName, thecolumns=MetadataCols)

  Filtered <- Coereba:::Coereba_Processing2(x=Hmm2)

  C <- Coereba:::Coereba_Processing3(x=Filtered)

  Ready <- Filtered %>% pivot_wider(names_from = Clusters, values_from = Count)
  Ready[is.na(Ready)] <- 0

  Aggregated <- Coereba_MarkerExpressions(data=Ready, binary=C,
                                          panel=panel, starter=starter)

  if(summarizeMedians==TRUE){

    TheMarkerMedians <- Aggregated %>% select(-c(1:8)) %>%
      summarise(across(everything(), median, na.rm = TRUE)) %>%
      round(., digits=2) %>% mutate(Population=TheName[[1]]) %>%
      relocate(Population, .before=1)

  } else {return(Aggregated)}

}

