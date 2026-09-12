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
