
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