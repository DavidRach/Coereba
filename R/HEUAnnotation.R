
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