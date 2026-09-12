
#' Internal for Coereba_FCS_Reversal, handles reversal of the individual Coereba
#'  metadata columns via the Dictionary keywords
#' 
#' @param x The iterated in Coereba_ column name for processing
#' @param data The retrieved exprs data.frame object
#' @param Coereba The flowframe/cytoframe object for referencing keyword data
#' 
#' @importFrom dplyr select left_join
#' @importFrom tidyselect all_of
#' @importFrom flowCore keyword
#' @importFrom tidyselect all_of
#' 
#' @return A reverted back data.frame column
#' 
#' @noRd
MetadataRetrieval <- function(x, data, Coereba){
    Internal <- data |> select(all_of(x))
    Without <- gsub("Coereba_", "", colnames(Internal))
    With <- paste0("Coereba_", Without)
    TheColumns <- c(With, Without)
    Description <- keyword(Coereba)
    # Description$ptype <- c("HEU-hi HEU-lo HU HEU-hi")
    Vaiya <- Description[TheColumns]
    # Vaiya <- Description[With]
    #str(Vaiya)

    WithoutFrame <- Vaiya[[Without]]
    if (length(WithoutFrame) == 1){WithoutFrame <- strsplit(WithoutFrame, " ")}
    WithoutFrame <- data.frame(WithoutFrame, check.names = FALSE)
    colnames(WithoutFrame) <- Without

    WithFrame <- Vaiya[[With]]
    if (is.numeric(WithFrame)){WithFrame <- as.character(WithFrame)}
    if (length(WithFrame) == 1){WithFrame <- strsplit(WithFrame, " ")}
    WithFrame <- data.frame(WithFrame, check.names = FALSE)
    colnames(WithFrame) <- With

    SpecimenDictionary <- cbind(WithoutFrame, WithFrame)
    Combined <- left_join(data, SpecimenDictionary, by=With)
    Retained <- Combined |> select(all_of(Without))
    return(Retained)
}