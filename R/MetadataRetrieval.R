
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
    # Description |> data.frame(check.names=FALSE) |> t() |> View()
    Vaiya <- Description[TheColumns]
    #names(Vaiya)

    WithoutFrame <- Vaiya[[Without]]
    WithoutFrame2 <- strsplit(WithoutFrame, " ") # Where issue arises
    #  length(WithoutFrame2[[1]])

    WithFrame <- Vaiya[[With]]
    if (is.numeric(WithFrame)){WithFrame2 <- as.character(WithFrame)
        } else {WithFrame2 <- WithFrame}
    if (length(WithFrame2) == 1){WithFrame3 <- strsplit(WithFrame2, " ")
        } else {WithFrame3 <- WithFrame2}
    # length(WithFrame2[[1]])

    if (any(grepl("+", WithFrame3, fixed = TRUE))) {
    WithFrame3 <- format(as.numeric(WithFrame3[[1]]), scientific = FALSE, trim = TRUE)
    WithFrame3 <- list(WithFrame3)
    }

    if (length(WithoutFrame2[[1]]) != length(WithFrame3[[1]])){
        warning("There is an extra space in the provided name")

        if (length(WithoutFrame2[[1]]) != 1){
            message("Replacing WithoutFrame")
            WithoutFrame2 <- list(WithoutFrame)}
        if (length(WithFrame3[[1]]) != 1){
            message("Replacing WithFrame")
            WithFrame3 <- list(WithFrame2)}
    }

    WithFrame <- data.frame(unlist(WithFrame3), check.names = FALSE)
    #View(WithFrame)
    colnames(WithFrame) <- With

    WithoutFrame <- data.frame(unlist(WithoutFrame2), check.names = FALSE)
    #View(WithoutFrame)
    colnames(WithoutFrame) <- Without

    SpecimenDictionary <- cbind(WithoutFrame, WithFrame)
    # View(SpecimenDictionary)
    Combined <- left_join(data, SpecimenDictionary, by=With)
    #View(Combined)
    Retained <- Combined |> select(all_of(Without))
    return(Retained)
}