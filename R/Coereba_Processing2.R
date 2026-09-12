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