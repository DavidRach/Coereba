
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