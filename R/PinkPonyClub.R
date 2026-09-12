#' Internal for Coereba_FCS, Dictionary conversion for Coereba
#' 
#' @param x Iterated column name to be appended
#' @param dataset The data.frame containing the column names
#' 
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom rlang sym
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr left_join
#' @importFrom tidyselect all_of
#' 
#' @return A list with Column and Description
#' 
#' @noRd
PinkPonyClub <- function(x, dataset){
  TheColumn <- dataset |> select(all_of(x))
  #str(TheColumn)
  Internal <- data.frame(table(TheColumn), check.names=FALSE)
  Internal <- Internal |> arrange(desc(Freq))
  KeywordName <- paste0("Coereba_", x)
  Internal <- Internal |> mutate(!!sym(KeywordName) := row_number()*1000)
  Internal <- Internal |> select(-Freq)
  Internal[[x]] <- as.character(Internal[[x]])

  if (is.numeric(TheColumn[,1]) && is.character(Internal[[x]])){
    TheColumn[,1] <- as.character(TheColumn[,1])
  }


  Hmm <- left_join(TheColumn, Internal, by = x)
  Hmm <- Hmm |> select(-all_of(x))

  TheList <- list(Column = Hmm, 
  Description = Internal)

  return(TheList)
}