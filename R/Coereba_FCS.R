#' Takes Coereba Output and generates both .fcs file and a dictionary.
#'
#' @param data The Coereba Data
#' @param gs Gating Set Object containing original parameters
#' @param outpath File path to desired storage location
#' @param filename Desired name for fcs file
#' @param returnType Whether to return "flowframe" or "fcs"
#' @param nameAppend For flowframe and fcs returnType, what gets appended before .fcs 
#'
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom flowCore parameters
#' @importFrom flowWorkspace keyword
#' @importFrom flowCore exprs
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom purrr map
#' @importFrom purrr flatten
#' @importFrom dplyr bind_cols
#' @importFrom Biobase pData
#' @importFrom methods new
#' @importFrom flowCore write.FCS
#'
#' @return A dictionary .csv file, and a .fcs file. 
#'
#' @noRd
Coereba_FCSExport <- function(data, gs, outpath, filename, fcsname,
                              returnType, nameAppend){
  # Retrieving param information
  cs <- gs_pop_get_data(gs, "root")
  cf <- cs[[1]]
  original_param <- parameters(cf)
  original_descr <- keyword(cf)
  original_exprs <- exprs(cf)
  
  # Identifying new columns
  OldColNames <- colnames(original_exprs) |> unname()
  NewColNames <- colnames(data)
  NovelColumns <- setdiff(NewColNames, OldColNames)
  Previously <- data |> select(!all_of(NovelColumns))
  AddThese <- data |> select(all_of(NovelColumns))
  
  # Adding to Descriptions
  Here <- map(.x=NovelColumns, .f=PinkPonyClub, dataset=AddThese)
  Descriptions <- lapply(Here, function(x) x$Description)
  Descriptions <- flatten(Descriptions)
  NewDescriptions <- c(original_descr, Descriptions)

  Columns <- lapply(Here, function(x) x$Column)
  NewColumns <- bind_cols(Columns)
  NewColumns <- as.matrix(NewColumns)
  OldCols <- as.matrix(Previously)
  
  # Updating Exprs Param
  new_fcs <- new("flowFrame", exprs=OldCols, parameters=original_param,
                 description=NewDescriptions)
  
  #Using Internal Function :( Bioconductor?
  new_pd <- flowCore:::cols_to_pd(fr=new_fcs, cols=NewColumns)
  
  # Attaching New Params and Exprs
  pd <- pData(parameters(new_fcs))
  pd <- rbind(pd, new_pd)
  new_fcs@exprs <- cbind(exprs(new_fcs), NewColumns)
  pData(parameters(new_fcs)) <- pd
  
  # Updating description
  
  new_pid <- rownames(new_pd)
  new_kw <- new_fcs@description
  
  for (i in new_pid){
    new_kw[paste0(i,"B")] <- new_kw["$P1B"] #Unclear Purpose
    new_kw[paste0(i,"E")] <- "0,0"
    new_kw[paste0(i,"N")] <- new_pd[[i,1]]
    #new_kw[paste0(i,"V")] <- new_kw["$P1V"] # Extra Unclear Purpose
    new_kw[paste0(i,"R")] <- new_pd[[i,5]]
    new_kw[paste0(i,"DISPLAY")] <- "LIN"
    new_kw[paste0(i,"TYPE")] <- "Identity"
    new_kw[paste0("flowCore_", i,"Rmax")] <- new_pd[[i,5]]
    new_kw[paste0("flowCore_", i,"Rmin")] <- new_pd[[i,4]]
  }
  
  UpdatedParameters <- parameters(new_fcs)
  UpdatedExprs <- exprs(new_fcs)
  
  #Temporary Fix to "Comp-" issue
  
  UpdatedParameters@data$name <- gsub("Comp-", "", UpdatedParameters@data$name)
  colnames(UpdatedExprs) <- gsub("Comp-", "", colnames(UpdatedExprs))
  
  #for (i in seq_along(new_kw)) {
  #  new_kw[[i]] <- gsub("Comp-", "", new_kw[[i]])
  #}
  
  new_fcs1 <- new("flowFrame", exprs=UpdatedExprs, parameters=UpdatedParameters, description=new_kw)
  
  AssembledName <- paste0(filename, ".fcs")
  
  new_fcs1@description$GUID <- AssembledName
  new_fcs1@description$`$FIL` <- AssembledName
  
  if (is.null(outpath)) {outpath <- getwd()}
  
  fileSpot <- file.path(outpath, AssembledName)
  
  if (returnType == "fcs") {write.FCS(new_fcs1, filename = fileSpot, delimiter="#")
  } else {return(new_fcs1)}
}

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
#' 
#' @return A list with Column and Description
#' 
#' @noRd
PinkPonyClub <- function(x, dataset){
  TheColumn <- dataset |> select(x)
  Internal <- data.frame(table(TheColumn), check.names=FALSE)
  Internal <- Internal |> arrange(desc(Freq))
  KeywordName <- paste0("Coereba_", x)
  Internal <- Internal |> mutate(!!sym(KeywordName) := row_number()*1000)
  Internal <- Internal |> select(-Freq)
  Internal[[x]] <- as.character(Internal[[x]])

  Hmm <- left_join(TheColumn, Internal, by = x)
  Hmm <- Hmm |> select(-x)

  TheList <- list(Column = Hmm, 
  Description = Internal)

  return(TheList)
}
