#' Takes Coereba Output and generates both .fcs file and a dictionary.
#'
#' @param data The Coereba Data
#' @param gs Gating Set Object containing original parameters
#' @param SaveDictionary Default is TRUE, exporting a Dictionary .csv
#' @param outpath File path to desired storage location
#' @param filename Desired name for csv dictionary
#' @param fcsname Desired name for concatenated fcs file
#' @param returntype Default is "fcs", alternatively "flowframe"
#'
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom flowWorkspace keyword
#' @importFrom flowCore parameters
#' @importFrom flowCore exprs
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr desc
#' @importFrom dplyr row_number
#' @importFrom utils write.csv
#' @importFrom Biobase pData
#' @importFrom methods new
#' @importFrom flowCore write.FCS
#'
#' @return A dictionary .csv file, and a .fcs file. 
#'
#' @noRd
Coereba_FCSExport <- function(data, gs, SaveDictionary=TRUE,
                              outpath, filename, fcsname,
                              returntype="fcs"){
  # Retrieving param information
  cs <- gs_pop_get_data(gs, "root")
  fr <- cs[[1, returnType = "flowFrame"]]
  original_param <- parameters(fr)
  original_descr <- keyword(fr)
  original_exprs <- exprs(fr)
  
  # Identifying new columns
  
  OldColNames <- colnames(original_exprs) %>% unname()
  NewColNames <- colnames(data)
  NovelColumns <- setdiff(NewColNames, OldColNames)
  Previously <- data %>% select(!all_of(NovelColumns))
  AddThese <- data %>% select(all_of(NovelColumns))
  
  ## For each AddThese, generalize on rewrite for X # cols
  ## Add way to removestrings approach for name specification
  ## Or provide alternate dictionary for specimen name
  
  # Cleaning up specimen name
  AddThese$specimen <- gsub("INF", "", AddThese$specimen)
  AddThese$specimen <- gsub("ND", "", AddThese$specimen)
  AddThese$specimen <- as.double(AddThese$specimen)
  
  # Creating a Cluster Dictionary
  TabledCluster <- table(AddThese$Cluster)
  TabledCluster <- as.data.frame(TabledCluster)
  TabledCluster <- TabledCluster %>% arrange(desc(Freq))
  Dictionary <- TabledCluster %>% mutate(ClusterNumber = row_number()*1000)
  colnames(Dictionary)[1] <- "Cluster"
  
  if (SaveDictionary == TRUE){
    TheFileName <- paste0(filename, ".csv")
    StoreCSV <- file.path(outpath, TheFileName)
    write.csv(Dictionary, file = StoreCSV, row.names = FALSE)
  }
  
  # Generating clean New Columns
  Hmm <- left_join(AddThese, Dictionary, by = "Cluster")
  Hmm <- Hmm %>% select(-all_of(c("Cluster","Freq")))
  
  # Converting both Old and New exprs to matrix format.
  NewCols <- as.matrix(Hmm)
  OldCols <- as.matrix(Previously)
  
  # Updating Exprs Param
  
  new_fcs <- new("flowFrame", exprs=OldCols, parameters=original_param,
                 description=original_descr)
  
  #Using Internal Function :( Bioconductor?
  new_pd <- flowCore:::cols_to_pd(fr=new_fcs, cols=NewCols)
  
  # Attaching New Params and Exprs
  pd <- pData(parameters(new_fcs))
  pd <- rbind(pd, new_pd)
  new_fcs@exprs <- cbind(exprs(new_fcs), NewCols)
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
  
  AssembledName <- paste0(fcsname, ".fcs")
  
  new_fcs1@description$GUID <- AssembledName
  new_fcs1@description$`$FIL` <- AssembledName
  
  if (is.null(outpath)) {outpath <- getwd()}
  
  fileSpot <- file.path(outpath, AssembledName)
  
  if (returntype == "fcs") {write.FCS(new_fcs1, filename = fileSpot, delimiter="#")
  } else {return(new_fcs1)}
}
