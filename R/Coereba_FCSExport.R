#' Takes Coereba Output and generates both .fcs file and a dictionary.
#'
#' @param data The Coereba Data
#' @param gs Gating Set Object containing original parameters
#' @param outpath File path to desired storage location
#' @param filename Desired name for fcs file
#' @param returnType Whether to return "flowframe" or "fcs"
#' @param nameAppend For flowframe and fcs returnType, what gets appended before .fcs 
#' @param Aggregate Combine the items, finish your documentation. 
#' @param combineCoereba Internal, used for Coereba flowframe aggregation
#'
#' @importFrom flowWorkspace gs_pop_get_data keyword 
#' @importFrom flowCore parameters exprs write.FCS parameters<-
#' @importFrom dplyr select bind_cols filter
#' @importFrom stringr str_detect
#' @importFrom tidyselect all_of
#' @importFrom purrr map flatten
#' @importFrom Biobase pData
#' @importFrom methods new
#'
#' @return A dictionary .csv file, and a .fcs file. 
#'
#' @export
#' 
#' @examples A <- 2+2
Coereba_FCSExport <- function(data, gs, outpath, filename, fcsname,
                              returnType, nameAppend, Aggregate=FALSE, 
                              coerebaCombine=FALSE){
  # Retrieving param information
  cs <- gs_pop_get_data(gs, "root")
  cf <- cs[[1]]
  original_param <- parameters(cf)
  original_descr <- keyword(cf)
  original_exprs <- exprs(cf)

  if (coerebaCombine==TRUE){
    paramdata <- original_param@data
    CoerebaParams <- paramdata |> filter(str_detect(name, "Coereba")) |> rownames()
    paramdata <- paramdata |> filter(!str_detect(name, "Coereba"))
    original_param@data <- paramdata

    original_exprs <- original_exprs[, !grepl("Coereba", colnames(original_exprs))]

  for(z in CoerebaParams){
  TheNKey <- names(original_descr)[grepl(paste0("\\", z, "N"), names(original_descr))]
  CoerebaIDName <- original_descr[TheNKey]
  DisplayKey <- paste0(sub("$", "", fixed=TRUE, z), "DISPLAY")
  BaseKey <- sub("Coereba_", "", CoerebaIDName)
  original_descr1 <- original_descr[!grepl(paste0("\\", z), names(original_descr))]
  original_descr1 <- original_descr1[!grepl(paste0("\\", CoerebaIDName), names(original_descr1))]
  original_descr1 <- original_descr1[!grepl(BaseKey, names(original_descr1), ignore.case = TRUE)]
  original_descr1 <- original_descr1[!grepl(paste0("\\", DisplayKey), names(original_descr1))]
  original_descr <- original_descr1
  }

  }
 
  # Identifying new columns
  OldColNames <- colnames(original_exprs) |> unname()
  NewColNames <- colnames(data)
  NovelColumns <- setdiff(NewColNames, OldColNames)
  Previously <- data |> select(!all_of(NovelColumns))
  AddThese <- data |> select(all_of(NovelColumns))

  #Validating
  #data.frame(table(AddThese$Cluster)) |> arrange(desc(Freq))
  
  # Adding to Descriptions
  # x <- NovelColumns[3]
  Here <- map(.x=NovelColumns, .f=PinkPonyClub, dataset=AddThese)
  
  #Validating
  #data.frame(table(Here[[1]][[1]])) |> arrange(desc(Freq))
  #Values match

  Descriptions <- lapply(Here, function(x) x$Description)
  Descriptions <- flatten(Descriptions)
  NewDescriptions <- c(original_descr, Descriptions)

  Columns <- lapply(Here, function(x) x$Column)
  NewColumns <- bind_cols(Columns)
  #Validating #Yup
  #data.frame(table(NewColumns$Coereba_Cluster)) |> arrange(desc(Freq))
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

  if(Aggregate == TRUE){
    new_fcs1@description$CREATOR <- "Coereba v0.99.6"
    new_fcs1@description$GROUPNAME <- filename
    new_fcs1@description$TUBENAME <- filename
    new_fcs1@description$USERSETTINGNAME <- filename
    Date <- Sys.time()
    Date <- as.Date(Date)
    new_fcs1@description$`$DATE` <- Date
  }
  
  if (is.null(outpath)) {outpath <- getwd()}
  
  fileSpot <- file.path(outpath, AssembledName)
  
  if (returnType == "fcs") {write.FCS(new_fcs1, filename = fileSpot, delimiter="#")
  } else {return(new_fcs1)}
}