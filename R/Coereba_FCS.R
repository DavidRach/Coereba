#' Takes Coereba Output and generates both .fcs file and a dictionary.
#'
#' @param data The Coereba Data
#' @param gs Gating Set Object containing original parameters
#' @param outpath File path to desired storage location
#' @param filename Desired name for fcs file
#' @param returnType Whether to return "flowframe" or "fcs"
#' @param nameAppend For flowframe and fcs returnType, what gets appended before .fcs 
#' @param Aggregate Combine the items, finish your documentation. 
#'
#' @importFrom flowWorkspace gs_pop_get_data keyword 
#' @importFrom flowCore parameters exprs write.FCS parameters<-
#' @importFrom dplyr select bind_cols
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
                              returnType, nameAppend, Aggregate=FALSE){
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
    new_fcs1@description$CREATOR <- "Coereba v0.99.2"
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
  Hmm <- Hmm |> select(-x)

  TheList <- list(Column = Hmm, 
  Description = Internal)

  return(TheList)
}

#' Handles Description Digging for Dictionary Reversal going FCS to data
#' 
#' @param Coereba Either a flow/cytoframe with embedded Keywords. Alternately, the file.path to the .fcs file
#'  with the same embedded keywords
#' 
#' @importFrom flowCore exprs keyword
#' @importFrom dplyr select across mutate bind_cols
#' @importFrom purrr map
#' @importFrom tidyselect starts_with everything
#' 
#' @return A data.frame containing the reversed columns to be passed on to SummarizedExperiment
#' 
#' @export
#' 
#' @examples A <- 2 + 2
Coereba_FCS_Reversal <- function(Coereba){
  
  if (!class(Coereba) %in% c("flowFrame", "cytoframe")){
    if (class(Coereba) %in% "character"){
      Coereba <- load_cytoframe_from_fcs(Coereba, transformation=FALSE, truncate_max_range = FALSE)
    } else {stop("Please provide either a flowframe, cytoframe or the path to an .fcs file")}
  }

    Data <- exprs(Coereba)
    Data <- data.frame(Data, check.names=FALSE)
    Original <- Data |> select(!starts_with("Coereba"))
    Data <- Data |> select(starts_with("Coereba"))
    Data <- Data |> mutate(across(everything(), as.character))
    These <- colnames(Data)

    # x <- These[2]
    # data <- Data
    Reverted <- map(.f=MetadataRetrieval, .x=These, data=Data, Coereba=Coereba) |> bind_cols()
    Assembled <- cbind(Original, Reverted)
  
    return(Assembled)
}

#' Internal for Coereba_FCS_Reversal, handles reversal of the individual Coereba
#'  metadata columns via the Dictionary keywords
#' 
#' @param x The iterated in Coereba_ column name for processing
#' @param data The retrieved exprs data.frame object
#' @param Coereba The flowframe/cytoframe object for referencing keyword data
#' 
#' @importFrom dplyr select left_join
#' @importFrom flowCore keyword
#' @importFrom tidyselect all_of
#' 
#' @return A reverted back data.frame column
#' 
#' @noRd
MetadataRetrieval <- function(x, data, Coereba){
    Internal <- data |> select(x)
    Without <- gsub("Coereba_", "", colnames(Internal))
    With <- paste0("Coereba_", Without)
    TheColumns <- c(With, Without)
    Description <- keyword(Coereba)
    Vaiya <- Description[TheColumns]
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
