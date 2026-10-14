#' Processes multiple Coereba flowframes, retrieves contents according to our Concatenate criteria, then cumulatively processes
#' 
#' @param Set The flow/cyto set we are working with
#' @param metadata_cols The columns from pData to retain.
#' @param inverse.transform Whether to reverse transform
#'  the exprs matrix, default is TRUE. 
#' @param outpath Where to store the .fcs file
#' @param filename What to call the .fcs file
#' @param returnType Whether to return "fcs", "data" or "flowframe"
#' @param specimenIndex Which specimen in the set to use for reference metadata
#' 
#' @export
#' 
#' @examples A <- 2+2
Coereba_Concatenate <- function(Set, metadata_cols,
  inverse.transform=TRUE, outpath=NULL,
  filename="CombinedCoerebaFile", returnType="fcs", specimenIndex=1){ 

  # x <- Set[1]
  AllData <- map(.x=Set, .f=Coereba_SingleFrame_Reversal,
     metadata_cols=metadata_cols,
     inverse.transform=inverse.transform) |> bind_rows()
  
  # colnames(AllData)
  # View(AllData)

  NewData <- AllData |> dplyr::select(where(is.character))
  desiredCols <- colnames(NewData)
  OldData <- AllData |> dplyr::select(!where(is.character))
  
  # x <- desiredCols[1]
  # data <- NewData
  Dictionaries <- purrr::map(.x=desiredCols, .f=ColumnToKeyword, data=NewData)

  EventsInTheGate <- flowWorkspace::gs_pop_get_data(Set[[specimenIndex]],
   inverse.transform=inverse.transform)
  flowFrame <- EventsInTheGate[[1, returnType = "flowFrame"]]
  OriginalParameters <- flowCore::parameters(flowFrame)
  OriginalDescription <- flowCore::keyword(flowFrame)

  NewKeywords <- purrr::flatten(Dictionaries)
  
  TheOriginalDescription <- OriginalDescription
  KeywordsBeingAdded <- names(NewKeywords)
  ExistingParameters <- OriginalParameters@data
  NeedToBlank <- ExistingParameters$name[ExistingParameters$name %in% KeywordsBeingAdded] 

  RemoveThese <- ExistingParameters |> filter(name %in% NeedToBlank) |> rownames()

  # OldParameters

  OriginalKeywordNames <- names(OriginalDescription)
  # Need to strip any existing old keywords out before reinserting...
  Escaped <- gsub("\\$", "\\\\$", RemoveThese)
  RegexFormatted <- paste0("^", Escaped, "($|[^0-9])")
  RegexCombinatorial <- paste(RegexFormatted, collapse = "|")
  IdentifiedElimination <- OriginalKeywordNames[
    grepl(RegexCombinatorial, OriginalKeywordNames)]
  IdentifiedRetention <- OriginalKeywordNames[
    !grepl(RegexCombinatorial, OriginalKeywordNames)]
  SecondRegexPattern <- paste0("(?<!\\d)", Escaped, "(?!\\d)")
  SecondRegexCombinatorial <- paste(SecondRegexPattern, collapse = "|")
  SecondElimination <- IdentifiedRetention[
    grepl(SecondRegexCombinatorial, IdentifiedRetention, perl = TRUE)]
  SecondRetention <- IdentifiedRetention[
    !grepl(SecondRegexCombinatorial, IdentifiedRetention, perl = TRUE)]
  NoDollars <- sub("^\\$", "", RemoveThese)
  NoDollarsRegex <- paste0("(?<!\\d)\\$?", NoDollars, "(?!\\d)")
  NoDollarsCombined <- paste(NoDollarsRegex, collapse = "|")
  ThirdElimination <- SecondRetention[
    grepl(NoDollarsCombined, SecondRetention, perl = TRUE)]
  ThirdRetention <- SecondRetention[
    !grepl(NoDollarsCombined, SecondRetention, perl = TRUE)]
  IntermediateDescription <- TheOriginalDescription[ThirdRetention]  
  
  NewDescriptions <- c(IntermediateDescription, NewKeywords)

  TranslatedNewData <- KeywordTranslate(data=NewData, DictionaryList=Dictionaries)

  NewDataMatrix <- as.matrix(TranslatedNewData)
  colnames(NewDataMatrix) <- paste0("Coereba_", colnames(NewDataMatrix))
  OldDataMatrix <- as.matrix(OldData)

  SlimmedParameters <- ExistingParameters |> filter(!name %in% NeedToBlank)
  OriginalParameters@data <- SlimmedParameters

  new_fcs <- new("flowFrame", exprs=OldDataMatrix, parameters=OriginalParameters,
                 description=NewDescriptions)

  NewParameters <- ParameterUpdate(flowFrame=new_fcs, NewColumns=NewDataMatrix)

  pd <- pData(parameters(new_fcs))
  pd <- rbind(pd, NewParameters)
  new_fcs@exprs <- cbind(exprs(new_fcs), NewDataMatrix)
  pData(parameters(new_fcs)) <- pd
  new_pid <- rownames(pd)
  new_kw <- new_fcs@description

  for (i in new_pid){
    new_kw[paste0(i,"B")] <- new_kw["$P1B"]
    new_kw[paste0(i,"E")] <- "0,0"
    new_kw[paste0(i,"N")] <- pd[[i,1]] 
    #new_kw[paste0(i,"V")] <- new_kw["$P1V"]
    new_kw[paste0(i,"R")] <- pd[[i,5]] + pd[[i,4]]
    new_kw[paste0(i,"DISPLAY")] <- "LIN"
    new_kw[paste0(i,"TYPE")] <- "Identity"
    new_kw[paste0("flowCore_", i,"Rmax")] <- pd[[i,5]]
    new_kw[paste0("flowCore_", i,"Rmin")] <- pd[[i,4]]
  }
  
  UpdatedParameters <- parameters(new_fcs)
  UpdatedExprs <- exprs(new_fcs)

  UpdatedFCS <- new("flowFrame", exprs=UpdatedExprs,
   parameters=UpdatedParameters, description=new_kw)

  AssembledName <- paste0(filename, ".fcs")
  UpdatedFCS@description$GUID <- AssembledName
  UpdatedFCS@description$`$FIL` <- AssembledName 

  if (is.null(outpath)){StorageLocation <- getwd()} else {StorageLocation <- outpath}

  StoreFCSFileHere <- file.path(StorageLocation, AssembledName)
  
  if (returnType == "fcs"){
    flowCore::write.FCS(UpdatedFCS, filename = StoreFCSFileHere, delimiter="#") 
  } else if (returnType == "data.frame"){
    return(Downsampled_DataFrame)
  } else {
    return(UpdatedFCS)
    }
}







