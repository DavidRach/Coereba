#' Function to guess the gate cutoff values
#'
#' @param x A GatingSet object
#' @param subset Gate node of interest
#' @param sample.name The keyword where specimens name is stored
#' @param desiredCols Column names of fluorophores you want to gate
#' @param returnTemplate Default FALSE, when set to TRUE returns a .csv of Gating
#' Template that can be modified and provisioned by the GatingTemplate argument
#' @param outpath Default NULL, provides file path to desired folder to store returnTemplate
#' @param GatingTemplate A file.path to the GatingTemplate .csv you want to swap in
#' @param InternalCheck Development, default is FALSE
#' @param returnPlots Development, when TRUE and Internal Check TRUE, returns troubleshooting plots
#' @param GateOverwrite Whether when an existing gate is present, to delete and recreate it. Default FALSE, set TRUE when updating gates
#'
#' @importFrom flowCore keyword
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom flowCore exprs
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom data.table fread
#' @importFrom purrr map
#' @importFrom purrr walk
#' @importFrom dplyr bind_rows
#' @importFrom dplyr pull
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_wider
#' @importFrom utils write.csv
#'
#' @return A data.frame with estimated gate cutoffs for every marker
#'  for every specimen
#' @export
#'
#' @examples
#'
#' library(flowCore)
#' library(flowWorkspace)
#' library(openCyto)
#' library(data.table)
#'
#' File_Location <- system.file("extdata", package = "Coereba")
#' FCS_Files <- list.files(path = File_Location, pattern = ".fcs", full.names = TRUE)
#' UnmixedFCSFiles <- FCS_Files[1]
#' UnmixedCytoSet <- load_cytoset_from_fcs(UnmixedFCSFiles,
#'  truncate_max_range = FALSE, transformation = FALSE)
#' UnmixedGatingSet <- GatingSet(UnmixedCytoSet)
#' Markers <- colnames(UnmixedCytoSet)
#' KeptMarkers <- Markers[-grep("Time|FS|SC|SS|Original|-W$|-H$|AF", Markers)]
#' biex_transform <- flowjo_biexp_trans(channelRange = 256, maxValue = 1000000,
#'  pos = 4.5, neg = 0, widthBasis = -1000)
#' TransformList <- transformerList(KeptMarkers, biex_transform)
#' flowWorkspace::transform(UnmixedGatingSet, TransformList)
#' UnmixedGates <- fread(file.path(path = File_Location,
#'  pattern = 'GatesUnmixed.csv'))
#' UnmixedGating <- gatingTemplate(UnmixedGates)
#' gt_gating(UnmixedGating, UnmixedGatingSet)
#'
#' TheGateCutoffs <- Coereba_GateCutoffs(gs=UnmixedGatingSet[1],
#'  subset="live", sample.name="GROUPNAME")
#'
Coereba_GateCutoffs <- function(x, subset, sample.name, desiredCols=NULL,
                                returnTemplate=FALSE, outpath=NULL,
                                GatingTemplate=NULL, returnPlots=FALSE,
                                InternalCheck=FALSE, GateOverwrite=FALSE){
    gs <- x
  
    if (length(sample.name) == 2){
      first <- sample.name[[1]]
      second <- sample.name[[2]]
      first <- keyword(gs, first)
      second <- keyword(gs, second)
      name <- paste(first, second, sep="_")
    } else {name <- keyword(gs, sample.name)}

    LiveCells <- gs_pop_get_data(gs, root=subset)
    TheCols <- colnames(LiveCells)

    TheExprs <- exprs(LiveCells[[1]])
    TheExprs <- data.frame(TheExprs, check.names=FALSE)


    if (is.null(desiredCols)){
      Data <- TheExprs[,-grep("Time|FS|SC|SS|Original|W$|H$",
                              names(TheExprs))]
    } else {Data <- TheExprs %>% select(all_of(desiredCols))
    }

    TheGates <- colnames(Data)
    TheData <- Data #Save for subsequent


    if (is.null(GatingTemplate)){
      FileLocation <- system.file("extdata", package = "Luciernaga")
      UnmixedGates <- fread(file.path(path = FileLocation,
                                      pattern = 'GatesUnmixed.csv'))
      Example <- UnmixedGates[6]
      Example[1,3] <- subset
      # Keeping the negative pop.
      # Retaining the gate_mindensity arg.

      Template <- map(.x=TheGates, .f=GateTemplateAssembly,
                      data=Example) %>% bind_rows()
    } else {Template <- fread(GatingTemplate)}

    if (returnTemplate == TRUE){
      if (is.null(outpath)){StorageLocation <- getwd()
      } else {StorageLocation <- outpath}

      StorageName <- file.path(StorageLocation, "TemplateForGates.csv")
      write.csv(Template, StorageName, row.names=FALSE)
      return(Template)
    }

    TheseOnes <- Template %>% pull(alias)
    # x <- TheseOnes[1]
    # data <- Template

    walk(.x=TheseOnes, .f=GateExecution, data=Template, gs=gs, GateOverwrite=GateOverwrite)

    Data <- map(.x=TheseOnes, .f=GateRetrieval, gs=gs) %>% bind_rows
    Data <- rownames_to_column(Data, var="Fluorophore")
    TheSplitpoints <- Data

    if (InternalCheck == TRUE) {
    Data <- GateChecks(splitpoint=TheSplitpoints, data=TheData)

    if (returnPlots== TRUE){return(Data)}
    }

    Data <- Data %>% pivot_wider(names_from = "Fluorophore",
                                 values_from = "Cutoff")

    specimen <- data.frame(specimen=name[[1]])
    Final <- cbind(specimen, Data)
    return(Final)
}