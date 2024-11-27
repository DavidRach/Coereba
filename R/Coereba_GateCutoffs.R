#' Function to guess the gate cutoff values
#'
#' @param gs A GatingSet object
#' @param subset Gate node of interest
#' @param sample.name The keyword where specimens name is stored
#' @param desiredCols Column names of fluorophores you want to gate
#' @param returnTemplate Default FALSE, when set to TRUE returns a .csv of Gating
#' Template that can be modified and provisioned by the GatingTemplate argument
#' @param outpath Default NULL, provides file path to desired folder to store returnTemplate
#' @param GatingTemplate A file.path to the GatingTemplate .csv you want to swap in
#' @param returnPlots Default FALSE, will instead return plots for troubleshooting
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
#' TheGateCutoffs <- Coereba_GateCutoffs(x=UnmixedGatingSet[1],
#'  subset="live", sample.name="GROUPNAME")
#'
Coereba_GateCutoffs <- function(gs, subset, sample.name, desiredCols=NULL,
                                returnTemplate=FALSE, outpath=NULL,
                                GatingTemplate=NULL, returnPlots=FALSE){
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

    walk(.x=TheseOnes, .f=GateExecution, data=Template, gs=gs)

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


#' Internal for Coereba_GateCutoffs
#'
#' @param splitpoint The data.frame of Fluorophore and Cutoff columns
#' @param data The sample data in a data.frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr pull
#' @importFrom purrr map
#' @importFrom tidyselect all_of
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr filter
#' @importFrom dplyr row_number

#' @importFrom dplyr lag
#'
#' @noRd
GateChecks <- function(splitpoint, data, returnPlots=FALSE) {
  TheMarkers <- splitpoint %>% pull(Fluorophore)
  #x <- TheMarkers[7]

  CorrectedData <- map(.x=TheMarkers, .f=FunctionStandin,
                       splitpoint=splitpoint, data=data,
                       returnPlots=returnPlots)

  if (returnPlots== TRUE){return(CorrectedData)}
}


#' Internal for Coereba_GateCutoffs
#'
#' @param x The Fluorophore being iterated on
#' @param splitpoint The data.frame of fluorophore cutoff splitpoints
#' @param data The data.frame of exprs for the fluorophores
#' @param returnPlots Default False, returns plots for troubleshooting
#'
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr row_number
#'
#' @noRd
FunctionStandin <- function(x, splitpoint, data, returnPlots=FALSE){
  Fluorophore <- x
  TheSplitpoint <- splitpoint %>% dplyr::filter(Fluorophore %in% x) %>%
    pull(Cutoff) %>% round(., 0)
  TheData <- data %>% select(all_of(x)) %>% round(., 0)
  colnames(TheData) <- "xVal"
  freq_table <- data.frame(table(TheData)) %>% rename(yVal = Freq)

  freq_table <- freq_table %>%
    mutate(OriginalX = as.numeric(as.character(xVal))) %>%
    relocate(OriginalX, .before = xVal)

  freq_table$xVal <- as.numeric(freq_table$xVal)
  freq_table$yVal <- as.numeric(freq_table$yVal)

  # Filtering out random dots that might throw off dimensions
  freq_table <- freq_table %>% dplyr::filter(yVal > 3)

  # TheMode <- TheMax
  TheMode <- freq_table %>% dplyr::filter(yVal == max(yVal)) %>%
    pull(OriginalX)
  if (length(TheMode) > 1){TheMode <- TheMode[[1]]}
  # TheModeCount <- TheYMax
  TheModeCount <- freq_table %>% dplyr::filter(yVal == max(yVal)) %>%
    pull(yVal)
  if (length(TheModeCount) > 1){TheModeCount <- TheModeCount[1]}

  # TheMin <- TheLow
  TheMin <- freq_table %>% dplyr::filter(row_number() == 1) %>% pull(OriginalX)
  # TheMax <- TheHigh
  TheMax <- freq_table %>% dplyr::filter(row_number() == nrow(freq_table)) %>%
    pull(OriginalX)
  TheRange <- TheMax-TheMin # Caution Here With Negative Values
  Middle <- (TheRange/2)+TheMin

  freqX <- freq_table %>% pull(OriginalX)
  freqY <- freq_table %>% pull(yVal)

  Minima <- LocalMinima(theX = freqX, theY = freqY)
  TheMinima <- Minima %>% pull(x)

  if (returnPlots == TRUE){

  MyData <- data.frame(MFI=freqX, Count=freqY)

  Plot <- MinimaPlot(data=MyData, TheMin=TheMin, TheMax=TheMax,
             Middle=Middle, TheSplitpoint=TheSplitpoint,
             LocalMinima=TheMinima, name=Fluorophore)

  return(Plot)
  }

  if (length(TheMinima) == 0){message("No minima present")}

  Options <- c(TheSplitpoint, TheMinima)

  if (TheMode <= Middle){orientation <- "left"
  } else {orientation <- "right"}

  if (all(Options <= TheMode)){shape <- "lesser"
  } else if (all(Options > TheMode)){shape <- "greater"
  } else {shape <- "sandwhich"}

  Summons <- paste(Fluorophore, shape, orientation, sep=" ")
  return(Summons)
}


#' Internal for Coereba_GateCutoffs
#'
#' @noRd
NewFunctionStandIn <- function(x){
  if (shape == "sandwhich" & length(Minima1) > 2){
    Minima1 <- Minima1[-c(1, length(Minima1))]
    if (all(Minima1 < TheMax)) {shape1 <- "greater"
    } else if (all(Minima1 > TheMax)) {
      shape1 <- "lesser"} else {shape1 <- "sandwhich"}

    if (shape1 != shape) {#message("Shape switch Occured ", Fluorophore)
      shape <- shape1}
  } else if (shape == "sandwhich" & length(Minima1) <= 2){
    shape <- "sandwhich"}

  #shape

  if (all(TheMax < Middle)){orientation <- "left"
  } else if (all(TheMax > Middle)) {orientation <- "right"
  } else {orientation <- "left"} #Need to fix for center
  #orientation

  if (shape == "greater" & orientation == "left"){
    code <- paste(shape, orientation, Fluorophore, sep = " ")
  TheYmaxLimit <- TheYMax*0.2
  freq_table2 <- freq_table1 %>% filter(xVal > TheMax) %>%
    filter(yVal < TheYmaxLimit)
  index <- which(freq_table2$yVal > lag(freq_table2$yVal),
                 arr.ind = TRUE)[1] - 1

  if(length(index) != 0 && !is.na(index)){
    CutoffMinima <- freq_table2[index,] %>% pull(xVal)} else{
    CutoffMinima <- Minima1[1] #Temporary Glue.
  }

  } else if (shape == "greater" & orientation == "right"){
    code <- paste(shape, orientation, Fluorophore, sep = " ")
  CutoffMinima <- Minima1[length(Minima1)]

  } else if (shape == "lesser" & orientation == "left"){
    code <- paste(shape, orientation, Fluorophore, sep = " ")
  CutoffMinima <- Minima1[1]

  } else if (shape == "lesser" & orientation == "right"){
    code <- paste(shape, orientation, Fluorophore, sep = " ")

  #Theoretical Flip Side, check the code with an example.
  TheYmaxLimit <- TheYMax*0.2
  freq_table2 <- freq_table1 %>% filter(xVal < TheMax) %>%
    filter(yVal < TheYmaxLimit)
  index <- which(freq_table2$yVal < lag(freq_table2$yVal), arr.ind = TRUE)
  index <- index[length(index)] - 1

  if(length(index) != 0 && !is.na(index)){CutoffMinima <- freq_table2[index,] %>%
    pull(xVal)} else{
    CutoffMinima <- Minima1[1] #Temporary Glue.
  }

  } else if (shape == "sandwhich" & orientation == "left") {
    code <- paste(shape, orientation, Fluorophore, sep = " ")
  Minima1 <- Minima1[Minima1 > TheMax]
  CutoffMinima <- Minima1[1]
  } else if (shape == "sandwhich" & orientation == "right") {
    code <- paste(shape, orientation, Fluorophore, sep = " ")

  Minima1 <- Minima1[Minima1 < TheMax]
  CutoffMinima <- Minima1[length(Minima1)]

  } else {#message("Not Applicable")
    }


  if (abs(CutoffMinima - TheMax)/TheRange > 0.3){
    #message("Range Exceeded ", Fluorophore)

    if (orientation == "left"){
      TheYmaxLimit <- TheYMax*0.2
      freq_table2 <- freq_table1 %>% filter(xVal > TheMax) %>%
        filter(yVal < TheYmaxLimit)
      index <- which(freq_table2$yVal > lag(freq_table2$yVal), arr.ind = TRUE)[1] - 1

      if(length(index) != 0 && !is.na(index)){
        CutoffMinima <- freq_table2[index,] %>% pull(xVal)} else{
        CutoffMinima <- CutoffMinima #Temporary Glue.
      }

    } else if (orientation == "right"){
      #Theoretical Flip Side, check the code with an example.
      TheYmaxLimit <- TheYMax*0.2
      freq_table2 <- freq_table1 %>% filter(xVal < TheMax) %>%
        filter(yVal < TheYmaxLimit)
      index <- which(freq_table2$yVal < lag(freq_table2$yVal), arr.ind = TRUE)
      # This is where
      index <- index[length(index)] - 1

      if(length(index) != 0 && !is.na(index)){CutoffMinima <- freq_table2[index,] %>%
        pull(xVal)} else{
        CutoffMinima <- CutoffMinima #Temporary Glue.
      }

    }

  }

  #message(code, CutoffMinima)

  CutoffMinima <- freq_table1 %>% filter(xVal == CutoffMinima) %>% pull(OriginalX)

  NewData <- data.frame(cbind(Fluorophore, CutoffMinima))
  NewData$CutoffMinima <- as.numeric(NewData$CutoffMinima)
  #Site of fail, error is above with NA location signal

  return(NewData)
}






#' Internal for Coereba_Gate Cutoffs
#'
#' @param theX A vector of MFI positions
#' @param theY A Vector of Counts for theX positions
#' @param span Default 0.11 passed to roll apply zoo
#' @param w Default 3 passed to roll apply zoo
#' @param therepeats Extra positions applied to both end to allow roll
#' @param alternatename Something
#'
#' @importFrom stats loess
#' @importFrom zoo rollapply
#' @importFrom zoo zoo
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @noRd
LocalMinima <- function(theX, theY, span=0.11, w=3, therepeats=3){
  # plot(theX, theY)

  # Setting up X Ranked
  repeats <- therepeats*2
  NewX <- length(theX) + repeats
  NewX <- seq_len(NewX)

  # Setting up equivalent Y
  LengthY <- length(theY)
  NewYmax <- theY[[LengthY]]*0.80
  replicatedYmax <- rep(NewYmax, each = therepeats)
  NewYmin <- theY[[1]]*0.99
  replicatedYmin <- rep(NewYmin, each = therepeats)
  NewY <- c(replicatedYmin, theY, replicatedYmax)

  # plot(NewX, NewY)

  y.smooth <- loess(NewY ~ NewX)$fitted
  # plot(NewX, y.smooth)

  y.min <- rollapply(zoo(y.smooth), 2*w+1, min, align="center")
  #length(y.smooth)
  #length(y.min)
  # plot(theX, y.min)

  # x <- NewX
  # y <- NewY
  n <- length(NewY)
  delta <- y.min - y.smooth[-c(1:w, n+1-1:w)]
  # plot(theX, delta)

  i.min <- which(delta >= 0) + w
  TheI <- theX[i.min]
  # plot(theX, y.min)
  # abline(v = TheI, col = "red")


  peaks <- list(x=TheI, i=i.min, y.hat=y.smooth[(therepeats + 1):(length(y.smooth) - therepeats)])
  peak_points <- peaks$x

  MainData <- data.frame(x = theX, y = theY, yhat = peaks$y.hat)

  PointData <- MainData %>% dplyr::filter(x %in% peak_points)

  PointData <- PointData %>% select(-y)

  return(PointData)
}



#' Internal for Coereba_GateCutoffs
#'
#' @param data The passed dataframe
#' @param TheMin The mininum MFI for the dataset with a count of 3
#' @param TheMax The maxinum MFI with a count of 3
#' @param TheMiddle The estimated middle point between TheMin and TheMax
#' @param TheSplitpoint The splitpoint retrieved by GateRetrieval
#' @param name The Fluorophore name for the plot
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#'
#' @noRd
MinimaPlot <- function(data, TheMin, TheMax, Middle, TheSplitpoint,
                       LocalMinima, name=Fluorophore){

    Plot <- ggplot(data, aes(x=MFI, y=Count)) + geom_line() +
      geom_vline(aes(xintercept = TheMax), color = "red", linewidth = 0.5) +
      geom_vline(aes(xintercept = TheMin), color = "red", linewidth = 0.5) +
      geom_vline(aes(xintercept = Middle), color = "red", linewidth = 0.5) +
      geom_vline(aes(xintercept = TheSplitpoint), color = "blue", linewidth = 1) +
      geom_vline(xintercept = LocalMinima, color = "black", linewidth = 1) +
      labs(title=name)+
      theme_bw()

  return(Plot)
}



#' Internal for Coereba_GateCutoffs
#'
#' @param x The passed gate corresponding fluorophore splitpoint
#' @param gs The iterated on GatingSet object
#'
#' @importFrom flowWorkspace gs_pop_get_gate
#'
#' @noRd
GateRetrieval <- function(x, gs){
  ReturnValue <- gs_pop_get_gate(gs, x)
  Cutoff <- ReturnValue[[1]]@min
  Cutoff <- data.frame(Cutoff, check.names=FALSE)
}

#' Internal for Coereba_GateCutoffs
#'
#' @param x The particular Fluorophore to be gated
#' @param data The data.frame containing the openCyto gating template
#' @param gs The iterated on GatingSet object
#'
#' @importFrom dplyr filter
#' @importFrom flowWorkspace gs_get_pop_paths
#' @importFrom stringr str_detect
#' @importFrom openCyto gs_add_gating_method
#'
#' @noRd
GateExecution <- function(x, data, gs){
  Data <- data %>% dplyr::filter(alias %in% x)
  if (nrow(Data) != 1){stop("Too many rows at GateFilter")}

  ExistingGates <- gs_get_pop_paths(gs, path="auto")
  #ExistingGates <- gs_get_leaf_nodes(gs)

  if(!any(str_detect(ExistingGates, x))){

    suppressMessages(
      gs_add_gating_method(gs, parent = Data[[1,3]], pop = "+",
                           alias = Data[[1,1]], gating_method = "gate_mindensity",
                           dims = Data[[1,4]])
    )
  } else {Harry <- "Youre a Wizard"}
}

#' Internal for Coereba_GateCutoffs
#'
#' @param x A fluorophore name
#' @param data The data.frame row of to-be-modified gating template
#'
#' @noRd
GateTemplateAssembly <- function(x, data){
  data[1,1] <- x
  data[1,4] <- x
  return(data)
}

