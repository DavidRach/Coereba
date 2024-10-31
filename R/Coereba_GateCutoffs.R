#' Function to guess the gate cutoff values
#'
#' @param gs A GatingSet object
#' @param subset Gate node of interest
#' @param sample.name The keyword where specimens name is stored
#' @param desiredColumns Column names of fluorophores you want to gate
#' @param returnTemplate Default FALSE, when set to TRUE returns a .csv of Gating
#' Template that can be modified and provisioned by the GatingTemplate argument
#' @param outpath Default NULL, provides file path to desired folder to store returnTemplate
#' @param GatingTemplate A file.path to the GatingTemplate .csv you want to swap in
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
                                GatingTemplate=NULL){
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
    Data <- Data %>% pivot_wider(names_from = "Fluorophore",
                                 values_from = "Cutoff")
    specimen <- data.frame(specimen=name[[1]])
    Final <- cbind(specimen, Data)
    return(Final)
}

#' Internal for Coereba_GateCutoffs
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
#' @importFrom dplyr filter
#' @importFrom openCyto gs_add_gating_method
#' @importFrom flowWorkspace gs_get_pop_paths
#' @importFrom stringr str_detect
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
#' @noRd
GateTemplateAssembly <- function(x, data){
  data[1,1] <- x
  data[1,4] <- x
  return(data)
}

#' The old version of the gate cutoffs
#'
#' @importFrom flowCore keyword
#' @importFrom Luciernaga NameCleanUp
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom flowCore exprs
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr rename
#'
#' @noRd
Coereba_GateCutoffsOld <- function(x, subset, sample.name, remove.strings,
  marker) {
  if (length(sample.name) == 2){
    first <- sample.name[[1]]
    second <- sample.name[[2]]
    first <- keyword(x, first)
    second <- keyword(x, second)
    name <- paste(first, second, sep="_")
  } else {name <- keyword(x, sample.name)}

  alternate.name <- NameCleanUp(name = name, remove.strings)
  cs <- gs_pop_get_data(x, subset, inverse.transform = FALSE)
  df <- exprs(cs[[1]])
  TheDFlocal <- data.frame(df, check.names = FALSE)

  TheColumnNames <- colnames(TheDFlocal)
  DFNames <- TheColumnNames[!grepl("^(Time|FS|SC|SS|Original|W$|H$)", TheColumnNames)]
  #x <- DFNames[1]

  if (!is.null(marker)){DFNames <- as.character(marker)}

  Data <- map(.x = DFNames, .f=ColumnExprs, TheDF = TheDFlocal) %>% bind_rows()
  Pivoted <- pivot_wider(Data, names_from = Fluorophore, values_from = CutoffMinima)
  Data <- cbind(alternate.name, Pivoted)
  Data <- Data %>% rename(specimen = alternate.name)

  return(Data)
}

#' Internal for Modern Gate Cutoff
#'
#' @param x Iterated Fluorophore Name
#' @param TheDF The sample data in a data.frame
#' @param w Argument passed to zoo for local minima
#' @param span Argument passed to zoo for local minima
#' @param ... Additional argument to zoo
#'
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr filter
#' @importFrom dplyr row_number
#' @importFrom magrittr %>%
#' @importFrom dplyr pull
#' @importFrom dplyr lag
#'
#' @noRd
ColumnExprs <- function(x, TheDF, w=2, span=0.1, ...) {
  Fluorophore <- x
  TheData <- TheDF %>% select(all_of(x)) %>% round(., 0)
  colnames(TheData) <- "xVal"
  freq_table <- data.frame(table(TheData)) %>% rename(yVal = Freq)
  freq_table1 <- freq_table

  freq_table1 <- freq_table1 %>%
    mutate(OriginalX = as.numeric(as.character(xVal))) %>%
    relocate(OriginalX, .before = xVal)

  freq_table1$xVal <- as.numeric(freq_table1$xVal)
  freq_table1$yVal <- as.numeric(freq_table1$yVal)

  TheMax <- freq_table1 %>% dplyr::filter(yVal == max(yVal)) %>%
    pull(xVal)
  if (length(TheMax) > 1){TheMax <- TheMax[[1]]} else {TheMax <- TheMax}
  TheYMax <- freq_table1 %>% dplyr::filter(yVal == max(yVal)) %>%
    pull(yVal)
  if (length(TheYMax) > 1){TheYMax <- TheYMax[1]} else {TheYMax <- TheYMax}
  TheLow <- freq_table1 %>% filter(row_number() == 1) %>% pull(xVal)
  TheHigh <- freq_table1 %>% filter(row_number() == nrow(freq_table1)) %>%
    pull(xVal)
  TheRange <- TheLow+TheHigh

  Middle <- TheRange/2

  freqX <- freq_table1 %>% pull(xVal)
  freqY <- freq_table1 %>% pull(yVal)

  Minima <- LocalMinima(theX = freqX, theY = freqY, w = w, therepeats = 4,
     span=span, alternatename = Fluorophore)

  if(nrow(Minima) == 0){
    #If no Minima is detected, use the max value
    Minima1 <- freq_table1 %>% filter(row_number() == nrow(freq_table1)) %>%
      pull(xVal)
    Minima2 <- freq_table1 %>% filter(row_number() == nrow(freq_table1)) %>%
      pull(yVal)

  } else {

    Minima1 <- Minima %>% pull(x) #What happens if it finds none?
    Minima2 <- Minima %>% pull(yhat)

  }

  #if(any(Minima2 > (TheYMax*0.1))) {message("Override is in play ", Fluorophore )}

  if (all(Minima1 < TheMax)){shape <- "greater"} else if (
    all(Minima1 > TheMax)) {shape <- "lesser"} else {
      shape <- "sandwhich"}

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




#' Internal for Gate Cutoffs
#'
#' @param theX Something
#' @param theY Something
#' @param w Something
#' @param therepeats Something
#' @param alternatename Something
#' @param ... Something
#'
#' @importFrom stats loess
#' @importFrom zoo rollapply
#' @importFrom zoo zoo
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom dplyr select
#'
#' @noRd
LocalMinima <- function(theX, theY, w, therepeats, alternatename, ...){

  repeats <- therepeats*2
  NewX <- length(theX) + repeats
  NewX <- 1:NewX
  NewYmin <- theY[[1]]*0.99
  LengthY <- length(theY)
  NewYmax <- theY[[LengthY]]*0.80
  replicatedYmin <- rep(NewYmin, each = therepeats)
  replicatedYmax <- rep(NewYmax, each = therepeats)
  NewY <- c(replicatedYmin, theY, replicatedYmax)

  x <- NewX
  y <- NewY

  n <- length(y)
  #y.smooth <- loess(y ~ x)$fitted
  y.smooth <- loess(y ~ x, ...)$fitted

  y.min <- rollapply(zoo(y.smooth), 2*w+1, min, align="center")
  delta <- y.min - y.smooth[-c(1:w, n+1-1:w)]
  i.min <- which(delta >= 0) + w
  peaks <- list(x=x[i.min]-therepeats, i=i.min-therepeats,
     y.hat=y.smooth[(therepeats + 1):(length(y.smooth) - therepeats)])

  peak_points <- peaks$x

  MainData <- data.frame(x = theX, y = theY, yhat = peaks$y.hat)

  PointData <- MainData %>% dplyr::filter(x %in% peak_points)

  Views <- ggplot(MainData, aes(x = x, y = y)) +
    geom_point(size = 2, color = "Gray") +
    geom_line(aes(y = yhat), linewidth = 1)  +
    geom_point(data = PointData, aes(x, yhat),
     color = "Red", shape = 19, size = 2) +
    geom_segment(data = PointData, aes(x = x,
      xend = x, y = 0, yend = yhat), color = "Red",
       linewidth = 1, linetype = "dashed") +
    labs(title = alternatename) + theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
           axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())

  #message(Views)

  PointData <- PointData %>% select(-y)

  return(PointData)
}

