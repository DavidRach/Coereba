
#' Improved Gate Estimates
#'
#' @param x A GatingSet object
#' @param subset Node level of interest
#' @param sample.name The keyword where names are stored
#' @param remove.strings Values to be removed from the name
#'
#' @importFrom flowCore exprs
#' @importFrom flowWorkspace keyword
#' @importFrom flowWorkspace gs_pop_get_data
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate_if
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom dplyr lag
#' @importFrom purrr map
#' @importFrom stats loess
#' @importFrom zoo zoo
#' @importFrom zoo rollapply
#' @import ggplot2
#'
#'
#' @return A data.frame with estimated gate cutoffs for every marker for every specimen
#' @export
#'
#' @examples NULL
#'
ModernGateCutoffs <- function(x, subset, sample.name, remove.strings) {

  name <- keyword(x, sample.name)
  alternate.name <- NameCleanUp(name = name, remove.strings)

  cs <- gs_pop_get_data(x, subset, inverse.transform = FALSE)
  df <- exprs(cs[[1]])
  TheDFlocal <- data.frame(df, check.names = FALSE)
  DFNames <- colnames(TheDFlocal[,-grep("Time|FS|SC|SS|Original|W$|H$",
                                        names(TheDFlocal))])

  #x <- DFNames[9]
  #DFNames <- DFNames[DFNames != "Comp-R780-A"] #Temporary Test

  AssembledData <- map(.x = DFNames, ColumnExprs, TheDF = TheDFlocal, w = 2, span = 0.1) %>% bind_rows()
  Pivoted <- pivot_wider(AssembledData, names_from = Fluorophore, values_from = CutoffMinima)
  Data <- cbind(alternate.name, Pivoted)
  Data <- Data %>% rename(specimen = alternate.name)

  return(Data)
}

ColumnExprs <- function(x, TheDF, w, span, ...) {
  Fluorophore <- x
  TheData <- TheDFlocal %>% select(all_of(x)) %>% round(., 0)
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

  Minima <- LocalMinima(theX = freqX, theY = freqY,
                        w = w, therepeats = 4, span=span, alternatename = Fluorophore)

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

  if(any(Minima2 > (TheYMax*0.1))) {print(paste0(
    "Override is in play ", Fluorophore ))}

  if (all(Minima1 < TheMax)){shape <- "greater"} else if (
    all(Minima1 > TheMax)) {shape <- "lesser"} else {
      shape <- "sandwhich"}

  if (shape == "sandwhich" & length(Minima1) > 2){
    Minima1 <- Minima1[-c(1, length(Minima1))]
    if (all(Minima1 < TheMax)) {shape1 <- "greater"
    } else if (all(Minima1 > TheMax)) {
      shape1 <- "lesser"} else {shape1 <- "sandwhich"}

    if (shape1 != shape) {print(paste0("Shape switch Occured ",
                                       Fluorophore))
      shape <- shape1}
  } else if (shape == "sandwhich" & length(Minima1) <= 2){
    shape <- "sandwhich"}

  #shape

  if (all(TheMax < Middle)){orientation <- "left"} else if (all(TheMax > Middle)) {orientation <- "right"} else {orientation <- "left"} #Need to fix for center
  #orientation

  if (shape == "greater" & orientation == "left"){ code <- paste(
    shape, orientation, Fluorophore, sep = " ")
  TheYmaxLimit <- TheYMax*0.2
  freq_table2 <- freq_table1 %>% filter(xVal > TheMax) %>%
    filter(yVal < TheYmaxLimit)
  index <- which(freq_table2$yVal > lag(freq_table2$yVal),
                 arr.ind = TRUE)[1] - 1
  CutoffMinima <- freq_table2[index,] %>% pull(xVal)

  } else if (shape == "greater" & orientation == "right"){code <- paste(shape, orientation, Fluorophore, sep = " ")
  CutoffMinima <- Minima1[length(Minima1)]

  } else if (shape == "lesser" & orientation == "left"){code <- paste(shape, orientation, Fluorophore, sep = " ")
  CutoffMinima <- Minima1[1]

  } else if (shape == "lesser" & orientation == "right"){code <- paste(shape, orientation, Fluorophore, sep = " ")

  #Theoretical Flip Side, check the code with an example.
  TheYmaxLimit <- TheYMax*0.2
  freq_table2 <- freq_table1 %>% filter(xVal < TheMax) %>% filter(yVal < TheYmaxLimit)
  index <- which(freq_table2$yVal < lag(freq_table2$yVal), arr.ind = TRUE)
  index <- index[length(index)] - 1
  CutoffMinima <- freq_table2[index,] %>% pull(xVal)

  } else if (shape == "sandwhich" & orientation == "left") {code <- paste(shape, orientation, Fluorophore, sep = " ")
  Minima1 <- Minima1[Minima1 > TheMax]
  CutoffMinima <- Minima1[1]
  } else if (shape == "sandwhich" & orientation == "right") {code <- paste(shape, orientation, Fluorophore, sep = " ")

  Minima1 <- Minima1[Minima1 < TheMax]
  CutoffMinima <- Minima1[length(Minima1)]

  } else {print("Not Applicable")}

  if (abs(CutoffMinima - TheMax)/TheRange > 0.3){print(paste0("Range Exceeded ", Fluorophore))

    if (orientation == "left"){
      TheYmaxLimit <- TheYMax*0.2
      freq_table2 <- freq_table1 %>% filter(xVal > TheMax) %>% filter(yVal < TheYmaxLimit)
      index <- which(freq_table2$yVal > lag(freq_table2$yVal), arr.ind = TRUE)[1] - 1
      CutoffMinima <- freq_table2[index,] %>% pull(xVal)
    } else if (orientation == "right"){
      #Theoretical Flip Side, check the code with an example.
      TheYmaxLimit <- TheYMax*0.2
      freq_table2 <- freq_table1 %>% filter(xVal < TheMax) %>% filter(yVal < TheYmaxLimit)
      index <- which(freq_table2$yVal < lag(freq_table2$yVal), arr.ind = TRUE) # This is where
      index <- index[length(index)] - 1

      if(length(index) != 0){CutoffMinima <- freq_table2[index,] %>% pull(xVal)} else{
        CutoffMinima <- CutoffMinima #Temporary Glue.
      }

    }

  }

  print(paste0(code, CutoffMinima))

  CutoffMinima <- freq_table1 %>% filter(xVal == CutoffMinima) %>% pull(OriginalX)

  NewData <- data.frame(cbind(Fluorophore, CutoffMinima))
  NewData$CutoffMinima <- as.numeric(NewData$CutoffMinima) #Site of fail, error is above with NA location signal

  return(NewData)
}

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
  peaks <- list(x=x[i.min]-therepeats, i=i.min-therepeats, y.hat=y.smooth[(therepeats + 1):(length(y.smooth) - therepeats)])

  peak_points <- peaks$x

  MainData <- data.frame(x = theX, y = theY, yhat = peaks$y.hat)

  PointData <- MainData %>% dplyr::filter(x %in% peak_points)

  Views <- ggplot(MainData, aes(x = x, y = y)) + geom_point(size = 2, color = "Gray") + geom_line(aes(y = yhat), linewidth = 1)  + geom_point(data = PointData, aes(x, yhat), color = "Red", shape = 19, size = 2) + geom_segment(data = PointData, aes(x = x, xend = x, y = 0, yend = yhat), color = "Red", linewidth = 1, linetype = "dashed") + labs(title = alternatename) + theme_bw() +  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  print(Views)

  PointData <- PointData %>% select(-y)

  return(PointData)
}

