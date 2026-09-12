
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