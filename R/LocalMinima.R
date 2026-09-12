
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