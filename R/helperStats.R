utils::globalVariables(c(".", ".data", "ART", "Cluster", "ClusterCount",
                         "Clusters", "Count", "Cutoff", "Fluorophore",
                         "Freq", "Identity", "InternalCheck", "MFI", "Marker",
                         "Middle", "OriginalX", "Plot_Name", "Population",
                         "SpecimenCount", "TheMax", "TheRange", "TheYMax",
                         "Time", "Total_Counts", "Value", "Viral", "X_Label",
                         "aggregate", "alias", "artexposure", "bid",
                         "complete.cases", "dagostino_result", "distinct",
                         "freq_table1", "levels_of_bid", "median", "n", "pData<-",
                         "parameters<-", "ptype", "residuals", "shapiro_result",
                         "specimen", "sym", "to", "value", "variable", "x", "xVal",
                         "y", "yVal"))

#' A bloody internal function that isn't worth all these fing issues it keeps causing
#' 
#' @param x A value to be rounder
#' @param showClose A work around to showing values between 0.05 vs 0.1 instead of ns
#' 
#' @return A rounded p-value
#' 
#' @noRd
pval_mold <- function(x, showClose=TRUE){

  if (showClose==TRUE){
  if (x > 0.10){"n.s"} else if (x > 0.01) {round(x, 2)} else if (x > 0.001) {
    round(x, 3)} else if (x > 0.0001) {round(x, 4)} else if (x > 0.00001) {
      round(x, 5)} else if (x > 0.000001) {round(x, 6)} else{return(x)}
  } else {
  if (x > 0.05){"n.s"} else if (x > 0.01) {round(x, 2)} else if (x > 0.001) {
      round(x, 3)} else if (x > 0.0001) {round(x, 4)} else if (x > 0.00001) {
      round(x, 5)} else if (x > 0.000001) {round(x, 6)} else{return(x)}
  } 
}

dago_wrapper <- function(x){
  A <- Coereba:::dagoTest(x)
  method <- A@test$method
  p.value <- A@test$p.value[[1]]
  C <- data.frame(cbind(p.value, method))
}

#' Wrapper for fBasics implementation of the Dagostino Omnibus K2 normality test.
#' Original goal was to replicate GraphPad Prism results, which have significiantly
#' lower required n than what fBasics test is set at. Since no other way to specify
#' alternate n to those functions, leveraging the GPL3-0 license and changing the
#' original function n to n < 8 for equivalency to avoid the original implementation
#' stop when n<20 not met. Leaving rest of the original functions untouched.
#'
#' @param x The data for your variable of interest
#' @param title Default is NULL in original
#' @param description Default is NULL in original
#'
#' @importClassesFrom fBasics fHTEST
#' @importFrom stats pchisq
#' @importFrom stats pnorm
#'
#' @return Normalization Test results
#'
#' @noRd
dagoTest <- function(x, title = NULL, description = NULL){
  # A function implemented by Diethelm Wuertz

  # Description:
  #   Performs the D'Agostino normality test

  # Arguments:
  #   x - a numeric vector or an univariate 'timeSeries' object.
  #   description - a brief description of the porject of type
  #       character.
  #   title - a character string which allows for a project title.

  # Source:
  #   This function was inspired by ...
  #   http://adela.karlin.mff.cuni.cz/~klaster/vyuka/

  # FUNCTION:

  # Data Set Name:
  DNAME = deparse(substitute(x))

  # Convert Type:
  if (inherits(x, "fREG")) x = residuals(x)
  x = as.vector(x)

  # Call:
  call = match.call()

  # Test:
  ans = NA
  test = .omnibus.test(x)
  skew = .skewness.test(x)
  kurt = .kurtosis.test(x)
  test$data.name = DNAME
  PVAL = c(test$p.value, skew$p.value, kurt$p.value)
  names(PVAL) = c(
    "Omnibus  Test",
    "Skewness Test",
    "Kurtosis Test")
  test$p.value = PVAL
  STATISTIC = c(test$statistic, skew$statistic, kurt$statistic)
  names(STATISTIC) = c(
    "Chi2 | Omnibus",
    "Z3  | Skewness",
    "Z4  | Kurtosis")
  test$statistic = STATISTIC
  class(test) = "list"

  # Add:
  if (is.null(title)) title = "D'Agostino Normality Test"
  if (is.null(description)) description = ""

  # Return Value:
  new("fHTEST",
      call = call,
      data = list(x = x),
      test = test,
      title = as.character(title),
      description = as.character(description) )
}


.omnibus.test <- function(x){
    # Internal Function for D'Agostino Normality Test:

    DNAME = deparse(substitute(x))
    if (exists("complete.cases")) {
      test = complete.cases(x)
    } else {
      test = !is.na(x)
    }
    x = x[test]
    n = length(x)
    if (n < 8) stop("sample size must be at least 8")
    meanX = mean(x)
    s =  sqrt(mean((x-meanX)**2))
    a3 = mean((x-meanX)**3)/s**3
    a4 = mean((x-meanX)**4)/s**4
    SD3 = sqrt(6*(n-2)/((n+1)*(n+3)))
    SD4 = sqrt(24*(n-2)*(n-3)*n/((n+1)**2*(n+3)*(n+5)))
    U3 = a3/SD3
    U4 = (a4-3+6/(n+1))/SD4
    b  = (3*(n**2+27*n-70)*(n+1)*(n+3))/((n-2)*(n+5)*(n+7)*(n+9))
    W2 = sqrt(2*(b-1))-1
    delta = 1/sqrt(log(sqrt(W2)))
    a = sqrt(2/(W2-1))
    Z3 = delta*log((U3/a)+sqrt((U3/a)**2+1))
    B = (6*(n*n-5*n+2)/((n+7)*(n+9)))*sqrt((6*(n+3)*(n+5))/(n*(n-2)*(n-3)))
    A = 6+(8/B)*((2/B)+sqrt(1+4/(B**2)))
    jm = sqrt(2/(9*A))
    pos0 = ((1-2/A)/(1+U4*sqrt(2/(A-4))))
    pos <- sign(pos0) * abs(pos0) ^ (1/3)

    Z4 = (1-2/(9*A)-pos)/jm
    omni = Z3**2+Z4**2
    pomni = 1-pchisq(omni,2)
    names(omni) = "Chi2"

    # Result:
    RVAL = list(
      statistic = omni,
      method = "D'Agostino Omnibus Normality Test",
      p.value = pomni,
      data.name = DNAME)

    # Return Value:
    class(RVAL) = "htest"
    RVAL
}
.kurtosis.test <-function(x) {
    # Internal Function for D'Agostino Normality Test:

    DNAME = deparse(substitute(x))

    if (exists("complete.cases")) {
      test = complete.cases(x)
    } else {
      test = !is.na(x)
    }
    x = x[test]
    n = length(x)
    if (n < 8) stop("Sample size must be at least 8")
    meanX = mean(x)
    s =  sqrt(mean((x-meanX)**2))
    a4 = mean((x-meanX)**4)/s**4
    SD4 = sqrt(24*(n-2)*(n-3)*n/((n+1)**2*(n+3)*(n+5)))
    U4 = (a4-3+6/(n+1))/SD4
    B = (6*(n*n-5*n+2)/((n+7)*(n+9)))*sqrt((6*(n+3)*(n+5))/(n*(n-2)*(n-3)))
    A = 6+(8/B)*((2/B)+sqrt(1+4/(B**2)))
    jm = sqrt(2/(9*A))
    pos0 = ((1-2/A)/(1+U4*sqrt(2/(A-4))))
    pos <- sign(pos0) * abs(pos0) ^ (1/3)

    Z4 = (1-2/(9*A)-pos)/jm
    pZ4 = 2*(1-pnorm(abs(Z4),0,1))
    names(Z4) = "Z4"

    # Result:
    RVAL = list(
      statistic = Z4,
      p.value = pZ4,
      method = "D'Agostino Kurtosis Normality Test",
      data.name = DNAME)

    # Return Value:
    class(RVAL) = "htest"
    RVAL
  }

.skewness.test <- function(x){
  # Internal Function for D'Agostino Normality Test:

  DNAME = deparse(substitute(x))
  if (exists("complete.cases")) {
    test = complete.cases(x)
  } else {
    test = !is.na(x)
  }
  x = x[test]
  n = length(x)
  if (n < 8) stop("Sample size must be at least 8")
  meanX = mean(x)
  s =  sqrt(mean((x-meanX)**2))
  a3 = mean((x-meanX)**3)/s**3
  SD3 = sqrt(6*(n-2)/((n+1)*(n+3)))
  U3 = a3/SD3
  b  = (3*(n**2+27*n-70)*(n+1)*(n+3))/((n-2)*(n+5)*(n+7)*(n+9))
  W2 = sqrt(2*(b-1))-1
  delta = 1/sqrt(log(sqrt(W2)))
  a = sqrt(2/(W2-1))
  Z3 = delta*log((U3/a)+sqrt((U3/a)**2+1))
  pZ3 = 2*(1-pnorm(abs(Z3),0,1))
  names(Z3) = "Z3"

  # Result:
  RVAL = list(
    statistic = Z3,
    p.value = pZ3,
    method = "D'Agostino Skewness Normality Test",
    data.name = DNAME)

  # Return Value:
  class(RVAL) = "htest"
  RVAL
}

