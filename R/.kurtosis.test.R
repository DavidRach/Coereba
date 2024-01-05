.kurtosis.test <-
  function(x)
  {
    # Internal Function for D'Agostino Normality Test:

    # FUNCTION:

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
    ## (2023-02-27) Georgi: 'pos' becomes NaN for a power of negative number, reported by
    ##         Cameron Wilden. Implementing his suggestion for fix.
    ## was: pos = ((1-2/A)/(1+U4*sqrt(2/(A-4))))**(1/3)
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
