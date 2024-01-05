.omnibus.test <-
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
    ## Georgi: see similar change in .kurtosis.test()
    ## was: pos = ((1-2/A)/(1+U4*sqrt(2/(A-4))))**(1/3)
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
