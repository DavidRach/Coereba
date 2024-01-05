.skewness.test <- function(x)
{
  # Internal Function for D'Agostino Normality Test:

  # Note:
  #   D'Agostino Test
  #   http://adela.karlin.mff.cuni.cz/~klaster/vyuka/
  #   Materi???ly pro cvicen????, kter??? byla v labu, jsou zde: cv01.txt,
  #   cv02.txt, cv03.txt, cv05.txt, cv06.txt, data maths, police a
  #   vysky a makro dagost.r. V????ber nejak????ch pr????kladu ze cvicen???? je
  #   tady.
  #   Program R lze zdarma (GNU General Public Licence) st???hnout z
  #   www.r-project.org. Alespon k letm???mu nahl???dnut???? doporucuji t????
  #   minimanu??/l An Introduction to R, kter???? roste tamt????. Dal?????
  #   materi???ly vcetne dvou zac???tecnick????ch pr????rucek najdete na
  #   str???nk???ch Dr. Kulicha.

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
