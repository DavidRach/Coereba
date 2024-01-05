dagoTest <- function(x, title = NULL, description = NULL)
{
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
