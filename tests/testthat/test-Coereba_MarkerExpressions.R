test_that("Coereba_MarkerExpressions returns a dataframe with
 more than 1 row", {
 
  Data <- Coereba_MarkerExpressions(x=TheBioconductor,
     returnType="All", theassay="ratios")
   
  expect_s3_class(Data, "data.frame")
  expect_equal(nrow(Data), 1)
  expect_equal(ncol(Data), 7)
 })
