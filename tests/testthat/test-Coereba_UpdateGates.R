test_that("Coereba_UpdateGates returns a dataframe with 1 row", {
  library(dplyr)

  # Prepare the test
  File_Location <- system.file("extdata", package = "Coereba")
  TheOldCSV <- file.path(File_Location, "GateCutoffsForNKs.csv")
  TheClickInfo <- file.path(File_Location, "ClickDataExample.csv")

  # Execute the test
  UpdatedCSV <- Coereba_UpdateGates(Clicks=TheClickInfo, Old=TheOldCSV,
    export=FALSE, outpath=NULL, fileName="UpdatedCSV")
  
  TheOld <- read.csv(TheOldCSV, check.name=FALSE)
  TheOldie <- TheOld %>% select(`BUV496-A`)
  TheNew <- UpdatedCSV %>% select(`BUV496-A`)
  
  # Check equal number of rows
  expect_equal(nrow(TheOldie), nrow(TheNew))

  # Verify not identical
  expect_false(identical(TheOldie, TheNew))
})