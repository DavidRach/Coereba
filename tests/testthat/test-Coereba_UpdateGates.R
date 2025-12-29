test_that("Coereba_UpdateGates returns a dataframe with 1 row", {

  File_Location <- system.file("extdata", package = "Coereba")
  TheOldCSV <- file.path(File_Location, "SecondGates.csv")
  TheClickInfo <- file.path(File_Location, "CorrectSplitpointLocations.csv")
  TheClickData <- read.csv(TheClickInfo, check.names=FALSE)

  # Execute the test
  UpdatedCSV <- Coereba_UpdateGates(Clicks=TheClickInfo, Old=TheOldCSV,
    export=FALSE, outpath=NULL, fileName="UpdatedCSV")
  
  TheOld <- read.csv(TheOldCSV, check.name=FALSE)
  TheOldie <- TheOld |> dplyr::select(`Comp-BV570-A`)
  TheNew <- UpdatedCSV |> dplyr::select(`Comp-BV570-A`)
  
  # Check equal number of rows
  expect_equal(nrow(TheOldie), nrow(TheNew))

  # Verify not identical
  expect_false(identical(TheOldie, TheNew))
})