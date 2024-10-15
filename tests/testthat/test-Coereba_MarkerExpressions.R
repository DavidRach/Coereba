test_that("Coereba_MarkerExpressions returns a dataframe with more than 1 row", {
  # Prepare the test

  File_Location <- system.file("extdata", package = "Coereba")
  panelPath <- file.path(File_Location, "ILTPanelTetramer.csv")
  binaryPath <- file.path(File_Location, "HeatmapExample.csv")
  dataPath <- file.path(File_Location, "ReadyFileExample.csv")
  panelData <- read.csv(panelPath, check.names=FALSE)
  binaryData <- read.csv(binaryPath, check.names=FALSE)
  dataData <- read.csv(dataPath, check.names=FALSE)

  # Execute the test
  Memory <- Coereba_MarkerExpressions(data=dataData, binary=binaryData,
     panel=panelData, starter="SparkBlue550", returnType = "Combinatorial",
     CombinatorialArgs=c("BV510", "APC-Fire 750"))
  
  All <- Coereba_MarkerExpressions(data=dataData, binary=binaryData,
    panel=panelData, starter="SparkBlue550")
  
  # Is it more than 1 row
  expect_gt(nrow(Memory), 1)

  expect_gt(nrow(All), 1)
})
