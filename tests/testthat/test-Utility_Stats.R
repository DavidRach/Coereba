test_that("Utility_Stats returns a dataframe with 1 row", {
  # Prepare the test

  File_Location <- system.file("extdata", package = "Coereba")
  panelPath <- file.path(File_Location, "ILTPanelTetramer.csv")
  binaryPath <- file.path(File_Location, "HeatmapExample.csv")
  dataPath <- file.path(File_Location, "ReadyFileExample.csv")
  panelData <- read.csv(panelPath, check.names=FALSE)
  binaryData <- read.csv(binaryPath, check.names=FALSE)
  dataData <- read.csv(dataPath, check.names=FALSE)
  
  All <- Coereba_MarkerExpressions(data=dataData, binary=binaryData,
    panel=panelData, starter="SparkBlue550")

  # Execute the test
  TheStats <- Utility_Stats(data=All, var="CD62L", 
    myfactor="ptype", normality="dagostino")

  # Is it more than 1 row
  expect_equal(nrow(TheStats), 1)
})