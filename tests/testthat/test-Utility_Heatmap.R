test_that("Utility_Heatmap returns a ggplot2 object", {

  # Prepare the test
  File_Location <- system.file("extdata", package = "Coereba")
  panelPath <- file.path(File_Location, "ILTPanelTetramer.csv")
  panelData <- read.csv(panelPath, check.names=FALSE)
  binaryPath <- file.path(File_Location, "HeatmapExample.csv")
  binaryData <- read.csv(binaryPath, check.names=FALSE)

  # Execute the test

  ThePlot <- Utility_Heatmap(binary=binaryData, panel=panelPath,
   export=FALSE, outpath=NULL, filename=NULL)

  # Did it return a data.frame
  expect_true(inherits(ThePlot, "gg"))
})