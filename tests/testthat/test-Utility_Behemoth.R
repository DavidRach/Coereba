test_that("Utility_Behemoth returns a ggplot2 object", {

  # Prepare the test
  shape_ptype <- c("HU" = 22, "HEU-lo" = 21, "HEU-hi" = 21)
  fill_ptype <- c("HU" = "white", "HEU-lo" = "darkgray", "HEU-hi" = "black")

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
  Plot <- Utility_Behemoth(data=All, var="CD62L", myfactor="ptype",
  normality="dagostino", correction="none", shape_palette=shape_ptype,
  fill_palette=fill_ptype, XAxisLevels = c("HU", "HEU-lo", "HEU-hi"))

  # Did it return a data.frame
  expect_true(inherits(Plot, "gg"))
})
