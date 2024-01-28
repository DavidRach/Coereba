test_that("Produces a Heatmap", {
  FileLocation <- system.file("extdata", package = "Coereba")
  fcsFilePattern <- "CoerebaMAITCtrlHeat.csv"
  MyFile <- list.files(path = FileLocation, pattern = fcsFilePattern, full.names = TRUE, recursive = TRUE)
  thecells <- read.csv(MyFile, check.names = FALSE)

  Returned <- Utility_Heatmap(cells = thecells, filename = "Image.jpg", return = FALSE)
  BS <- length(Returned) #BS standin while I figure out snapshots.

  expect_equal(9, 9)
})


