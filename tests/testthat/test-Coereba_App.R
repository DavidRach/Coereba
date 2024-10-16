library(testthat)
library(shinytest2)

File_Location <- system.file("extdata", package = "Coereba")
TheCSV <- file.path(File_Location, "GateCutoffsForNKs.csv")
ShinyLocation <- system.file("shiny", package = "Coereba")

test_that("{shinytest2} recording: CVSLocate", {
  app <- AppDriver$new(app_dir = ShinyLocation, name = "CVSLocate",
    height = 959, width = 1619)
  app$upload_file(file = TheCSV)
  app$set_inputs(menu = "gatingset")
  app$expect_values()
})