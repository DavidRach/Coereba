library(testthat)
library(shinytest2)

File_Location <- system.file("extdata", package = "Coereba")
TheCSV <- file.path(File_Location, "GateCutoffsForNKs.csv")
ShinyLocation <- system.file("shiny", package = "Coereba")

test_that("{shinytest2} recording: CoerebaApp_CSVOpens", {
  app <- AppDriver$new(app_dir = ShinyLocation, name = "CoerebaApp_CSVOpens", 
  height = 698, width = 1235)
  app$expect_values()
  app$upload_file(file = TheCSV)
  app$set_inputs(menu = "gatingset")
})