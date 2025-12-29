test_that("Coereba_FCSExport returns a dataframe with 1 row", {

expect_true(length(TheCoerebaData) > 0)
  
  tmp <- withr::local_tempdir(pattern = "Coereba")
  withr::local_dir(tmp)

  FCSFile <- Coereba::Coereba_FCSExport(data=TheCoerebaData[[1]],
     gs=UnmixedGatingSet[1], returnType="fcs",
      outpath=tmp, filename="CoerebaTest", nameAppend="",
      Aggregate=FALSE)
  
  FCSFiles <- list.files(tmp, pattern="CoerebaTest.fcs")

expect_true(length(FCSFiles) == 1)
  
  FCSPath <- file.path(tmp, "CoerebaTest.fcs")
  RetrievedData <- Coereba::Coereba_FCS_Reversal(Coereba=FCSPath)

  expect_s3_class(RetrievedData, "data.frame") 
  expect_true(all(c("specimen", "Cluster") %in% colnames(RetrievedData)))
  
})