test_that("Utility_Coereba returns with additional columns Clusters", {

expect_true(length(UnmixedGatingSet) > 0)
 
File_Location <- system.file("extdata", package = "Coereba")

CheckedGates <- file.path(File_Location, "ShinyGates.csv")

CoerebaData <- Utility_Coereba(gs=UnmixedGatingSet, subsets="root",
 sample.name="GROUPNAME", reference=CheckedGates, starter="Comp-PE-Vio770-A",
 inverse.transform = TRUE, returnType="data", Individual=FALSE, outpath=NULL,
 columns = MarkersForToday)
  
 expect_s3_class(CoerebaData[[1]], "data.frame") 
 expect_true(all(c("specimen", "Cluster") %in% colnames(CoerebaData[[1]])))
})