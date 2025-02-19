test_that("Utility_Coereba returns with additional columns Clusters", {

# Prepare the test
library(flowCore)
library(flowWorkspace)
library(openCyto)
library(data.table)

File_Location <- system.file("extdata", package = "Coereba")
TheCSV <- file.path(File_Location, "GateCutoffsForNKs.csv")
FCS_Files <- list.files(path = File_Location, pattern = ".fcs", full.names = TRUE)
UnmixedFCSFiles <- FCS_Files[1]
UnmixedCytoSet <- load_cytoset_from_fcs(UnmixedFCSFiles,
  truncate_max_range = FALSE, transformation = FALSE)
UnmixedGatingSet <- GatingSet(UnmixedCytoSet)
Markers <- colnames(UnmixedCytoSet[[1]])
KeptMarkers <- Markers[-grep("Time|FS|SC|SS|Original|-W$|-H$|AF", Markers)]
biex_transform <- flowjo_biexp_trans(channelRange = 256, maxValue = 1000000,
  pos = 4.5, neg = 0, widthBasis = -1000)
TransformList <- transformerList(KeptMarkers, biex_transform)
flowWorkspace::transform(UnmixedGatingSet, TransformList)
UnmixedGates <- fread(file.path(path = File_Location,
  pattern = 'GatesUnmixed.csv'))
UnmixedGating <- gatingTemplate(UnmixedGates)
gt_gating(UnmixedGating, UnmixedGatingSet)
  
# Execute the test

CoerebaIDs <- Utility_Coereba(gs=UnmixedGatingSet[1], subsets="live",
sample.name="GROUPNAME", reference=TheCSV, starter="Spark Blue 550-A",
returnType = "data")
  
TheOldie <- length(Markers)
TheNew <- ncol(CoerebaIDs)

# Check Added number of rows
expect_equal((TheOldie+2), TheNew)

# Verify not identical
expect_false(identical(TheOldie, TheNew))
})