test_that("Coereba_GateCutoffs returns a dataframe with 1 row", {

# Prepare the test
library(flowCore)
library(flowWorkspace)
library(openCyto)
library(data.table)
library(plotly)

File_Location <- system.file("extdata", package = "Coereba")
FCS_Files <- list.files(path = File_Location, pattern = ".fcs", full.names = TRUE)
UnmixedFCSFiles <- FCS_Files[1]
UnmixedCytoSet <- load_cytoset_from_fcs(UnmixedFCSFiles,
  truncate_max_range = FALSE, transformation = FALSE)
UnmixedGatingSet <- GatingSet(UnmixedCytoSet)
Markers <- colnames(UnmixedCytoSet[[1]])
KeptMarkers <- Markers[-grep("Time|FS|SC|SS|Original|-W$|-H$|AF", Markers)]
biex_transform <- flowjo_biexp_trans(channelRange = 256, maxValue = 1000000,
  pos = 4.5, neg = 0, widthBasis = -1000)
TransformList <- flowWorkspace::transformerList(KeptMarkers, biex_transform)
UnmixedGatingSet <- flowWorkspace::transform(UnmixedGatingSet, TransformList)
UnmixedGates <- fread(file.path(path = File_Location,
  pattern = 'GatesUnmixed.csv'))
UnmixedGating <- gatingTemplate(UnmixedGates)
gt_gating(UnmixedGating, UnmixedGatingSet)

# Execute the test
TheGateCutoffs <- Coereba_GateCutoffs(gs=UnmixedGatingSet[1],
  subset="live", sample.name="GROUPNAME")

# Is it more than 1 row
expect_equal(nrow(TheGateCutoffs), 1)
})
