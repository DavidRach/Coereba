tmp <- withr::local_tempdir(pattern = "Coereba")
withr::local_dir(tmp)

# Prepare the test
library(flowCore)
library(flowWorkspace)
library(openCyto)
library(data.table)
library(plotly)

File_Location <- system.file("extdata", package = "Coereba")
CheckedGates <- file.path(File_Location, "ShinyGates.csv")

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

MarkersForToday <- c("Comp-BV570-A", "Comp-RB780-A", "Comp-BUV661-A", "Comp-PE-Vio770-A")

TheCoerebaData <- Utility_Coereba(gs=UnmixedGatingSet, subsets="root",
 sample.name="GROUPNAME", reference=CheckedGates, starter="Comp-PE-Vio770-A",
 inverse.transform = TRUE, returnType="data", Individual=FALSE, outpath=NULL,
 columns = MarkersForToday)

ThePanel <- data.frame(
  Fluorophore=c("BUV661", "BV570", "RB780", "PE-Vio770"),
  Marker=c("Vd2", "CD16", "CXCR5", "HLA-DR"))

File_Location <- system.file("extdata", package = "Coereba")
StudyMetadataPath <- file.path(File_Location, "SDY3080.csv")
StudyMetadata <- read.csv(StudyMetadataPath, check.names=FALSE)

  
MetadataOutpath <- tmp

MetadataTemplate <- Coereba_Processing(x=TheCoerebaData[[1]], metadataTemplate=TRUE,
   outpath=MetadataOutpath, panel=ThePanel)

MetadataLocation <- file.path(MetadataOutpath, "Coereba_metadataTemplate.csv")
InitialMetadata <- read.csv(MetadataLocation, check.names=FALSE)
  
Specimens <- InitialMetadata |> dplyr::pull(specimen) |> unique()

# Adding Adult Normalization Controls Not Present in Study Metadata
Specimens <- c(Specimens, "NY068_02", "NY068_03", "NY068_03", "NY068_4",
 "NY068_5", "NY068_6", "NY068_7", "NY068_8")
StudyMetadata <- StudyMetadata |> dplyr::filter(bid %in% Specimens)
StudyMetadata <- StudyMetadata |> dplyr::rename(specimen=bid)

CoerebaMetadata <- dplyr::left_join(InitialMetadata, StudyMetadata, by="specimen")

TheBioconductor <- Coereba_Processing(x=TheCoerebaData[[1]], panel=ThePanel,
     themetadata=CoerebaMetadata)

Data <- Coereba_MarkerExpressions(x=TheBioconductor,
     returnType="All", theassay="ratios")

CordOnly <- Data |> dplyr::filter(ptype %in% c("HU", "HEU-lo", "HEU-hi"))

shape_ptype <- c("HU" = 22, "HEU-lo" = 21, "HEU-hi" = 21)
fill_ptype <- c("HU" = "white", "HEU-lo" = "darkgray", "HEU-hi" = "black")
