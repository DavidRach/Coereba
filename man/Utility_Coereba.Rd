% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utility_Coereba.R
\name{Utility_Coereba}
\alias{Utility_Coereba}
\title{Runs Coereba for Dichotomized Gating Annotation}
\usage{
Utility_Coereba(
  gs,
  subsets,
  sample.name,
  subsample = NULL,
  columns = NULL,
  notcolumns = NULL,
  reference,
  starter,
  inverse.transform = TRUE,
  returnType,
  Individual = FALSE,
  outpath = NULL,
  filename = NULL,
  nameAppend = NULL
)
}
\arguments{
\item{gs}{A Gating Set object}

\item{subsets}{The desired GatingHierarchy subset}

\item{sample.name}{Keyword for sample name}

\item{subsample}{An optional downsample, doesn't work with inverse.transform=TRUE}

\item{columns}{An optional way to select columns to keep.}

\item{notcolumns}{An optional way to remove select columns.}

\item{reference}{The external data.frame or path to the .csv with the
specified gate split points by specimen and marker.}

\item{starter}{The column name to start the splits with}

\item{inverse.transform}{Whether to reverse the data transform after Coereba cluster
is calculated, Default is set to TRUE to allow for .fcs export.}

\item{returnType}{Whether to return data, flowframe or fcs}

\item{Individual}{Default FALSE, when TRUE, returns individual fcs instead of grouped.}

\item{outpath}{Default NULL, file.path for fcs file storage}

\item{filename}{Default NULL, sets name for aggregated flowframe or fcs}

\item{nameAppend}{For flowframe and fcs returnType, what gets appended before .fcs}
}
\value{
Either data, flowframe or fcs, individually or concatinated
}
\description{
Runs Coereba for Dichotomized Gating Annotation
}
\examples{

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

CoerebaIDs <- Utility_Coereba(gs=UnmixedGatingSet[1], subsets="live",
 sample.name="GROUPNAME", reference=TheCSV, starter="Spark Blue 550-A",
 returnType="flowframe", Individual=TRUE)

}
