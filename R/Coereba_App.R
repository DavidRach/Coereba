#' Deploy a Shiny app to modify the estimated gate cutoff placement.
#' 
#' @importFrom shiny runApp tags tagList fileInput tableOutput sidebarLayout
#' @importFrom shiny sidebarPanel mainPanel textInput selectInput numericInput
#' @importFrom shiny checkboxInput actionButton uiOutput fluidRow column icon
#' @importFrom shiny reactive reactiveValues req renderTable observe observeEvent
#' @importFrom shiny updateSelectInput renderUI isolate showModal modalDialog
#' @importFrom shiny removeModal 
#' 
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#' @importFrom shinydashboard dashboardBody sidebarMenu menuItem tabItems tabItem
#'
#' @importFrom shinyjs useShinyjs
#' 
#' @importFrom plotly plotlyOutput renderPlotly ggplotly style
#'
#' @importFrom htmltools div
#' 
#' @importFrom htmlwidgets onRender
#'
#' @importFrom DT DTOutput renderDT datatable
#' 
#' @importFrom utils read.csv write.csv
#'
#' @return Launches the Shiny app
#' @export
#'
#' @examples
#'
#' library(flowCore)
#' library(flowWorkspace)
#' library(openCyto)
#' library(data.table)
#'
#' File_Location <- system.file("extdata", package = "Coereba")
#' TheCSV <- file.path(File_Location, "GateCutoffsForNKs.csv")
#'
#' FCS_Files <- list.files(path = File_Location, pattern = ".fcs", full.names = TRUE)
#' UnmixedFCSFiles <- FCS_Files[1]
#' UnmixedCytoSet <- load_cytoset_from_fcs(UnmixedFCSFiles,
#'  truncate_max_range = FALSE, transformation = FALSE)
#' UnmixedGatingSet <- GatingSet(UnmixedCytoSet)
#' Markers <- colnames(UnmixedCytoSet)
#' KeptMarkers <- Markers[-grep("Time|FS|SC|SS|Original|-W$|-H$|AF", Markers)]
#' biex_transform <- flowjo_biexp_trans(channelRange = 256, maxValue = 1000000,
#'  pos = 4.5, neg = 0, widthBasis = -1000)
#' TransformList <- transformerList(KeptMarkers, biex_transform)
#' flowWorkspace::transform(UnmixedGatingSet, TransformList)
#' UnmixedGates <- fread(file.path(path = File_Location,
#'  pattern = 'GatesUnmixed.csv'))
#' UnmixedGating <- gatingTemplate(UnmixedGates)
#' gt_gating(UnmixedGating, UnmixedGatingSet)
#'
#' # Coereba_App()
#'
Coereba_App <- function() {
    runApp(
    system.file("shiny", package="Coereba"),
    display.mode = "normal")
}
