#' Behind the Scenes UI for CoerebaApp
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
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
#' @return Shiny App UI side logic
#' @noRd
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Coereba"),

  ## Sidebar
  shinydashboard::dashboardSidebar(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$script(src = "tips.js"),
    shinydashboard::sidebarMenu(
      id = "menu",
      shinydashboard::menuItem("Upload CSV File", tabName = "data", icon = icon("table")),
      shinydashboard::menuItem("Upload a GatingSet", tabName = "gatingset", icon = icon("table")),
      shinydashboard::menuItem("Click Data", tabName = "clickdata", icon = icon("table"))
    )
  ),

  ## Body
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "data",
        titlePanel("Upload CSV File"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 3,
            fileInput("file", "Choose CSV File")
          ),
          shiny::mainPanel(
            htmltools::div(
              style = "max-height: 400px; overflow-y: auto;",
              tableOutput("contents")
            )
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "gatingset",
        titlePanel("GatingSet Explorer"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 3,
            textInput("gatingSetName", "Enter GatingSet Object Name", value = "gs"),
            selectInput("x_variable", "Select x-axis variable", choices = NULL),
            selectInput("y_variable", "Select y-axis variable", choices = NULL),
            textInput("sampleName", "Enter Sample Name Keyword", value = "GROUPNAME"),
            numericInput("bins", "Enter desired bins", value = 70),
            numericInput("clearance", "Enter clearance multiplier", value = 0.2),
            textInput("removeStrings", "Enter string values to remove from the name", value = "c"),
            textInput("marginSubset", "Enter desired margin subset", value = "Tcells"),
            textInput("gateSubset", "Enter desired gate subset", value = "Vd2"),
            checkboxInput("gateLines", "Display Estimated Gate Cutoffs", value = TRUE),
            actionButton("generatePlots", "Generate Plots")
          ),
          shiny::mainPanel(
            width = 9,
            uiOutput("plots")
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "clickdata",
        titlePanel("Click Data"),
        shiny::fluidRow(
          column(
            width = 6,
            textInput("export_filename", "Export Filename", value = "click_data")
          ),
          column(
            width = 6,
            textInput("export_directory", "Export Directory Path", placeholder = "/path/to/directory")
          )
        ),
        shiny::actionButton("export_button", "Export Click Data", icon = icon("download")),
        DT::DTOutput("clickDataTable")
      )
    )
  )
)
