ui <- dashboardPage(
  dashboardHeader(title = "Coereba"),

  ## Sidebar
  dashboardSidebar(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$script(src = "tips.js"),
    sidebarMenu(
      id = "menu",
      menuItem("Upload CSV File", tabName = "data", icon = icon("table")),
      menuItem("Upload a GatingSet", tabName = "gatingset", icon = icon("table"))
    )
  ),

  ## Body
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(
        tabName = "data",
        titlePanel("Upload CSV File"),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            fileInput("file", "Choose CSV File")
          ),
          mainPanel(
            div(
              style = "max-height: 400px; overflow-y: auto;",
              tableOutput("contents")
            )
          )
        )
      ),
      tabItem(
        tabName = "gatingset",
        titlePanel("GatingSet Explorer"),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            textInput("gatingSetName", "Enter GatingSet Object Name", value = ""),
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
          mainPanel(
            width = 9,
            uiOutput("plots")
          )
        )
      )
    )
  )
)
