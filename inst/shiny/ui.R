
# Home ---------
ui <- fluidPage(
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
  ),
  titlePanel("GatingSet Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("gatingSetName", "Enter GatingSet Object Name", value = ""),
      textInput("x", "Enter x-axis value", value = "Comp-APC-A"),
      textInput("y", "Enter y-axis value", value = "Comp-Spark Blue 550-A"),
      textInput("sampleName", "Enter Sample Name Keyword", value = "GROUPNAME"),
      numericInput("bins", "Enter desired bins", value = 70),
      numericInput("clearance", "Enter clearance multiplier", value = 0.2),
      textInput("removeStrings", "Enter string values to remove from the name", value = "c"),
      textInput("marginSubset", "Enter desired margin subset", value = "Tcells"),
      textInput("gateSubset", "Enter desired gate subset", value = "Vd2"),
      checkboxInput("gateLines", "Display Estimated Gate Cutoffs", value = TRUE),
      checkboxInput("pdfOutput", "Generate a PDF Output", value = FALSE),
      textInput("outputPath", "Enter a desired outpath for the pdf", value = "NULL"),
      actionButton("generatePlots", "Generate Plots")
    ),
    mainPanel(
      width = 9,
      uiOutput("plots")
    )
  )
)
