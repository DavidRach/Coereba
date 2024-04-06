ui <- fluidPage(
  titlePanel("GatingSet Explorer"),
  sidebarLayout(
    sidebarPanel(
      textInput("gatingSetName", "Enter GatingSet Object Name", value = ""),
      textInput("x", "Enter x value", value = "Comp-BUV737-A"),
      textInput("y", "Enter y value", value = "Comp-Spark Blue 550-A"),
      textInput("sampleName", "Enter Sample Name", value = "GROUPNAME"),
      numericInput("bins", "Enter bins", value = 70),
      numericInput("clearance", "Enter clearance", value = 0.2),
      textInput("removeStrings", "Enter remove strings", value = ""),
      textInput("marginSubset", "Enter margin subset", value = "Tcells"),
      textInput("gateSubset", "Enter gate subset", value = "Vd2"),
      checkboxInput("gateLines", "Gate lines", value = TRUE),
      checkboxInput("pdfOutput", "Generate PDF Output", value = TRUE),
      textInput("outputPath", "Enter Output Path", value = ""),
      actionButton("generatePlots", "Generate Plots")
    ),
    mainPanel(
      uiOutput("plots")
    )
  )
)
