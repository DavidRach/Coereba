server <- function(input, output, session) {
  # Access the GatingSet object from the active R environment
  gatingSet_data <- reactive({
    gatingSetName <- input$gatingSetName
    if (is.null(gatingSetName) || gatingSetName == "") {
      return(NULL)
    } else {
      gatingSet <- get(gatingSetName, envir = .GlobalEnv)
      return(gatingSet)
    }
  })

  # Render ggplot objects dynamically
  observeEvent(input$generatePlots, {
    if (!is.null(gatingSet_data())) {
      # Call your function here with the provided inputs
      # Assuming your_function returns a list of ggplot objects
      plots_list <- Luciernaga::Utility_UnityPlot(
        x = input$x,
        y = input$y,
        GatingSet = gatingSet_data(),
        sample.name = input$sampleName,
        bins = input$bins,
        clearance = input$clearance,
        removestrings = input$removeStrings,
        marginsubset = input$marginSubset,
        gatesubset = input$gateSubset,
        gatelines = input$gateLines,
        reference = MyGateCutoffs, # Assuming MyGateCutoffs is already defined
        pdf = input$pdfOutput,
        outpath = input$outputPath
      )
      plot_output_list <- lapply(seq_along(plots_list), function(i) {
        plotOutput(paste0("plot", i))
      })
      lapply(seq_along(plots_list), function(i) {
        output[[paste0("plot", i)]] <- renderPlot({
          print(plots_list[[i]])
        })
      })
      output$plots <- renderUI({
        tagList(plot_output_list)
      })
    }
  })
}




