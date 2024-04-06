server <- function(input, output, session) {
  
  # Read uploaded CSV file into a data frame
  data <- reactive({
    req(input$file) # Make sure file is uploaded
    inFile <- input$file
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, check.names = FALSE)
  })
  
  # Show contents of the uploaded file as a table
  output$contents <- renderTable({
    data()
  })
  
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
  output$plots <- renderUI({
    req(gatingSet_data())
    
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
      plotOutput(paste0("plot", i), click = paste0("plot", i, "_click"))
    })
    tagList(plot_output_list)
  })
  
  # Initialize reactiveVals for clicked_x
  clicked_x_list <- reactiveVal(list())
  
  # Render each plot
  observeEvent(input$generatePlots, {
    req(gatingSet_data())
    
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
    
    for (i in seq_along(plots_list)) {
      output[[paste0("plot", i)]] <- renderPlot({
        # Get clicked x-coordinate for this plot
        clicked_x <- clicked_x_list()[[paste0("plot", i)]]
        
        # Add blue line if a click has occurred
        if (!is.null(clicked_x)) {
          plots_list[[i]] <- plots_list[[i]] +
            geom_vline(xintercept = clicked_x, color = "blue")
        }
        
        print(plots_list[[i]])
      })
    }
    
    # Capture click event and store clicked x-coordinate
    observeEvent(input$plot_click, {
      click <- input$plot_click
      if (!is.null(click)) {
        # Determine which plot was clicked
        plot_id <- gsub("_click", "", click$id)
        # Get x-coordinate of the click
        clicked_x_list(list_modify(clicked_x_list(), !!plot_id := click$x))
      }
    })
  })
}


workingserver <- function(input, output, session) {
  # Reactive values to store clicked x-coordinate and plot ID
  clicked_x <- reactiveValues(value = NULL, plot_id = 1)
  
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
      
      # Store the number of plots
      num_plots <- length(plots_list)
      
      # Dynamically generate plots
      output$plots <- renderUI({
        plot_output_list <- lapply(seq_along(plots_list), function(i) {
          plotOutput(paste0("plot", i), click = "plot_click")
        })
        tagList(plot_output_list)
      })
      
      lapply(seq_along(plots_list), function(i) {
        output[[paste0("plot", i)]] <- renderPlot({
          # Check if a click has occurred and if it matches the current plot
          if (!is.null(clicked_x$value) && i == clicked_x$plot_id) {
            plots_list[[i]] <- plots_list[[i]] +
              geom_vline(xintercept = clicked_x$value, color = "blue")
          }
          print(plots_list[[i]])
        })
      })
    }
  })
  
  # Capture click event and store clicked x-coordinate
  observeEvent(input$plot_click, {
    click <- input$plot_click
    if (!is.null(click)) {
      # Determine which plot was clicked
      plot_id <- as.numeric(gsub("plot", "", click$id))
      # Get x-coordinate of the click
      clicked_x$value <- click$x
      clicked_x$plot_id <- plot_id
    }
  })
}
