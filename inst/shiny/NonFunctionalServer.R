click_data <- reactiveValues(data = data.table(plot_name = character(), x_coord = numeric()))

server <- function(input, output, session) {

  # Read the uploaded CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, check.names = FALSE)
  })

  # Render the contents of the uploaded CSV file
  output$contents <- renderTable({
    data()
  })

  # Extract column names for dropdown menu
  observe({
    col_names <- names(data())
    updateSelectInput(session, "x_variable", choices = col_names)
  })

  observe({
    col_names <- names(data())
    updateSelectInput(session, "y_variable", choices = col_names)
  })

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

  observeEvent(input$generatePlots, {
    req(data())
    gatingSet <- gatingSet_data()
    req(gatingSet)

    # Call your function here with the provided inputs
    # Assuming your_function returns a list of ggplot objects
    plots_list <- Luciernaga::Utility_UnityPlot(
      x = input$x_variable,
      y = input$y_variable,
      GatingSet = gatingSet,
      sample.name = input$sampleName,
      bins = input$bins,
      clearance = input$clearance,
      removestrings = input$removeStrings,
      marginsubset = input$marginSubset,
      gatesubset = input$gateSubset,
      gatelines = input$gateLines,
      reference = data(),
      pdf = FALSE,
      outpath = NULL
    )

    # Output each plot
    output$plots <- renderUI({
      tagList(
        lapply(seq_along(plots_list), function(i) {
          plotlyOutput(paste0("plot", i))
        })
      )
    })

    for (i in seq_along(plots_list)) {
      output[[paste0("plot", i)]] <- renderPlotly({
        plots_list[[i]] %>%
          event_register("plotly_click") # Register click event
      })
    }
  })

  # Handle click events
  observeEvent(event_data("plotly_click"), {
    click_info <- event_data("plotly_click")

    # Extract plot name and x-coordinate
    plot_name <- click_info$source
    x_coord <- click_info$x

    # Add click information to reactive dataset
    click_data$data <- rbind(click_data$data, data.table(plot_name = plot_name, x_coord = x_coord))
  })

  output$clickDataTable <- renderDataTable({
    click_data$data
  })

}



