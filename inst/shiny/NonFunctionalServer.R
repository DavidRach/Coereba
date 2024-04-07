server <- function(input, output, session) {
  #click_info <- reactiveValues(click_data = data.frame(Plot_Name = character(), X_Coordinate = numeric()))

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
      div(
        style = "display: flex; flex-wrap: wrap;",
        lapply(seq_along(plots_list), function(i) {
          div(
            style = "width: 33.33%;",
            plotlyOutput(paste0("plot_", i), width = "100%")
          )
        })
      )
    })

    for (i in seq_along(plots_list)) {
      local({
        idx <- i  # Create a local copy of i
        output[[paste0("plot_", idx)]] <- renderPlotly({
          p <- ggplotly(plots_list[[idx]])

          # Capture plotly click event
          p <- htmlwidgets::onRender(
            p,
            "
            function(el, x) {
              el.on('plotly_click', function(data) {
                console.log('Click event captured');
                console.log(data);
                Shiny.setInputValue('plot_click', {
                  plot_name: x.layout.title.text,
                  x_coordinate: data.points[0].x
                });
              });
            }
            "
          )

          p

        })
      })
    }

  })

  # Observe the click event and store information
  observeEvent(input$plot_click, {
    print("Click event captured!")

    print(paste("Plot Name:", input$plot_click$plot_name))
    print(paste("X Coordinate:", input$plot_click$x_coordinate))

    #if(nrow(click_info$click_data) == 0) {
    #  click_info$click_data <- data.frame(Plot_Name = input$plot_click$plot_name,
    #                                      X_Coordinate = input$plot_click$x_coordinate)
    #} else {
    #  click_info$click_data <- rbind(click_info$click_data,
    #                                 data.frame(Plot_Name = input$plot_click$plot_name,
    #                                            X_Coordinate = input$plot_click$x_coordinate))
    #}
  })

  # Render the clickDataTable with the updated click data
  output$clickDataTable <- renderDT({
    print("Rendering clickDataTable...")
    datatable(
      click_info$click_data,
      options = list(
        dom = 'tip'
      )
    )
  })

}



