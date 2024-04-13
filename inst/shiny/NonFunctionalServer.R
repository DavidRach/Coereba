#' @noRd
#' @importFrom utils write.csv

server <- function(input, output, session) {
  click_info <- reactiveValues(click_data = data.frame(Plot_Name = character(),
                                                       X_Label = character(),
                                                       X_Coordinate = numeric(),
                                                       Time = character()))

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
                  x_coordinate: data.points[0].x,
                  annotation: x.layout.annotations[0].text
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

  observeEvent(input$plot_click, {
    print("Click event captured!")

    print(paste("Plot Name:", input$plot_click$plot_name))
    print(paste("X Label:", input$plot_click$annotation))
    print(paste("X Coordinate:", input$plot_click$x_coordinate))

    if (!is.null(input$plot_click$plot_name) && !is.null(input$plot_click$x_coordinate)) {
      timestamp <- Sys.time()  # Get current system time

      # Clean annotation text
      cleaned_annotation <- gsub("<b> | </b>", "", input$plot_click$annotation)

      cleaned_annotation <- gsub(" [^ ]+$", "", cleaned_annotation)

      if(nrow(click_info$click_data) == 0) {
        click_info$click_data <- data.frame(Plot_Name = input$plot_click$plot_name,
                                            X_Label = cleaned_annotation,
                                            X_Coordinate = input$plot_click$x_coordinate,
                                            Time = timestamp)
      } else {
        click_info$click_data <- rbind(click_info$click_data,
                                       data.frame(Plot_Name = input$plot_click$plot_name,
                                                  X_Label = cleaned_annotation,
                                                  X_Coordinate = input$plot_click$x_coordinate,
                                                  Time = timestamp))
      }
    }
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

  observeEvent(input$export_button, {
    export_filename <- paste(input$export_filename, ".csv", sep = "")
    export_path <- normalizePath(input$export_directory)

    # Check if the directory exists
    if (!dir.exists(export_path)) {
      showModal(modalDialog(
        title = "Error",
        "The specified directory does not exist.",
        footer = tagList(
          actionButton("close_modal", "Close")
        )
      ))
    } else {
      write.csv(click_info$click_data, file = file.path(export_path, export_filename), row.names = FALSE)
      showModal(modalDialog(
        title = "Export Complete",
        "Click data has been exported successfully.",
        footer = tagList(
          actionButton("close_modal", "Close")
        )
      ))
    }
  })

  observeEvent(input$close_modal, {
    removeModal()
  })

}



