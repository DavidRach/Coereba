#' @noRd
#' @importFrom utils write.csv

server <- function(input, output, session) {
  click_info <- reactiveValues(click_data = data.frame(Plot_Name = character(),
                                                       X_Label = character(),
                                                       X_Coordinate = numeric(),
                                                       Time = character()))

  # Finding the CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, check.names = FALSE)
  })

  # Seeing the CSV file
  output$contents <- renderTable({
    data()
  })

  # Selecting the GatingSet
  gatingSet_data <- reactive({
    gatingSetName <- input$gatingSetName
    if (is.null(gatingSetName) || gatingSetName == "") {
      return(NULL)
    } else {
      gatingSet <- get(gatingSetName, envir = .GlobalEnv)
      return(gatingSet)
    }
  })

  # Selecting the X and Y parameters
  observe({
    col_names <- names(data())
    updateSelectInput(session, "x_variable", choices = col_names)
    updateSelectInput(session, "y_variable", choices = col_names)
  })

  # When we click Generate Plots

  observeEvent(input$generatePlots, {
    req(data())
    gatingSet <- gatingSet_data()
    req(gatingSet)

    # Refreshing (attempt to fix slow bug)
    output$plots <- renderUI({NULL})

    # Additional Attempt to Speed Along
    isolate({

    # Sending variables to Luciernaga
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
      returntype = "plots",
      outpath = NULL
    )

    # Getting the Plots
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
        idx <- i  # Makes a local copy - DTR
        output[[paste0("plot_", idx)]] <- renderPlotly({
          p <- ggplotly(plots_list[[idx]])

          # Detects the click event  - DTR
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

    gc() # May not be Bioconductor Legal

    })
  })

  observeEvent(input$plot_click, {
    print("Click event captured!")

    print(paste("Plot Name:", input$plot_click$plot_name))
    print(paste("X Label:", input$plot_click$annotation))
    print(paste("X Coordinate:", input$plot_click$x_coordinate))

    if (!is.null(input$plot_click$plot_name) && !is.null(input$plot_click$x_coordinate)) {
      timestamp <- Sys.time()

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

  output$clickDataTable <- renderDT({
    print("Rendering clickDataTable...")
    datatable(
      click_info$click_data, options = list(dom = 'tip'
      )
    )
  })

  observeEvent(input$export_button, {
    export_filename <- paste(input$export_filename, ".csv", sep = "")
    export_path <- normalizePath(input$export_directory)

    if (!dir.exists(export_path)) {
      showModal(modalDialog(
        title = "Error",
        "The specified directory does not exist.",
        footer = tagList(actionButton("close_modal", "Close")
        )
      ))
    } else {
      write.csv(click_info$click_data, file = file.path(export_path, export_filename), row.names = FALSE)
      showModal(modalDialog(
        title = "Export Complete",
        "Click data has been exported successfully.",
        footer = tagList(actionButton("close_modal", "Close")
        )
      ))
    }
  })

  observeEvent(input$close_modal, {removeModal()})

}



