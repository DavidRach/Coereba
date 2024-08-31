#' Behind the Scenes server for CoerebaApp
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @importFrom DT datable
#' @importFrom DT renderDT
#' @importFrom htmltools div
#' @importFrom htmlwidgets onRender
#' @importFrom Luciernaga Utility_UnityPlot
#' @importFrom plotly plotlyOutput
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny renderTable
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny showModal
#' @importFrom shiny updateSelectInput
#' @importFrom utils read.csv
#' @importFrom utils write.csv
server <- function(input, output, session) {

  # What information from a recorded click
  # That becomes exported .csv data
  click_info <- reactiveValues(
    click_data = data.frame(Plot_Name = character(),
                            X_Label = character(),
                            X_Coordinate = numeric(),
                            Time = character()))

  # Finding the CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, check.names = FALSE)
  })

  # Reading the CSV file
  output$contents <- renderTable({data()})

  # Selecting GatingSet And Locating It
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

    # Clear Out Previous?
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
            plotly::plotlyOutput(paste0("plot_", i), width = "100%")
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

    })
  })

  observeEvent(input$plot_click, {

    message("Click event captured!")
    message("Plot Name:", input$plot_click$plot_name)
    message("X Label:", input$plot_click$annotation)
    message("X Coordinate:", input$plot_click$x_coordinate)

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

  output$clickDataTable <- DT::renderDT({
    #print("Rendering clickDataTable...")
    DT::datatable(
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



