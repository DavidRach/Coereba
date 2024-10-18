
#' A function to generate the heatmaps seen in the paper, not yet generalizable
#' to other panel context.
#'
#' @param data A path to a .csv file or a data.frame of Markers by Cell Pop
#' @param RemoveMarkers Default is NULL, otherwise a list of markers to exclude.
#' @param MarkerOrder Default is null, otherwise provide a list of markers in order desired.
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom utils read.csv
#' @importFrom dplyr select
#' @importFrom tidyselect any_of
#' @importFrom tidyselect all_of
#' @importFrom tidyselect where
#' @importFrom tidyselect everything
#' @importFrom dplyr relocate
#' @importFrom tibble rownames_to_column
#' @importFrom gt gt
#' @importFrom gt tab_style
#' @importFrom gt cell_fill
#' @importFrom gt cell_text
#' @importFrom gt cells_body
#' @importFrom gt cols_width
#' @importFrom gt sub_values
#' @importFrom gt opt_table_font
#' @importFrom gt cols_align
#'
#' @return A gt table ready for modification
#' @export
#'
#' @noRd
Coereba_Heatmap <- function(data, RemoveMarkers=NULL, MarkerOrder=NULL,
                            ColorFill=TRUE, ShapeFill=TRUE){

  Color1 <- brewer.pal(5, "OrRd")[1]
  Color2 <- brewer.pal(5, "OrRd")[2]
  Color3 <- brewer.pal(5, "OrRd")[3]
  Color4 <- brewer.pal(5, "OrRd")[4]
  Color5 <- brewer.pal(5, "OrRd")[5]

  if (!is.data.frame(data)){
    TheExpressionData <- read.csv(data, check.names = FALSE)
  } else {TheExpressionData <- data}

  if(!is.null(RemoveMarkers)){
    TheExpressionData <- TheExpressionData %>% select(-any_of(RemoveMarkers))
  }

  if(!is.null(MarkerOrder)){
    Pops <- TheExpressionData %>% select(!where(is.numeric))
    Markers <- TheExpressionData %>% select(where(is.numeric)) %>%
      relocate(all_of(MarkerOrder))
    TheExpressioNData <- cbind(Pops, Markers)
  }

  RoundedExpressions <- TheExpressionData %>%
    select(where(is.numeric)) %>% round(.,1)
  Pops <- TheExpressionData %>% select(!where(is.numeric))
  UpdatedData <- cbind(Pops, RoundedExpressions)
  TheTable <- data.frame(t(UpdatedData), check.names=FALSE)
  colnames(TheTable) <- TheTable[1,]
  TheTable <- TheTable[-1,]
  TheTable <- TheTable %>% rownames_to_column(., var="Marker")
  TheColumns <- colnames(TheTableMedians)
  TheColumns <- TheColumns[-1]

  TheDataTable <- TheTable %>% gt()

  if (ColorFill == TRUE){
  Filled <- TheDataTable |> tab_style(
    style = list(
      cell_fill(color = "white"),
      cell_text(weight = "bold"),
      cell_text(align = "center")
    ),
    locations = lapply(c(TheColumns), revbuilder, Limit = 0.1)
  ) |> tab_style(
    style = list(
      cell_fill(color = Color1),
      cell_text(weight = "bold"),
      cell_text(align = "center")
    ),
    locations = lapply(c(TheColumns), betweenbuilder, Limit1 = 0.11, Limit2 = 0.3)
  ) |> tab_style(
    style = list(
      cell_fill(color = Color2),
      cell_text(weight = "bold"),
      cell_text(align = "center")
    ),
    locations = lapply(c(TheColumns), betweenbuilder, Limit1 = 0.3, Limit2 = 0.5)
  ) |> tab_style(
    style = list(
      cell_fill(color = Color3),
      cell_text(weight = "bold"),
      cell_text(align = "center")
    ),
    locations = lapply(c(TheColumns), betweenbuilder, Limit1 = 0.5, Limit2 = 0.8)
  ) |> tab_style(
    style = list(
      cell_fill(color = Color4),
      cell_text(weight = "bold"),
      cell_text(align = "center")
    ),
    locations = lapply(c(TheColumns), builder, Limit = 0.8)
  ) |> tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_text(align = "left")
    ),
    locations = cells_body(columns = Marker)
  ) |> cols_width(Marker ~ px(110),
                  everything() ~ px(80))
  }

  if (ShapeFill == TRUE && ColorFill == TRUE){
    Substituted <- Filled  |>
      sub_values(values= c(0.8, 0.9, 1), replacement = "+++") |>
      sub_values(values= "1.0", replacement = "+++") |>
      sub_values(values= c(0, 0.1), replacement = "-") |>
      sub_values(values= "0.0", replacement = "-") |>
      sub_values(values= c(0.11, 0.2), replacement = "-/+")|>
      sub_values(values= c(0.3, 0.4), replacement = "+") |>
      sub_values(values= c(0.5, 0.6, 0.7), replacement = "++")
  }

  Bolded <- Substituted |>
    opt_table_font(font = "Montserrat") |>
    cols_align(align = "center")

  return(Bolded)
}

builder <- function(x, Limit){
  #Credit for solving: #StackOverflow #61435048
  cells_body(columns = !!sym(x), rows = !!sym(x) >= Limit)
}

revbuilder <- function(x, Limit){
  #Credit for solving: #StackOverflow #61435048
  cells_body(columns = !!sym(x), rows = !!sym(x) < Limit)
}

betweenbuilder <- function(x, Limit1, Limit2){
  #Credit for solving: #StackOverflow #61435048
  cells_body(columns = !!sym(x), rows = !!sym(x) >= Limit1 & !!sym(x) < Limit2)
}






