#' Returns a Coereba Heatmap
#'
#' @param cells Unclear, Coereba output including Cluster column
#' @param filename Name to save .png file as
#' @param return Whether to save the file as a png, TRUE/FALSE
#' @param panel A csv file containing a panel
#'
#' @importFrom utils read.csv
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr relocate
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot
#' @importFrom viridis scale_fill_viridis
#' @importFrom ggplot ggsave
#' @importFrom magrittr %>%
#'
#' @return Some additional value to edit
#' @export
#'
#' @examples NULL
Utility_Heatmap <- function(cells, filename, return, panel){

  MyPanel <- read.csv(panel)
  filteredCells <- cells
  Fluorophore <- filteredCells %>% select(2:length(filteredCells)) %>%
    colnames()
  Fluorophore <- data.frame(Fluorophore)
  Attempt2 <- left_join(Fluorophore, MyPanel, by = "Fluorophore")
  Names <- Attempt2$Marker
  colnames(filteredCells)[2:length(filteredCells)] <- Names
  #filteredCells

  theCells <- filteredCells %>% mutate(Cluster = row_number()) %>%
    relocate(Cluster, .after = Identity)
  retained <- colnames(theCells[,1:2])
  MeltedCells <- melt(theCells, id = retained)
  MeltedCells$Cluster <- factor(MeltedCells$Cluster)
  MeltedCells$variable <- factor(MeltedCells$variable)

  MyHeatmap <- ggplot(MeltedCells, aes(Cluster, variable, fill = value)) +
    geom_tile() +
    scale_fill_viridis(option = "cividis", discrete=FALSE) +
    theme_classic() +
    theme(legend.position = "none", axis.text.x = element_text(
      size = 5, angle = 300)) + labs(y = NULL)
  MyHeatmap

  if (return == TRUE){ggsave(filename, MyHeatmap,
                             dpi = 600, units = "in", width = 6, height = 4)}

  return(MyHeatmap)
}



#' Internal for Utility_Heatmap
#'
#' @param filteredCells Something
#' @param panel Something
#' @param ... Something
#'
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr row_number
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot
#' @importFrom viridis scale_fill_viridis
#'
#' @keywords internal
.Internal_Heatmap <- function(filteredCells, panel, ...){
  #MyPanel <- read.csv(panel, check.names = FALSE)
  MyPanel <- panel
  Fluorophore <- filteredCells %>% select(2:length(filteredCells)) %>%
    colnames()
  Fluorophore <- data.frame(Fluorophore)
  Attempt2 <- left_join(Fluorophore, MyPanel, by = "Fluorophore")
  Names <- Attempt2$Marker
  colnames(filteredCells)[2:length(filteredCells)] <- Names
  #filteredCells

  theCells <- filteredCells %>% mutate(Cluster = row_number()) %>%
    relocate(Cluster, .after = Identity)
  retained <- colnames(theCells[,1:2])
  MeltedCells <- melt(theCells, id = retained)
  MeltedCells$Cluster <- factor(MeltedCells$Cluster)
  MeltedCells$variable <- factor(MeltedCells$variable)

  MyHeatmap <- ggplot(MeltedCells, aes(Cluster, variable, fill = value)) +
    geom_tile() +
    scale_fill_viridis(option = "cividis", discrete=FALSE) +
    theme_classic() +
    theme(legend.position = "none", axis.text.x = element_text(
      size = 5, angle = 300)) + labs(y = NULL)
  MyHeatmap
}


