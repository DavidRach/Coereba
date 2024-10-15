#' Returns a Coereba Heatmap
#'
#' @param binary A data.frame of markers by cluster
#' @param panel A csv or data.frame containing Fluorophore and Marker
#' @param return Whether to save the file as a png, TRUE/FALSE
#' @param filename Name to save .png file as
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
#' @importFrom ggplot2 ggsave
#' @importFrom magrittr %>%
#'
#' @return Some additional value to edit
#' @export
#'
#' @examples
#' 
#' File_Location <- system.file("extdata", package = "Coereba")
#' panelPath <- file.path(File_Location, "ILTPanelTetramer.csv")
#' panelData <- read.csv(panelPath, check.names=FALSE)
#' binaryPath <- file.path(File_Location, "HeatmapExample.csv")
#' binaryData <- read.csv(binaryPath, check.names=FALSE)
#' 
#' 
Utility_Heatmap <- function(binary, panel, return, filename){

  # Swapping out Fluorophores for Markers
  if(is.data.frame(panel)){MyPanel <- panel
    } else {MyPanel <- read.csv(panel)}

  Fluorophore <- binary %>% select(2:length(binary)) %>%
    colnames()
  Fluorophore <- data.frame(Fluorophore)
  Attempt2 <- left_join(Fluorophore, MyPanel, by = "Fluorophore")
  Names <- Attempt2$Marker
  colnames(binary)[2:length(binary)] <- Names

  # Pivoting Longer
  theCells <- binary %>% mutate(Cluster = row_number()) %>%
    relocate(Cluster, .after = Identity)
  retained <- colnames(theCells[,1:2])
  MeltedCells <- melt(theCells, id = retained)
  MeltedCells$Cluster <- factor(MeltedCells$Cluster)
  MeltedCells$variable <- factor(MeltedCells$variable)

  # Generating the Plot
  MyHeatmap <- ggplot(MeltedCells, aes(Cluster, variable, fill = value)) +
    geom_tile() +
    scale_fill_viridis(option = "cividis", discrete=FALSE) +
    theme_classic() +
    theme(legend.position = "none", axis.text.x = element_text(
      size = 5, angle = 300)) + labs(y = NULL)
  
  # Export options

  if (return == TRUE){ggsave(filename, MyHeatmap,
                             dpi = 600, units = "in", width = 6, height = 4)}

  return(MyHeatmap)
}



#' Internal for Utility_Heatmap
#'
#' @param binary Something
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
.Internal_Heatmap <- function(binary, panel, ...){
  #MyPanel <- read.csv(panel, check.names = FALSE)
  MyPanel <- panel
  Fluorophore <- binary %>% select(2:length(binary)) %>%
    colnames()
  Fluorophore <- data.frame(Fluorophore)
  Attempt2 <- left_join(Fluorophore, MyPanel, by = "Fluorophore")
  Names <- Attempt2$Marker
  colnames(binary)[2:length(binary)] <- Names
  #filteredCells

  theCells <- binary %>% mutate(Cluster = row_number()) %>%
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


