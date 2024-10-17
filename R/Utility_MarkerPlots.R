#' Takes Coereba_MarkerExpressions data.frame and returns Marker Expression Beeswarm plots
#'
#' @param data A data.frame of metadata and summarized markers from Utility_MarkerExpressions
#' @param panel A .csv or data.frame containing Fluorophore and Marker columns of your panel markers.
#' @param myfactor The column name by which you want to group outputs in the plot
#' @param shape_palette A scale manual list assigning shape to factor level
#' @param fill_palette A scale fill list assigning fill to factor level
#' @param cex geom_beeswarm argument, default 1
#' @param size geom_beeswarm argument, default 1.5
#' @param corral.width geom_beeswarm argument, default 1
#' @param crossbar geom_boxplot argument, default "median"
#' @param XAxisLevels Provide list marker names correct order for x-axis reordering, default NULL
#' @param savePlot Whether to save ggplot object to outfolder, default = FALSE
#' @param outpath Specify file.path to desired storage location
#' @param filename Specify desired filename
#' @param dpi Specify desired pixel resolution
#' @param width Specify desired width inches
#' @param height Specify desired height inches
#' @param filterForThese A list containing names of markers desire to include final plot, default NULL
#' @param combinatorialStartsWith Default NULL, if aggregated data is combinatorial, starting string
#' of characters (ex. CD45)
#'
#' @importFrom utils read.csv
#' @importFrom dplyr pull
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 stat_summary
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 ggsave
#' @importFrom dplyr filter
#'
#' @return Returns a ggplot object to R or designated Folder
#' @export
#'
#' @examples
#'
#' shape_ptype <- c("HU" = 22, "HEU-lo" = 21, "HEU-hi" = 21)
#' fill_ptype <- c("HU" = "white", "HEU-lo" = "darkgray", "HEU-hi" = "black")
#'
#' File_Location <- system.file("extdata", package = "Coereba")
#' panelPath <- file.path(File_Location, "ILTPanelTetramer.csv")
#' binaryPath <- file.path(File_Location, "HeatmapExample.csv")
#' dataPath <- file.path(File_Location, "ReadyFileExample.csv")
#' panelData <- read.csv(panelPath, check.names=FALSE)
#' binaryData <- read.csv(binaryPath, check.names=FALSE)
#' dataData <- read.csv(dataPath, check.names=FALSE)
#'
#' All <- Coereba_MarkerExpressions(data=dataData, binary=binaryData,
#'  panel=panelData, starter="SparkBlue550")
#'
#' ThePlot <- Utility_MarkerPlots(data=All, panel=panelData,
#'  myfactor="ptype", shape_palette = shape_ptype,
#'  fill_palette = fill_ptype, filterForThese=c("CD7", "CD4", "CD8"),
#'  XAxisLevels = c("CD7", "CD4", "CD8"))
#'
Utility_MarkerPlots <- function(data, panel, myfactor, shape_palette,
   fill_palette, cex=1, size=1.5, corral.width=1, crossbar="median", XAxisLevels=NULL,
   savePlot=FALSE, outpath=NULL, filename = NULL, dpi = 600, width=9, height=3,
   filterForThese=NULL, combinatorialStartsWith=NULL){

  if (!is.data.frame(panel)) {MyPanel <- read.csv(panel, check.names = FALSE)
  } else {MyPanel <- panel}

  TheColumnNames <- colnames(data)

  if(is.null(combinatorialStartsWith)) {DataColNames <- MyPanel %>% pull(Marker)
  } else {DataColNames <- data %>% select(starts_with(combinatorialStartsWith)) %>% colnames()}

  MetaColumns <- setdiff(TheColumnNames, DataColNames)

  MeltedData <- data %>% pivot_longer(
    cols = -all_of(MetaColumns),
    names_to = "Marker",
    values_to = "Value"
  )

  if(!is.null(filterForThese))(MeltedData <- MeltedData %>% dplyr::filter(Marker %in% filterForThese))

  if(!is.null(XAxisLevels)){
    MeltedData$Marker <- factor(MeltedData$Marker, levels = XAxisLevels)}

  ThePlot <- ggplot(MeltedData, aes(x = Marker, y = Value)) +
    geom_boxplot(show.legend = FALSE) + stat_summary(fun = crossbar,
       show.legend = FALSE, geom = "crossbar", width = 0.75) +
    geom_beeswarm(show.legend = FALSE, aes(shape = .data[[myfactor]],
       fill = .data[[myfactor]]), method = "center", side = 0, priority = "density",
        cex = cex, size = size, corral = "wrap", corral.width = corral.width) +
    scale_shape_manual(values = shape_palette) +
    scale_fill_manual(values = fill_palette) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_bw() + theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        plot.title = element_text(hjust = 0.5, size = 8))

  if (savePlot == TRUE){
    if(is.null(outpath)){StorageLocation <- getwd()
    } else {StorageLocation <- outpath}
    TheFileName <- paste0(filename, ".png")
    SendMeOnMyWay <- file.path(StorageLocation, TheFileName)
    ggsave(SendMeOnMyWay, ThePlot, dpi = dpi, units = "in",
     width = width, height = height)
    } else {return(ThePlot)}
}
