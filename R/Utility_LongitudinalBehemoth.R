#' Scales up Utility_Behemoth to handle two factors on the x-axis, allowing for longitudinal plotting
#' 
#' @param data The data.frame for plotting
#' @param Variable The column name of data you want visualized
#' @param GlobalFactor The column name that you want to use as the primary factor on the x-axis
#' @param LocalFactor The column name that you want to use as the secondary factor on the x-axis
#' @param PositionDodgeWidth Default 0.8, sets how far apart the beeswarm and boxplots are from
#' those of the other LocalFactor
#' @param DotSize Default 3, sets the beeswarm dot size
#' @param DotSpacing Default 2, dictates area the dots sprawl across
#' @param shape_palette A list dictating the shapes the LocalFactor values should correspond to
#' @param fill_palette A list dictating the fills the LocalFactor values should correspond to
#' @param savePlot Default FALSE, saves a .png file to the designated location
#' @param outpath Default NULL, provide a file.path to storage location for savePlot
#' @param filename Default NULL sets filename to LongitudinalBehemoth, alternatively provide
#' your own filename.
#' @param dpi Default 600, sets pixels for savePlot
#' @param width Default 6, sets width for savePlot
#' @param height Default 3, sets height for savePlot
#' 
#' @importFrom ggplot2 ggplot aes geom_boxplot position_dodge scale_fill_manual
#' scale_shape_manual ggsave labs theme_bw theme element_blank
#' @importFrom ggbeeswarm geom_beeswarm
#' 
#' @export
Utility_LongitudinalBehemoth <- function(data, Variable, GlobalFactor, LocalFactor,
  PositionDodgeWith=0.8, DotSize=3, DotSpacing=2, shape_palette, fill_palette,
  savePlot = FALSE, outpath=NULL, filename=NULL, dpi=600, width=6, height=3){

  plot <- ggplot(data, aes(x = .data[[GlobalFactor ]], y = .data[[Variable]])) + 
    geom_boxplot(aes(group = interaction(.data[[GlobalFactor]], .data[[LocalFactor]]), fill = .data[[LocalFactor]]), alpha = 0.5, position = position_dodge(width = PositionDodgeWith), width = 0.6, outlier.shape = NA, show.legend = FALSE) +
   geom_beeswarm(aes(shape = .data[[LocalFactor]], fill = .data[[LocalFactor]], 
   group = interaction(.data[[GlobalFactor]], .data[[LocalFactor]])), method = "center", dodge.width = PositionDodgeWith, size = DotSize, corral = "wrap", cex=DotSpacing, show.legend = FALSE) +
  scale_fill_manual(values = fill_palette) +  scale_shape_manual(values = shape_palette) + 
  labs(title = NULL, x = NULL) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  if (savePlot == TRUE){
    if(is.null(outpath)){StorageLocation <- getwd()
    } else {StorageLocation <- outpath}
    if(is.null(filename)){filename <- "LongitudinalBehemoth"}
    TheFileName <- paste0(filename, ".png")
    SendMeOnMyWay <- file.path(StorageLocation, TheFileName)
    ggsave(SendMeOnMyWay, plot, dpi = dpi, units = "in",
     width = width, height = height)
    } else {return(plot)}

}