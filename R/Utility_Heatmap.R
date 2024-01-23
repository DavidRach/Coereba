#' Returns a Coereba Heatmap
#'
#' @param cells Unclear, Coereba output including Cluster column
#' @param filename Name to save .png file as
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom reshape2 melt
#' @import ggplot2
#'
#' @return NULL
#' @export
#'
#' @examples NULL
Utility_Heatmap <- function(cells, filename){
  Cells <- Cells %>% mutate(Cluster = row_number()) %>% relocate(Cluster, .after = Identity)
  Cz <- Cells
  Ca <- colnames(Cz[,1:2]) #Likely Source Loss BV711
  Cc <- melt(Cz, id = Ca)
  Cc$Identity <- factor(Cc$Identity)
  Cc$Cluster <- factor(Cc$Cluster)
  Cc$variable <- factor(Cc$variable)
  #View(Cc)
  #str(Cc)

  Overide <- c("Viability", "CD3", "CD26", "Lin-", "PD1", "TNFa", "CD25", "NKG2D", "Va24Ja18", "CCR6", "IFNg", "CCR7", "CD56", "CD45RA", "CD161", "CD127", "CD4", "CXCR3", "Vd2", "CCR4", "CD69", "CD8", "CD62L", "Va7.2", "CD107a", "CD38", "CD27", "CD16", "CD7")
  Overide1 <- rev(Overide)

  MyHeatmap <- ggplot(Cc, aes(Cluster, variable, fill = value)) + geom_tile() + scale_fill_viridis(option = "cividis", discrete=FALSE) + theme_classic() + theme(legend.position = "none", axis.text.x = element_text(size = 5, angle = 300)) + scale_y_discrete(labels = Overide1) + labs(y = NULL)
  MyHeatmap
  ggsave(filename, MyHeatmap, dpi = 600, units = "in", width = 6, height = 4)
}
