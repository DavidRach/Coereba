Internal_Heatmap <- function(filteredCells, panel, ...){
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
  MeltedCells <- reshape2::melt(theCells, id = retained)
  MeltedCells$Cluster <- factor(MeltedCells$Cluster)
  MeltedCells$variable <- factor(MeltedCells$variable)

  MyHeatmap <- ggplot(MeltedCells, aes(Cluster, variable, fill = value)) +
    geom_tile() +
    scale_fill_viridis(option = "cividis", discrete=FALSE) +
    theme_classic() +
    theme(legend.position = "none", axis.text.x = element_text(size = 5,
                                                               angle = 300)) + labs(y = NULL)
  MyHeatmap
}
