Complicated <-  function(x, cutoff, starter){
  df <- flowCore::exprs(x[,11:39])
  name <- keyword(x, "GROUPNAME")
  dsf <- data.frame(df, check.names = FALSE)
  colnames(dsf) <- gsub("-A", "", colnames(dsf), fixed = TRUE)
  colnames(dsf) <- gsub("-", "", colnames(dsf), fixed = TRUE)
  colnames(dsf) <- gsub(" ", "", colnames(dsf), fixed = TRUE)
  colnames(dsf) <- gsub(".", "", colnames(dsf), fixed = TRUE)
  starter <- gsub("-A", "", starter, fixed = TRUE)
  starter <- gsub("-", "", starter, fixed = TRUE)
  starter <- gsub(" ", "", starter, fixed = TRUE)
  starter <- gsub(".", "", starter, fixed = TRUE)
  Columns <- colnames(dsf)
  Columns <- Columns[ !Columns == starter]
  #Columns <- Columns[1:10]
  #Columns

  My.Data <- dsf
  My.Data <- My.Data %>% mutate(Cluster = case_when(
    My.Data[[starter]] < New[New$GROUPNAME == name, starter] ~ paste(
      starter, "neg", sep = "", collapse = NULL),
    My.Data[[starter]] > New[New$GROUPNAME == name, starter] ~ paste(
      starter, "pos", sep = "", collapse = NULL)))

  My.Data$Cluster <- factor(My.Data$Cluster)
  A <- data.frame(table(My.Data$Cluster))
  Ranks <- pivot_wider(A, names_from = Var1, values_from = Freq)
  Clusters <- colnames(Ranks)

  KeptClusters <- function(x, l){
    if (x[l] > cutoff) {NewData <- My.Data %>% dplyr::filter(Cluster == l)}}

  MyNewestData <- lapply(x = Ranks, FUN = KeptClusters, Clusters) %>% bind_rows()
  #.GlobalEnv$MyNewestData <- MyNewestData #BruteForceButWorks

  #RemovedClusters <- function(x, l){
  #  if (x[l] < cutoff) {NewlyBanished <- My.Data %>% dplyr::filter(Cluster == l)}}

  #BanishedData <- lapply(x = Ranks, FUN = RemovedClusters, Clusters) %>% bind_rows()
  #.GlobalEnv$BanishedData <- BanishedData #BruteForceButWorks

  for(i in Columns) {MyNewestData <- MyNewestData %>% mutate(Cluster = case_when(
    MyNewestData[[i]] < New[New$GROUPNAME == name, i] ~ paste(MyNewestData$Cluster, i, "neg", sep = ""),
    MyNewestData[[i]] > New[New$GROUPNAME == name, i] ~ paste(MyNewestData$Cluster, i, "pos", sep = "")
  ))
  }

  MyUpdatedData <- MyNewestData

  MyUpdatedData$Cluster <- factor(MyUpdatedData$Cluster)
  A <- data.frame(table(MyUpdatedData$Cluster))
  Ranks <- pivot_wider(A, names_from = Var1, values_from = Freq)
  Clusters <- colnames(Ranks)

  ExternalData <- data.frame()
  BanishedData <- data.frame()

  for(l in Clusters){if (Ranks[l] > cutoff) {Bagh <- MyUpdatedData %>% dplyr::filter(Cluster == l)
  ExternalData <- rbind(ExternalData, Bagh)}
    else {BanishedData <- rbind(BanishedData, MyUpdatedData %>% dplyr::filter(Cluster == l))}
  }

  MyNewestData <- ExternalData

  .GlobalEnv$MyNewestData <- MyNewestData #BruteForce Works For Single Specimens, overrides multiple
  .GlobalEnv$BanishedData <- BanishedData #BruteForce Works For Single Specimens, overrides multiple
}
