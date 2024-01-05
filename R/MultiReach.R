MultiReach <- function(x, column, subsample = NULL, ratio = NULL, starter, sample.name, experiment = NULL, experiment.name = NULL, condition = NULL, condition.name = NULL, bins, cutoff = NULL, reference){
  library(dplyr)
  New <- reference
  name <- keyword(x, sample.name)
  if(is.null(experiment)){experiment <- keyword(x, experiment.name)
  experiment <- gsub("DTR_2023_", "", experiment) #Not Tidy Data Manipulation
  experiment <- gsub("^(.*?\\d{2}).*", "\\1", experiment) #Not Tidy Data Manipulation;
  } else {experiment = experiment}
  if(is.null(condition)) {condition <- keyword(x, condition.name)
  } else {condition = condition}


  df <- flowCore::exprs(x[,column])
  dsf <- data.frame(df, check.names = FALSE)
  colnames(dsf) <- gsub("-A", "", colnames(dsf), fixed = TRUE)
  colnames(dsf) <- gsub("-", "", colnames(dsf), fixed = TRUE)
  colnames(dsf) <- gsub(" ", "", colnames(dsf), fixed = TRUE)
  colnames(dsf) <- gsub(".", "", colnames(dsf), fixed = TRUE)
  starter <- gsub("-A", "", starter, fixed = TRUE)
  starter <- gsub("-", "", starter, fixed = TRUE)
  starter <- gsub(" ", "", starter, fixed = TRUE)
  starter <- gsub(".", "", starter, fixed = TRUE)

  set.seed(1989)
  if(!is.null(subsample)){My.Data <- slice_sample(dsf, n = subsample, replace = FALSE)
  } else{My.Data <- dsf}

  StartingCount <- nrow(My.Data)
  #StartingCount
  decimal_places <- nchar(sub("\\d+\\.", "", as.character(StartingCount)))
  #str(decimal_places)

  Columns <- colnames(My.Data)
  Columns <- Columns[ !Columns == starter]

  MyNewestData <- My.Data %>% mutate(Cluster = case_when(
    My.Data[[starter]] < New[New[[sample.name]] == name, starter] ~ paste(
      starter, "neg", sep = "", collapse = NULL),
    My.Data[[starter]] > New[New[[sample.name]] == name, starter] ~ paste(
      starter, "pos", sep = "", collapse = NULL)))

  for(i in Columns) {MyNewestData <- MyNewestData %>% mutate(Cluster = case_when(
    MyNewestData[[i]] < New[New[[sample.name]] == name, i] ~ paste(MyNewestData$Cluster, i, "neg", sep = ""),
    MyNewestData[[i]] > New[New[[sample.name]] == name, i] ~ paste(MyNewestData$Cluster, i, "pos", sep = "")
  ))
  }

  #MyNewestData is the dataframe plus the Cluster column.
  #At this point we would export out an .fcs file, probably via Source
  #source("NormParams.R")

  MyNewestData$Cluster <- factor(MyNewestData$Cluster)
  A <- data.frame(table(MyNewestData$Cluster))

  #View(A)
  if(!is.null(cutoff)){A1 <- A[A$Freq >= cutoff, , drop = FALSE]
  } else{A1 <- A}

  #nrow(A1)
  if(ratio == TRUE){A2 <- A1 %>% mutate(Freq = (Freq/StartingCount)) %>%
    mutate(Freq = round(Freq, decimal_places))
  } else{A2 <- A1}
  #View(A2)

  Data <- cbind(name, experiment, condition, A2)
  colnames(Data)[colnames(Data) == sample.name] <- "specimen"
  colnames(Data)[colnames(Data) == "Var1"] <- "Clusters"
  colnames(Data)[colnames(Data) == "Freq"] <- "Count"
  return(Data)
}
