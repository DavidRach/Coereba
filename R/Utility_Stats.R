#' Normality Test followed by t-test/Anova
#'
#' @param data A data.frame object with metadata and data columns
#' @param var The column name for your variable of interest
#' @param myfactor The column name for your column containing your factor to group by
#' @param normality The Normality test to be applied, "dagostino" or "shapiro". Default NULL
#' @param specifiedNormality Default NULL leading to non-parametric, can switch by specifying
#' "parametric" or "nonparametric".
#' @param correction Multiple comparison correction argument, default is set at "none"
#' @param override Internal, default 0.05. Set to 0.99 to force pairwise comparison in anova/kw.
#' @param returnType Internal, default is "stats". Alternate "mean", "median". "behemoth" for internal usage.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr unnest
#' @importFrom stats shapiro.test
#' @importFrom broom tidy
#' @importFrom stats t.test
#' @importFrom stats aov
#' @importFrom stats pairwise.t.test
#' @importFrom stats wilcox.test
#' @importFrom stats kruskal.test
#' @importFrom stats pairwise.wilcox.test
#'
#' @return A data.frame of test results
#' @export
#'
#' @examples
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
#' TheStats <- Utility_Stats(data=All, var="CD62L",
#'  myfactor="ptype", normality="dagostino")
#'
Utility_Stats <- function(data, var, myfactor, normality=NULL, specifiedNormality=NULL,
  correction="none", override=0.05, returnType="stats"){

  theYlim <- max(data[[var]])
  FactorLevels <- unique(data[[myfactor]])
  FactorLevelsCount <- length(unique(data[[myfactor]]))

  ####################################
  # Do you belive in normality test? #
  ####################################

  if (!is.null(normality)){

    if (normality == "dagostino") {
      Stashed <- data %>% group_by(.data[[myfactor]]) %>%
      summarize(dagostino_result = dago_wrapper(.data[[var]])) %>%
      unnest(dagostino_result)

      Distribution <- if(all(Stashed$p.value > 0.05)) {"parametric"
        } else {"nonparametric"}

    } else if (normality == "shapiro") {Stashed <- data %>% group_by(
      .data[[myfactor]]) %>%
      summarize(shapiro_result = list(tidy(shapiro.test(.data[[var]])))) %>%
      unnest(shapiro_result)

      Distribution <- if(all(Stashed$p.value > 0.05)) {"parametric"
      } else{"nonparametric"}

    } else {stop(
      "Only normality test currently supported are
        `dagostino` and `shapiro` or NULL, please correct")
      }
  } else {Distribution <- "nonparametric"}

  #####################################
  # Override and Specify Distribution #
  #####################################

  if (!is.null(specifiedNormality)){Distribution <- specifiedNormality}

  ################
  # Testing Tree #
  ################

  TheTest <- if (Distribution == "parametric" & FactorLevelsCount == 2) {
    tt<- tidy(t.test(data[[var]] ~ data[[myfactor]],
                     alternative = "two.sided", var.equal = TRUE))
  } else if (Distribution == "parametric" & FactorLevelsCount > 2){
    at <- tidy(aov(data[[var]] ~ data[[myfactor]], data = data))
    at$method <- "One-way Anova"
    if(at$p.value[1] < override) {
      subset_data <- subset(data, select = c(var, myfactor))
      ptt <- tidy(pairwise.t.test(subset_data[[var]], subset_data[[myfactor]],
                                  p.adjust.method = correction))
      ptt$method <- "Pairwise t-test"
      ptt
    } else(at)
  } else if (Distribution == "nonparametric" & FactorLevelsCount == 2){
    wt <- tidy(wilcox.test(data[[var]] ~ data[[myfactor]],
                           alternative = "two.sided", var.equal = TRUE))
  } else if (Distribution == "nonparametric" & FactorLevelsCount > 2){
    kt <- tidy(kruskal.test(data[[var]] ~ data[[myfactor]], data = data))
    if (kt$p.value < override) {
      subset_data <- subset(data, select = c(var, myfactor))
      pwt <- tidy(pairwise.wilcox.test(subset_data[[var]],
                                       g=subset_data[[myfactor]], p.adjust.method = correction))
      pwt$method <- "Pairwise Wilcox test"
      pwt
    } else {kt}
  }

  if (returnType == "mean"){TheMean <- MeanSD(data=data, var=var, myfactor=myfactor)
                          TheMean <- TheMean %>% mutate(Marker=var) %>%
                            relocate(Marker, .before=1)
                          return(TheMean)}

  if (returnType == "median"){TheMedian <- MedianIQR(data=data, var=var, myfactor=myfactor)
                            TheMedian <- TheMedian %>% mutate(Marker=var) %>%
                              relocate(Marker, .before=1)
                            return(TheMedian)

  }

  if (returnType == "behemoth"){
    TheReturnPacket <- list(TheTest, Distribution, theYlim)
    return(TheReturnPacket)
  } else if (returnType == "stats"){
    Values <- TheTest$p.value %>% unlist()
    Values <- Values[!is.na(Values)]

    Returns <- cbind(var, min(Values), Distribution)
    Returns <- data.frame(Returns)
    colnames(Returns)[1] <- "marker"
    colnames(Returns)[2] <- "pvalue"
    colnames(Returns)[3] <- "normality"
    return(Returns)
  }
}


MeanSD <- function(data, var, myfactor) {
  Tada <- data %>% group_by(.data[[myfactor]]) %>%
    summarize(Mean = mean(.data[[var]], na.rm = TRUE),
      SD = sd(.data[[var]], na.rm = TRUE), .groups = "drop")
  return(Tada)
}

MedianIQR <- function(data, var, myfactor) {
  Tada <- data %>% group_by(.data[[myfactor]]) %>%
    summarize(Median = median(.data[[var]], na.rm = TRUE),
      IQR = IQR(.data[[var]], na.rm = TRUE),
      P25 = quantile(.data[[var]], 0.25, na.rm = TRUE),
      P75 = quantile(.data[[var]], 0.75, na.rm = TRUE),
      .groups = "drop")
  return(Tada)
}
