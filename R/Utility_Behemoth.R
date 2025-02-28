#' From a data.frame, runs stats on column of interest, and returns the ggplot for it.
#'
#' @param data A data.frame object with metadata and data columns
#' @param var The column name for your variable of interest
#' @param myfactor The column name for your column containing your factor to group by
#' @param normality The Normality test to be applied, "dagostino" or "shapiro". Default NULL
#' @param specifiedNormality Default NULL leading to non-parametric, can switch by specifying
#' "parametric" or "nonparametric".
#' @param correction Multiple comparison correction argument, default is set at "none"
#' @param override Internal, default 0.05. Set to 0.99 to force pairwise comparison in anova/kw.
#' @param shape_palette Palette corresponding to factor levels, designating each's shape
#' @param fill_palette Palette corresponding to factor levels, designating each's fill
#' @param cex The width of the ggbeeswarm bin
#' @param size Size for the ggbeeswarm circles.
#' @param corral.width width of corral bin argument for beeswarm.
#' @param XAxisLevels Provide list marker names correct order for x-axis reordering, default NULL
#' @param statLines Default is TRUE, otherwise skips plotting pvalue and brackets
#' @param statsHeight Default is NULL, when provided, sets stat line y-axis height. 
#'
#' @importFrom dplyr select
#' @importFrom stringr str_wrap
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_line
#' @importFrom tibble tibble
#' @importFrom ggplot2 geom_text
#'
#' @return A ggplot2 object for the corresponding data of interest.
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
#' Plot <- Utility_Behemoth(data=All, var="CD62L", myfactor="ptype",
#'  normality="dagostino", correction="none", shape_palette=shape_ptype,
#'  fill_palette=fill_ptype, XAxisLevels = c("HU", "HEU-lo", "HEU-hi"))
#'
Utility_Behemoth <- function(data, var, myfactor, normality=NULL, specifiedNormality = NULL,
  correction = "none", override=0.05, shape_palette, fill_palette, cex=2, size=3, corral.width=1,
  XAxisLevels=NULL, statLines=TRUE, statsHeight=NULL){

  TheStatsReturn <- Utility_Stats(data=data, var=var, myfactor=myfactor, normality=normality,
     specifiedNormality=specifiedNormality, correction=correction, override=override,
    returnType="behemoth")

  TheTest <- TheStatsReturn[[1]]
  Distribution <- TheStatsReturn[[2]]
  theYlim <- TheStatsReturn[[3]]

  Method <- unique(TheTest$method)
  if (Distribution == "parametric"){ MethodDictate <- "mean"
    } else if (Distribution == "nonparametric") {MethodDictate <- "median"}

  MyPval <- if(Method %in% c("Two Sample t-test",
            "Wilcoxon rank sum test with continuity correction", "Wilcoxon rank sum exact test")){
    thepvalue <- Coereba:::pval_mold(TheTest$p.value)
  } else if (Method %in% c("One-way Anova", "Kruskal-Wallis rank sum test")){
    Pval <- TheTest$p.value
    CleanedP <- Pval[!is.na(Pval)]
    thepvalue <- Coereba:::pval_mold(CleanedP)
  } else if (Method %in% c("Pairwise t-test", "Pairwise Wilcox test")) {
    map(TheTest$p.value, Coereba:::pval_mold)
  } else {"NA"}

  #MyPval

  if(length(MyPval)  == 1){SingleP <- MyPval
  SingleY <- theYlim*1.1
  } else {FirstP <- MyPval[1]
  SecondP <- MyPval[2]
  ThirdP <- MyPval[3]
  FirstY <- theYlim * 1.1
  SecondY<- theYlim * 1.2
  ThirdY <- theYlim * 1.1
  }

  var1 <- var
  var1 <- gsub("pos", "pos-", var1)
  var1 <- gsub("neg", "neg-", var1)
  words <- unlist(strsplit(var1, "-"))
  filtered_words <- words[!grepl("neg", words)]
  cleaned_string <- paste(filtered_words, collapse = " ")
  Cleaned_string2 <- paste(cleaned_string, "", sep = " ") #Site for an append argument

  wrapped_title <- str_wrap(Cleaned_string2, width = 100)

  if(!is.null(XAxisLevels)){
    data[[myfactor]] <- factor(data[[myfactor]], levels = XAxisLevels)
  }

  plot <- ggplot(data, aes(x =.data[[myfactor]], y = .data[[var]])) +
    geom_boxplot(show.legend = FALSE) +
    geom_beeswarm(show.legend = FALSE, aes(shape = .data[[myfactor]],
      fill = .data[[myfactor]]), method = "center", side = 0,
      priority = "density", cex = cex, size = size, corral = "wrap",
      corral.width = corral.width) +
    scale_shape_manual(values = shape_palette) +
    scale_fill_manual(values = fill_palette) +
    labs(title = wrapped_title, x = NULL, y = NULL) + theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 8))

  if (statLines==TRUE){

    if (!is.null(statsHeight)){
      FirstY <- statsHeight
      SingleY <- statsHeight
      SecondY<- statsHeight * 1.2
      ThirdY <- statsHeight * 1.1
    }

  if (Method %in% c("Two Sample t-test",
      "Wilcoxon rank sum test with continuity correction", "Wilcoxon rank sum exact test")){
    plot <- plot + geom_line(data=tibble(x=c(1,2), y=c(SingleY, SingleY)),
      aes(x=x, y=y), inherit.aes = FALSE)+
      geom_line(data=tibble(x=c(1,1), y=c(SingleY*0.98,SingleY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE)+
      geom_line(data=tibble(x=c(2,2), y=c(SingleY*0.98,SingleY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=1.5, y=SingleY*1.04),
          aes(x=x, y=y, label = SingleP), size = 4, inherit.aes = FALSE) +
      labs(caption = Method)
  } else if (Method %in% c("One-way Anova", "Kruskal-Wallis rank sum test")){
    plot <- plot + geom_line(data=tibble(x=c(1,3), y=c(SingleY, SingleY)),
                     aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,1), y=c(SingleY*0.98,SingleY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(3,3), y=c(SingleY*0.98,SingleY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=2, y=SingleY*1.04), aes(x=x, y=y, label = SingleP),
                size = 4, inherit.aes = FALSE) +
      labs(caption = Method)
  } else if (Method %in% c("Pairwise t-test", "Pairwise Wilcox test")) {
    plot <- plot + geom_line(data=tibble(x=c(1,1.9), y=c(FirstY, FirstY)), aes(x=x, y=y),
                     inherit.aes = FALSE)+
      geom_line(data=tibble(x=c(1,1), y=c(FirstY*0.98,FirstY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE)+
      geom_line(data=tibble(x=c(1.9,1.9), y=c(FirstY*0.98,FirstY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=1.5, y=FirstY*1.04), aes(x=x, y=y, label = FirstP),
                size = 4, inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(2.1,3), y=c(ThirdY, ThirdY)),
                aes(x=x, y=y), inherit.aes = FALSE)+
      geom_line(data=tibble(x=c(2.1,2.1), y=c(ThirdY*0.98,ThirdY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE)+
      geom_line(data=tibble(x=c(3,3), y=c(ThirdY*0.98,ThirdY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=2.55, y=ThirdY*1.04), aes(x=x, y=y, label = ThirdP),
                size = 4, inherit.aes = FALSE)+
      geom_line(data=tibble(x=c(1,3), y=c(SecondY, SecondY)), aes(x=x, y=y),
                inherit.aes = FALSE)+
      geom_line(data=tibble(x=c(1,1), y=c(SecondY*0.98,SecondY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE)+
      geom_line(data=tibble(x=c(3,3), y=c(SecondY*0.98,SecondY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=2, y=SecondY*1.04), aes(x=x, y=y, label = SecondP),
                size = 4, inherit.aes = FALSE) +
      labs(caption = Method)
  } else {message("Test type not recognized")}

  }

 return(plot)
}
