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
#' @param showClose Default is TRUE, displays pvalues between 0.05 and 0.10 instead returning n.s
#' @param scalePercent Default is FALSE, scales 0 to 1 to 0-100
#' @param keepNS Default is TRUE, when false, removes ns from the plots. 
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
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales percent
#' @importFrom ggplot2 lims
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
  XAxisLevels=NULL, statLines=TRUE, statsHeight=NULL, showClose=TRUE, scalePercent=FALSE, keepNS=TRUE){

  TheStatsReturn <- Utility_Stats(data=data, var=var, myfactor=myfactor, normality=normality,
     specifiedNormality=specifiedNormality, correction=correction, override=override,
    returnType="behemoth")

  TheTest <- TheStatsReturn[[1]]
  theYlim <- TheStatsReturn[[3]]
  Method <- unique(TheTest$method)

  if (!is.null(statsHeight)){
    SingleY <- statsHeight
  } else {SingleY <- theYlim*1.1}
  
  MyPval <- if(Method %in% c("Two Sample t-test",
            "Wilcoxon rank sum test with continuity correction", "Wilcoxon rank sum exact test")){
    thepvalue <- Coereba:::pval_mold(TheTest$p.value, showClose=showClose)
  } else if (Method %in% c("One-way Anova", "Kruskal-Wallis rank sum test")){
    Pval <- TheTest$p.value
    CleanedP <- Pval[!is.na(Pval)]
    thepvalue <- Coereba:::pval_mold(CleanedP, showClose=showClose)
  } else if (Method %in% c("Pairwise t-test", "Pairwise Wilcox test")) {
    map(.x=TheTest$p.value, .f=Coereba:::pval_mold, showClose=showClose)
  } else {"NA"}

  # Creating an index p value table
  if (keepNS == FALSE){
    Show <- which("n.s" != MyPval)
    ShownPvalues <- MyPval[Show]
    ShownPvalues <- unlist(ShownPvalues)
    ThePData <- data.frame(Index=Show, ThePvalues=ShownPvalues)
    if (nrow(ThePData) == 0){ThePData <- NULL}
  } else {
    Show <- seq_along(MyPval)
    ShownPvalues <- MyPval
    ShownPvalues <- unlist(ShownPvalues)
    ThePData <- data.frame(Index=Show, ThePvalues=ShownPvalues)
  }

  # Setting plot title (especially for Coereba nodes)
  var1 <- var
  var1 <- gsub("pos", "pos-", var1)
  var1 <- gsub("neg", "neg-", var1)
  words <- unlist(strsplit(var1, "-"))
  filtered_words <- words[!grepl("neg", words)]
  cleaned_string <- paste(filtered_words, collapse = " ")
  Cleaned_string2 <- paste(cleaned_string, "", sep = " ") #Site for an append argument
  wrapped_title <- str_wrap(Cleaned_string2, width = 100)

  # Setting factor for axis arrangement
  if(!is.null(XAxisLevels)){
    data[[myfactor]] <- factor(data[[myfactor]], levels = XAxisLevels)
  }

  # Creating base plot
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
    plot <- LineAddition(plot=plot, Method=Method, ThePData=ThePData,
       SingleY=SingleY)
  }
  
  if(!is.null(statsHeight)){plot <- plot + lims(y=c(0,statsHeight))}

  if (scalePercent == TRUE){
    if (is.null(statsHeight)){
    plot <- plot + scale_y_continuous(labels = scales::percent)
      } else if (is.null(statLines)){plot <- plot + 
        scale_y_continuous(labels = scales::percent, limits=c(0, statsHeight))
      } else{plot <- plot + 
        scale_y_continuous(labels = scales::percent, limits=c(0, statsHeight * 1.3))
      }
    }

 return(plot)
}



#' Internal for Utility_Behemoth, adds lines
#'
#' @param plot The ggplot2 object
#' @param Method Passed method for statistics
#' @param ThePData Dataframe containing index and pvalues
#' @param SingleY The derrived height at which to place the lines
#'
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr pull
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
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales percent
#' @importFrom ggplot2 lims
#'
#' @return Updated ggplot2 plot with stat lines
#'
#' @noRd
LineAddition <- function(plot, Method, ThePData, SingleY){
  if (!is.null(ThePData)){
    if (nrow(ThePData) > 0){
      IndexSlots <- ThePData |> pull(Index)
    } else {IndexSlots <- NULL} 
  } else {IndexSlots <- NULL}

  if (!is.null(IndexSlots)){
  if (length(IndexSlots) < 3 & Method %in% c("Pairwise t-test", "Pairwise Wilcox test")){
    Index1 <- ThePData |> filter(Index == 1)
    Index2 <- ThePData |> filter(Index == 2)
    Index3 <- ThePData |> filter(Index == 3)
    
    if (nrow(Index1) > 0){
      FirstY <- SingleY*1
      FirstP <- ThePData |> filter(Index == 1) |> pull(ThePvalues)
      
      plot <- plot +
        geom_line(data=tibble(x=c(1,1.9), y=c(FirstY, FirstY)), aes(x=x, y=y), inherit.aes = FALSE) +
        geom_line(data=tibble(x=c(1,1), y=c(FirstY*0.98,FirstY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
        geom_line(data=tibble(x=c(1.9,1.9), y=c(FirstY*0.98,FirstY*1.02)),aes(x=x, y=y), inherit.aes = FALSE) +
        geom_text(data=tibble(x=1.5, y=FirstY*1.04), aes(x=x, y=y, label = FirstP),size = 4, inherit.aes = FALSE)
    }

    if (nrow(Index2) > 0){
      SecondY <- SingleY*1.1
      SecondP <- ThePData |> filter(Index == 2) |> pull(ThePvalues)
      plot <- plot +
      geom_line(data=tibble(x=c(1,3), y=c(SecondY, SecondY)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,1), y=c(SecondY*0.98,SecondY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(3,3), y=c(SecondY*0.98,SecondY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=2, y=SecondY*1.04), aes(x=x, y=y, label = SecondP), size = 4, inherit.aes = FALSE) 
    }

    if (nrow(Index3) > 0){
      ThirdY <- SingleY*1
      ThirdP <- ThePData |> filter(Index == 3) |> pull(ThePvalues)
      plot <- plot + geom_line(data=tibble(x=c(2.1,3), y=c(ThirdY, ThirdY)),aes(x=x, y=y), inherit.aes = FALSE) +
        geom_line(data=tibble(x=c(2.1,2.1), y=c(ThirdY*0.98,ThirdY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
        geom_line(data=tibble(x=c(3,3), y=c(ThirdY*0.98,ThirdY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
        geom_text(data=tibble(x=2.55, y=ThirdY*1.04), aes(x=x, y=y, label = ThirdP), size = 4, inherit.aes = FALSE)
    }

  } else if (length(IndexSlots) == 3 & Method %in% c("Pairwise t-test", "Pairwise Wilcox test")){

    FirstP <- ThePData |> filter(Index == 1) |> pull(ThePvalues)
    SecondP <- ThePData |> filter(Index == 2) |> pull(ThePvalues)
    ThirdP <- ThePData |> filter(Index == 3) |> pull(ThePvalues)
    FirstY <- SingleY*1
    SecondY <- SingleY*1.1
    ThirdY <- SingleY*1

    plot <- plot +
      geom_line(data=tibble(x=c(1,1.9), y=c(FirstY, FirstY)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,1), y=c(FirstY*0.98,FirstY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1.9,1.9), y=c(FirstY*0.98,FirstY*1.02)),aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=1.5, y=FirstY*1.04), aes(x=x, y=y, label = FirstP),size = 4, inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(2.1,3), y=c(ThirdY, ThirdY)),aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(2.1,2.1), y=c(ThirdY*0.98,ThirdY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(3,3), y=c(ThirdY*0.98,ThirdY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=2.55, y=ThirdY*1.04), aes(x=x, y=y, label = ThirdP), size = 4, inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,3), y=c(SecondY, SecondY)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,1), y=c(SecondY*0.98,SecondY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(3,3), y=c(SecondY*0.98,SecondY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=2, y=SecondY*1.04), aes(x=x, y=y, label = SecondP), size = 4, inherit.aes = FALSE) +
      labs(caption = Method)
  } else if (length(IndexSlots) == 1 & Method %in% c("Two Sample t-test",
  "Wilcoxon rank sum test with continuity correction", "Wilcoxon rank sum exact test")){
    SingleP <- ThePData |> pull(ThePvalues)
    plot <- plot + geom_line(data=tibble(x=c(1,2), y=c(SingleY, SingleY)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,1), y=c(SingleY*0.98,SingleY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(2,2), y=c(SingleY*0.98,SingleY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=1.5, y=SingleY*1.04), aes(x=x, y=y, label = SingleP), size = 4, inherit.aes = FALSE) +
      labs(caption = Method)
  } else if (length(IndexSlots) == 1 & Method %in% c("One-way Anova", "Kruskal-Wallis rank sum test")){
    SingleP <- ThePData |> pull(ThePvalues)
    plot <- plot + geom_line(data=tibble(x=c(1,3), y=c(SingleY, SingleY)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,1), y=c(SingleY*0.98,SingleY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(3,3), y=c(SingleY*0.98,SingleY*1.02)), aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=2, y=SingleY*1.04), aes(x=x, y=y, label = SingleP), size = 4, inherit.aes = FALSE) +
      labs(caption = Method)
  } else {return(plot)}
  } else {return(plot)}

  return(plot)
  }
