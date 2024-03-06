#' Coereba's main function. Runs normality, runs stats, plots the results.
#'
#' @param data A data.frame of your data.
#' @param var A column containing your variables of interest.
#' @param myfactor A column containing the factor you want to compare
#' @param normality Choice of normality test, "dagostino" or "shapiro"
#' @param shape_palette Palette corresponding to factor levels, designating each's shape
#' @param fill_palette Palette corresponding to factor levels, designating each's fill
#' @param switch Unclear
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr unnest
#' @importFrom broom tidy
#' @importFrom stats shapiro.test
#' @importFrom stats t.test
#' @importFrom stats wilcox.test
#' @importFrom stats pairwise.t.test
#' @importFrom stats aov
#' @importFrom stats kruskal.test
#' @importFrom stats pairwise.wilcox.test
#' @import ggplot2
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom stringr str_wrap
#'
#' @return Some additional value to edit
#' @export
#'
#' @examples NULL
#'
Utility_Behemoth <- function(data, var, myfactor, normality, shape_palette,
                             fill_palette, switch, scalefactor, scalefactorlabel, ...){

  theYlim <- max(data[[var]])
  #FactorLevels <- levels(data[[myfactor]] %>% factor(.))
  FactorLevelsCount <- length(unique(data[[myfactor]]))

  dago_wrapper <- function(x){
    A <- Coereba::dagoTest(x)
    method <- A@test$method
    p.value <- A@test$p.value[[1]]
    C <- data.frame(cbind(p.value, method))
  }

  if (normality == "dagostino") {
    Stashed <- data %>% group_by(.data[[myfactor]]) %>%
    summarize(dagostino_result = dago_wrapper(.data[[var]])) %>%
      unnest(dagostino_result)
  } else if (normality == "shapiro") {
    Stashed <- data %>% group_by(.data[[myfactor]]) %>%
    summarize(shapiro_result = list(tidy(shapiro.test(.data[[var]])))) %>%
      unnest(shapiro_result)
  } else ("Forgot to input Normality test choice. Use 'dagostino' or 'shapiro'")

  Distribution <- if(all(Stashed$p.value > 0.05)) {
    "parametric"} else{"nonparametric"}

  TheTest <- if (Distribution == "parametric" & FactorLevelsCount == 2) {
    tt<- tidy(t.test(data[[var]] ~ data[[myfactor]],
                     alternative = "two.sided", var.equal = TRUE))
  } else if (Distribution == "parametric" & FactorLevelsCount > 2){
    at<- tidy(aov(data[[var]] ~ data[[myfactor]], data = data))
    at$method <- "One-way Anova"
    if(at$p.value[1] < 0.05) {
      subset_data <- subset(data, select = c(var, myfactor))
      ptt <- tidy(pairwise.t.test(subset_data[[var]], subset_data[[myfactor]],
                                  p.adjust.method = "BH"))
      ptt$method <- "Pairwise t-test"
      ptt
    } else(at)
  } else if (Distribution == "nonparametric" & FactorLevelsCount == 2){
    wt <- tidy(wilcox.test(data[[var]] ~ data[[myfactor]],
                           alternative = "two.sided", var.equal = TRUE))
  } else if (Distribution == "nonparametric" & FactorLevelsCount > 2){
    kt <- tidy(kruskal.test(data[[var]] ~ data[[myfactor]], data = data))
    if (kt$p.value < 0.05) {
      subset_data <- subset(data, select = c(var, myfactor))
      pwt <- tidy(pairwise.wilcox.test(subset_data[[var]],
                    g=subset_data[[myfactor]], p.adjust.method = "BH"))
      pwt$method <- "Pairwise Wilcox test"
      pwt
    } else {kt}
  }

  #My pvalue cleanup function
  pval_mold <- function(x){
    if (x > 0.1){"n.s"} else if (x > 0.01) {round(x, 2)} else if (x > 0.001) {
      round(x, 3)} else if (x > 0.0001) {round(x, 4)} else if (x > 0.00001) {
        round(x, 5)} else if (x > 0.000001) {round(x, 6)}
  }

  Method <- unique(TheTest$method)
  MyPval <- if(Method %in% c("Two Sample t-test",
            "Wilcoxon rank sum test with continuity correction")){
    thepvalue <- pval_mold(TheTest$p.value)
  } else if (Method %in% c("One-way Anova", "Kruskal-Wallis rank sum test")){
    Pval <- TheTest$p.value
    CleanedP <- Pval[!is.na(Pval)]
    thepvalue <- pval_mold(CleanedP)
  } else if (Method %in% c("Pairwise t-test", "Pairwise Wilcox test")) {
    map(TheTest$p.value, pval_mold)
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
  Cleaned_string2 <- paste(cleaned_string, "Cells", sep = " ")

  wrapped_title <- str_wrap(Cleaned_string2, width = 100)


  plot <- ggplot(data, aes(x =.data[[myfactor]], y = .data[[var]])) +
    geom_boxplot(show.legend = FALSE) + stat_summary(fun = median,
              show.legend = FALSE, geom = "crossbar", width = 0.75) +
    geom_beeswarm(show.legend = FALSE, aes(shape = .data[[myfactor]],
      fill = .data[[myfactor]]), method = "center", side = 0,
      priority = "density", cex = 1.8, size = 4) +
    scale_shape_manual(values = shape_palette) +
    scale_fill_manual(values = fill_palette) +
    labs(title = wrapped_title, x = NULL, y = NULL) + theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 8))

  plot2 <- if(Method %in% c("Two Sample t-test",
      "Wilcoxon rank sum test with continuity correction")){
    plot + geom_line(data=tibble(x=c(1,2), y=c(SingleY, SingleY)),
      aes(x=x, y=y), inherit.aes = FALSE)+
      geom_line(data=tibble(x=c(1,1), y=c(SingleY*0.98,SingleY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE)+
      geom_line(data=tibble(x=c(2,2), y=c(SingleY*0.98,SingleY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=1.5, y=SingleY*1.04),
          aes(x=x, y=y, label = SingleP), size = 4, inherit.aes = FALSE) +
      labs(caption = Method)
  } else if (Method %in% c("One-way Anova", "Kruskal-Wallis rank sum test")){
    plot + geom_line(data=tibble(x=c(1,3), y=c(SingleY, SingleY)),
                     aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(1,1), y=c(SingleY*0.98,SingleY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE) +
      geom_line(data=tibble(x=c(3,3), y=c(SingleY*0.98,SingleY*1.02)),
                aes(x=x, y=y), inherit.aes = FALSE) +
      geom_text(data=tibble(x=2, y=SingleY*1.04), aes(x=x, y=y, label = SingleP),
                size = 4, inherit.aes = FALSE) +
      labs(caption = Method)
  } else if (Method %in% c("Pairwise t-test", "Pairwise Wilcox test")) {
    plot + geom_line(data=tibble(x=c(1,1.9), y=c(FirstY, FirstY)), aes(x=x, y=y),
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
  } else {plot}


}
