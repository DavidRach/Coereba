#' Normality Test followed by t-test/Anova
#'
#' @param data A data.frame object containing the data values
#' @param var A data.frame column of interest
#' @param myfactor A data.frame column for which you want to differentionally compare if they are different
#' @param normality The Normality test to be applied, "dagostino" or "shapiro"
#' @param shape_palette provide the shape palette matches your provided factor names
#' @param fill_palette provide the fill palette matches your provided factor names
#' @param switch Unclear
#'
#' @importFrom tidyr unnest
#' @importFrom broom tidy
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom stats shapiro.test
#' @importFrom stats t.test
#' @importFrom stats wilcox.test
#' @importFrom stats pairwise.t.test
#' @importFrom stats aov
#' @importFrom stats kruskal.test
#' @importFrom stats pairwise.wilcox.test
#'
#' @return NULL
#' @export
#'
#' @examples NULL
Utility_Stats <- function(data, var, myfactor, normality, shape_palette, fill_palette, switch){

  theYlim <- max(data[[var]])
  FactorLevels <- levels(data[[myfactor]])
  FactorLevelsCount <- length(levels(data[[myfactor]]))

  dago_wrapper <- function(x){
    A <- Coereba::dagoTest(x)
    method <- A@test$method
    p.value <- A@test$p.value[[1]]
    C <- data.frame(cbind(p.value, method))
  }

  if (normality == "dagostino") {Stashed <- data %>% group_by(.data[[myfactor]]) %>% summarize(dagostino_result = dago_wrapper(.data[[var]])) %>% unnest(dagostino_result)
  } else if (normality == "shapiro") {Stashed <- data %>% group_by(.data[[myfactor]]) %>% summarize(shapiro_result = list(tidy(shapiro.test(.data[[var]])))) %>% unnest(shapiro_result)
  } else ("Forgot to input Normality test choice. Use 'dagostino' or 'shapiro'")

  Distribution <- if(all(Stashed$p.value > 0.05)) {"parametric"} else{"nonparametric"}

  TheTest <- if (Distribution == "parametric" & FactorLevelsCount == 2) {
    tt<- tidy(t.test(data[[var]] ~ data[[myfactor]], alternative = "two.sided", var.equal = TRUE))
  } else if (Distribution == "parametric" & FactorLevelsCount > 2){
    at<- tidy(aov(data[[var]] ~ data[[myfactor]], data = data))
    at$method <- "One-way Anova"
    if(at$p.value[1] < 0.05) {
      subset_data <- subset(data, select = c(var, myfactor))
      ptt <- tidy(pairwise.t.test(subset_data[[var]], subset_data[[myfactor]], p.adjust.method = "BH"))
      ptt$method <- "Pairwise t-test"
      ptt
    } else(at)
  } else if (Distribution == "nonparametric" & FactorLevelsCount == 2){
    wt <- tidy(wilcox.test(data[[var]] ~ data[[myfactor]], alternative = "two.sided", var.equal = TRUE))
  } else if (Distribution == "nonparametric" & FactorLevelsCount > 2){
    kt <- tidy(kruskal.test(data[[var]] ~ data[[myfactor]], data = data))
    if (kt$p.value < 0.05) {
      subset_data <- subset(data, select = c(var, myfactor))
      pwt <- tidy(pairwise.wilcox.test(subset_data[[var]], g=subset_data[[myfactor]], p.adjust.method = "BH"))
      pwt$method <- "Pairwise Wilcox test"
      pwt
    } else {kt}
  }

  Returns <- cbind(var, min(TheTest$p.value))
  Returns <- data.frame(Returns)
}
