Utility_Stats <- function(data, var, myfactor, normality, shape_palette, fill_palette, switch){
  library(ggplot2); library(tidyr); library(broom); library(purrr); library(ggpubr); library(ggbeeswarm); library(Rita); library(moments)
  theme_set(theme_bw())
  set.seed(1989)

  theYlim <- max(data[[var]])
  FactorLevels <- levels(data[[myfactor]])
  FactorLevelsCount <- length(levels(data[[myfactor]]))

  dago_wrapper <- function(x){
    A <- fBasics::dagoTest(x)
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
