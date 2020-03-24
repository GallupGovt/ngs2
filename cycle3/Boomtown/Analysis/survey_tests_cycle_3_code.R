## set up R enviroment ----
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
library ("pacman")
pacman::p_load(dplyr, ggplot2, Hmisc, gplots, car, tidyr)

## define constants ----
age_breaks <- c(18, 24, 34, 44, 54, 999)
age_label  <- c("18-24","25-34","35-44", "45-54", "55+")
gend_label <- c("Male", "Female", "Other", "RF")
educ_label <- c("No education", "Elementrary School", "High School", "Some colleage", "Colleage Degree", "Post-Graducate")
empl_label <- c("Yes", "No")
onre_label <- c("Yes", "No")
comp_label <- c("None", "Weak", "Normal", "Strong")
time_label <- c("Certain Rounds", "Uncertain Rounds")
tole_label <- c("Ambiguity Low", "Ambiguity High")
supp_label <- c("HighStatus_HighLegitimacy", "LowStatus_HighLegitimacy")

## define functions ----
TA_calc <- function(x){
  revs <- c(1, 4, 5, 9, 10, 11, 12)
  x[, paste0("Q", revs, "_1")] <- apply(x[, paste0("Q", revs, "_1")], 2, function(y){return(6-y)})
  return(apply(x[, paste0("Q", 1:12, "_1")], 1, function(y) {sum(y)})) 
}

chisq_test_plot <- function(data, key.var, group.var){
  
  # Chi-sq test
  chisq_test <- chisq.test(table(data[,key.var], data[,group.var]))
  test_summary <- data.frame('key var' = gsub("_", " ", key.var),
                           'group var' = gsub("_", " ", group.var),
                           "test" = "Chi-square Test of Indepedence",
                           'statistic' = round(chisq_test$statistic,3), 
                           'df' = as.character(chisq_test$parameter),
                           'p value' = round(chisq_test$p.value,3), 
                           stringsAsFactors = F)
  row.names(test_summary) <- NULL
  
  # barplot
  p <- ggplot(data, aes(x = data[,key.var], fill = data[,group.var], group = data[,group.var])) +
    geom_bar(aes(y = ..prop..), stat = "count", position = position_dodge()) + 
    geom_text(aes(label = scales::percent(..prop.., accuracy = 1), y = ..prop..), 
              stat = "count", size = 4, vjust = -0.5, position = position_dodge(0.9)) +
    labs(title = paste0("Comparison of ", gsub("_", " ", key.var), " by ", gsub("_", " ", group.var), " Condition"), 
         subtitle = paste0("Chi-squre test: X²(", test_summary[1,5],") = ", test_summary[1,4], ", p value = ", test_summary[1,6], "."),
         x = gsub("_", " ", key.var), 
         y = "Perent (within each group)", 
         fill = paste(gsub("_", " ", group.var), "Condition")) +
    scale_y_continuous(labels=scales::percent) + 
    theme_classic() + 
    theme(legend.position = "bottom")
  
  return(list(barplot = p, test_summary = test_summary))
}

unpaired_samples_test <- function(data, key.var, key.var.label = gsub("_", " ", key.var), 
  group.var, group.var.label = gsub("_", " ", group.var), question = "", 
  alternative = c("two.sided", "less", "greater"), t.var.equal = FALSE){
  
  data <- data[,c(group.var, key.var)]
  colnames(data) <- c("condition", "measurement")
  
  # summary statistics
  stat_summary <- data %>%
    group_by(condition) %>%
    summarise(
      count = n(), 
      mean = round(mean(measurement, na.rm = T), 2),
      median = round(median(measurement, na.rm = T), 2) ,
      sd = round(sd(measurement, na.rm = T), 2),
      IQR = round(IQR(measurement, na.rm = T), 2))
  
  # two-sided two-sample t-test
  t_test <- t.test(
    x = data$measurement[data$condition==levels(data$condition)[1]],
    y = data$measurement[data$condition==levels(data$condition)[2]],
    alternative = alternative, 
    paired = FALSE,
    var.equal = t.var.equal)
  
  # wilcoxin test
  wilcox_test <- wilcox.test(
    x = data$measurement[data$condition==levels(data$condition)[1]],
    y = data$measurement[data$condition==levels(data$condition)[2]],
    alternative = alternative, 
    paired = FALSE)
  
  # bar plot
  bar_plot = ggplot(data = stat_summary, aes(x = condition, y = mean, fill = condition)) +
    geom_bar(stat = "identity") + 
    geom_text(aes(label = mean), vjust = -0.5, color = "black", size = 3.5) +
    labs(title = paste0("Average ", key.var.label, " by ", group.var.label, " Condition"),
         subtitle = paste0(question, "\nUnpaired two-Samples t-test: t(", round(t_test$parameter,0), ") = ", round(t_test$statistic,3),
                           ", p value = ", round(t_test$p.value, 3), 
                           ", 95% CI = (", paste(round(t_test$conf.int,3), collapse = ", "),
                           "). \nUnpaired two-samples wilcoxon test: W = ", round(wilcox_test$statistic, 3), 
                           ", p value = ", round(wilcox_test$p.value, 3), "."),
         x = paste0(group.var.label, " Condition"),  
         y = paste0("Average ", key.var.label, collapse = ), 
         fill = paste0(group.var.label, " Condition")) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  # box plot
  box_plot = ggplot() +
    geom_boxplot(data = data, aes(x = condition, y = measurement, color = condition)) +
    geom_point(data = stat_summary, aes(x = condition, y = mean, color = condition), shape = 1) +
    geom_text(data = stat_summary, aes(x = condition, y = mean, color = condition, label = mean), vjust = 1.5) +
    labs(title = paste0(key.var.label, " by ", group.var.label, " Condition"), 
         subtitle = paste0(question, "\nUnpaired two-Samples t-test: t(", round(t_test$parameter,0), ") = ", round(t_test$statistic,3),
                           ", p value = ", round(t_test$p.value, 3), 
                           ", 95% CI = (", paste(round(t_test$conf.int,3), collapse = ", "),
                           "). \nUnpaired two-samples wilcoxon test: W = ", round(wilcox_test$statistic, 3), 
                           ", p value = ", round(wilcox_test$p.value, 3), "."),
         x = paste0(group.var.label, " Condition"), 
         y = key.var.label,
         color = paste0(group.var.label, " Condition")) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  test_summary <- data.frame(
    'key var' = rep(key.var.label, 2), 
    "group var" = rep(group.var.label, 2), 
    "test" = c("Unpaired Two-Samples T-test", "Unpaired Two-Samples Wilcoxon Test"),
    "statistic" = round(c(t_test$statistic, wilcox_test$statistic),3),
    "df" = as.character(c(round(t_test$parameter,0), NA)),
    "p value" = round(c(t_test$p.value, wilcox_test$p.value),3),
    "CI" = c(paste0("(", paste(round(t_test$conf.int,3), collapse = ","),")"), NA),
    stringsAsFactors = F
  )
  row.names(test_summary) <- NULL
  
  # output result
  return(list(stat_summary = as.data.frame(stat_summary), 
              barplot = bar_plot, 
              boxplot = box_plot, 
              test_summary = test_summary))
}

paired_samples_test <- function(data, vars, conditions, names, question,alternative = c("two.sided", "less", "greater"), t.var.equal = FALSE){
  
  data <- gather(data[,vars], condition, measurement, factor_key = T)
  data$condition <- factor(ifelse(data$condition== vars[1], conditions[1], conditions[2]), levels = conditions)
  
  # summary statistics
  stat_summary <- data %>%
    group_by(condition) %>%
    summarise(
      count = n(),
      mean = round(mean(measurement, na.rm=T), 2), 
      median = round(median(measurement, na.rm=T), 2), 
      sd = round(sd(measurement, na.rm=T),2), 
      IQR = round(IQR(measurement, na.rm=T),2))
  
  # two-sided two-sample t-test
  t_test <- t.test(
    x = data$measurement[data$condition==levels(data$condition)[1]],
    y = data$measurement[data$condition==levels(data$condition)[2]],
    alternative = alternative, 
    paired = TRUE,
    var.equal = t.var.equal)
  
  # wilcoxin test
  wilcox_test <- wilcox.test(
    x = data$measurement[data$condition==levels(data$condition)[1]],
    y = data$measurement[data$condition==levels(data$condition)[2]],
    alternative = alternative, 
    paired = TRUE)
  
  # bar plot
  bar_plot = ggplot(data = stat_summary, aes(x = condition, y = mean, fill = condition)) +
    geom_bar(stat = "identity") + 
    geom_text(aes(label = mean), vjust = -0.5, color = "black", size = 3.5) +
    labs(title = paste0("Average ", names[2], " by ", names[1]),
         subtitle = paste0(question, "\nUnpaired two-Samples t-test: t(", round(t_test$parameter,0), ") = ", round(t_test$statistic,3),
                           ", p value = ", round(t_test$p.value, 3), 
                           ", 95% CI = (", paste(round(t_test$conf.int,3), collapse = ", "),
                           "). \nUnpaired two-samples wilcoxon test: W = ", round(wilcox_test$statistic, 3), 
                           ", p value = ", round(wilcox_test$p.value, 3), "."),
         x = paste0(names[1]),  
         y = paste("Average", names[2]), 
         fill = names[1]) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  # box plot 
  box_plot <- ggplot() +
    geom_boxplot(data = data, aes(x = condition, y = measurement, color = condition)) +
    geom_point(data = stat_summary, aes(x = condition, y = mean, color = condition), shape = 1) +
    geom_text(data = stat_summary, aes(x = condition, y = mean, label = round(mean, 2), color = condition), vjust=1.5) +
    labs(title = paste0(names[2], " by ", names[1]), 
         subtitle = paste0(question, "\nUnpaired two-Samples t-test: t(", round(t_test$parameter,0), ") = ", round(t_test$statistic,3),
                           ", p value = ", round(t_test$p.value, 3), 
                           ", 95% CI = (", paste(round(t_test$conf.int,3), collapse = ", "),
                           "). \nUnpaired two-samples wilcoxon test: W = ", round(wilcox_test$statistic, 3), 
                           ", p value = ", round(wilcox_test$p.value, 3), "."),
         x = names[1], 
         y = names[2], 
         color = names[1]) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  test_summary <- data.frame(
    'key var' = rep(names[2], 2), 
    "group var" = rep(names[1], 2), 
    "test" = c("Paired Two-Sample T-test", "Paired Two-Sample Wilcoxon Test"),
    "statistic" = round(c(t_test$statistic, wilcox_test$statistic),3),
    "df" = as.character(c(round(t_test$parameter,0), NA)),
    "p value" = round(c(t_test$p.value, wilcox_test$p.value),3),
    "CI" = c(paste0("(", paste(round(t_test$conf.int,3), collapse = ","),")"), NA),
    stringsAsFactors = F)
  row.names(test_summary) <- NULL
  
  # output result
  return(list(stat_summary = as.data.frame(stat_summary), 
              barplot = bar_plot, 
              boxplot = box_plot, 
              test_summary = test_summary))
}

two_proportions_test <- function( data = gs, key.var, key.var.label = "Percent", group.var, group.var.label = gsub("_", " ", group.var), title, alternative = c("two.sided", "less", "greater")){
  data <- data[, c(group.var, key.var)]
  names(data) <- c("condition", "measurement")
  
  # stat_summary
  stat_summary <- data %>%
    group_by(condition) %>%
    summarise(
      N = n(), 
      n = sum(measurement == "Yes"),
      prop = round(n/N, 2))
  
  # two-proportion z-test
  z_test <- prop.test(x = stat_summary$n, n = stat_summary$N, alternative = alternative)
  
  test_summary <- data.frame(
    "key var" = gsub("_", " ", key.var),
    "group var"  = gsub("_", " ", group.var),
    "test" = "Two-Proportions Z-Test",
    "statistic" = z_test$statistic,
    "p value" = round(z_test$p.value,3), 
    "CI" = paste0("(", paste(round(z_test$conf.int,3), collapse = ", "), ")"), 
    stringsAsFactors = F)
  
  # bar plot 
  p <- ggplot(data = stat_summary, aes(x = condition, y = prop, fill = condition)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::percent(prop, accuracy = 1)), size = 4, vjust = -0.5) +
    labs(title = paste0(title, " by ", tolower(group.var.label), " condition"),
         subtitle = paste0(
           "Two-proportions z-test: z = ", round(z_test$statistic,3),
           ", p value = ", round(z_test$p.value, 3), 
           ", 95% CI = (", paste(round(z_test$conf.int,3), collapse = ", "),
           ")."),
         x = paste(group.var.label, "Condition"), 
         y = key.var.label, 
         fill = group.var.label) +
    scale_y_continuous(labels=scales::percent) + 
    theme_classic() +
    theme(legend.position = "bottom")
  
  return(list(stat_summary = as.data.frame(stat_summary),
              barplot = p,
              test_summary = test_summary)) 
}

oneway_anova_test <- function(data, key.var, key.var.label = gsub("_", " ", key.var), group.var, group.var.label = gsub("_", " ", group.var), question = ""){
  
  data <- data[, c(group.var, key.var)]
  colnames(data) <- c("condition","measurement")
  
  # stat_summary
  stat_summary <- data %>%
    group_by(condition) %>%
    summarise(
      count = n(), 
      mean = round( mean(measurement, na.rm = T), 2 ), 
      median = round( median(measurement, na.rm = T), 2 ), 
      sd = round( sd(measurement, na.rm = T), 2 ), 
      IQR = round( IQR(measurement, na.rm = T), 2))
  
  # one-way anova
  anova <- aov(measurement ~ condition, data = data)
  
  # test of homogeneity of variance: residuals versus fits plot + Levene's test
  plot(anova, 1)
  homogeneity_check_test <- leveneTest(measurement ~ condition, data = data)
  
  # test of normality: QQ plot + Shapiro-Wilk test
  plot(anova, 2)
  normality_check_test <- shapiro.test(residuals(anova))
  
  # Non-parametric alternative to one-way ANOVA test: Kruskal-Wallis rank sum test
  kruskal <- kruskal.test(measurement ~ condition, data = data)
  
  # test_summary
  test_summary <- data.frame(
    'key var' = rep(key.var.label, 2), 
    "group var" = rep(group.var.label, 2), 
    "test" = c("One-way ANOVA Test", "Kruskal-Wallis Test"),
    "statistic" = round(c(summary(anova)[[1]][[1,"F value"]], kruskal$statistic),3),
    "df" = c(paste0("(", summary(anova)[[1]][[1,"Df"]],", ", summary(anova)[[1]][[2,"Df"]], ")"), kruskal$parameter),
    "p value" = round(c(summary(anova)[[1]][[1,"Pr(>F)"]], kruskal$p.value),3),
    stringsAsFactors = F)
  row.names(test_summary) <-  NULL
  
  # barplot
  bar_plot <- ggplot(data = stat_summary, aes(x = condition, y = mean, fill = condition)) +
    geom_bar(stat = "identity") + 
    geom_text(aes(label = mean), vjust = -0.5, color = "black", size = 3.5) +
    labs(title = paste0("Average ", key.var.label, " by ", group.var.label, " Condition"),
         subtitle = paste0(
           question, "\n",
           "One-way ANOVA test: F(", summary(anova)[[1]][[1,"Df"]], ",", summary(anova)[[1]][[2,"Df"]], ") = ", 
           round(summary(anova)[[1]][[1,"F value"]],3),", ", 
           "p value = ", round(summary(anova)[[1]][[1,"Pr(>F)"]], 3), ". \n", 
           "Kruskal-Wallis test: X²(", kruskal$parameter,") = ", round(kruskal$statistic, 3), ", ", 
           "p value = ", round(kruskal$p.value, 3), "."),
         x = paste0(group.var.label, " Condition"),  
         y = paste0("Average ", key.var.label, collapse = ), 
         fill = paste0(group.var.label, " Condition")) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  # boxplot 
  box_plot <-  ggplot() +
    geom_boxplot(data = data, aes(x = condition, y = measurement, color = condition)) +
    geom_point(data = stat_summary, aes(x = condition, y = mean, color = condition), shape = 1) +
    geom_text(data = stat_summary, aes(x = condition, y = mean, color = condition, label = mean), vjust = 1.5) +
    labs(title = paste0(key.var.label, " by ", group.var.label, " Condition"), 
         subtitle = paste0(
           question, "\n",
           "One-way ANOVA test: F(", summary(anova)[[1]][[1,"Df"]], ",", summary(anova)[[1]][[2,"Df"]], ") = ", 
           round(summary(anova)[[1]][[1,"F value"]],3),", ", 
           "p value = ", round(summary(anova)[[1]][[1,"Pr(>F)"]], 3), ". \n", 
           "Kruskal-Wallis test: X²(", kruskal$parameter,") = ", round(kruskal$statistic, 3), ", ", 
           "p value = ", round(kruskal$p.value, 3), "."),
         x = paste0(group.var.label, " Condition"), 
         y = key.var.label,
         color = paste0(group.var.label, " Condition")) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  # output result
  return(list(stat_summary = as.data.frame(stat_summary), 
              barplot = bar_plot, 
              boxplot = box_plot, 
              test_summary = test_summary, 
              homogeneity_check_test = homogeneity_check_test,
              normality_check_test =  normality_check_test))
}

## simulate survey data ----
n <- 1000

col_names <- c(paste0("Q", 1:12, "_1"), paste0("Q", 1:10, "_2"))
df1 <- matrix(sample(1:5, n*length(col_names), replace = T), ncol = length(col_names))
colnames(df1) <- col_names

unique_values <- list(Q13_1=18:100, Q14_1=1:4, Q15_1=1:6, Q16_1=1:2, Q17_1=1:2, 
                      competitionLabel = comp_label, timeUncertaintyLabel = time_label, 
                      toleranceLabel = tole_label, supportLabel = supp_label)
df2 <- bind_cols(lapply(unique_values, sample, n, replace = T))

game_survey_data <- cbind(df1, df2)

## recode variables (where analysis starts) ----
gs <- game_survey_data %>%
  mutate(
    Age = Q13_1, 
    Age_Group = cut(Q13_1,
                    breaks = age_breaks,
                    labels = age_label,
                    include.lowest = T),
    Gender = factor(gend_label[Q14_1], levels = gend_label),
    Education_Group = factor(educ_label[Q15_1], levels = educ_label),
    Employment_Status = factor(empl_label[Q16_1], levels = empl_label),
    Online_Research_Experience = factor(onre_label[Q17_1], levels = onre_label),
    competitionLabel = factor(competitionLabel, levels = comp_label),
    timeUncertaintyLabel = factor(timeUncertaintyLabel, levels = time_label),
    toleranceLabel = factor(toleranceLabel, levels = tole_label),
    supportLabel = factor(supportLabel, levels = supp_label),
    TA_score = TA_calc(game_survey_data)
  ) %>%
  rename(
    Competition = competitionLabel,
    Time_Uncertainty = timeUncertaintyLabel,
    Tolerance = toleranceLabel,
    Support = supportLabel, 
    Perceived_Opponent_Strength = Q2_2,
    Perceived_Competition = Q3_2,
    Rounds_Certainty = Q10_2
  )

## Chi-square Test of Indepedence ----
# Age_Group by Competition, Time Urcertainty, Tolerance and Support Conditions
age_comp_chisq <- chisq_test_plot(data = gs, key.var = "Age_Group", group.var = "Competition")
age_time_chisq <- chisq_test_plot(data = gs, key.var = "Age_Group", group.var = "Time_Uncertainty")
age_tole_chisq <- chisq_test_plot(data = gs, key.var = "Age_Group", group.var = "Tolerance")
age_supp_chisq <- chisq_test_plot(data = gs, key.var = "Age_Group", group.var = "Support")

# Gender by Competition, Time Urcertainty, Tolerance and Support Conditions
gen_comp_chisq <- chisq_test_plot(data = gs, key.var = "Gender", group.var = "Competition")
gen_time_chisq <- chisq_test_plot(data = gs, key.var = "Gender", group.var = "Time_Uncertainty")
gen_tole_chisq <- chisq_test_plot(data = gs, key.var = "Gender", group.var = "Tolerance")
gen_supp_chisq <- chisq_test_plot(data = gs, key.var = "Gender", group.var = "Support")

# Education Group by Competition, Time Urcertainty, Tolerance and Support Conditions
edu_comp_chisq <- chisq_test_plot(data = gs, key.var = "Education_Group", group.var = "Competition")
edu_time_chisq <- chisq_test_plot(data = gs, key.var = "Education_Group", group.var = "Time_Uncertainty")
edu_tole_chisq <- chisq_test_plot(data = gs, key.var = "Education_Group", group.var = "Tolerance")
edu_supp_chisq <- chisq_test_plot(data = gs, key.var = "Education_Group", group.var = "Support")

# Employment Status by Competition
emp_comp_chisq <- chisq_test_plot(data = gs, key.var = "Employment_Status", group.var = "Competition")

# Online Research Experience by Competition
ore_comp_chisq <- chisq_test_plot(data = gs, key.var = "Online_Research_Experience", group.var = "Competition")

## Two-proportions z-test -----
# Employment Status by Time Uncertainty, Tolerance, and Support
emp_time_prop <- two_proportions_test(data = gs, key.var = "Employment_Status", group.var = "Time_Uncertainty", 
                                      title = "Proportions of players who currently have a job")
emp_tole_prop <- two_proportions_test(data = gs, key.var = "Employment_Status", group.var = "Tolerance", 
                                      title = "Proportions of players who currently have a job")
emp_supp_prop <- two_proportions_test(data = gs, key.var = "Employment_Status", group.var = "Support", 
                                      title = "Proportions of players who currently have a job")

# Online Research Experience by Time Uncertainty, Tolerance, and Support
ore_time_prop <- two_proportions_test(data = gs, key.var = "Employment_Status", group.var = "Time_Uncertainty", 
                                      title = "Proportions of players who had online research experiment experience")
ore_tole_prop <- two_proportions_test(data = gs, key.var = "Employment_Status", group.var = "Tolerance", 
                                      title = "Proportions of players who had online research experiment experience")
ore_supp_prop <- two_proportions_test(data = gs, key.var = "Employment_Status", group.var = "Support", 
                                      title = "Proportions of players who had online research experiment experience")

## Unpaired two-sample t-test -----
# Age by Time Urcertainty, Tolerance and Support 
age_time_t <- unpaired_samples_test(data = gs, key.var = "Age", group.var = "Time_Uncertainty")
age_tole_t <- unpaired_samples_test(data = gs, key.var = "Age", group.var = "Tolerance")
age_supp_t <- unpaired_samples_test(data = gs, key.var = "Age", group.var = "Support")

# TA score by Tolerance Condition
TA_tole_t <- unpaired_samples_test(data = gs, key.var = "TA_score", group.var = "Tolerance")

# Certainty about Number of Rounds (Q10_2) by Time Uncertaint Condition
RC_time_t <- unpaired_samples_test(data = gs, key.var = "Rounds_Certainty", group.var = "Time_Uncertainty", 
                                   question = "Q: When I was playing the game I was certain about the number of rounds I had to play.")

## Paired two sample t-test ----
# Time Pressure Level (Q4_2 and Q6_2) by Round Type (Quick/Slow)
TP_round_pt <- paired_samples_test(
  data = gs, 
  vars = c("Q4_2", "Q6_2"),
  conditions = c("Quick Voting","Slow Voting"),
  names = c("Round Type", "Preceived Time Pressure"), 
  question = "Q: I felt a great amount of time pressure when voting for an item."
)

# Need of Fast Decision (Q5_2 and Q7_2) by Round Type (Quick/Slow) 
FD_round_pt <- paired_samples_test(
  data = gs, 
  vars = c("Q5_2", "Q7_2"),
  conditions = c("Quick Voting","Slow Voting"),
  names = c("Round Type", "Preceived Fast Decision Need"), 
  question = "Q: I had to make my decisions very fast."
)

# Amount of Information Processed (Q8_2 and Q9_2) by Votting Chioces 
IP_tool_pt <- paired_samples_test(
  data = gs, 
  vars = c("Q8_2", "Q9_2"),
  conditions = c("Explosives","Mines"),
  names = c("Tool Type", "Amount of Information Processed"), 
  question = "Q: When I had to choose between different types of explosives/mins, I felt like I had to process too much information."
)

## one way ANOVA -----------
# Age by Competition 
age_comp_aov <- oneway_anova_test(data = gs, key.var = "Age", group.var = "Competition", question = "")

# Perceived Opponent Strength (Q2_2) by Competition
POS_comp_aov <- oneway_anova_test(data = gs, key.var = "Perceived_Opponent_Strength", group.var = "Competition", 
                                  question = "Q: The opposing teams were stronger than my team.")

# Perceived Competition (Q3_2) by Competition 
PC_comp_aov <- oneway_anova_test(data = gs, key.var = "Perceived_Competition", group.var = "Competition", 
                                 question = "Q: The relationship between my team and the other teams was competitive.")

## Test Results Summary
test_summary <- bind_rows(
  age_comp_aov$test_summary,
  age_time_t$test_summary,
  age_tole_t$test_summary,
  age_supp_t$test_summary,
  age_comp_chisq$test_summary,
  age_time_chisq$test_summary,
  age_tole_chisq$test_summary,
  age_supp_chisq$test_summary,
  gen_comp_chisq$test_summary,
  gen_time_chisq$test_summary,
  gen_tole_chisq$test_summary,
  gen_supp_chisq$test_summary,
  edu_comp_chisq$test_summary,
  edu_time_chisq$test_summary,
  edu_tole_chisq$test_summary,
  edu_supp_chisq$test_summary,
  emp_comp_chisq$test_summary,
  emp_time_prop$test_summary,
  emp_tole_prop$test_summary,
  emp_supp_prop$test_summary,
  ore_comp_chisq$test_summary,
  ore_time_prop$test_summary,
  ore_tole_prop$test_summary,
  ore_supp_prop$test_summary,
  TA_tole_t$test_summary,
  POS_comp_aov$test_summary,
  PC_comp_aov$test_summary,
  TP_round_pt$test_summary,
  FD_round_pt$test_summary,
  IP_tool_pt$test_summary,
  RC_time_t$test_summary)




