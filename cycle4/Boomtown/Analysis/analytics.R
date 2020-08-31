## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019
## Bayesian hypothesis testing (Individual-level outcomes)

# Number of valid experiments
factorial$group <- factorial$matchid
nGames<-length(unique(factorial$group))

# Number of unique experimental conditions played
nConditions<-length(unique(factorial$settingsNum))
agg <- aggregate(data=factorial, matchid ~ settingsNum, function(x) length(unique(x)))
df<- data.frame(c(1:96))
colnames(df)<- "settingsNum"
setting_counts<- merge(df, agg, by="settingsNum", all=TRUE)
setting_counts[is.na(setting_counts)] <- 0

# Game dates
factorial$matchDate <- as.POSIXct(factorial$matchDate,format="%d/%m/%Y")
dates<-aggregate(matchDate ~ matchid, data=factorial, mean)

# Game times (date.time deprecated in Cycle 4)
tool_checks$date.time <- as.POSIXct(tool_checks$date.time,format="%d/%m/%Y %H:%M")
times<-aggregate(date.time ~ matchid, data=tool_checks, mean)

# Games by hour
times$hour <- strftime(times$date.time, format="%d/%m/%Y %H")
times$hour2 <- substr(times$hour, nchar(times$hour) - 2 + 1, nchar(times$hour))
times$day <- strftime(times$date.time, format="%Y-%m-%d")

hourly_plot <- ggplot(data=times, mapping=aes(x=hour2)) + geom_bar() + 
  facet_grid(facets = day ~ ., margins = FALSE) + theme_bw() + 
  labs(title="Number of Experiments by Hour and Day", x="Hour of the Day", y="Number of Experiments Played")

factorial.tools<-subset(factorial, tools!="9" & tools!="10" & tools!="11" & tools!="12")
factorial.tools$innovation2<- as.numeric(factorial.tools$innovation)
tool_rate1<-factorial.tools%>%
  group_by(tools)%>%
  summarise(rate_inn=mean(innovation2, na.rm=TRUE))
ggplot(data=tool_rate1, aes(x=tools, y=rate_inn)) +
  geom_bar(stat="identity") +
  ggtitle("Innovative Choices by Tool Choice") + 
  xlab("Tool Choice") +
  ylab("Innovative Choice Rate")

# Tool controls

tool_checks<-subset(tool_checks, tools=="9" | tools=="10" | tools=="11" | tools=="12")
tool_checks$choice <- "Wrong"
tool_checks$choice[tool_checks$tools=="9" & tool_checks$FinalItemSelected=="TNTbarrel"] <- "Correct"
tool_checks$choice[tool_checks$tools=="10" & tool_checks$FinalItemSelected=="SatchelCharge"] <- "Correct"
tool_checks$choice[tool_checks$tools=="11" & tool_checks$FinalItemSelected=="SatchelCharge"] <- "Correct"
tool_checks$choice[tool_checks$tools=="12" & tool_checks$FinalItemSelected=="SatchelCharge"] <- "Correct"

tool_checks$choice2 <- 0
tool_checks$choice2[tool_checks$tools=="9" & tool_checks$FinalItemSelected=="TNTbarrel"] <- 1
tool_checks$choice2[tool_checks$tools=="10" & tool_checks$FinalItemSelected=="SatchelCharge"] <- 1
tool_checks$choice2[tool_checks$tools=="11" & tool_checks$FinalItemSelected=="SatchelCharge"] <- 1
tool_checks$choice2[tool_checks$tools=="12" & tool_checks$FinalItemSelected=="SatchelCharge"] <- 1

tool_rate2 <- tool_checks %>%
  group_by(tools, choice) %>%
  summarise(counts  = n()) 
toolControls<-ggplot(tool_rate2, aes(x = tools, y = counts)) +
  geom_bar(
    aes(color = choice, fill = choice),
    stat = "identity", position = position_stack()) +
  ggtitle("Correct Choices for Check Test Items") +
  ylab("Number of Choices") +
  scale_x_continuous ("Check Test Item", breaks=9:12,
                      labels=c("9"  = "45*100% vs 22*35%", 
                               "10" = "7*100% vs 76*65%",
                               "11" = "76*65% vs 162*6%",
                               "12" = "76*65% vs 22*35%"))

tool_rate3 <- tool_checks %>%
  group_by(matchid) %>%
  summarise(mean(choice2))

allWrong<-as.data.frame(tool_rate3  %>% filter(`mean(choice2)` == 0))

# Manipulation checks

oneway_anova_test2 <- function(data, key.var, key.var.label = gsub("_", " ", key.var), group.var, group.var.label = gsub("_", " ", group.var), question = ""){
  
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
  
  homogeneity_check_test <- leveneTest(measurement ~ condition, data = data)
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
              boxplot = box_plot, 
              barplot = bar_plot, 
              test_summary = test_summary, 
              homogeneity_check_test = homogeneity_check_test))
}

#Availability heuristic check 

# Framed item

tool_checks <- read.csv(file="tool_checks.csv", stringsAsFactors = FALSE)
tool_checks <- cbind(tool_checks,
                     data.frame(do.call('rbind', 
                                        strsplit(tool_checks$toolsLabel, ',', fixed=TRUE)),
                                stringsAsFactors = FALSE))

tool_checks$vote1_left <- tool_checks$PlayerVote1 == tool_checks$X1
tool_checks$vote1_right <- tool_checks$PlayerVote1 == tool_checks$X2
tool_checks$vote2_left <- tool_checks$PlayerVote2 == tool_checks$X1
tool_checks$vote2_right <- tool_checks$PlayerVote2 == tool_checks$X2
tool_checks$finalvote_left <- tool_checks$PlayerVote2 == tool_checks$X1
tool_checks$finalvote_right <- tool_checks$PlayerVote2 == tool_checks$X2

tool_checks[,"left_framing"] <- case_when(
  tool_checks[,"round"] %in% c(1, 4, 7) ~ "0. No framing",
  tool_checks[,"settingsNum"] %% 2==1 & tool_checks[,"round"] %in% c(2, 6, 8, 12) ~ "3. Discouraged-passive",
  tool_checks[,"settingsNum"] %% 2==1 & tool_checks[,"round"] %in% c(3, 9) ~ "2. Encouraged-passive",
  tool_checks[,"settingsNum"] %% 2==1 & tool_checks[,"round"] %in% c(5, 10, 13) ~ "1. Encouraged-active",
  tool_checks[,"settingsNum"] %% 2==1 & tool_checks[,"round"] %in% c(11) ~ "4. Discouraged-active",
  tool_checks[,"settingsNum"] %% 2==0 & tool_checks[,"round"] %in% c(3, 5, 9) ~ "3. Discouraged-passive",
  tool_checks[,"settingsNum"] %% 2==0 & tool_checks[,"round"] %in% c(2, 8, 10) ~ "2. Encouraged-passive",
  tool_checks[,"settingsNum"] %% 2==0 & tool_checks[,"round"] %in% c(6, 11, 12) ~ "1. Encouraged-active", 
  tool_checks[,"settingsNum"] %% 2==0 & tool_checks[,"round"] %in% c(13) ~ "4. Discouraged-active")

tool_checks[,"right_framing"] <- case_when(
  tool_checks[,"round"] %in% c(1, 4, 7) ~ "0. No framing",
  tool_checks[,"settingsNum"] %% 2==1 & tool_checks[,"round"] %in% c(2, 6, 8, 12) ~ "1. Encouraged-active",
  tool_checks[,"settingsNum"] %% 2==1 & tool_checks[,"round"] %in% c(11) ~ "2. Encouraged-passive",
  tool_checks[,"settingsNum"] %% 2==1 & tool_checks[,"round"] %in% c(3, 9) ~ "4. Discouraged-active",
  tool_checks[,"settingsNum"] %% 2==1 & tool_checks[,"round"] %in% c(5, 10, 13) ~ "3. Discouraged-passive",
  tool_checks[,"settingsNum"] %% 2==0 & tool_checks[,"round"] %in% c(3, 5, 9) ~ "1. Encouraged-active",
  tool_checks[,"settingsNum"] %% 2==0 & tool_checks[,"round"] %in% c(2, 8, 10) ~ "4. Discouraged-active",
  tool_checks[,"settingsNum"] %% 2==0 & tool_checks[,"round"] %in% c(6, 11, 12) ~ "3. Discouraged-passive",
  tool_checks[,"settingsNum"] %% 2==0 & tool_checks[,"round"] %in% c(13) ~ "2. Encouraged-passive")

tool_checks[,"left_framing_simple"] <- case_when(
  tool_checks[,"round"] %in% c(1, 4, 7) ~ "0. No framing",
  tool_checks[,"settingsNum"] %% 2==1 & tool_checks[,"round"] %in% c(2, 6, 8, 11, 12) ~ "2. Discouraged",
  tool_checks[,"settingsNum"] %% 2==1 & tool_checks[,"round"] %in% c(3, 5, 9, 10, 13) ~ "1. Encouraged",
  tool_checks[,"settingsNum"] %% 2==0 & tool_checks[,"round"] %in% c(3, 5, 9, 13) ~ "2. Discouraged",
  tool_checks[,"settingsNum"] %% 2==0 & tool_checks[,"round"] %in% c(2, 6, 8, 10, 11, 12) ~ "1. Encouraged")

tool_checks[,"right_framing_simple"] <- case_when(
  tool_checks[,"round"] %in% c(1, 4, 7) ~ "0. No framing",
  tool_checks[,"settingsNum"] %% 2==1 & tool_checks[,"round"] %in% c(2, 6, 8, 11, 12) ~ "1. Encouraged",
  tool_checks[,"settingsNum"] %% 2==1 & tool_checks[,"round"] %in% c(3, 5, 9, 10, 13) ~ "2. Discouraged",
  tool_checks[,"settingsNum"] %% 2==0 & tool_checks[,"round"] %in% c(3, 5, 9, 13) ~ "1. Encouraged",
  tool_checks[,"settingsNum"] %% 2==0 & tool_checks[,"round"] %in% c(2, 6, 8, 10, 11, 12) ~ "2. Discouraged")

# Explore effect across multiple outcomes (though primary effect should be on vote 1)

tool_checks$vote1_left <- as.integer(as.logical(tool_checks$vote1_left))
tool_checks$finalvote_left <- as.integer(as.logical(tool_checks$finalvote_left))
tool_checks$vote1_right <- as.integer(as.logical(tool_checks$vote1_right))
tool_checks$finalvote_right <- as.integer(as.logical(tool_checks$finalvote_right))

# Run charts for multiple outcomes

tool_checks_vote1_left <- tool_checks[complete.cases(tool_checks$vote1_left),]
availPlots_left_vote1_simple <- oneway_anova_test2 (
  data = tool_checks_vote1_left, key.var="vote1_left", group.var="left_framing_simple")

availPlots_left_vote1 <- oneway_anova_test2 (
  data = tool_checks_vote1_left, key.var="vote1_left", group.var="left_framing")

tool_checks_finalvote_left <- tool_checks[complete.cases(tool_checks$finalvote_left),]
availPlots_left_final <- oneway_anova_test2 (
  data = tool_checks_finalvote_left, key.var="finalvote_left", group.var="left_framing")

tool_checks_vote1_right <- tool_checks[complete.cases(tool_checks$vote1_right),]

availPlots_right_vote1_simple <- oneway_anova_test2 (
  data = tool_checks_vote1_left, key.var="vote1_right", group.var="right_framing_simple")
availPlots_right_vote1 <- oneway_anova_test2 (
  data = tool_checks_vote1_right, key.var="vote1_right", group.var="right_framing")

tool_checks_finalvote_right <- tool_checks[complete.cases(tool_checks$finalvote_right),]
availPlots_right_final <- oneway_anova_test2 (
  data = tool_checks_finalvote_right, key.var="finalvote_right", group.var="right_framing")

availdf <- factorial %>% group_by(framing) %>% 
  summarise_at(c("inmot1", "inmot2", "innovation"), mean, na.rm = TRUE) %>% 
  gather(Outcome, value, -c(framing))

factorial$framing2 <- factor(factorial$framing)
availPlots_inmot1 <- oneway_anova_test2 (
  data = factorial, key.var="inmot1", group.var="framing2")
availPlots_inmot2 <- oneway_anova_test2 (
  data = factorial, key.var="inmot2", group.var="framing2")
availPlots_innovation <- oneway_anova_test2 (
  data = factorial, key.var="innovation", group.var="framing2")

# Network Density check 

factorial$density2 <- cut(factorial$density, seq(0, 1, 0.25), include.lowest = TRUE)
densityPlots <- oneway_anova_test2 (
  data = factorial, key.var="chat_per_round", group.var="density2")
