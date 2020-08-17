## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019
## Bayesian hypothesis testing (Individual-level outcomes)

# Number of valid games
factorial$group <- factorial$matchid
nGames<-length(unique(factorial$group))

# Number of players connected
nPlayers<-length(unique(factorial$player))

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

# Game times
factorial$date.time <- as.POSIXct(factorial$date.time,format="%d/%m/%Y %H:%M")
times<-aggregate(date.time ~ matchid, data=factorial, mean)

# Games by hour
times$hour <- strftime(times$date.time, format="%d/%m/%Y %H")
times$hour2 <- substr(times$hour, nchar(times$hour) - 2 + 1, nchar(times$hour))
times$day <- strftime(times$date.time, format="%Y-%m-%d")

hourly_plot <- ggplot(data=times, mapping=aes(x=hour2)) + geom_bar() + 
facet_grid(facets = day ~ ., margins = FALSE) + theme_bw() + 
labs(title="Number of games by hour and day", x="Hour of the Day", y="Number of Games Played")
                 
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

factorial.tests<-subset(factorial, tools=="9" | tools=="10" | tools=="11" | tools=="12")
factorial.tests$choice <- "Wrong"
factorial.tests$choice[factorial.tests$tools=="9" & factorial.tests$FinalItemSelected=="TNTbarrel"] <- "Correct"
factorial.tests$choice[factorial.tests$tools=="10" & factorial.tests$FinalItemSelected=="SatchelCharge"] <- "Correct"
factorial.tests$choice[factorial.tests$tools=="11" & factorial.tests$FinalItemSelected=="SatchelCharge"] <- "Correct"
factorial.tests$choice[factorial.tests$tools=="12" & factorial.tests$FinalItemSelected=="SatchelCharge"] <- "Correct"

factorial.tests$choice2 <- 0
factorial.tests$choice2[factorial.tests$tools=="9" & factorial.tests$FinalItemSelected=="TNTbarrel"] <- 1
factorial.tests$choice2[factorial.tests$tools=="10" & factorial.tests$FinalItemSelected=="SatchelCharge"] <- 1
factorial.tests$choice2[factorial.tests$tools=="11" & factorial.tests$FinalItemSelected=="SatchelCharge"] <- 1
factorial.tests$choice2[factorial.tests$tools=="12" & factorial.tests$FinalItemSelected=="SatchelCharge"] <- 1

tool_rate2 <- factorial.tests %>%
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

tool_rate3 <- factorial.tests %>%
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
                 
availdf <- factorial %>% group_by(framing) %>% 
  summarise_at(c("inmot1", "inmot2", "innovation"), mean, na.rm = TRUE) %>% 
  gather(Outcome, value, -c(framing))

avail <- ggplot(availdf, aes(fill=Outcome, y=value, x=framing)) + 
  geom_bar(position="dodge", stat="identity") +
  ylab ("Probability of innovation") +
  scale_x_continuous ("Framing Condition", breaks=0:2,
                    labels=c("0" = "No framing", 
                             "1" = "Negative framing",
                             "2" = "Positive framing"))
                 
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
