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
factorial$matchDate <- as.POSIXct(factorial$matchDate,format="%Y-%m-%d")
dates<-aggregate(matchDate ~ matchid, data=factorial, mean)

# Game times
factorial$date.time <- as.POSIXct(factorial$date.time,format="%Y-%m-%d %H:%M:%S")
times<-aggregate(date.time ~ matchid, data=factorial, mean)

# Games by hour

times$hour <- strftime(times$date.time, format="%Y-%m-%d %H")
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
                   labels=c("9"  = "100% of 45 vs 35% of 22", 
                            "10" = "100% of 7 vs 65% of 76",
                            "11" = "65% of 76 vs 6% of 162",
                            "12" = "65% of 76 vs 35% of 22"))

tool_rate3 <- factorial.tests %>%
  group_by(matchid) %>%
  summarise(mean(choice2))

allWrong<-as.data.frame(tool_rate3  %>% filter(`mean(choice2)` == 0))
