## Created by Pablo Diego Rosell, PhD, for Gallup inc. in December 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

## Positive controls for experimental manipulations
# Competition
# We anticipate that groups assigned to the low competition condition will develop high Collective Efficacy (CSE), 
# whereas groups assigned to the high competition condition will develop low CE

factorial$CSEnum<-scale(as.numeric(as.character(factorial$CSE)))
h3.4.table<-aggregate(factorial$CSEnum, list(factorial$h1.1), mean, na.rm=T)
xx<-barplot(h3.4.table[,2], ylab=c("Average CSE Score"), xaxt='n', ylim=c(-0.5,1), main="Collective Self-efficacy by Competition Levels")
text(x = xx, y = h3.4.table[,2], label = round(h3.4.table[,2], 3), pos = 3, cex = 0.8, col = "red")
axis(1, at=xx, labels=c("None","Low", "Medium", "High"), tick=FALSE, las=2, line=-0.5)

factorial$competition3[factorial$h1.1==1]<- "Low Competition"
factorial$competition3[factorial$h1.1==2 | factorial$h1.1==0]<- "Medium or No Competition"
factorial$competition3[factorial$h1.1==3]<- "High Competition"
competitionPlots<- ggplot(factorial, aes(x=competition3, y=CSEnum, color=competition3)) + 
  geom_boxplot(notch=TRUE) + theme_grey () + theme(legend.position="none")

#h3.4CSE.formula <- CSEnum~competition2+h1.3+h2.1+h3.1+h3.2+h3.3+h3.4+h3.5+tools+(1|matchid)
#glmm3.4.CSE <- glmer(h3.4CSE.formula, data=factorial, family = gaussian, chains = 3, iter = 500)

# Uncertainty/Risk

factorial.tests<-subset(factorial, tools=="9" | tools=="11" | tools=="12")
factorial.tests$choice <- "Wrong"
factorial.tests$choice[factorial.tests$tools=="9" & factorial.tests$leaderChoice=="TNTbarrel"] <- "Correct"
factorial.tests$choice[factorial.tests$tools=="11" & factorial.tests$leaderChoice=="SatchelCharge"] <- "Correct"
factorial.tests$choice[factorial.tests$tools=="12" & factorial.tests$leaderChoice=="SatchelCharge"] <- "Correct"
tool_rate2 <- factorial.tests %>%
  group_by(tools, choice) %>%
  summarise(counts  = n()) 
toolControls<-ggplot(tool_rate2, aes(x = tools, y = counts)) +
  geom_bar(
    aes(color = choice, fill = choice),
    stat = "identity", position = position_stack()) +
  ggtitle("Correct Choices for Check Test Items") + 
  xlab("Check Test Item") +
  ylab("Number of Choices") +
  scale_x_discrete(labels=c("9" = "100% of 45 vs 35% of 22", "11" = "65% of 76 vs 6% of 162",
                                "12" = "65% of 76 vs 35% of 22"))
tool_rate3 <- factorial.tests %>%
  group_by(matchid) %>%
  summarise(mean(correct)) 
allWrong<-as.data.frame(tool_rate3  %>% filter(`mean(correct)` == 0))

# Group composition

empanelment_and_log<-read.csv(paste(od, "empanelment_and_log.csv", sep = '/'))
factorial$matchid2<-as.integer(as.character(factorial$matchid))
factorial$round2<-as.integer(as.character(factorial$round))
empanelment_and_log$matchid2<-as.integer(empanelment_and_log$matchid)
empanelment_and_log$round2<-as.integer(empanelment_and_log$round)
empanelment_and_log <- subset(empanelment_and_log, select = -c("round", "matchid"))
factorial <- merge(factorial, empanelment_and_log, by = c("matchid2", "round2"))

factorial$leaderTA[factorial$h3.2==0]<- "Low TA"
factorial$leaderTA[factorial$h3.2==1]<- "High TA"
leaderTAplot<- ggplot(factorial, aes(x=leaderTA, y=tol_ambiguity_Leader, color=leaderTA)) + 
  geom_boxplot(notch=TRUE) + theme_grey () + theme(legend.position="none") + 
  ggtitle("Average Leader TA Score") 

factorial$leaderTL[factorial$h3.4==0]<- "Low TL"
factorial$leaderTL[factorial$h3.4==1]<- "High TL"
leaderTLplot<- ggplot(factorial, aes(x=leaderTL, y=trans_leadership_mean_Leader, color=leaderTL)) + 
  geom_boxplot(notch=TRUE) + theme_grey () + theme(legend.position="none") + 
  ggtitle("Average Leader TL Score") 

cols <- grep("tol_ambiguity_Player", names(factorial), value=T)
factorial$group_tol_ambiguity<-rowMeans(factorial[,cols], na.rm = TRUE)
factorial$groupTA[factorial$h3.1==0]<- "Low TA"
factorial$groupTA[factorial$h3.1==1]<- "High TA"
groupTAplot<- ggplot(factorial, aes(x=groupTA, y=group_tol_ambiguity, color=groupTA)) + 
  geom_boxplot(notch=TRUE) + theme_grey () + theme(legend.position="none") + 
  ggtitle("Average Group TA Score") 

