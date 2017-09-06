# Created by Pablo Diego Rosell, PhD, for Gallup inc. in August 2017

# Load data for analysis

exp2_cooperation <- read.csv('NGS2-Cycle1-Experiment2/cooperation_exp2.csv',
                             header = TRUE, sep = ',')

exp2_rewire <- read.csv('NGS2-Cycle1-Experiment2/rewire_exp2.csv',
                             header = TRUE, sep = ',')

empanelment <- read.csv('data/empanelment_cleaned.csv',
                        header = TRUE, sep = ',')

# Merge experimental and empanelment data

empanelment$empanel_id <- empanelment$ExternalReference

merged.exp2 <- merge (empanelment, exp2_cooperation, by = "empanel_id")

########################################################################################################################
# Descriptive analyses - Cooperation (From Tian Zeng)
########################################################################################################################

# Number of unique players, sessions, conditions, players by condition, sessions by condition

length(unique(exp2_cooperation$empanel_id))
length(unique(exp2_cooperation$session))
unique(exp2_cooperation$condition)
aggregate(empanel_id ~ condition, data = merged.exp2[merged.exp2$round==1,], FUN = length)
sessions.condition <- aggregate(empanel_id ~ condition+session, 
                                data = merged.exp2[merged.exp2$round==1,], 
                                FUN = length)
aggregate(session ~ condition, data = sessions.condition, FUN = length)

# Explore overall patterns of experimental sessions

session_info=exp2_cooperation%>%
  filter(round_num==1)%>%
  group_by(session)%>%
  summarise(
    num_player=n(),
    condition=unique(condition)[1]
  )%>%
  arrange(condition)

# Display session_info

datatable(session_info, 
          caption = "Sessions: number of players and game conditions",
          options = list(
            scrollX = TRUE,
            scrollCollapse = TRUE))

# Number of players within and between experimental conditions.

ggplot(data = session_info, 
       aes(x=num_player))+
  geom_histogram(aes(weights=..count.., 
                     fill=condition))+
  scale_fill_brewer(palette="Set3")+
  facet_wrap(~condition, ncol=1)

# Cooperation rate over the course of a game.

session_round_rate=exp2_cooperation%>%
  group_by(session, 
           round_num)%>%
  summarise(
    rate_contr=mean(decision0d1c)
  )
session_round_rate=left_join(session_round_rate, 
                             session_info,
                             by="session")

ggplot(session_round_rate, 
       aes(x=factor(round_num), 
           y=rate_contr,
           fill=condition))+
  geom_boxplot()+
  facet_grid(.~condition)+
  labs(x="round")+
  theme(axis.text.x=element_text(angle=0, 
                                 vjust=0.4,
                                 hjust=1))

# Cooperation rate over the course of a game by ingroup

session_round_rate=exp2_cooperation%>%
  group_by(session, 
           round_num, 
           ingroup)%>%
  summarise(
    rate_contr=mean(decision0d1c)
  )
session_round_rate=left_join(session_round_rate, 
                             session_info,
                             by="session")

ggplot(session_round_rate, 
       aes(x=factor(round_num), 
           y=rate_contr,
           fill=condition))+
  geom_boxplot()+
  facet_grid(.~condition+ingroup)+
  labs(x="round")+
  theme(axis.text.x=element_text(angle=0, 
                                 vjust=0.4,
                                 hjust=1))

# Connections with in-group members and out-group members

session_info=exp2_rewire%>%
  filter(round_num==3)%>%
  group_by(session)%>%
  summarise(
    num_player=n(),
    condition=unique(condition)[1]
  )%>%
  arrange(condition)

session_round_connect=exp2_rewire%>%
  group_by(session, 
           round_num, 
           ingroup)%>%
  summarise(
    rate_contr=mean(connect)
  )
session_round_connect=left_join(session_round_connect, 
                             session_info,
                             by="session")

ggplot(session_round_connect, 
       aes(x=factor(round_num), 
           y=rate_contr,
           fill=condition))+
  geom_boxplot()+
  facet_grid(.~condition+ingroup)+
  labs(x="round")+
  theme(axis.text.x=element_text(angle=0, 
                                 vjust=0.4,
                                 hjust=1))

########################################################################################################################
# Exploratory analyses - Empanelment Predictors of Cooperation
########################################################################################################################

# Prob to cooperate

myvars <- c("empanel_id", "decision0d1c")
newdata <- merged.exp2[myvars]

av.cooperation <-aggregate(newdata$decision0d1c, 
                           by=list(newdata$empanel_id), 
                           FUN=mean, 
                           na.rm=TRUE)
colnames(av.cooperation)<-c("empanel_id", "prob.cooperate")

# Group continuous variables

NameListCont <- c("Q11_age",
                  "Q25_adult_hh_num",
                  "Q26_child_hh_num",
                  "Q26_child_hh_num",
                  "Q34_internet_hours_day",
                  "Q35_social_networks",
                  "Q37_social_media_hours_day",
                  "soc_dom_orient",
                  "comm_orient_scale",
                  "horiz_indiv",
                  "vert_indiv",
                  "horiz_collect",
                  "vert_collect",
                  "tipi_extraversion",
                  "tipi_agreeableness",
                  "tipi_conscientiousness",
                  "tipi_emot_stability",
                  "tipi_open_experiences")

# Group categorical variables
# !!!Add "time of the day", "computer settings", "Race/ethnicity", "technology access"

NameListCat <- c("Q12_gender",
                 "Q13_education",
                 "Q14_job",
                 "Q17_occupation",
                 "religion",
                 "Q24_religion_freq",
                 "Q27_marital_status",
                 "Q29_born_in_country",
                 "Q22_income_feeling")

empanelment$empanel_id<-empanelment$ExternalReference

merged.exp2.agg <- merge(empanelment, av.cooperation, by = "empanel_id")

col.num <- which(colnames(merged.exp2.agg) %in% NameListCont)
cont.vars <- merged.exp2.agg[,col.num]
cont.data<-cbind(cont.vars, merged.exp2.agg$prob.cooperate)

col.num <- which(colnames(empanelment) %in% NameListCat)
cat.vars<- merged.exp2.agg[,col.num]
cat.data<-cbind(cat.vars, merged.exp2.agg$prob.cooperate)

all.vars<-c(NameListCat, NameListCont)

# Correlation visualizations for continuous variables

corrgram (cont.vars)
chart.Correlation(cont.vars)
summary(cont.vars$Q35_social_networks)

# Association visualizations from categorical variables

BoxPlotter<-function(var.pos) {
fill <- "gold1"
line <- "goldenrod2"
var.name<-colnames(cat.vars)[var.pos]
levels(cat.vars[,var.pos]) <- gsub(" ", "\n", levels(cat.vars[,var.pos]))
levels(cat.vars[,var.pos]) <- gsub("/", "/\n", levels(cat.vars[,var.pos]))
customboxplot <- ggplot(cat.vars, aes(cat.vars[, var.pos], merged.exp2.agg$prob.cooperate)) +
  geom_boxplot(fill = fill, colour = line, notch = TRUE) +
  xlab(paste(var.name))
print(customboxplot)
}
for (i in 1:(length(NameListCat))){
  cat.factor<-colnames(cat.vars)[i]
  print(cat.factor)
  BoxPlotter(i)
  plotname<-paste(cat.factor, "_prob.cooperate.png", sep="")
  ggsave(filename=plotname, height=7,width=9)
  }

########################################################################################################################
# Univariate test function for predictors using logistic regression
########################################################################################################################

logit.tester <- function(ind.var) {
  vars<-paste(c(ind.var))
  formula.logit<-as.formula(paste("decision0d1c~", vars, sep=""))
  logit.test <- glm(formula.logit, data = merged.exp2, family = "binomial")
  logit.multiwayvcov.test <- cluster.vcov(logit.test, cbind(merged.exp2$session, merged.exp2$pid))
  output<-coeftest(logit.test, logit.multiwayvcov.test)
  return(output)
}

# Loop over all variables. 
# It will throw an error ("non-conformable arrays") if there is only one session

logistic.coeffs.frame = list()
for (i in all.vars) {
  report.logistic<-logit.tester(i)
  report.rows<-length(report.logistic[,1])
  logistic.coeffs.frame[[i]]<-as.data.frame(report.logistic[,1:4])[2:report.rows,]
}
report.logistic.all<-do.call(rbind, logistic.coeffs.frame)
report.logistic.all<-cbind(row.names(report.logistic.all), report.logistic.all)
write.csv(report.logistic.all, file = 'logit_coop_exp2.csv', row.names = FALSE, na = '')
