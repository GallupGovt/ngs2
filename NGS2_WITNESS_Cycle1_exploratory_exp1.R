#Created by Pablo Diego Rosell, PhD, for Gallup inc. in August 2017

# Load data for analysis
exp1_cooperation <- read.csv('NGS2-Cycle1-Experiment1/cooperation_exp1.csv',
                             header = TRUE, sep = ',')
empanelment <- read.csv('data/empanelment_cleaned.csv',
                        header = TRUE, sep = ',')
empanelment$empanel_id <- empanelment$ExternalReference
merged.exp1 <- merge (empanelment, exp1_cooperation, by = "empanel_id")

########################################################################################################################
#Descriptive analyses - Cooperation (From Tian Zeng)
########################################################################################################################

### Number of unique players, sessions, conditions.

length(unique(merged.exp1$empanel_id))
length(unique(merged.exp1$session))
unique(merged.exp1$condition)

### Explore overall patterns of experimental sessions
session_info=exp1_cooperation%>%
  filter(round==1)%>%
  group_by(session)%>%
  summarise(
    num_player=n(),
    condition=unique(condition)[1]
  )%>%
  arrange(condition)

## Display session_info
datatable(session_info, 
          caption = "Sessions: number of players and game conditions",
          options = list(
            scrollX = TRUE,
            scrollCollapse = TRUE))

#### The number of players vary within and between experimental conditions.
ggplot(data = session_info, 
       aes(x=num_player))+
  geom_histogram(aes(weights=..count.., 
                     fill=condition))+
  scale_fill_brewer(palette="Set3")+
  facet_wrap(~condition, ncol=1)

#### Cooperation rate over the course of a game.

session_round_rate=exp1_cooperation%>%
  group_by(session, 
           round)%>%
  summarise(
    rate_contr=mean(action)
  )
session_round_rate=left_join(session_round_rate, 
                             session_info,
                             by="session")

ggplot(session_round_rate, 
       aes(x=factor(round), 
           y=rate_contr,
           fill=condition))+
  geom_boxplot()+
  facet_grid(.~condition)+
  labs(x="round")+
  theme(axis.text.x=element_text(angle=0, 
                                 vjust=0.4,
                                 hjust=1))

#### Association between behavior and social connections.

coop_neighbor=
  exp1_cooperation%>%
  group_by(condition, 
           num_neighbors, 
           session)%>%
  summarise(
    rate_contr=mean(action)
    )

ggplot(coop_neighbor, 
       aes(x=factor(num_neighbors), 
           y=rate_contr,
           fill=condition))+
  geom_boxplot()+
  facet_grid(.~condition)+
  labs(x="number of neighbors")+
  theme(axis.text.x=element_text(angle=0, 
                                 vjust=0.4,
                                 hjust=1))

ggplot(exp1_cooperation, 
       aes(x=factor(round), 
           y=num_neighbors,
           fill=condition))+
  geom_boxplot()+
  facet_grid(.~condition)+
  labs(x="round")+
  theme(axis.text.x=element_text(angle=0, 
                                 vjust=0.4,
                                 hjust=1))

########################################################################################################################
#Exploratory analyses - Empanelment Predictors of Cooperation
########################################################################################################################
###Show stopper

print("ATENTION!!! Need to come back here and create summary cooperation variables")

#Prob to cooperate

myvars <- c("empanel_id", "action")
newdata <- merged.exp1[myvars]

av.cooperation <-aggregate(newdata$action, 
                           by=list(newdata$empanel_id), 
                           FUN=mean, 
                           na.rm=TRUE)
colnames(av.cooperation)<-c("empanel_id", "prob.cooperate")
merged.exp1.agg <- merge (empanelment, av.cooperation, by = "empanel_id")

#Group continuous variables

NameListCont <- c("prob.cooperate", 
                  "Q11_age",
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

#Group categorical variables
#!!!Add "time of the day", "computer settings", "Race/ethnicity", "technology access"

NameListCat <- c("prob.cooperate", 
                 "Q12_gender",
                 "Q13_education",
                 "Q14_job",
                 "Q17_occupation",
                 "religion",
                 "Q24_religion_freq",
                 "Q27_marital_status",
                 "Q29_born_in_country",
                 "Q22_income_feeling")

empanelment$empanel_id<-empanelment$ExternalReference

col.num <- which(colnames(merged.exp1.agg) %in% NameListCont)
cont.vars <- merged.exp1.agg[,col.num]
cont.data<-cbind(cont.vars, merged.exp1.agg$prob.cooperate)

col.num <- which(colnames(empanelment) %in% NameListCat)
cat.vars<- merged.exp1.agg[,col.num]
cat.data<-cbind(cat.vars, merged.exp1.agg$prob.cooperate)

all.vars<-c(NameListCat, NameListCont)

### Correlation visualizations for continuous variables

corrgram (cont.vars)
chart.Correlation(cont.vars)

### Association visualizations from categorical variables

BoxPlotter<-function(var.pos) {
fill <- "gold1"
line <- "goldenrod2"
var.name<-colnames(cat.vars)[var.pos]
levels(cat.vars[,var.pos]) <- gsub(" ", "\n", levels(cat.vars[,var.pos]))
levels(cat.vars[,var.pos]) <- gsub("/", "/\n", levels(cat.vars[,var.pos]))
customboxplot <- ggplot(cat.vars, aes(cat.vars[, var.pos], merged.exp1.agg$prob.cooperate)) +
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
  formula.logit<-as.formula(paste("action~", vars, sep=""))
  logit.test <- glm(formula.logit, data = merged.exp1, family = "binomial")
  logit.multiwayvcov.test <- cluster.vcov(logit.test, cbind(merged.exp1$session, merged.exp1$pid))
  output<-coeftest(logit.test, logit.multiwayvcov.test)
  return(output)
}

#Loop over all variables. 
#It will throw an error ("non-conformable arrays") if there is only one session

logistic.coeffs.frame = list()
for (i in all.vars) {
  report.logistic<-logit.test(i)
  report.rows<-length(report.logistic[,1])
  logistic.coeffs.frame[[i]]<-as.data.frame(report.logistic[,1:4])[2:report.rows,]
}
report.logistic.all<-do.call(rbind, logistic.coeffs.frame)
report.logistic.all<-cbind(row.names(report.logistic.all), report.logistic.all)
write.csv(report.logistic.all, file = 'logit_coop_exp1.csv', row.names = FALSE, na = '')

