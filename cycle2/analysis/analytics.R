## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Read in data
if('gamesData.csv' %in% list.files(paste(od, sep = '/'))) {
    factorial <- read.csv(file = paste(od, "gamesData.csv", sep = '/'))
} else {
    stop('WARNING - Metadata is missing; download by hand and merge to continue.')
}
factorial<-data.frame(lapply(factorial, factor))

# Number of valid games
length(unique(factorial$matchid))

# Number of players connected
factorial$nConnected<-as.numeric(levels(factorial$nConnected))[factorial$nConnected]
nConnected<-aggregate(nConnected ~ matchid, data=factorial, mean)
sum(nConnected$nConnected)

# Game dates
factorial$date.time<-as.Date(levels(factorial$date.time))[factorial$date.time]
dates<-aggregate(date.time ~ matchid, data=factorial, mean)
barplot(table(dates$date.time))

# Number of unique experimental conditions played
factorial$settingsNum<-as.numeric(levels(factorial$settingsNum))[factorial$settingsNum]
length(unique(factorial$settingsNum))

# List of conditions played
settingsNum<-aggregate(settingsNum ~ matchid, data=factorial, mean)
settingsNumTab<-as.data.frame(table(settingsNum$settingsNum))
colnames(settingsNumTab)<-c("settingsNum", "gamesPlayed")
write.csv(settingsNumTab, paste(od, "settingsNumTab.csv", sep = '/'))

# List of conditions pending
allConditions<-1:208
playedConditions<-unique(factorial$settingsNum)
pendingConditions<-setdiff(allConditions, playedConditions)
pendingConditions<-as.data.frame(pendingConditions)
colnames(pendingConditions)<-c("settingsNum")
write.csv(pendingConditions, paste(od, "pendingConditions.csv", sep = '/'))

# Positive Controls
# When offered choice given by tools == 9, should always choose "TNTbarrel"
table (subset(factorial, tools == 9)$leaderChoice)
# When offered choice given by tools == 10, 11, or 12, should always choose "SatchelCharge"
table (subset(factorial, tools == 10)$leaderChoice)
table (subset(factorial, tools == 11)$leaderChoice)
table (subset(factorial, tools == 12)$leaderChoice)

# Bayesian modelling

main.formula <- innovation~h1.1+h1.3+h2.1+h3.1+h3.2+h3.3+h3.4+h3.5+tools+(1|matchid)
weak_prior <- cauchy(0, 2.5)

# Bayesian GLMM function

bayesGlmer<-function(formula, priors) {
  fittedGlmer<- stan_glmer(formula,
                           data=factorial,
                           family = binomial(link = "logit"),
                           prior = priors,
                           prior_intercept = weak_prior,
                           chains = 3, iter = 10000,
                           diagnostic_file = "df1.csv")
  return(fittedGlmer)
}

glmmoverall <- bayesGlmer(main.formula, weak_prior)
glmmoverall
