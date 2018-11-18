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
nGames<-length(unique(factorial$matchid))

# Number of players connected
factorial$nConnected<-as.numeric(levels(factorial$nConnected))[factorial$nConnected]
nConnected<-aggregate(nConnected ~ matchid, data=factorial, mean)
nPlayers<-sum(nConnected$nConnected)

# Game dates
factorial$date.time<-as.Date(levels(factorial$date.time))[factorial$date.time]
dates<-aggregate(date.time ~ matchid, data=factorial, mean)

# Number of unique experimental conditions played
factorial$settingsNum<-as.numeric(levels(factorial$settingsNum))[factorial$settingsNum]
nConditions<-length(unique(factorial$settingsNum))

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

# Bayesian GLMM function

main.formula <- innovation~h1.1+h1.3+h2.1+h3.1+h3.2+h3.3+h3.4+h3.5+tools+(1|matchid)
weak_prior <- cauchy(0, 2.5)

bayesGlmer<-function(formula, priors) {
  fittedGlmer<- stan_glmer(formula,
                           data=factorial,
                           family = binomial(link = "logit"),
                           prior = priors,
                           prior_intercept = weak_prior,
                           chains = 3, iter = nIter,
                           diagnostic_file = "df1.csv")
  return(fittedGlmer)
}

glmmoverall <- bayesGlmer(main.formula, weak_prior)
glmmoverall
posterior <- as.array(glmmoverall)
posteriorAreas <- mcmc_areas(posterior, pars = c("h1.11","h1.12","h1.13","h1.31","h1.32",
                               "h2.11","h3.11","h3.21","h3.31","h3.32","h3.33",
                               "h3.41","h3.51","h3.52","tools2","tools3","tools4",
                               "tools5","tools6","tools7","tools8"), 
           prob = 0.8, # 80% intervals
           prob_outer = 0.99, # 99%
           point_est = "mean")
