## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019
## Bayesian hypothesis testing (Individual-level outcomes)

# Number of valid games

factorial$group <- factorial$matchid
nGames<-length(unique(factorial$group))

# Number of players connected

nPlayers<-length(unique(factorial$player))

# Number of unique experimental conditions played

#factorial$settingsNum<-as.numeric(levels(factorial$settingsNum))[factorial$settingsNum]
#nConditions<-length(unique(factorial$settingsNum))

