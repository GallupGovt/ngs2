## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

rm(list = ls(all = TRUE))
set.seed(12345)

# Small, medium, large per Cohen's (1987), and Polanin & Snilstveit (2016)

log.odds.small<-0.2/(sqrt(3)/pi)
log.odds.medium<-0.5/(sqrt(3)/pi)
log.odds.large<-0.8/(sqrt(3)/pi)

# Read in data
if('gamesData.csv' %in% list.files(paste(od, sep = '/'))) {
    factorial <- read.csv(file = paste(od, "gamesData.csv", sep = '/'))
} else {
    stop('WARNING - Metadata is missing; download by hand and merge to continue.')
}
factorial<-data.frame(lapply(factorial, factor))

# Controls
# When offered choice given by tools == 9, should always choose "TNTbarrel"
table (subset(factorial, tools == 9)$leaderChoice)
# When offered choice given by tools == 10, 11, or 12, should always choose "SatchelCharge"
table (subset(factorial, tools == 10)$leaderChoice)
table (subset(factorial, tools == 11)$leaderChoice)
table (subset(factorial, tools == 12)$leaderChoice)

# Bayesian modelling

main.formula <- innovation~h1.1+h1.3+h2.1+h3.1+h3.2+h3.3+h3.4+h3.5+tools+(1|matchid)

weak_prior <- cauchy(0, 2.5)

glmmoverall <- stan_glmer(main.formula, data=factorial, family = binomial(link = "logit"),
                          prior = weak_prior, prior_intercept = weak_prior,
                          diagnostic_file = "df1.csv")
glmmoverall
