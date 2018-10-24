## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

rm(list = ls(all = TRUE))
set.seed(12345)

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
