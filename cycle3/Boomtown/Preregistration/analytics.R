## Created by Pablo Diego Rosell, PhD, for Gallup inc. in July 2019

## Game parameters


nPlayers<-7 # Typically 7 or 10

nBatches<-1 # Number of batches of data collection (192 games per batch, 1,344 players per game. Viable range = 1 to 4 batches)


## Stan parameters

weak_prior <- normal(0, 2.5)

nIter<-10000 # Iterations for MCMC sampler (Per standard recommendations. Numbers under 1,000 create some errors in simulations)



## Source modelling and plotting functions



source("functions.R")



## Simulate data

source("dataprep.R")


## Bayesian hypothesis testing (Individual-level outcomes)

# Baseline priors based on Cycle 2 data: log-odds = 0.5, SD = 0.2

priorSD <- 0.2

# h1.null priors: Competition has no effect on innovation

h1.null <- normal(location = c(0.00, 0.00, 0.00, rep(0, 18)), scale = c(rep(priorSD,3), rep(2.5,18)), autoscale=FALSE)

# h1.1 priors: Intergroup competition increases individual motivation to innovate

h1.1 <- normal(location = c(-0.50, 0.00, 0.50, rep(0, 18)), scale = c(rep(priorSD,3), rep(2.5,18)), autoscale=FALSE)

# Setup formulas for test examples

# h1 formulas: 

formula.h1.0<-as.formula("inmot1.null~competition+risk+complexity+tools+tolerance+support+centralization+leaderWeight+pressure+framing+density+timeUncertainty+(1|player)") #Formula h1.null

formula.h1.1<-as.formula("inmot1.1~competition+risk+complexity+tools+tolerance+support+centralization+leaderWeight+pressure+framing+density+timeUncertainty+(1|player)") #Formula h1.1


# Run models (Pre-run and saved)


bayesGlmer.1.1<- bayesGlmer(formula.h1.1, h1.1)

#bridge_1.1 <- bridge_sampler(bayesGlmer.1.1, silent=TRUE)



save (bayesGlmer.1.1, file ="bayesGlmer.1.1")

#save (bridge_1.1, file ="bridge_1.1")



bayesGlmer.1.0<- bayesGlmer(formula.h1.1, h1.null)

#bridge_1.0 <- bridge_sampler(bayesGlmer.1.0, silent=TRUE)



save (bayesGlmer.1.0, file ="bayesGlmer.1.0")

#save (bridge_1.0, file ="bridge_1.0")



## Bayesian hypothesis testing (Group-level outcomes)

## Baseline priors based on simulated data: log-odds = 0.5, SD = 0.2

priorSD <- 0.07

# h12.null priors: Competition has no effect on conformity

h12.null <- normal(location = c(0.00, 0.00, 0.00, rep(0, 18)), scale = c(rep(priorSD,3), rep(2.5,18)), autoscale=FALSE)

# h12.1 priors: Intergroup competition increases group conformity

h12.1 <- normal(location = c(-0.20, 0.00, 0.20, rep(0, 18)), scale = c(rep(priorSD,3), rep(2.5,18)), autoscale=FALSE)

# h12 formulas: 

formula.h12.0<-as.formula("conformity.null~competition+risk+complexity+tools+tolerance+support+centralization+leaderWeight+pressure+framing+density+timeUncertainty+(1|group)") #Formula h12.null

formula.h12.1<-as.formula("conformity.1~competition+risk+complexity+tools+tolerance+support+centralization+leaderWeight+pressure+framing+density+timeUncertainty+(1|group)") #Formula h12.1

# Bayesian hypothesis testing (Group-level outcomes)

# Run models

#bayeslmer.12.1<- bayeslmer(formula.h12.1, h12.1)

#bayeslmer.12.0<- bayeslmer(formula.h12.1, h12.null)



# Calculate Bayes Factors

#bridge_12.1 <- bridge_sampler(bayeslmer.12.1, silent=TRUE)

#bridge_12.0 <- bridge_sampler(bayeslmer.12.0, silent=TRUE)



#save (bayeslmer.12.0, file ="bayeslmer.12.0")

#save (bridge_12.0, file ="bridge_12.0")

#save (bayeslmer.12.1, file ="bayeslmer.12.1")

#save (bridge_12.1, file ="bridge_12.1")



## Quick frequentist models to test effects show up as intended

#summary(glmer(formula.h1.0, data = factorial, family="binomial"))

#summary(glmer(formula.h1.1, data = factorial, family="binomial"))

#summary(glmer(formula.h12.0, data = factorial))

#summary(glmer(formula.h12.1, data = factorial))


