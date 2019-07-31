## Created by Pablo Diego Rosell, PhD, for Gallup inc. in July 2019
## Set system parameters

if (!require("pacman")) install.packages("pacman")
library ("pacman")
pacman::p_load(rstan, rstanarm, bridgesampling, blavaan, bayesplot, ggplot2,dplyr, lme4)
options(mc.cores = parallel::detectCores()) # Use multiple cores for Stan to speed up performance

## Source modelling and plotting functions

source("functions.R")

## Simulate data

source("dataprep.R")

# Setup formulas for test examples
## Baseline priors based on Cycle 2 data: log-odds = 0.5, SD = 0.2
priorSD <- 0.2
# h1.null priors: Competition has no effect on innovation
h1.null <- normal(location = c(0.00, 0.00, 0.00, rep(0, 17)), scale = c(rep(priorSD,3), rep(2.5,17)), autoscale=FALSE)
# h1.1 priors: Intergroup competition increases individual motivation to innovate
h1.1 <- normal(location = c(-0.50, 0.00, 0.50, rep(0, 17)), scale = c(rep(priorSD,3), rep(2.5,17)), autoscale=FALSE)
# h1 formulas: 
formula.h1.0<-as.formula("inmot1.null~competition+risk+complexity+tools+tolerance+support+centralization+leaderWeight+pressure+framing+density+(1|player)") #Formula h1.null
formula.h1.1<-as.formula("inmot1.1~competition+risk+complexity+tools+tolerance+support+centralization+leaderWeight+pressure+framing+density+(1|player)") #Formula h1.1

## Baseline priors based on simulated data: log-odds = 0.5, SD = 0.2
priorSD <- 0.03
# h12.null priors: Competition has no effect on conformity
h12.null <- normal(location = c(0.00, 0.00, 0.00, rep(0, 17)), scale = c(rep(priorSD,3), rep(2.5,17)), autoscale=FALSE)
# h12.1 priors: Intergroup competition increases group conformity
h12.1 <- normal(location = c(-0.20, 0.00, 0.20, rep(0, 17)), scale = c(rep(priorSD,3), rep(2.5,17)), autoscale=FALSE)
# h12 formulas: 
formula.h12.0<-as.formula("conformity.null~competition+risk+complexity+tools+tolerance+support+centralization+leaderWeight+pressure+framing+density+(1|group)") #Formula h12.null
formula.h12.1<-as.formula("conformity.1~competition+risk+complexity+tools+tolerance+support+centralization+leaderWeight+pressure+framing+density+(1|group)") #Formula h12.1
## Test effects show up as intended

#summary(glmer(formula.h1.0, data = factorial, family="binomial"))
#summary(glmer(formula.h1.1, data = factorial, family="binomial"))
#summary(glmer(formula.h12.0, data = factorial))
#summary(glmer(formula.h12.1, data = factorial))

## Stan parameters

weak_prior <- normal(0, 2.5)
nIter<-10000 # Iterations for MCMC sampler (Per standard recommendations. Numbers under 1,000 create some errors in simulations)

## Bayesian Power (simulate BFs for null and true scenarios)
# Long runtime (~48 hours locally), so commented out 
#source ("Bayes_power.R")

# Bayesian hypothesis testing

#source("h1.R")

