## Created by Pablo Diego Rosell, PhD, for Gallup inc. in June 2019

## Set system parameters

rm(list = ls(all = TRUE))
if (!require("pacman")) install.packages("pacman")
library ("pacman")
pacman::p_load(rstan, rstanarm, bridgesampling, blavaan, bayesplot, ggplot2,dplyr, RCurl)
options(mc.cores = parallel::detectCores()) # Use multiple cores for Stan (speeds up performance)

## Download scripts from Github

dlScripts <- function (scriptNames) {
  fileHolder <- getURL(paste(githubRepo, scriptNames, sep = "/"), ssl.verifypeer = FALSE)
  fileConn<-file(scriptNames)
  writeLines(fileHolder, fileConn)
  close(fileConn)
}
githubRepo <- "https://raw.githubusercontent.com/GallupGovt/ngs2/master/cycle3/Polycraft/preregistration"
scriptNames <- c("functions.R", "dataprep.R", "Bayes_power.R")
lapply(scriptNames, dlScripts)

## Source modelling and plotting functions

source("functions.R")

## Set parameters for Bayesian estimation

nIter<-10000 # Iterations for MCMC sampler (Per standard recommendations. Numbers under 1,000 create some errors in simulations)
## Baseline priors based on Cycle 2 data: log-odds = 0.5, SD = 0.2
priorSD <- 0.2
# h1.1.null priors: Competition has no effect on innovation
h1.1.null <- normal(location = rep(0,3), scale = rep(priorSD,3), autoscale=FALSE) 
# h1.1.1 priors: Innovation is inverse u-shaped on competition
h1.1.1 <- normal(location = c(-0.50, 0.50, -0.50), scale = rep(priorSD,3), autoscale=FALSE)

#Formula null
formula.null<-as.formula("innovate.null~h1.1.dummy+(1|group)")
#Formula h1.1.1
formula.h1.1.1<-as.formula("innovate.1.1.1~h1.1.dummy+(1|group)")

## Bayesian Power (simulate BFs for null and true scenarios)
# Long runtime (~48 hours locally), so commented out 
# source ("Bayes_power.R")

# Bayesian hypothesis testing

set.seed = 12345
nGroups<-200 # For a balanced design with 4 conditions, nGroups needs to be a multiple of 4
source("dataprep.R")
glmer.h1.1.1<- bayesGlmer(formula.h1.1.1, h1.1.1)
glmer.h1.1.null<- bayesGlmer(formula.h1.1.1, h1.1.null)

## Conduct basic diagnostics

# Examine if there are any Rhat values > 1.1 per Gelman and Rubin's recommendation. 

rhat <- summary(glmer.h1.1.null)[, "Rhat"]
rhat[rhat > 1.1]

rhat <- summary(glmer.h1.1.1)[, "Rhat"]
rhat[rhat > 1.1]

# Check for convergence in the traceplots

plot(glmer.h1.1.1, "trace", pars = c("(Intercept)",
                                     "h1.1.dummy1", 
                                     "h1.1.dummy2",
                                     "h1.1.dummy3"))
plot(glmer.h1.1.null, "trace", pars = c("(Intercept)",
                                        "h1.1.dummy1", 
                                        "h1.1.dummy2",
                                        "h1.1.dummy3"))

# Example hypothesis test using Bayes Factors: marginal log likelihood of Prediction 1 given data via bridge_sampler

bridge_1.1.1 <- bridge_sampler(glmer.h1.1.1)
bridge_1.1.null <- bridge_sampler(glmer.h1.1.null)
BFs1.1<-bf(bridge_1.1.1, bridge_1.1.null)

# Plot distributions of prior and posterior

testnull<-BFs1.1[1]
bayesPlotter_3curves (glmer.h1.1.1, -0.5, 0.5, -0.5, priorSD, 
                            "h1.1.dummy1", "h1.1.dummy2", "h1.1.dummy3", testnull)
bayesPlotter_3curves (glmer.h1.1.null, 0, 0, 0, priorSD, 
               "h1.1.dummy1", "h1.1.dummy2", "h1.1.dummy3", testnull)
