## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Test of Hypothesis 2.1 and all associated predictions
# Set priors for all h2.1. predictions

ndim.2.1 <- nCoef-7

# Assume SD = half of a small effect

test.SD<-log.odds.small/2

# Null hypothesis: Exogenous uncertainty (ignorance) does not affect matchid motivation to innovate.

h2.1.null <- cauchy(location = 0, 
                    scale = c(rep(2.5,5), test.SD, rep(2.5,ndim.2.1)), autoscale = FALSE)

# Test hypothesis: Increased exogenous uncertainty (ignorance) will enhance group motivation to innovate. 

h2.1.test <- cauchy(location = c(rep(0,5), log.odds.small, rep(0,ndim.2.1)), 
                    scale = c(rep(2.5,5), test.SD, rep(2.5,ndim.2.1)), autoscale = FALSE)

# Alternative hypothesis: Exogenous uncertainty (ignorance) will decrease motivation to innovate in the early stages of the game.
# Uncertainty interacts with round number (new formula required, need to center round)

factorial$roundCent <- (factorial$round-7)

h2.1alt.formula <- innovation~h1.1+h1.3+h2.1*roundCent+h3.1+h3.2+
  h3.3+h3.4+h3.5+tools+(1|matchid)

coefficients.h2.1alt <- stan_glmer(h2.1alt.formula, data=factorial, family = binomial(link = "logit"), 
                                   chains = 1, iter = 100)

# Identify location of relevant coefficients for alternative formula

ndim.2.1alt <- length(coefficients.h2.1alt$prior.info$prior$location)

effect.alt2.1<- log(exp(log.odds.small^1/13))

h2.1.alt1 <- cauchy(location = c(rep(0,ndim.2.1alt-1), effect.alt2.1), 
                    scale = c(rep(2.5,ndim.2.1alt-1), effect.alt2.1/2), autoscale = FALSE)

# Estimate and save all models

glmm2.1.null<- bayesGlmer(main.formula, h2.1.null)
glmm2.1.test<- bayesGlmer(main.formula, h2.1.test)
glmm2.1.alt1<-bayesGlmer(h2.1alt.formula, h2.1.alt1)

# Estimate marginal likelihood

bridge_2.1.null <- bridge_sampler(glmm2.1.null)
bridge_2.1.test <- bridge_sampler(glmm2.1.test)
bridge_2.1.alt1 <- bridge_sampler(glmm2.1.alt1)

# Calculate BFs for all comparisons

testalt1.2.1<-bf(bridge_2.1.test, bridge_2.1.alt1)$bf
testnull.2.1<-bf(bridge_2.1.test, bridge_2.1.null)$bf
alt1null.2.1<-bf(bridge_2.1.alt1, bridge_2.1.null)$bf

