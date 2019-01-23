## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Test of Hypothesis 2.1 and all associated predictions
# Set priors for all h2.1. predictions

ndim.2.1 <- nCoef-7

# Assume SD = half of a small effect

test.SD<-log.odds.small/3

# Null hypothesis: Exogenous uncertainty (ignorance) does not affect matchid motivation to innovate.

h2.1.null <- normal(location = 0, 
                    scale = c(rep(2.5,5), test.SD, rep(2.5,ndim.2.1)), autoscale = FALSE)

# Test hypothesis: Increased exogenous uncertainty (ignorance) will enhance group motivation to innovate. 

h2.1.test <- normal(location = c(rep(0,5), log.odds.small, rep(0,ndim.2.1)), 
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

h2.1.alt1 <- normal(location = c(rep(0,ndim.2.1alt-1), effect.alt2.1), 
                    scale = c(rep(2.5,ndim.2.1alt-1), effect.alt2.1/3), autoscale = FALSE)

# Estimate and save all models

glmm2.1.null<- stan_glmer(main.formula, factorial, binomial(link = "logit"),
                          prior = h2.1.null, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.1.null.csv")

glmm2.1.test<- stan_glmer(main.formula, factorial, binomial(link = "logit"),
                          prior = h2.1.test, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.1.test.csv")

glmm2.1.alt1<- stan_glmer(h2.1alt.formula, factorial, binomial(link = "logit"),
                          prior = h2.1.alt1, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.1.alt1.csv")

# Estimate marginal likelihood

bridge_2.1.null <- bridge_sampler(glmm2.1.null)
bridge_2.1.test <- bridge_sampler(glmm2.1.test)
bridge_2.1.alt1 <- bridge_sampler(glmm2.1.alt1)

# Calculate BFs for all comparisons

testalt1.2.1<-bf(bridge_2.1.test, bridge_2.1.alt1)$bf
testnull.2.1<-bf(bridge_2.1.test, bridge_2.1.null)$bf
alt1null.2.1<-bf(bridge_2.1.alt1, bridge_2.1.null)$bf

# Store BFs

BFs2.1 <- data.frame(2.1, testalt1.2.1, NA, NA, testnull.2.1, alt1null.2.1, NA)
colnames(BFs2.1) <- c("Hypothesis", 
                   "Prediction 1 vs. Prediction 2", 
                   "Prediction 1 vs. Prediction 3", 
                   "Prediction 2 vs. Prediction 3", 
                   "Prediction 1 vs. Null", 
                   "Prediction 2 vs. Null", 
                   "Prediction 3 vs. Null")
write.csv(BFs2.1, paste(od, "BFs2.1.csv", sep = '/'))
save (glmm2.1.null, file ="glmm2.1.null")
save (glmm2.1.test, file ="glmm2.1.test")
save (glmm2.1.alt1, file ="glmm2.1.alt1")
