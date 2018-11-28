## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Test of Hypothesis 3.1 and all associated predictions
# Manually set priors for all h3.1. predictions

ndim.3.1 <- nCoef-8
# Assume SD = half of a small effect

test.SD<-log.odds.medium/3

# Null hypothesis: Leader tolerance of ambiguity does not affect matchid motivation to innovate.

h3.1.null <- normal(location = 0, 
                    scale = c(rep(2.5,6), test.SD, rep(2.5,ndim.3.1)), autoscale = FALSE)

# Test hypothesis: Average levels of tolerance of ambiguity in a group will increase motivation to innovate.


h3.1.test <- normal(location = c(rep(0,6), log.odds.medium, rep(0,ndim.3.1)), 
                    scale = c(rep(2.5,6), test.SD, rep(2.5,ndim.3.1)), autoscale = FALSE)

# Alternative hypothesis: Average levels of tolerance of ambiguity in a group will decrease motivation to innovate for complex innovations
# TA interacts with tool complexity (new formula required)

factorial$complex <- 0
factorial$complex[factorial$tools==5 | factorial$tools==6 | factorial$tools==7 | factorial$tools==8]<-1

h3.1alt.formula <- innovation~h1.1+h1.3+h2.1+h3.1*complex+h3.2+
  h3.3+h3.4+h3.5+tools+(1|matchid)

coefficients.h3.1alt <- stan_glmer(h3.1alt.formula, data=factorial, family = binomial(link = "logit"), 
                                   chains = 1, iter = 100)

# Identify location of relevant coefficients for alternative formula

ndim.3.1alt <- length(coefficients.h3.1alt$prior.info$prior$location)

h3.1.alt1 <- normal(location = c(rep(0,ndim.3.1alt-1), log.odds.medium), 
                    scale = c(rep(2.5,ndim.3.1alt-1), test.SD), autoscale = FALSE)

# Estimate and save all models

glmm3.1.test<- stan_glmer(main.formula, factorial, binomial(link = "logit"),
                          prior = h3.1.test, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm3.1.test.csv")

glmm3.1.null<- stan_glmer(main.formula, factorial, binomial(link = "logit"),
                          prior = h3.1.null, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm3.1.null.csv")

glmm3.1.alt1<- stan_glmer(h3.1alt.formula, factorial, binomial(link = "logit"),
                          prior = h3.1.alt1, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm3.1.alt1.csv")

# Estimate marginal likelihood

bridge_3.1.null <- bridge_sampler(glmm3.1.null)
bridge_3.1.test <- bridge_sampler(glmm3.1.test)
bridge_3.1.alt1 <- bridge_sampler(glmm3.1.alt1)

# Calculate BFs for all comparisons

testalt1.3.1<-bf(bridge_3.1.test, bridge_3.1.alt1)$bf
testnull.3.1<-bf(bridge_3.1.test, bridge_3.1.null)$bf
alt1null.3.1<-bf(bridge_3.1.alt1, bridge_3.1.null)$bf

# Store BFs

BFs3.1 <- data.frame(3.1, testalt1.3.1, NA, NA, testnull.3.1, alt1null.3.1, NA)
colnames(BFs) <- c("Hypothesis", 
                   "Prediction 1 vs. Prediction 2", 
                   "Prediction 1 vs. Prediction 3", 
                   "Prediction 2 vs. Prediction 3", 
                   "Prediction 1 vs. Null", 
                   "Prediction 2 vs. Null", 
                   "Prediction 3 vs. Null")

