## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Test of Hypothesis 3.2 and all associated predictions
# Manually set priors for all h3.2. predictions

ndim.3.2 <- nCoef-9
# Assume SD = half of a small effect

test.SD<-log.odds.large/3

# Null hypothesis: Leader tolerance of ambiguity does not affect matchid motivation to innovate.

h3.2.null <- normal(location = 0, 
                    scale = c(rep(2.5,7), test.SD, rep(2.5,ndim.3.2)), autoscale = FALSE)

# Test hypothesis: Average levels of tolerance of ambiguity in a group will increase motivation to innovate.


h3.2.test <- normal(location = c(rep(0,7), log.odds.large, rep(0,ndim.3.2)), 
                    scale = c(rep(2.5,7), test.SD, rep(2.5,ndim.3.2)), autoscale = FALSE)

# Alternative hypothesis: Groups with leaders high in TA will be less willing to innovate as the game progresses.
# Leader TA interacts with round number (new formula required)

factorial$roundCent <- (factorial$round-7)

h3.2alt.formula <- innovation~h1.1+h1.3+h2.1+h3.1+h3.2*roundCent+
  h3.3+h3.4+h3.5+tools+(1|matchid)

coefficients.h3.2alt <- stan_glmer(h3.2alt.formula, data=factorial, family = binomial(link = "logit"), 
                                   chains = 1, iter = 100)

# Identify location of relevant coefficients for alternative formula

ndim.3.2alt <- length(coefficients.h3.2alt$prior.info$prior$location)

h3.2.alt1 <- normal(location = c(rep(0,ndim.3.2alt-1), log.odds.large), 
                    scale = c(rep(2.5,ndim.3.2alt-1), test.SD), autoscale = FALSE)

# Estimate and save all models

glmm3.2.test<- stan_glmer(main.formula, factorial, binomial(link = "logit"),
                          prior = h3.2.test, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm3.2.test.csv")

glmm3.2.null<- stan_glmer(main.formula, factorial, binomial(link = "logit"),
                          prior = h3.2.null, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm3.2.null.csv")

glmm3.2.alt1<- stan_glmer(h3.2alt.formula, factorial, binomial(link = "logit"),
                          prior = h3.2.alt1, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm3.2.alt1.csv")

# Estimate marginal likelihood

bridge_3.2.null <- bridge_sampler(glmm3.2.null)
bridge_3.2.test <- bridge_sampler(glmm3.2.test)
bridge_3.2.alt1 <- bridge_sampler(glmm3.2.alt1)

# Calculate BFs for all comparisons

testalt1.3.2<-bf(bridge_3.2.test, bridge_3.2.alt1)$bf
testnull.3.2<-bf(bridge_3.2.test, bridge_3.2.null)$bf
alt1null.3.2<-bf(bridge_3.2.alt1, bridge_3.2.null)$bf

# Store BFs

BFs3.2 <- data.frame(3.2, testalt1.3.2, NA, NA, testnull.3.2, alt1null.3.2, NA)
colnames(BFs3.2) <- c("Hypothesis", 
                   "Prediction 1 vs. Prediction 2", 
                   "Prediction 1 vs. Prediction 3", 
                   "Prediction 2 vs. Prediction 3", 
                   "Prediction 1 vs. Null", 
                   "Prediction 2 vs. Null", 
                   "Prediction 3 vs. Null")

