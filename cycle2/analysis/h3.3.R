## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Test of Hypothesis 3.3 and all associated predictions
# Manually set priors for all h3.3. predictions
# Assume SD = half of a medium effect

test.SD<-log.odds.medium/2

# Null hypothesis: Group status will not affect willingness to innovate, irrespective of competition levels (new formula)

factorial$competition[factorial$h1.1==1] <- 0
factorial$competition[factorial$h1.1==2 |factorial$h1.1==3] <- 1

h3.3main.formula <- innovation~h1.3+h2.1+h3.1+h3.2+h3.3*competition+h3.4+h3.5+tools+(1|matchid)

coefficients.h3.3null <- stan_glmer(h3.3main.formula, data=factorial, family = binomial(link = "logit"), 
                                    chains = 1, iter = 100)
ndim.3.3null <- length(coefficients.h3.3null$prior.info$prior$location)

h3.3.null <- cauchy(location = 0,
                    scale = c(rep(2.5,5), 
                              rep(test.SD, 4), 
                              rep(2.5,ndim.3.3null-12), 
                              rep(test.SD, 3)), autoscale = FALSE)

# Test hypothesis: Higher-status groups will be less willing to innovate under low competition than under balanced or strong competition.
# Group status interacts with competition level (new formula required)

h3.3.test <- cauchy(location = c(rep(0,ndim.3.3null-3), 
                                 0, -0.91, -0.91), 
                    scale = c(rep(2.5,5), 
                              rep(test.SD, 4), 
                              rep(2.5,ndim.3.3null-12), 
                              rep(test.SD, 3)), 
                    autoscale = FALSE)

# Alternative hypothesis: When status is perceived as illegitimate, low-status groups will be more willing to innovate.
# Legitimacy interacts with status (new formula required)

h3.3.alt1 <- cauchy(location = c(rep(0,7), 
                                 1.45, rep(0,ndim.3.3null-8)), 
                    scale = c(rep(2.5,5), 
                              rep(test.SD, 4), 
                              rep(2.5,ndim.3.3null-12), 
                              rep(test.SD, 3)), 
                    autoscale = FALSE)

# Estimate and save all models
nIter<- 10000
glmm3.3.null <- bayesGlmer(h3.3main.formula, h3.3.null)
glmm3.3.test <- bayesGlmer(h3.3main.formula, h3.3.test)
glmm3.3.alt1 <- bayesGlmer(h3.3main.formula, h3.3.alt1)

# Estimate marginal likelihood

bridge_3.3.null <- bridge_sampler(glmm3.3.null)
bridge_3.3.test <- bridge_sampler(glmm3.3.test)
bridge_3.3.alt1 <- bridge_sampler(glmm3.3.alt1)

# Calculate BFs for all comparisons

testalt1.3.3<-bf(bridge_3.3.test, bridge_3.3.alt1)$bf
testnull.3.3<-bf(bridge_3.3.test, bridge_3.3.null)$bf
alt1null.3.3<-bf(bridge_3.3.alt1, bridge_3.3.null)$bf

# Store BFs

BFs3.3 <- data.frame(3.3, testalt1.3.3, NA, NA, testnull.3.3, alt1null.3.3, NA)
colnames(BFs3.3) <- c("Hypothesis", 
                   "Prediction 1 vs. Prediction 2", 
                   "Prediction 1 vs. Prediction 3", 
                   "Prediction 2 vs. Prediction 3", 
                   "Prediction 1 vs. Null", 
                   "Prediction 2 vs. Null", 
                   "Prediction 3 vs. Null")

h3.3.Ivs<-c("h3.32","h3.33","h3.34", "competition1", 
            "h3.32:competition1", "h3.33:competition1", "h3.34:competition1")
modelPlotter(glmm3.3.null, h3.3.Ivs)
modelPlotter(glmm3.3.test, h3.3.Ivs)
modelPlotter(glmm3.3.alt1, h3.3.Ivs)


