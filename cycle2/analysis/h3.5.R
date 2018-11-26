## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Test of Hypothesis 3.5 and all associated predictions
# Manually set priors for all h3.5. predictions
# Assume SD = half of a medium effect

test.SD<-log.odds.medium/2
nCoef3.5<-nCoef-15

# Null hypothesis: Group communications will not affect willingness to innovate

h3.5.null <- cauchy(location = 0,
                    scale = c(rep(2.5,12), 
                              rep(test.SD, 2), 
                              rep(2.5,nCoef3.5)), 
                    autoscale = FALSE)

# Test hypothesis: If groups are allowed to communicate, they will take greater risks. 

h3.5.test <- cauchy(location = c(rep(0, 12), 
                    0.91, 0.91, 
                    rep(0,nCoef3.5)),
                    scale = c(rep(2.5,12), 
                              rep(test.SD, 2), 
                              rep(2.5,nCoef3.5)), 
                    autoscale = FALSE)

# Alternative hypothesis: When status is perceived as illegitimate, low-status groups will be more willing to innovate.
# Legitimacy interacts with status (new formula required)

h3.5.alt1 <- cauchy(location = c(rep(0, 12), 
                                 0, 0.91, 
                                 rep(0,nCoef3.5)),
                    scale = c(rep(2.5,12), 
                              rep(test.SD, 2), 
                              rep(2.5,nCoef3.5)), 
                    autoscale = FALSE)


# Estimate and save all models

glmm3.5.null <- bayesGlmer(main.formula, h3.5.null)
glmm3.5.test <- bayesGlmer(main.formula, h3.5.test)
glmm3.5.alt1 <- bayesGlmer(main.formula, h3.5.alt1)

# Estimate marginal likelihood

bridge_3.5.null <- bridge_sampler(glmm3.5.null)
bridge_3.5.test <- bridge_sampler(glmm3.5.test)
bridge_3.5.alt1 <- bridge_sampler(glmm3.5.alt1)

# Calculate BFs for all comparisons

testalt1.3.5<-bf(bridge_3.5.test, bridge_3.5.alt1)$bf
testnull.3.5<-bf(bridge_3.5.test, bridge_3.5.null)$bf
alt1null.3.5<-bf(bridge_3.5.alt1, bridge_3.5.null)$bf

# Store BFs

BFs3.5 <- data.frame(3.5, testalt1.3.5, NA, NA, testnull.3.5, alt1null.3.5, NA)
colnames(BFs3.5) <- c("Hypothesis", 
                      "Prediction 1 vs. Prediction 2", 
                      "Prediction 1 vs. Prediction 3", 
                      "Prediction 2 vs. Prediction 3", 
                      "Prediction 1 vs. Null", 
                      "Prediction 2 vs. Null", 
                      "Prediction 3 vs. Null")

modelPlotter(glmm3.5.null, overallIvs)
modelPlotter(glmm3.5.test, overallIvs)
modelPlotter(glmm3.5.alt1, overallIvs)
