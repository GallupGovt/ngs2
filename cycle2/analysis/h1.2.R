## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
# Test of Hypothesis 1.2 and all associated predictions

# Recode tools into specific test

factorial$h1.2[factorial$tools==1]<- 0
factorial$h1.2[factorial$tools==4]<- 1

# Recode competition variable

factorial$competition2[factorial$h1.1==0]<- 0
factorial$competition2[factorial$h1.1==1 | factorial$h1.1==2 |factorial$h1.1==3]<- 1

# Declare formula

formula.h1.2 <- innovation~competition2*h1.2+h1.3+h2.1+h3.1+h3.2+
  h3.3+h3.4+h3.5+(1|matchid)

# Identify location of relevant coefficients for main formula

coefficients <- stan_glmer(formula.h1.2, data=factorial, family = binomial(link = "logit"), 
                           chains = 1, iter = 100)
ndim.1.2 <- length(coefficients$prior.info$prior$location)
# set dimension placeholder

# Manually set priors for all h1.2. predictions
# Assume SD = one third of a large effect

test.SD<-log.odds.large/3
test.SDinter<-(log.odds.large*log.odds.large)/3

# Null hypothesis: Competition levels and prospects have 0 effect on risk-seeking behavior 

h1.2.null <- normal(location = rep(0,ndim.1.2), 
                    scale = c(0.01,0.01,rep(2.5,ndim.1.2-3), 0.01), autoscale = FALSE)

# Test hypothesis: Groups will be more risk-seeking in a competitive environment than in a non-competitive environment.  

h1.2.test <- normal(location = c(0, log.odds.large, rep(0,ndim.1.2-3), log.odds.large), 
                    scale = c(test.SD,test.SD,rep(2.5,ndim.1.2-3), test.SDinter), autoscale = FALSE)

# Alt hypothesis 1: Groups will be equally risk-seeking, irrespective of competition levels. 

h1.2.alt1 <- normal(location = c(0, log.odds.large, rep(0,ndim.1.2-3), 0), 
                    scale = c(test.SD,test.SD,rep(2.5,ndim.1.2-3), test.SDinter), autoscale = FALSE)

# Alt hypothesis 2: Group motivation to innovate will increase linearly with the expected value of the innovation, irrespective of competition levels.

h1.2.alt2 <- normal(location = c(0, 0, rep(0,ndim.1.2-3), 0), 
                    scale = c(test.SD,test.SD,rep(2.5,ndim.1.2-3), test.SDinter), autoscale = FALSE)

# Estimate and save all models

glmm1.2.null<- stan_glmer(formula.h1.2, factorial, binomial(link = "logit"),
                          prior = h1.2.null, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm1.2.null.csv")

glmm1.2.test<- stan_glmer(formula.h1.2, factorial, binomial(link = "logit"),
                          prior = h1.2.test, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm1.2.test.csv")

glmm1.2.alt1<- stan_glmer(formula.h1.2, factorial, binomial(link = "logit"),
                          prior = h1.2.alt1, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm1.2.alt1.csv")

glmm1.2.alt2<- stan_glmer(formula.h1.2, factorial, binomial(link = "logit"),
                          prior = h1.2.alt2, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm1.2.alt2.csv")

# Estimate marginal likelihood

bridge_1.2.null <- bridge_sampler(glmm1.2.null)
bridge_1.2.test <- bridge_sampler(glmm1.2.test)
bridge_1.2.alt1 <- bridge_sampler(glmm1.2.alt1)
bridge_1.2.alt2 <- bridge_sampler(glmm1.2.alt2)

# Calculate BFs for all comparisons

testalt1.1.2<-bf(bridge_1.2.test, bridge_1.2.alt1)$bf
testalt2.1.2<-bf(bridge_1.2.test, bridge_1.2.alt2)$bf
alt1alt2.1.2<-bf(bridge_1.2.alt1, bridge_1.2.alt2)$bf
testnull.1.2<-bf(bridge_1.2.test, bridge_1.2.null)$bf
alt1null.1.2<-bf(bridge_1.2.alt1, bridge_1.2.null)$bf
alt2null.1.2<-bf(bridge_1.2.alt2, bridge_1.2.null)$bf

BFs1.2 <- data.frame(1.2, testalt1.1.2, testalt2.1.2, alt1alt2.1.2, testnull.1.2, alt1null.1.2, alt2null.1.2)
colnames(BFs1.2) <- c("Hypothesis", 
                   "Prediction 1 vs. Prediction 2", 
                   "Prediction 1 vs. Prediction 3", 
                   "Prediction 2 vs. Prediction 3", 
                   "Prediction 1 vs. Null", 
                   "Prediction 2 vs. Null", 
                   "Prediction 3 vs. Null")
