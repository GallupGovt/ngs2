## Created by Pablo Diego Rosell, PhD, for Gallup inc. in October 2018
## Test of Hypothesis 1.1 and all associated predictions

# Set SD of predicted effect size to half of the predicted effect size (log.odds.large/2)
# Reflecting directional certainty (positive effect), but wide uncertainty on true effect size
test.SD<-log.odds.large/3
nCoef1.1<-nCoef-4

# Prediction 1
# Set priors according to prediction 1: No comp (ref), Lo comp (-1.45), Med comp (1.45), Hi comp (-1.45)
h1.1.test <- normal(location = c(logodds$h1.1.locomp,logodds$h1.1.medcomp,logodds$h1.1.hicomp,rep(0,nCoef1.1)), 
                     scale = c(test.SD,test.SD,test.SD,rep(2.5,nCoef1.1)), autoscale = FALSE)
# Update posterior model parameters via bayesGlmer function, based on priors for Prediction 1
glmm1.1.test<- stan_glmer(main.formula, factorial, binomial(link = "logit"),
                          prior = h1.1.test, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm1.1.test.csv")

# Estimate marginal log likelihood of Prediction 1 given data via bridge_sampler
bridge_1.1.test <- bridge_sampler(glmm1.1.test)

# Prediction 2
# Set priors according to prediction 2: No comp (ref), Lo comp (0.00), Med comp (0.00), Hi comp (0.00)
h1.1.null <- normal(location = 0, 
                     scale = c(test.SD,test.SD,test.SD,rep(2.5,nCoef1.1)), autoscale = FALSE)

# Update posterior model parameters via bayesGlmer function, based on priors for Prediction 2
glmm1.1.null<- stan_glmer(main.formula, factorial, binomial(link = "logit"),
                          prior = h1.1.null, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm1.1.null.csv")

# Estimate marginal log likelihood of Prediction 2 given data via bridge_sampler
bridge_1.1.null <- bridge_sampler(glmm1.1.null)

# Estimate Bayes Factors for the comparison of prediction 1 over prediction 2 
testnull<-bf(bridge_1.1.test, bridge_1.1.null)
testnull
testnullBF<-testnull$bf
# A bf>10 is considered strong evidence in favor of prediction 1

# Prediction 3
# Set priors according to prediction 3: No comp (ref), Lo comp (-1.45), Med comp (0.00), Hi comp (1.45)
h1.1.alt <- normal(location = c(logodds$h1.1.locomp,0, logodds$h1.1.medcomp,rep(0,nCoef1.1)), 
                    scale = c(test.SD,test.SD,test.SD,rep(2.5,nCoef1.1)), autoscale = FALSE)

# Update posterior model parameters via bayesGlmer function, based on priors for Prediction 3
glmm1.1.alt<- stan_glmer(main.formula, factorial, binomial(link = "logit"),
                          prior = h1.1.alt, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm1.1.alt.csv")

# Estimate marginal log likelihood of Prediction 3 given data via bridge_sampler
bridge_1.1.alt <- bridge_sampler(glmm1.1.alt)

# Estimate Bayes Factors for the comparison of prediction 1 over prediction 3
testalt<-bf(bridge_1.1.test, bridge_1.1.alt)
testaltBF<-testalt$bf

# Estimate Bayes Factors for the comparison of prediction 2 over prediction 3 
testalt2<-bf(bridge_1.1.null, bridge_1.1.alt)
testalt2BF<-testalt2$bf

# Estimate Bayes Factors for the comparison of prediction 3 over prediction 2 (Null)
testnull2<-bf(bridge_1.1.alt, bridge_1.1.null)
testnull2BF<-testnull2$bf

BFs1.1 <- data.frame(1.1, testnullBF, testaltBF, testalt2BF, testnullBF, c(NA), testnull2BF)
colnames(BFs1.1) <- c("Hypothesis", 
                   "Prediction 1 vs. Prediction 2", 
                   "Prediction 1 vs. Prediction 3", 
                   "Prediction 2 vs. Prediction 3", 
                   "Prediction 1 vs. Null", 
                   "Prediction 2 vs. Null", 
                   "Prediction 3 vs. Null")


