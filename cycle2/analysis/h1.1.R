## Created by Pablo Diego Rosell, PhD, for Gallup inc. in October 2018
## Test of Hypothesis 1.1 and all associated predictions

# Set SD of predicted effect size to half of the predicted effect size (log.odds.large/2)
# Reflecting directional certainty (positive effect), but wide uncertainty on true effect size
test.SD<-log.odds.large/2

# Prediction 1
# Set priors according to prediction 1: No comp (ref), Lo comp (-1.45), Med comp (1.45), Hi comp (-1.45)
test_prior <- cauchy(location = c(logodds$h1.1.locomp,logodds$h1.1.medcomp,logodds$h1.1.hicomp,rep(0,21)), 
                     scale = c(test.SD,test.SD,test.SD,rep(2.5,21)))
# Update posterior model parameters via bayesGlmer function, based on priors for Prediction 1
glmm1.1.test<- bayesGlmer(main.formula, test_prior)
# Estimate marginal log likelihood of Prediction 1 given data via bridge_sampler
bridge_1.1.test <- bridge_sampler(glmm1.1.test)

# Prediction 2
# Set priors according to prediction 2: No comp (ref), Lo comp (0.00), Med comp (0.00), Hi comp (0.00)
null_prior <- cauchy(location = 0, 
                     scale = c(test.SD,test.SD,test.SD,rep(2.5,21)))

# Update posterior model parameters via bayesGlmer function, based on priors for Prediction 2
glmm1.1.null<- bayesGlmer(main.formula, null_prior)

# Estimate marginal log likelihood of Prediction 2 given data via bridge_sampler
bridge_1.1.null <- bridge_sampler(glmm1.1.null)

# Estimate Bayes Factors for the comparison of prediction 1 over prediction 2 
bf(bridge_1.1.test, bridge_1.1.null)
# A bf>10 is considered strong evidence in favor of prediction 1

# Prediction 3
# Set priors according to prediction 3: No comp (ref), Lo comp (-1.45), Med comp (0.00), Hi comp (1.45)
alt_prior <- cauchy(location = c(logodds$h1.1.locomp,0, logodds$h1.1.medcomp,rep(0,21)), 
                    scale = c(test.SD,test.SD,test.SD,rep(2.5,21)))

# Update posterior model parameters via bayesGlmer function, based on priors for Prediction 3
glmm1.1.alt<- bayesGlmer(main.formula, alt_prior)

# Estimate marginal log likelihood of Prediction 3 given data via bridge_sampler
bridge_1.1.alt <- bridge_sampler(glmm1.1.alt)

# Estimate Bayes Factors for the comparison of prediction 1 over prediction 3 
bf(bridge_1.1.test, bridge_1.1.alt)
# A bf>10 is considered strong evidence in favor of prediction 1
