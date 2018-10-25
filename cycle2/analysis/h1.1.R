## Created by Pablo Diego Rosell, PhD, for Gallup inc. in October 2018
# Test of Hypothesis 1.1 and all associated predictions

test.SD<-log.odds.large/2

test_prior <- cauchy(location = c(logodds$h1.1.locomp,logodds$h1.1.medcomp,logodds$h1.1.hicomp,rep(0,21)), 
                     scale = c(test.SD,test.SD,test.SD,rep(2.5,21)))
glmm1.1.test<- bayesGlmer(main.formula, test_prior)
bridge_1.1.test <- bridge_sampler(glmm1.1.test)

null_prior <- cauchy(location = 0, 
                     scale = c(test.SD,test.SD,test.SD,rep(2.5,21)))
glmm1.1.null<- bayesGlmer(main.formula, null_prior)
bridge_1.1.null <- bridge_sampler(glmm1.1.null)

bf(bridge_1.1.test, bridge_1.1.null)

alt_prior <- cauchy(location = c(logodds$h1.1.locomp,0, logodds$h1.1.medcomp,rep(0,21)), 
                    scale = c(test.SD,test.SD,test.SD,rep(2.5,21)))

glmm1.1.alt<- bayesGlmer(main.formula, alt_prior)
prior_summary(glmm1.1.alt)
bridge_1.1.alt <- bridge_sampler(glmm1.1.alt)

bf(bridge_1.1.test, bridge_1.1.alt)

