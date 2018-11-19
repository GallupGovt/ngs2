## Created by Pablo Diego Rosell, PhD, for Gallup inc. in October 2018
## Test of Hypothesis 1.1 and all associated predictions

# Set SD of predicted effect size to half of the predicted effect size (log.odds.large/2)
# Reflecting directional certainty (positive effect), but wide uncertainty on true effect size
test.SD<-log.odds.large/2
nCoef1.1<-nCoef-4

# Prediction 1
# Set priors according to prediction 1: No comp (ref), Lo comp (-1.45), Med comp (1.45), Hi comp (-1.45)
test_prior <- cauchy(location = c(logodds$h1.1.locomp,logodds$h1.1.medcomp,logodds$h1.1.hicomp,rep(0,nCoef1.1)), 
                     scale = c(test.SD,test.SD,test.SD,rep(2.5,nCoef1.1)))
# Update posterior model parameters via bayesGlmer function, based on priors for Prediction 1
glmm1.1.test<- bayesGlmer(main.formula, test_prior)
# Estimate marginal log likelihood of Prediction 1 given data via bridge_sampler
bridge_1.1.test <- bridge_sampler(glmm1.1.test)

# Prediction 2
# Set priors according to prediction 2: No comp (ref), Lo comp (0.00), Med comp (0.00), Hi comp (0.00)
null_prior <- cauchy(location = 0, 
                     scale = c(test.SD,test.SD,test.SD,rep(2.5,nCoef1.1)))

# Update posterior model parameters via bayesGlmer function, based on priors for Prediction 2
glmm1.1.null<- bayesGlmer(main.formula, null_prior)

# Estimate marginal log likelihood of Prediction 2 given data via bridge_sampler
bridge_1.1.null <- bridge_sampler(glmm1.1.null)

# Estimate Bayes Factors for the comparison of prediction 1 over prediction 2 
testnull<-bf(bridge_1.1.test, bridge_1.1.null)
testnull
testnullBF<-testnull$bf
# A bf>10 is considered strong evidence in favor of prediction 1

# Prediction 3
# Set priors according to prediction 3: No comp (ref), Lo comp (-1.45), Med comp (0.00), Hi comp (1.45)
alt_prior <- cauchy(location = c(logodds$h1.1.locomp,0, logodds$h1.1.medcomp,rep(0,nCoef1.1)), 
                    scale = c(test.SD,test.SD,test.SD,rep(2.5,nCoef1.1)))

# Update posterior model parameters via bayesGlmer function, based on priors for Prediction 3
glmm1.1.alt<- bayesGlmer(main.formula, alt_prior)

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

# Plot the "test" vs. "null" BF
plotIters<-nIter*1.5
draws <- as.data.frame(glmm1.1.test)
a <- rcauchy(plotIters, location=logodds$h1.1.locomp, scale=test.SD)
b <- rcauchy(plotIters, location=logodds$h1.1.medcomp, scale=test.SD)
c <- rcauchy(plotIters, location=logodds$h1.1.hicomp, scale=test.SD)
d <- draws$h1.11
e <- draws$h1.12
f <- draws$h1.13
frame <- data.frame(value=c(a, b, c, d, e, f), 
                    Distribution=c(rep("Prior", plotIters*3),
                            rep("Posterior", plotIters*3)), 
                    Level=c(rep("Lo Comp", plotIters),
                               rep("Med Comp", plotIters),
                               rep("Hi Comp", plotIters), 
                               rep("Lo Comp", plotIters),
                               rep("Med Comp", plotIters),
                               rep("Hi Comp", plotIters)))

frame.posterior<-subset(frame, Distribution=="Posterior")

h1.1.post<-ggplot(frame.posterior, aes(value, fill=Level, linetype=Distribution)) + 
  geom_density(alpha=0.4) + 
  scale_x_continuous(limits = c(-5, 5)) + 
  scale_y_continuous(limits = c(0, 2))

h1.1.prior<-ggplot(frame, aes(value, fill=Level, linetype=Distribution)) + 
  geom_density(alpha=0.4) + 
  scale_x_continuous(limits = c(-5, 5)) + 
  scale_y_continuous(limits = c(0, 2)) +
  annotate("text", x=2, y=1.7, label = paste("H1.1.1 vs. H1.1.2 (null) BF = ", sprintf("%0.2f", testnullBF))) +
  geom_vline(xintercept = 0, linetype="dashed")
