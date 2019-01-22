
## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Test of Hypothesis 1.3 and all associated predictions
# Exploratory analysis of frequencies

h1.3.table<-prop.table (table(factorial$innovation, factorial$h1.3), 2)
xx<-barplot(h1.3.table[2,], ylab=c("Innovation Rate"), xaxt='n', ylim=c(0,1), main="Innovation Rate for H1.3")
text(x = xx, y = h1.3.table[2,], label = round(h1.3.table[2,], 3), pos = 3, cex = 0.8, col = "red")
axis(1, at=xx, labels=c("No framing","- framing", "+ framing"), tick=FALSE, las=2, line=-0.5)

# Set formula for h1.3. predictions

h1.3.formula <- innovation~h1.3*h2.1+h1.1+h3.1+h3.2+h3.3+h3.4+h3.5+tools+(1|matchid)
h1.3.coefs <- stan_glmer(h1.3.formula, factorial, binomial(link = "logit"), chains = 1, iter = 100)
ndim.1.3 <- length(h1.3.coefs $prior.info$prior$location)

# Manually set priors for all h1.3. predictions
# Assume SD = half of a medium effect

test.SD<-log.odds.large/3

# Null hypothesis: Information about a competitor's performance will not affect group motivation to innovate

h1.3.null <- normal(location = rep(0, ndim.1.3),
                    scale = c(test.SD, test.SD, 
                              rep(2.5,ndim.1.3-4), 
                              test.SD, test.SD), 
                    autoscale = FALSE)

glmm1.3.null<- stan_glmer(h1.3.formula, factorial, binomial(link = "logit"),
                          prior = h1.3.null, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm1.3.null.csv")

bridge_1.3.null <- bridge_sampler(glmm1.3.null)

# Test hypothesis: Groups will be risk seeking for gains of low probability. 
# E.g. groups will prefer an innovative tactic offering 5% chance of winning 200 points, 
# than a sure tactic with 100% chance of winning 20 points or less. 

h1.3.test <- normal(location = c(logodds$h1.3.negprime, 
                                 logodds$h1.3.posprime, 
                                 rep(0, ndim.1.3-2)),
                    scale = c(test.SD, test.SD, 
                              rep(2.5,ndim.1.3-4), 
                              test.SD, test.SD), 
                    autoscale = FALSE)

glmm1.3.test<- stan_glmer(h1.3.formula, factorial, binomial(link = "logit"),
                          prior = h1.3.test, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm1.3.test.csv")

bridge_1.3.test <- bridge_sampler(glmm1.3.test)

# Alternative hypothesis 1:  

h1.3.alt1 <- normal(location = c(rep(0, ndim.1.3-2), 
                                 logodds$h1.3.negprime, 
                                 logodds$h1.3.posprime),
                    scale = c(test.SD, test.SD, 
                              rep(2.5,ndim.1.3-4), 
                              test.SD, test.SD), 
                    autoscale = FALSE)

glmm1.3.alt1<- stan_glmer(h1.3.formula, factorial, binomial(link = "logit"),
                          prior = h1.3.alt1, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm1.3.alt1.csv")

bridge_1.3.alt1 <- bridge_sampler(glmm1.3.alt1)

# Calculate BFs for all comparisons

testalt1.1.3<-bf(bridge_1.3.test, bridge_1.3.alt1)$bf
testnull.1.3<-bf(bridge_1.3.test, bridge_1.3.null)$bf
alt1null.1.3<-bf(bridge_1.3.alt1, bridge_1.3.null)$bf

# Store BFs

BFs1.3 <- data.frame(1.3, testalt1.1.3, NA, NA, testnull.1.3, alt1null.1.3, NA)
colnames(BFs1.3) <- c("Hypothesis", 
                      "Prediction 1 vs. Prediction 2", 
                      "Prediction 1 vs. Prediction 3", 
                      "Prediction 2 vs. Prediction 3", 
                      "Prediction 1 vs. Null", 
                      "Prediction 2 vs. Null", 
                      "Prediction 3 vs. Null")

Ivs1.3<-c("h1.31", "h1.32", "h1.31:h2.11", "h1.32:h2.11")
modelPlotter(glmm1.3.null, Ivs1.3)
write.csv(BFs1.3, paste(od, "BFs1.3.csv", sep = '/'))                      
