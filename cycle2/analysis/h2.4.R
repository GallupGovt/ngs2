## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Test of Hypothesis 2.4 and all associated predictions
# Exploratory analysis of frequencies

factorial2.4<-subset(factorial, tools == 1 | tools == 4)
h2.4.table<-prop.table (table(factorial2.4$innovation, factorial2.4$tools), 2)
h2.4.table<-h2.4.table[,c(1,4)]
xx<-barplot(h2.4.table[2,], ylab=c("Innovation Rate"), xaxt='n', ylim=c(0,1), main="Innovation Rate for H2.4")
text(x = xx, y = h2.4.table[2,], label = round(h2.4.table[2,], 3), pos = 3, cex = 0.8, col = "red")
axis(1, at=xx, labels=c("High prob","Low prob"), tick=FALSE, las=2, line=-0.5)

# Set formula

h2.4.formula <- innovation~h2.4+h1.1+h1.3+h2.1+h3.1+h3.2+h3.3+h3.4+h3.5+(1|matchid)
h2.4.coef <- stan_glmer(h2.4.formula, data=factorial, chains = 1, iter = 100, family = binomial(link = "logit"))
ndim.2.4<-length(h2.4.coef$prior.info$prior$location)

# Manually set priors for all h2.4. predictions
# Assume SD = half of a medium effect

test.SD<-log.odds.large/3

# Null hypothesis: Group willingness to innovate will increase linearly with the expected value of the innovation, irrespective of uncertainty.

h2.4.null <- normal(location = rep(0, ndim.2.4),
                    scale = c(test.SD, 
                              rep(2.5,ndim.2.4-1)), 
                    autoscale = FALSE)

glmm2.4.null<- stan_glmer(h2.4.formula, factorial, binomial(link = "logit"),
                          prior = h2.4.null, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.4.null.csv")

bridge_2.4.null <- bridge_sampler(glmm2.4.null)

# Test hypothesis: Groups will be risk seeking for gains of low probability. 
# E.g. groups will prefer an innovative tactic offering 5% chance of winning 200 points, 
# than a sure tactic with 100% chance of winning 20 points or less. 

h2.4.test <- normal(location = c(logodds$h2.4, rep(0, ndim.2.4-1)),
                    scale = c(test.SD, 
                              rep(2.5,ndim.2.4-1)), 
                    autoscale = FALSE)

glmm2.4.test<- stan_glmer(h2.4.formula, factorial, binomial(link = "logit"),
                          prior = h2.4.test, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.4.test.csv")

bridge_2.4.test <- bridge_sampler(glmm2.4.test)

# Alternative hypothesis 1:  

h2.4.alt1.prior <- normal(location = 1.45, scale = test.SD, autoscale = FALSE)

glmm2.4.alt1<- stan_glmer(h2.4.formula, factorial, binomial(link = "logit"),
                          prior = h2.4.null, prior_intercept = h2.4.alt1.prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.4.alt1.csv")

bridge_2.4.alt1 <- bridge_sampler(glmm2.4.alt1)

# Alternative hypothesis 2: 

h2.4.alt2.prior <- normal(location = -1.45, scale = test.SD, autoscale = FALSE)

glmm2.4.alt2<- stan_glmer(h2.4.formula, factorial, binomial(link = "logit"),
                          prior = h2.4.null, prior_intercept = h2.4.alt2.prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.4.alt1.csv")

bridge_2.4.alt2 <- bridge_sampler(glmm2.4.alt2)

# Calculate BFs for all comparisons

testalt1.2.4<-bf(bridge_2.4.test, bridge_2.4.alt1)$bf
testalt2.2.4<-bf(bridge_2.4.test, bridge_2.4.alt2)$bf
alt1alt2.2.4<-bf(bridge_2.4.alt1, bridge_2.4.alt2)$bf
testnull.2.4<-bf(bridge_2.4.test, bridge_2.4.null)$bf
alt1null.2.4<-bf(bridge_2.4.alt1, bridge_2.4.null)$bf
alt2null.2.4<-bf(bridge_2.4.alt2, bridge_2.4.null)$bf

# Store BFs

BFs2.4 <- data.frame(2.4, testalt1.2.4, testalt2.2.4, alt1alt2.2.4, testnull.2.4, alt1null.2.4, alt2null.2.4)
colnames(BFs2.4) <- c("Hypothesis", 
                      "Prediction 1 vs. Prediction 2", 
                      "Prediction 1 vs. Prediction 3", 
                      "Prediction 2 vs. Prediction 3", 
                      "Prediction 1 vs. Null", 
                      "Prediction 2 vs. Null", 
                      "Prediction 3 vs. Null")

Ivs2.4<-c("(Intercept)","h2.41")
modelPlotter(glmm2.4.null, Ivs2.4)
write.csv(BFs2.4, paste(od, "BFs2.4.csv", sep = '/'))
save (glmm2.4.null, file ="glmm2.4.null")
save (glmm2.4.test, file ="glmm2.4.test")
save (glmm2.4.alt1, file ="glmm2.4.alt1")
save (glmm2.4.alt2, file ="glmm2.4.alt2")
