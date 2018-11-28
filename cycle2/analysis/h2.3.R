
## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Test of Hypothesis 2.3 and all associated predictions
# Exploratory analysis of frequencies

factorial2.3<-subset(factorial, tools == 1 | tools == 3)
h2.3.table<-prop.table (table(factorial2.3$innovation, factorial2.3$tools), 2)
h2.3.table<-h2.3.table[,c(1,3)]
xx<-barplot(h2.3.table[2,], ylab=c("Innovation Rate"), xaxt='n', ylim=c(0,1), main="Innovation Rate for H2.3")
text(x = xx, y = h2.3.table[2,], label = round(h2.3.table[2,], 3), pos = 3, cex = 0.8, col = "red")
axis(1, at=xx, labels=c("Low risk","High risk"), tick=FALSE, las=2, line=-0.5)

# Set formula

h2.3.formula <- innovation~h2.3+h1.1+h1.3+h2.1+h3.1+h3.2+h3.3+h3.4+h3.5+(1|matchid)
h2.3.coef <- stan_glmer(h2.3.formula, data=factorial, chains = 1, iter = 100, family = binomial(link = "logit"))
ndim.2.3<-length(h2.3.coef$prior.info$prior$location)

# Manually set priors for all h2.3. predictions
# Assume SD = half of a medium effect

test.SD<-log.odds.large/3

# Null hypothesis: Group willingness to innovate will increase linearly with the expected value of the innovation, irrespective of uncertainty.

h2.3.null <- normal(location = rep(0, ndim.2.3),
                    scale = c(test.SD, 
                              rep(2.5,ndim.2.3-1)), 
                    autoscale = FALSE)

glmm2.3.null<- stan_glmer(h2.3.formula, factorial, binomial(link = "logit"),
                          prior = h2.3.null, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.3.null.csv")

bridge_2.3.null <- bridge_sampler(glmm2.3.null)

# Test hypothesis: Groups will be risk seeking for gains of low probability. 
# E.g. groups will prefer an innovative tactic offering 5% chance of winning 200 points, 
# than a sure tactic with 100% chance of winning 20 points or less. 

h2.3.test <- normal(location = c(logodds$h2.3, rep(0, ndim.2.3-1)),
                    scale = c(test.SD, 
                              rep(2.5,ndim.2.3-1)), 
                    autoscale = FALSE)

glmm2.3.test<- stan_glmer(h2.3.formula, factorial, binomial(link = "logit"),
                          prior = h2.3.test, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.3.test.csv")

bridge_2.3.test <- bridge_sampler(glmm2.3.test)

# Alternative hypothesis 1:  

h2.3.alt1.prior <- normal(location = -1.45, scale = test.SD, autoscale = FALSE)

glmm2.3.alt1<- stan_glmer(h2.3.formula, factorial, binomial(link = "logit"),
                          prior = h2.3.null, prior_intercept = h2.3.alt1.prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.3.alt1.csv")

bridge_2.3.alt1 <- bridge_sampler(glmm2.3.alt1)

# Alternative hypothesis 2: 

h2.3.alt2.prior <- normal(location = 1.45, scale = test.SD, autoscale = FALSE)

glmm2.3.alt2<- stan_glmer(h2.3.formula, factorial, binomial(link = "logit"),
                          prior = h2.3.null, prior_intercept = h2.3.alt2.prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.3.alt1.csv")

bridge_2.3.alt2 <- bridge_sampler(glmm2.3.alt2)

# Calculate BFs for all comparisons

testalt1.2.3<-bf(bridge_2.3.test, bridge_2.3.alt1)$bf
testalt2.2.3<-bf(bridge_2.3.test, bridge_2.3.alt2)$bf
alt1alt2.2.3<-bf(bridge_2.3.alt1, bridge_2.3.alt2)$bf
testnull.2.3<-bf(bridge_2.3.test, bridge_2.3.null)$bf
alt1null.2.3<-bf(bridge_2.3.alt1, bridge_2.3.null)$bf
alt2null.2.3<-bf(bridge_2.3.alt2, bridge_2.3.null)$bf

# Store BFs

BFs2.3 <- data.frame(2.3, testalt1.2.3, testalt2.2.3, alt1alt2.2.3, testnull.2.3, alt1null.2.3, alt2null.2.3)
colnames(BFs2.3) <- c("Hypothesis", 
                      "Prediction 1 vs. Prediction 2", 
                      "Prediction 1 vs. Prediction 3", 
                      "Prediction 2 vs. Prediction 3", 
                      "Prediction 1 vs. Null", 
                      "Prediction 2 vs. Null", 
                      "Prediction 3 vs. Null")

Ivs2.3<-c("(Intercept)","h2.31")
modelPlotter(glmm2.3.null, Ivs2.3)

