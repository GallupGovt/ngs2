## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Test of Hypothesis 2.5 and all associated predictions
# Exploratory analysis of frequencies

factorial2.5<-subset(factorial, tools == 5 | tools == 6)
h2.5.table<-prop.table (table(factorial2.5$innovation, factorial2.5$tools), 2)
h2.5.table<-h2.5.table[,c(5,6)]
xx<-barplot(h2.5.table[2,], ylab=c("Innovation Rate"), xaxt='n', ylim=c(0,1), main="Innovation Rate for H2.5")
text(x = xx, y = h2.5.table[2,], label = round(h2.5.table[2,], 3), pos = 3, cex = 0.8, col = "red")
axis(1, at=xx, labels=c("High expectancy","Low expectancy"), tick=FALSE, las=2, line=-0.5)

# Set formula

h2.5.formula <- innovation~h2.5+h1.1+h2.1+h3.1+h3.2+h3.3+h3.4+(1|matchid)
h2.5.coef <- stan_glmer(h2.5.formula, data=factorial, chains = 1, iter = 100, family = binomial(link = "logit"))
ndim.2.5<-length(h2.5.coef$prior.info$prior$location)

# Manually set priors for all h2.5. predictions
# Assume SD = half of a medium effect

test.SD<-log.odds.large/3

# Null hypothesis: Group willingness to innovate will increase linearly with the expected value of the innovation, irrespective of uncertainty.

h2.5.null <- normal(location = rep(0, ndim.2.5),
                    scale = c(test.SD, 
                              rep(2.5,ndim.2.5-1)), 
                    autoscale = FALSE)

glmm2.5.null<- stan_glmer(h2.5.formula, factorial, binomial(link = "logit"),
                          prior = h2.5.null, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.5.null.csv")

bridge_2.5.null <- bridge_sampler(glmm2.5.null)

# Test hypothesis: Groups will be risk seeking for gains of low probability. 
# E.g. groups will prefer an innovative tactic offering 5% chance of winning 200 points, 
# than a sure tactic with 100% chance of winning 20 points or less. 

h2.5.test <- normal(location = c(logodds$h2.5, rep(0, ndim.2.5-1)),
                    scale = c(test.SD, 
                              rep(2.5,ndim.2.5-1)), 
                    autoscale = FALSE)

glmm2.5.test<- stan_glmer(h2.5.formula, factorial, binomial(link = "logit"),
                          prior = h2.5.test, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm2.5.test.csv")

bridge_2.5.test <- bridge_sampler(glmm2.5.test)

# Calculate BFs for all comparisons

testnull.2.5<-bf(bridge_2.5.test, bridge_2.5.null)$bf

# Store BFs

BFs2.5 <- data.frame(2.5, testnull.2.5, NA, NA, testnull.2.5, NA, NA)
colnames(BFs2.5) <- c("Hypothesis", 
                      "Prediction 1 vs. Prediction 2", 
                      "Prediction 1 vs. Prediction 3", 
                      "Prediction 2 vs. Prediction 3", 
                      "Prediction 1 vs. Null", 
                      "Prediction 2 vs. Null", 
                      "Prediction 3 vs. Null")
