## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Test of Hypothesis 3.4 and all associated predictions
# Manually set priors for all h3.4. predictions
# Assume SD = half of a medium effect

test.SD<-log.odds.large/3

# Preliminary analysis: 
# We anticipate that groups assigned to the low competition condition will develop high Collective Efficacy (CE), 
# whereas groups assigned to the high competition condition will develop low CE

factorial$CSEnum<-scale(as.numeric(as.character(factorial$CSE)))
h3.4.table<-aggregate(factorial$CSEnum, list(factorial$h1.1), mean, na.rm=T)
xx<-barplot(h3.4.table[,2], ylab=c("Average CSE Score"), xaxt='n', ylim=c(-0.5,1), main="Collective Self-efficacy by Competition Levels")
text(x = xx, y = h3.4.table[,2], label = round(h3.4.table[,2], 3), pos = 3, cex = 0.8, col = "red")
axis(1, at=xx, labels=c("None","Low", "Medium", "High"), tick=FALSE, las=2, line=-0.5)

# Test manipulation in multivariate

#h3.4CSE.formula <- CSEnum~h1.1+h1.3+h2.1+h3.1+h3.2+h3.3+h3.4+h3.5+tools+(1|matchid)
#glmm3.4.CSE <- stan_glmer(h3.4CSE.formula, data=factorial, family = gaussian, chains = 1, iter = 100)

#ndim.3.4CSE <- length(glmm3.4.CSE$prior.info$prior$location)
#CSE.SD<-0.7/2

#h3.4.CSEnull <- cauchy(location = rep(0,ndim.3.4CSE),
#                    scale = c(CSE.SD, 
#                              rep(2.5, ndim.3.4CSE-1)), 
#                    autoscale = FALSE)
#h3.4.CSEtest <- cauchy(location = rep(0.7,ndim.3.4CSE),
#                       scale = c(CSE.SD, 
#                                 rep(2.5, ndim.3.4CSE-1)), 
#                       autoscale = FALSE)

# Estimate and save test vs. null models

#CSE3.4.null<- stan_glmer(h3.4CSE.formula,
#                           data=factorial,
#                           family = gaussian,
#                           prior = h3.4.CSEnull,
#                           prior_intercept = weak_prior,
#                           chains = 2, iter = 1000,
#                           diagnostic_file = "df1.csv")

# Estimate marginal likelihood and Bayes factor
#bridge_3.4.CSEnull <- bridge_sampler(CSE3.4.null)
#bridge_3.4.CSEtest <- bridge_sampler(CSE3.4.test)
#testnull.CSE3.4<-bf(bridge_3.4.CSEtest, bridge_3.4.CSEnull)$bf
#testnull.CSE3.4

# Get formula to test all hypotheses

h3.4.formula <- innovation~h1.1*h3.4+h1.3+h2.1+h3.2+h3.3+h3.4+h3.5+tools+(1|matchid)
h3.4.coef <- stan_glmer(h3.4.formula, data=factorial, chains = 1, iter = 100, family = binomial(link = "logit"))
ndim.3.4<-length(h3.4.coef$prior.info$prior$location)

# Null hypothesis: Collective Efficacy will not affect willingness to innovate

h3.4.null <- normal(location = rep(0, ndim.3.4),
                    scale = c(rep(test.SD, 4), 
                              rep(2.5,ndim.3.4-7), 
                              rep(test.SD, 3)), 
                    autoscale = FALSE)

# Test hypothesis: Groups with high collective efficacy will be more willing to innovate.

h3.4.test <- normal(location = c(1.45, 0, -1.45, 0,
                                 rep(0,ndim.3.4-4)),
                    scale = c(rep(test.SD, 4), 
                              rep(2.5,ndim.3.4-7), 
                              rep(test.SD,3)), 
                    autoscale = FALSE)

# Alternative hypothesis: Groups with transformative leaders who foster high collective self-efficacy will be more willing to innovate
# Competition interacts with leadership style (new formula required)

h3.4.alt1 <- normal(location = c(1.45, 0, -1.45, 0,
                                 rep(0,ndim.3.4-7),
                                 1.45, 0, 0),
                    scale = c(rep(test.SD, 4), 
                              rep(2.5,ndim.3.4-7), 
                              rep(test.SD,3)),
                    autoscale = FALSE)

# Estimate and save all models

glmm3.4.null<- stan_glmer(h3.4.formula, factorial, binomial(link = "logit"),
                          prior = h3.4.null, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm3.4.null.csv")

glmm3.4.test<- stan_glmer(h3.4.formula, factorial, binomial(link = "logit"),
                          prior = h3.4.test, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm3.4.test.csv")

glmm3.4.alt1<- stan_glmer(h3.4.formula, factorial, binomial(link = "logit"),
                          prior = h3.4.alt1, prior_intercept = weak_prior,
                          chains = 3, iter = nIter, diagnostic_file = "glmm3.4.alt1.csv")

# Estimate marginal likelihood

bridge_3.4.null <- bridge_sampler(glmm3.4.null)
bridge_3.4.test <- bridge_sampler(glmm3.4.test)
bridge_3.4.alt1 <- bridge_sampler(glmm3.4.alt1)

# Calculate BFs for all comparisons

testalt1.3.4<-bf(bridge_3.4.test, bridge_3.4.alt1)$bf
testnull.3.4<-bf(bridge_3.4.test, bridge_3.4.null)$bf
alt1null.3.4<-bf(bridge_3.4.alt1, bridge_3.4.null)$bf

# Store BFs

BFs3.4 <- data.frame(3.4, testalt1.3.4, NA, NA, testnull.3.4, alt1null.3.4, NA)
colnames(BFs3.4) <- c("Hypothesis", 
                      "Prediction 1 vs. Prediction 2", 
                      "Prediction 1 vs. Prediction 3", 
                      "Prediction 2 vs. Prediction 3", 
                      "Prediction 1 vs. Null", 
                      "Prediction 2 vs. Null", 
                      "Prediction 3 vs. Null")
Ivs3.4<-c("h1.11","h1.12","h1.13","h3.41","h1.11:h3.41", "h1.12:h3.41", "h1.13:h3.41")
modelPlotter(glmm3.4.null, Ivs3.4)
modelPlotter(glmm3.4.test, Ivs3.4)
modelPlotter(glmm3.4.alt1, Ivs3.4)
