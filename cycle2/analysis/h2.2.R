## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Test of Hypothesis 2.2 and all associated predictions
# Exploratory analysis of frequencies

factorial2.2<-subset(factorial, tools == 1 | tools == 2)
h2.2.table<-prop.table (table(factorial2.2$innovation, factorial2.2$tools), 2)
h2.2.table<-h2.2.table[,1:2]
xx<-barplot(h2.2.table[2,], ylab=c("Innovation Rate"), xaxt='n', ylim=c(0,1), main="Innovation Rate for H2.2")
text(x = xx, y = h2.2.table[2,], label = round(h2.2.table[2,], 3), pos = 3, cex = 0.8, col = "red")
axis(1, at=xx, labels=c("Low risk","Medium risk"), tick=FALSE, las=2, line=-0.5)

# Set formula

h2.2.formula <- innovation~h2.2+h1.1+h1.3+h2.1+h3.1+h3.2+h3.3+h3.4+h3.5+(1|matchid)
h2.2.coef <- stan_glmer(h2.2.formula, data=factorial, chains = 1, iter = 100, family = binomial(link = "logit"))
ndim.2.2<-length(h2.2.coef$prior.info$prior$location)

# Manually set priors for all h2.2. predictions
# Assume SD = half of a medium effect

test.SD<-log.odds.large/3

# Null hypothesis: Group willingness to innovate will increase linearly with the expected value of the innovation, irrespective of uncertainty.

h2.2.null <- normal(location = rep(0, ndim.2.2),
                    scale = c(test.SD, 
                              rep(2.5,ndim.2.2-1)), 
                    autoscale = FALSE)

# Test hypothesis: If groups are allowed to communicate, they will take greater risks. 

h2.2.test <- normal(location = c(logodds$h2.2, rep(0, ndim.2.2-1)),
                    scale = c(test.SD, 
                              rep(2.5,ndim.2.2-1)), 
                    autoscale = FALSE)

# Estimate and save all models
glmm2.2.null<- stan_glmer(h2.2.formula, factorial, binomial(link = "logit"),
                         prior = h2.2.null, prior_intercept = weak_prior,
                         chains = 3, iter = nIter, diagnostic_file = "glmm2.2.null.csv")

glmm2.2.test<- stan_glmer(h2.2.formula, factorial, binomial(link = "logit"),
                         prior = h2.2.test, prior_intercept = weak_prior,
                         chains = 3, iter = nIter, diagnostic_file = "glmm2.2.test.csv")

# Estimate marginal likelihood

bridge_2.2.null <- bridge_sampler(glmm2.2.null)
bridge_2.2.test <- bridge_sampler(glmm2.2.test)

# Calculate BFs for all comparisons

testnull.2.2<-bf(bridge_2.2.test, bridge_2.2.null)$bf

# Store BFs

BFs2.2 <- data.frame(2.2, NA, NA, NA, testnull.2.2, NA, NA)
colnames(BFs2.2) <- c("Hypothesis", 
                      "Prediction 1 vs. Prediction 2", 
                      "Prediction 1 vs. Prediction 3", 
                      "Prediction 2 vs. Prediction 3", 
                      "Prediction 1 vs. Null", 
                      "Prediction 2 vs. Null", 
                      "Prediction 3 vs. Null")

Ivs2.2<-c("h2.11","h1.11","h1.12","h1.13","h1.31","h1.32",
              "h2.11","h3.11","h3.21","h3.32","h3.33","h3.34",
              "h3.41","h3.51","h3.52")
modelPlotter(glmm2.2.null, Ivs2.2)
modelPlotter(glmm2.2.test, Ivs2.2)
write.csv(BFs2.2, paste(od, "BFs2.2.csv", sep = '/'))
save (glmm2.2.null, file ="glmm2.2.null")
save (glmm2.2.test, file ="glmm2.2.test")
