## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 50000

# Formula
formula.h21<-as.formula("innovation~leaderWeight+grmot2+centralization+density+conformity+grmot1+support+framing+complexity+timeUncertainty+pressure+tolerance+competition+(1|group)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glmer(formula.h21, data=factorialGroup, family=binomial(link = "logit"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-2

# Declare priors
# Baseline priors based on DESIM (split effect of 0.70 into two levels)
priorSD <- 0.2
# h21.0 priors (null): Leader vote weight will not increase Group Motivation to Innovate (T2). 
h21.0 <- normal(location = c(0.0, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h21.1 priors: Leader vote weight will increase Group Motivation to Innovate (T2). 
h21.1 <- normal(location = c(0.4, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_21.0 <- bayesLmer(formula.h21, h21.0, factorialGroup)
bridge_21.1 <- bayesLmer(formula.h21, h21.1, factorialGroup)

# Calculate BFs for all comparisons

test_1_0<-bf(bridge_21.1, bridge_21.0)$bf

# Save BFs

BFs <- data.frame(21, test_1_0)
colnames(BFs) <- c("Hypothesis",
                    "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs21.csv", sep = '/'))                      

summarize_delete ("bayesLmer_h21_h21.0")
summarize_delete ("bayesLmer_h21_h21.1")
