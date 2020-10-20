## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 50000

# Formula
formula.h22<-as.formula("inmot2~inmot1+centralization+density+conformity+grmot1+support+framing+complexity+timeUncertainty+pressure+tolerance+competition+leaderWeight+(1|player)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glmer(formula.h22, data=factorial, family=binomial(link="logit"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-2

# Declare priors
# Baseline priors based on DESIM (split effect of 0.70 into two levels)
priorSD <- 0.5
# h22.0 priors (null): Individual motivation to innovate (T1) will not increase Individual motivation to innovate (T2)
h22.0 <- normal(location = c(0.0, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h22.1 priors: Individual motivation to innovate (T1) will increase Individual motivation to innovate (T2)
h22.1 <- normal(location = c(1.45, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_22.0 <- bayesGlmer(formula.h22, h22.0)
bridge_22.1 <- bayesGlmer(formula.h22, h22.1)

# Calculate BFs for all comparisons

test_1_0<-bf(bridge_22.1, bridge_22.0)$bf

# Save BFs

BFs <- data.frame(22, test_1_0)
colnames(BFs) <- c("Hypothesis",
                    "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs22.csv", sep = '/'))                      

summarize_delete ("bayesGlmer_h22_h22.0")
summarize_delete ("bayesGlmer_h22_h22.1")
