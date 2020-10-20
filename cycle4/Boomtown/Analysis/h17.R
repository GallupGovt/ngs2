## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 50000

# Formula
formula.h17<-as.formula("inmot2~support+framing+complexity+timeUncertainty+pressure+tolerance+competition+centralization+leaderWeight+density+(1|player)")

# Extract number of prior parameters ('ndim') to be declared
fittedGlmer <- stan_glmer(formula.h17, data=factorial, family=binomial(link="logit"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-2

# Declare priors
# Baseline priors based on DESIM
priorSD <- 0.4
# h17.0 priors (null): Group Support for Innovation will not increase Individual Motivation to Innovate (T2)
h17.0 <- normal(location = c(0.0, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h17.1 priors: Group Support for Innovation will increase Individual Motivation to Innovate (T2)
h17.1 <- normal(location = c(0.8, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 
bridge_17.0 <- bayesGlmer(formula.h17, h17.0)
bridge_17.1 <- bayesGlmer(formula.h17, h17.1)

# Calculate BFs for all comparisons
test_1_0<-bf(bridge_17.1, bridge_17.0)$bf

# Save BFs
BFs <- data.frame(17, test_1_0)
colnames(BFs) <- c("Hypothesis",
                    "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs17.csv", sep = '/'))                      

summarize_delete ("bayesGlmer_h17_h17.0")
summarize_delete ("bayesGlmer_h17_h17.1")
