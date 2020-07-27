## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Formula
formula.h15.1<-as.formula("conformity~tolerance+competition+pressure+grmot1+framing+complexity+timeUncertainty+support+centralization+leaderWeight+density+(1|group)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glmer(formula.h15.1, data=factorialGroup, family=gaussian(link = "identity"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-2

# Declare priors
# Baseline priors based on DESIM
priorSD <- 0.05
# h15.0 priors (null): Tolerance of Ambiguity does not decrease Conformity. 
h15.0 <- normal(location = c(0.0, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h15.1 priors: Tolerance of Ambiguity decreases Conformity. 
h15.1 <- normal(location = c(-0.1, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_15.0 <- bayesLmer(formula.h15.1, h15.0, factorialGroup)
bridge_15.1 <- bayesLmer(formula.h15.1, h15.1, factorialGroup)

# Calculate BFs for all comparisons

test_1_0<-bf(bridge_15.1, bridge_15.0)$bf

# Save BFs

BFs <- data.frame(15, test_1_0)
colnames(BFs) <- c("Hypothesis", 
                   "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs15.csv", sep = '/'))                      
