## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Formula
formula.h13.1<-as.formula("conformity~pressure+grmot1+framing+complexity+timeUncertainty+tolerance+competition+support+centralization+leaderWeight+density+(1|group)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glmer(formula.h13.1, data=factorialGroup, family=gaussian(link = "identity"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-2

# Declare priors
# Baseline priors based on DESIM
priorSD <- 0.1
# h13.0 priors (null): Time Pressure does not increase conformity.
h13.0 <- normal(location = c(0.0, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h13.1 priors: Time Pressure increases conformity.
h13.1 <- normal(location = c(0.2, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_13.0 <- bayesLmer(formula.h13.1, h13.0, factorialGroup)
bridge_13.1 <- bayesLmer(formula.h13.1, h13.1, factorialGroup)

# Calculate BFs for all comparisons

test_1_0<-bf(bridge_13.1, bridge_13.0)$bf

# Save BFs

BFs <- data.frame(13, test_1_0)
colnames(BFs) <- c("Hypothesis", 
                   "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs13.csv", sep = '/'))                      
