## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Formula
formula.h14.1<-as.formula("conformity~competition+(1|group)+(1|settingsNum)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glmer(formula.h14.1, data=factorialGroup, family=gaussian(link = "identity"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-4

# Declare priors
# Baseline priors based on DESIM
priorSD <- 0.075
# h14.0 priors (null): Intergroup Competition does not increase Conformity
h14.0 <- normal(location = c(0.0, 0.0, 0.0, rep(0, ndim)), 
                scale = c(priorSD, priorSD, priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h14.1 priors: Intergroup Competition increases Conformity
h14.1 <- normal(location = c(-0.15, 0.0, 0.15, rep(0, ndim)), 
                scale = c(priorSD, priorSD, priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_14.0 <- bayesLmer(formula.h14.1, h14.0, factorialGroup)
bridge_14.1 <- bayesLmer(formula.h14.1, h14.1, factorialGroup)

# Calculate BFs for all comparisons

test_1_0<-bf(bridge_14.1, bridge_14.0)$bf

# Save BFs

BFs <- data.frame(14, test_1_0)
colnames(BFs) <- c("Hypothesis", 
                   "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs14.csv", sep = '/'))                      