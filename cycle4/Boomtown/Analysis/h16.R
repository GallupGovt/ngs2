## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 100000

# Formula
formula.h16.1<-as.formula("conformity~complexity+tolerance+competition+pressure+grmot1+framing+timeUncertainty+support+centralization+leaderWeight+density+(1|group)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- suppressWarnings(stan_glmer(formula.h16.1, data=factorialGroup, family=gaussian(link = "identity"), iter=3, chains=1, refresh=0))
ndim<-length(fittedGlmer$covmat[1,])-2

# Declare priors
# Baseline priors based on DESIM
priorSD <- 0.05
# h16.0 priors (null): Innovation Complexity does not decrease Conformity. 
h16.0 <- normal(location = c(0.0, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h16.1 priors: Innovation Complexity increases Conformity.  
h16.1 <- normal(location = c(0.1, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_16.0 <- bayesLmer(formula.h16.1, h16.0, factorialGroup)
bridge_16.1 <- bayesLmer(formula.h16.1, h16.1, factorialGroup)

# Calculate BFs for all comparisons

test_1_0<-bf(bridge_16.1, bridge_16.0)$bf

# Save BFs

BFs <- data.frame(16, test_1_0)
colnames(BFs) <- c("Hypothesis", 
                   "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs16.csv", sep = '/'))                      

summarize_delete ("bayesLmer_h16.1_h16.0")
summarize_delete ("bayesLmer_h16.1_h16.1")

# Render results into notebook
rmarkdown::render("NGS2_WITNESS_Cycle4_h16.rmd")
