## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 100000

# Formula
formula.h20<-as.formula("inmot2~centralization+density+conformity+grmot1+support+framing+complexity+timeUncertainty+pressure+tolerance+competition+leaderWeight+(1|player)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- suppressWarnings(stan_glmer(formula.h20, data=factorial, family=binomial(link="logit"), iter=3, chains=1, refresh=0))
ndim<-length(fittedGlmer$covmat[1,])-3

# Declare priors
# Baseline priors based on DESIM (split effect of 0.70 into two levels)
priorSD <- 0.2
# h20.0 priors (null): Network Centralization will not decrease Individual motivation to innovate (T2)
h20.0 <- normal(location = c(0.0, 0.00, rep(0, ndim)), scale = c(priorSD, priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h20.1 priors: Network Centralization will decrease Individual motivation to innovate (T2)
h20.1 <- normal(location = c(-0.35, -0.70, rep(0, ndim)), scale = c(priorSD, priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_20.0 <- bayesGlmer(formula.h20, h20.0)
bridge_20.1 <- bayesGlmer(formula.h20, h20.1)

# Calculate BFs for all comparisons

test_1_0<-bf(bridge_20.1, bridge_20.0)$bf

# Save BFs

BFs <- data.frame(20, test_1_0)
colnames(BFs) <- c("Hypothesis",
                    "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs20.csv", sep = '/'))                      

summarize_delete ("bayesGlmer_h20_h20.0")
summarize_delete ("bayesGlmer_h20_h20.1")

# Render results into notebook
rmarkdown::render("NGS2_WITNESS_Cycle4_h20.rmd")
