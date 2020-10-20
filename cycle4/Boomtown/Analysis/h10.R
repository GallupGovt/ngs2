## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 50000

# Formula
formula.h10<-as.formula("inmot1~framing*risk+prb+timeUncertainty+pressure+tolerance+competition+support+centralization+leaderWeight+density+(1|player)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- suppressWarnings(stan_glmer(formula.h10, data=factorial, family=binomial(link="logit"), iter=3, chains=1))
ndim<-length(fittedGlmer$covmat[1,])-3

# Declare priors
# Baseline priors based on Cycle 2 data: log-odds = -0.3, 0.1, SD = 0.1
priorSD <- 0.1
# h10.0 priors (null): Uncertainty does not moderate  the effect of heuristic reasoning on individual motivation to innovate (T1): The availability heuristic will have the same effect on individuals for high uncertainty prospects than low uncertainty prospects. 
h10.0 <- normal(location = c(rep(0, ndim), 0, 0), scale = c(rep(2.5,ndim), priorSD, priorSD), autoscale=FALSE)
# h10.1 priors: Uncertainty moderates the effect of heuristic reasoning on individual motivation to innovate (T1): The availability heuristic will have a greater effect on individuals for high uncertainty prospects than low uncertainty prospects. 
h10.1 <- normal(location = c(rep(0, ndim), -0.3, 0.1), scale = c(rep(2.5,ndim), priorSD, priorSD), autoscale=FALSE)
# h10.2 priors: TA moderates the effect of heuristic reasoning on individual motivation to innovate (T1): The availability heuristic will have a smaller effect on individuals with high TA than those with low TA.
h10.2 <- normal(location = c(rep(0, ndim), 0.3, -0.1), scale = c(rep(2.5,ndim), priorSD, priorSD), autoscale=FALSE)

# Run models 

bridge_10.0 <- bayesGlmer(formula.h10, h10.0)
bridge_10.1 <- bayesGlmer(formula.h10, h10.1)
bridge_10.2 <- bayesGlmer(formula.h10, h10.2)

# Calculate BFs for all comparisons

test_1_2<-bf(bridge_10.1, bridge_10.2)$bf
test_1_0<-bf(bridge_10.1, bridge_10.0)$bf
test_2_0<-bf(bridge_10.2, bridge_10.0)$bf

# Save BFs

BFs <- data.frame(10, test_1_2, test_1_0, test_2_0)
colnames(BFs) <- c("Hypothesis", 
                    "Prediction 1 vs. Prediction 2", 
                    "Prediction 1 vs. Null", 
                    "Prediction 2 vs. Null")
write.csv(BFs, paste(od, "BFs10.csv", sep = '/'))                      

summarize_delete ("bayesGlmer_h10_h10.0")
summarize_delete ("bayesGlmer_h10_h10.1")
summarize_delete ("bayesGlmer_h10_h10.2")

# Render results into notebook
rmarkdown::render("NGS2_WITNESS_Cycle4_h10.rmd")
