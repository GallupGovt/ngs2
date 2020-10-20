## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 50000

# Formula
formula.h9<-as.formula("inmot1~framing*pressure+tolerance+competition+timeUncertainty+tools+support+centralization+leaderWeight+density+(1|player)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glmer(formula.h9, data=factorial, family=binomial(link="logit"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-3

# Declare priors
# Baseline priors based on Cycle 2 data: log-odds = 0.6, SD = 0.3
priorSD <- 0.3
# h9.0 priors (null): TA does not moderate the effect of heuristic reasoning on individual motivation to innovate (T1): The availability heuristic will have the same effect on individuals with high TA than those with low TA. 
h9.0 <- normal(location = c(rep(0, ndim), 0, 0), scale = c(rep(2.5,ndim), priorSD, priorSD), autoscale=FALSE)
# h9.1 priors: TA moderates the effect of heuristic reasoning on individual motivation to innovate (T1): The availability heuristic will have a greater effect on individuals with high TA than those with low TA. 
h9.1 <- normal(location = c(rep(0, ndim), -0.6, 0.6), scale = c(rep(2.5,ndim), priorSD, priorSD), autoscale=FALSE)
# h9.2 priors: TA moderates the effect of heuristic reasoning on individual motivation to innovate (T1): The availability heuristic will have a smaller effect on individuals with high TA than those with low TA.
h9.2 <- normal(location = c(rep(0, ndim), 0.6, -0.6), scale = c(rep(2.5,ndim), priorSD, priorSD), autoscale=FALSE)

# Run models 

bridge_9.0 <- bayesGlmer(formula.h9, h9.0)
bridge_9.1 <- bayesGlmer(formula.h9, h9.1)
bridge_9.2 <- bayesGlmer(formula.h9, h9.2)

# Calculate BFs for all comparisons

test_1_2<-bf(bridge_9.1, bridge_9.2)$bf
test_1_0<-bf(bridge_9.1, bridge_9.0)$bf
test_2_0<-bf(bridge_9.2, bridge_9.0)$bf

# Save BFs

BFs <- data.frame(9, test_1_2, test_1_0, test_2_0)
colnames(BFs) <- c("Hypothesis", 
                    "Prediction 1 vs. Prediction 2", 
                    "Prediction 1 vs. Null", 
                    "Prediction 2 vs. Null")
write.csv(BFs, paste(od, "BFs9.csv", sep = '/'))                      

summarize_delete ("bayesGlmer_h9_h9.0")
summarize_delete ("bayesGlmer_h9_h9.1")
summarize_delete ("bayesGlmer_h9_h9.2")

# Render results into notebook
rmarkdown::render("NGS2_WITNESS_Cycle4_h9.rmd")
