## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 200000

# Formula
formula.h7<-as.formula("inmot1~framing*compStrong+timeUncertainty+tools+tolerance+support+centralization+leaderWeight+pressure+density+(1|player)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- suppressWarnings(stan_glmer(formula.h7, data=factorial, family=binomial(link="logit"), iter=3, chains=1, refresh=0))
ndim<-length(fittedGlmer$covmat[1,])-3

# Declare priors
# Baseline priors based on Cycle 2 data: log-odds = 0.2, SD = 0.1
priorSD <- 0.1
# h7.0 priors (null): Perceived intergroup competition does not moderate the effect of heuristic reasoning on individual motivation to innovate (T1): The availability heuristic will have the same effect on individuals facing strong competition as those facing balanced or weak competition. 
h7.0 <- normal(location = c(rep(0, ndim), 0, 0), scale = c(rep(2.5,ndim), priorSD, priorSD), autoscale=FALSE)
# h7.1 priors: Perceived intergroup competition moderates the effect of heuristic reasoning on individual motivation to innovate (T1): The availability heuristic will have a greater effect on individuals facing strong competition than those facing balanced or weak competition. 
h7.1 <- normal(location = c(rep(0, ndim), -0.2, 0.2), scale = c(rep(2.5,ndim), priorSD, priorSD), autoscale=FALSE)
# h7.2 priors: Perceived intergroup competition moderates the effect of heuristic reasoning on individual motivation to innovate (T1): The availability heuristic will have a smaller effect on individuals facing strong competition than those facing balanced or weak competition.
h7.2 <- normal(location = c(rep(0, ndim), 0.2, -0.2), scale = c(rep(2.5,ndim), priorSD, priorSD), autoscale=FALSE)

# Run models 

bridge_7.0 <- bayesGlmer(formula.h7, h7.0)
bridge_7.1 <- bayesGlmer(formula.h7, h7.1)
bridge_7.2 <- bayesGlmer(formula.h7, h7.2)

# Calculate BFs for all comparisons

test_1_2<-bf(bridge_7.1, bridge_7.2)$bf
test_1_0<-bf(bridge_7.1, bridge_7.0)$bf
test_2_0<-bf(bridge_7.2, bridge_7.0)$bf

# Save BFs

BFs <- data.frame(7, test_1_2, test_1_0, test_2_0)
colnames(BFs) <- c("Hypothesis", 
                    "Prediction 1 vs. Prediction 2", 
                    "Prediction 1 vs. Null", 
                    "Prediction 2 vs. Null")
write.csv(BFs, paste(od, "BFs7.csv", sep = '/'))                      

summarize_delete ("bayesGlmer_h7_h7.0")
summarize_delete ("bayesGlmer_h7_h7.1")
summarize_delete ("bayesGlmer_h7_h7.2")

# Render results into notebook
rmarkdown::render("NGS2_WITNESS_Cycle4_h7.rmd")
