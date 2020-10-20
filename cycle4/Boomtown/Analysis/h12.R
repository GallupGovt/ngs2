## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 30000

# Formula
formula.h12.1<-as.formula("conformity~grmot1+framing+complexity+timeUncertainty+pressure+tolerance+competition+support+centralization+leaderWeight+density+(1|group)")
formula.h12.2<-as.formula("conformity~unanimous+framing+complexity+timeUncertainty+pressure+tolerance+competition+support+centralization+leaderWeight+density+(1|group)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glmer(formula.h12.2, data=factorialGroup, family=gaussian(link = "identity"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-2

# Declare priors
# Baseline priors based on literature and simulations: log-odds = 0.05, SD = 0.025
priorSD <- 0.025
# h12.0 priors (null): Group Aggregate Motivation to Innovate (T1) will not increase Conformity.
h12.0 <- normal(location = c(0.0, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h12.1 priors: Group Aggregate Motivation to Innovate (T1) will increase Conformity.
h12.1 <- normal(location = c(0.05, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h12.2 priors: Group Aggregate Motivation to Innovate (T1) will increase Conformity when the group is unanimous. 
h12.2 <- normal(location = c(0.3, rep(0, ndim)), scale = c(0.1, rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_12.1null <- bayesLmer(formula.h12.1, h12.0, factorialGroup)
bridge_12.1 <- bayesLmer(formula.h12.1, h12.1, factorialGroup)
bridge_12.2null <- bayesLmer(formula.h12.2, h12.0, factorialGroup)
bridge_12.2 <- bayesLmer(formula.h12.2, h12.2, factorialGroup)

# Calculate BFs for all comparisons

test_1_2<-bf(bridge_12.1, bridge_12.2)$bf
test_1_0<-bf(bridge_12.1, bridge_12.1null)$bf
test_2_0<-bf(bridge_12.2, bridge_12.2null)$bf

# Save BFs

BFs <- data.frame(12, test_1_2, test_1_0, test_2_0)
colnames(BFs) <- c("Hypothesis", 
                   "Prediction 1 vs. Prediction 2", 
                   "Prediction 1 vs. Null", 
                   "Prediction 2 vs. Null")
write.csv(BFs, paste(od, "BFs12.csv", sep = '/'))                      

summarize_delete ("bayesLmer_h12.1_h12.0")
summarize_delete ("bayesLmer_h12.1_h12.1")
summarize_delete ("bayesLmer_h12.2_h12.0")
summarize_delete ("bayesLmer_h12.2_h12.2")

# Render results into notebook
rmarkdown::render("NGS2_WITNESS_Cycle4_h12.rmd")
