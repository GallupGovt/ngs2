## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 50000

# Formula
formula.h18<-as.formula("inmot2~conformity*grmot1+support+framing+complexity+timeUncertainty+pressure+tolerance+competition+centralization+leaderWeight+density+(1|player)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glmer(formula.h18, data=factorial, family=binomial(link="logit"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-2

# Declare priors
# Baseline priors based on DESIM: log-odds = -0.5, 0.5, SD = 0.3
priorSD <- 0.4
# h18.0 priors (null): Group Conformity will not moderate Individual Motivation to Innovate (T2).
h18.0 <- normal(location = c(rep(0, ndim), 0.0), scale = c(rep(2.5,ndim), priorSD), autoscale=FALSE)
# h18.1 priors: Group Conformity will moderate Individual Motivation to Innovate (T2). When group motivation to innovate is positive, conformity increases Individual Motivation to Innovate (T2). When group motivation to innovate is negative, conformity decreases Individual Motivation to Innovate (T2).
h18.1 <- normal(location = c(rep(0, ndim), 0.6), scale = c(rep(2.5,ndim), priorSD), autoscale=FALSE)

# Run models 

bridge_18.0 <- bayesGlmer(formula.h18, h18.0)
bridge_18.1 <- bayesGlmer(formula.h18, h18.1)

# Calculate BFs for all comparisons

test_1_0<-bf(bridge_18.1, bridge_18.0)$bf

# Save BFs

BFs <- data.frame(18, test_1_0)
colnames(BFs) <- c("Hypothesis",
                    "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs18.csv", sep = '/'))             

summarize_delete ("bayesGlmer_h18_h18.0")
summarize_delete ("bayesGlmer_h18_h18.1")

# Render results into notebook
rmarkdown::render("NGS2_WITNESS_Cycle4_h18.rmd")
