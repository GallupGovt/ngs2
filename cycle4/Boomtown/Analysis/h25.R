## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 50000

# Formula
formula.h25<-as.formula("innovation~grmot2+leaderWeight+centralization+density+conformity+grmot1+support+framing+complexity+timeUncertainty+pressure+tolerance+competition+(1|group)")

# Extract number of prior parameters ('ndim') to be declared
fittedGlmer <- suppressWarnings(stan_glmer(formula.h25, data=factorialGroup, family=binomial(link = "logit"), iter=3, chains=1, refresh=0))
ndim<-length(fittedGlmer$covmat[1,])-2

# Declare priors
# Baseline priors based on DESIM (split effect of 0.70 into two levels)
priorSD <- 0.5
# h25.0 priors (null): Group Aggregate motivation to innovate (T2) will not increase Group Motivation to Innovate
h25.0 <- normal(location = c(0.0, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h25.1 priors: Group Aggregate motivation to innovate (T2) will increase Group Motivation to Innovate
h25.1 <- normal(location = c(1.45, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 
bridge_25.0 <- bayesGlmer(formula.h25, h25.0, factorialGroup)
bridge_25.1 <- bayesGlmer(formula.h25, h25.1, factorialGroup)

# Calculate and save BFs for all comparisons
test_1_0<-bf(bridge_25.1, bridge_25.0)$bf
BFs <- data.frame(25, test_1_0)
colnames(BFs) <- c("Hypothesis",
                   "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs25.csv", sep = '/'))                      

# Summarize/delete models
summarize_delete ("bayesGlmer_h25_h25.0")
summarize_delete ("bayesGlmer_h25_h25.1")

# Render results into notebook
rmarkdown::render("NGS2_WITNESS_Cycle4_h25.rmd")
