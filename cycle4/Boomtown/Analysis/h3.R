## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 100000

# Formula
formula.h3<-as.formula("inmot1~risk+prb+competition+tolerance+support+structure+pressure+framing+density+timeUncertainty+(1|player)") #Formula h1.null

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- suppressWarnings(stan_glmer(formula.h3, data=factorial, family=binomial(link="logit"), iter=3, chains=1, refresh=0))
ndim<-length(fittedGlmer$covmat[1,])-2

# Declare priors
# Baseline priors based on Cycle 2 data: log-odds = -1.7, SD = 0.25
priorSD <- 0.25
# h3.0 priors (null): Controlling for expected gains, individuals will not be more likely to select a certain tool over an uncertain tool.
h3.0 <- normal(location = c(0.00, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h3.1 priors: Controlling for expected gains, individuals will be more likely to select a certain tool over an uncertain tool.
h3.1 <- normal(location = c(-1.7, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_3.0 <- bayesGlmer(formula.h3, h3.0)
bridge_3.1 <- bayesGlmer(formula.h3, h3.1)

# Calculate BFs for all comparisons
test_1_0<-bf(bridge_3.1, bridge_3.0)$bf

BFs <- data.frame(3, test_1_0)
colnames(BFs) <- c("Hypothesis",
                     "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs3.csv", sep = '/'))                      

summarize_delete ("bayesGlmer_h3_h3.0")
summarize_delete ("bayesGlmer_h3_h3.1")

# Render results into notebook
rmarkdown::render("NGS2_WITNESS_Cycle4_h3.rmd")
