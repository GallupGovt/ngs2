## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 100000

# Formula
formula.h4<-as.formula("inmot1~risk*tolerance+prb+competition+support+centralization+leaderWeight+pressure+framing+density+timeUncertainty+(1|player)") #Formula h1.null

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- suppressWarnings(stan_glmer(formula.h4, data=factorial, family=binomial(link="logit"), iter=3, chains=1, refresh=0))
ndim<-length(fittedGlmer$covmat[1,])-4

# Declare priors
# Baseline priors based on Cycle 2 data: log-odds = -1.7, SD = 0.25
priorSD <- 0.25
# h4.0 priors (null): Innovation uncertainty will not -affect individual motivation to innovate
h4.0 <- normal(location = c(-1.90, 0.10, rep(0, ndim), 0.00), scale = c(2.5, 2.5, rep(2.5,ndim), priorSD), autoscale=FALSE)
# h4.1 priors: TA will moderate the effect of Innovation Uncertainty on Innovative Prospect Value. 
# Individuals high in TA will show a lower certainty effect than individuals low in TA.
h4.1 <- normal(location = c(-1.90, 0.10, rep(0, ndim), 0.40), scale = c(2.5, 2.5, rep(2.5,ndim), priorSD), autoscale=FALSE)

# Run models 

bridge_4.0 <- bayesGlmer(formula.h4, h4.0)
bridge_4.1 <- bayesGlmer(formula.h4, h4.1)

# Calculate BFs for all comparisons
test_1_0<-bf(bridge_4.1, bridge_4.0)$bf

BFs <- data.frame(4, test_1_0)
colnames(BFs) <- c("Hypothesis",
                     "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs4.csv", sep = '/'))                      

summarize_delete ("bayesGlmer_h4_h4.0")
summarize_delete ("bayesGlmer_h4_h4.1")

# Render results into notebook
rmarkdown::render("NGS2_WITNESS_Cycle4_h4.rmd")
