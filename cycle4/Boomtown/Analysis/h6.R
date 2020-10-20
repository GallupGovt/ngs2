## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 100000

# Turn 'round' into numeric for interaction testing purposes
# then turn back into factorial at the end of the script

factorial$round <- as.numeric(factorial$round)

# Formula
formula.h6.1<-as.formula("inmot1~timeUncertainty+framing+competition+tools+tolerance+support+centralization+leaderWeight+pressure+density+(1|player)")
formula.h6.2<-as.formula("inmot1~timeUncertainty*round+framing+competition+tools+tolerance+support+centralization+leaderWeight+pressure+density+(1|player)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glmer(formula.h6.1, data=factorial, family=binomial(link="logit"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-2
fittedGlmer <- stan_glmer(formula.h6.2, data=factorial, family=binomial(link="logit"), iter=3, chains=1)
ndim2<-length(fittedGlmer$covmat[1,])-2

# Declare priors
# Baseline priors based on Cycle 2 data: log-odds = 0.2, SD = 0.1
priorSD <- 0.1
# h6.1 priors (null): Increased exogenous uncertainty (ignorance) will not affect individual motivation to innovate (T1).
h6.1null <- normal(location = c(0.0, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h6.1 priors: Increased exogenous uncertainty (ignorance) will increase individual motivation to innovate (T1).
h6.1 <- normal(location = c(0.2, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h6.2 priors (null): Exogenous uncertainty (ignorance) will not affect individual motivation to innovate (T1) in the early stages of the game.
h6.2null <- normal(location = c(rep(0, ndim2), 0), scale = c(rep(2.5,ndim2), 0.01), autoscale=FALSE)
# h6.2 priors: Exogenous uncertainty (ignorance) will decrease individual motivation to innovate (T1) in the early stages of the game.
# Priors based on Cycle 2 data: log-odds = 0.2 over 13 rounds, SD = 0.01
h6.2int <- -1*(log(exp(0.2^1/13)))
h6.2 <- normal(location = c(rep(0, ndim2), h6.2int), scale = c(rep(2.5,ndim2), 0.01), autoscale=FALSE)

# Run models 

bridge_6.1null <- bayesGlmer(formula.h6.1, h6.1null)
bridge_6.1 <- bayesGlmer(formula.h6.1, h6.1)
bridge_6.2null <- bayesGlmer(formula.h6.2, h6.2null)
bridge_6.2 <- bayesGlmer(formula.h6.2, h6.2)

# Calculate BFs for all comparisons

test_1_2<-bf(bridge_6.1, bridge_6.2)$bf
test_1_0<-bf(bridge_6.1, bridge_6.1null)$bf
test_2_0<-bf(bridge_6.2, bridge_6.2null)$bf

# Save BFs

BFs <- data.frame(6, test_1_2, test_1_0, test_2_0)
colnames(BFs) <- c("Hypothesis", 
                    "Prediction 1 vs. Prediction 2", 
                    "Prediction 1 vs. Null", 
                    "Prediction 2 vs. Null")
write.csv(BFs, paste(od, "BFs6.csv", sep = '/'))                      

factorial$round <- factor(factorial$round)

summarize_delete ("bayesGlmer_h6.1_h6.1null")
summarize_delete ("bayesGlmer_h6.1_h6.1")
summarize_delete ("bayesGlmer_h6.2_h6.2null")
summarize_delete ("bayesGlmer_h6.2_h6.2")

# Render results into notebook
rmarkdown::render("NGS2_WITNESS_Cycle4_h6.rmd")
