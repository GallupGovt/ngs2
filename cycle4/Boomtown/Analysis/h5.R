## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Environment
source("functions.R")
source("prep.R")

# Iterations
nIter <- 200000

# Formula
formula.h5<-as.formula("inmot1~framing+competition+tools+tolerance+support+centralization+leaderWeight+pressure+density+timeUncertainty+(1|player)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- suppressWarnings(stan_glmer(formula.h5, data=factorial, family=binomial(link="logit"), iter=3, chains=1, refresh=0))
ndim<-length(fittedGlmer$covmat[1,])-3

# Declare priors
# Baseline priors based on Cycle 2 data: log-odds = 0.3, SD = 0.15
priorSD <- 0.15
# h5.0 priors (null): Availability of positive or negative examples will not affect individual motivation to innovate (T1)
h5.0 <- normal(location = c(0.0, 0.0, rep(0, ndim)), scale = c(priorSD, priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h5.1 priors: Availability of a positive example will increase individual motivation to innovate (T1), while availability of a negative example will decrease individual motivation to innovate (T1)
h5.1 <- normal(location = c(-0.3, 0.3, rep(0, ndim)), scale = c(priorSD, priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h5.2 priors: Availability of a negative example will decrease individual motivation to innovate (T1)
h5.2 <- normal(location = c(-0.3, 0.0, rep(0, ndim)), scale = c(priorSD, priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_5.0 <- bayesGlmer(formula.h5, h5.0)
bridge_5.1 <- bayesGlmer(formula.h5, h5.1)
bridge_5.2 <- bayesGlmer(formula.h5, h5.2)

# Calculate BFs for all comparisons

test_1_2<-bf(bridge_5.1, bridge_5.2)$bf
test_1_0<-bf(bridge_5.1, bridge_5.0)$bf
test_2_0<-bf(bridge_5.2, bridge_5.0)$bf

# Save BFs

BFs <- data.frame(5, test_1_2, test_1_0, test_2_0)
colnames(BFs) <- c("Hypothesis", 
                    "Prediction 1 vs. Prediction 2", 
                    "Prediction 1 vs. Null", 
                    "Prediction 2 vs. Null")
write.csv(BFs, paste(od, "BFs5.csv", sep = '/'))                      

summarize_delete ("bayesGlmer_h5_h5.0")
summarize_delete ("bayesGlmer_h5_h5.1")
summarize_delete ("bayesGlmer_h5_h5.2")

# Render results into notebook
rmarkdown::render("NGS2_WITNESS_Cycle4_h5.rmd")
