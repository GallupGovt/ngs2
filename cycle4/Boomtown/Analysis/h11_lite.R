## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Formula
formula.h11<-as.formula("inmot1~framing*complexity+timeUncertainty+pressure+tolerance+competition+support+centralization+leaderWeight+density")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glm(formula.h11, data=factorial, family=binomial(link="logit"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-3

# Declare priors
# Baseline priors based on DESIM: log-odds = -0.5, 0.5, SD = 0.25
priorSD <- 0.25
# h11.0 priors (null): Innovation complexity does not moderate the effect of heuristic reasoning on individual motivation to innovate (T1): The availability heuristic will not have a greater effect on individual motivation to innovate under high complexity than under low complexity
h11.0 <- normal(location = c(rep(0, ndim), 0, 0), scale = c(rep(2.5,ndim), priorSD, priorSD), autoscale=FALSE)
# h11.1 priors: Innovation complexity moderates the effect of heuristic reasoning on individual motivation to innovate (T1): The availability heuristic will have a greater effect on individual motivation to innovate under high complexity than under low complexity
h11.1 <- normal(location = c(rep(0, ndim), -0.5, 0.5), scale = c(rep(2.5,ndim), priorSD, priorSD), autoscale=FALSE)

# Run models 

bridge_11.0 <- bayesGlm(formula.h11, h11.0)
bridge_11.1 <- bayesGlm(formula.h11, h11.1)

# Calculate BFs for all comparisons

test_1_0<-bf(bridge_11.1, bridge_11.0)$bf

# Save BFs

BFs <- data.frame(11, test_1_0)
colnames(BFs) <- c("Hypothesis",
                   "Prediction 1 vs. Null")
write.csv(BFs, paste(od, "BFs11.csv", sep = '/'))                      
