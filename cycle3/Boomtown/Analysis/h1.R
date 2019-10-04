## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Formula
formula.h1<-as.formula("inmot1~competition+tools+tolerance+support+structure+pressure+framing+density+timeUncertainty+(1|player)+(1|group)") #Formula h1.null

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glmer(formula.h1, data=factorial, family=binomial(link="logit"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-4

# Declare priors
# Baseline priors based on Cycle 2 data: log-odds = 0.5, SD = 0.2
priorSD <- 0.2
# h1.0 priors (null): Competition has no effect on innovation
h1.0 <- normal(location = c(0.00, 0.00, 0.00, rep(0, ndim)), scale = c(rep(priorSD,3), rep(2.5,ndim)), autoscale=FALSE)
# h1.1 priors: Intergroup competition increases individual motivation to innovate
h1.1 <- normal(location = c(-0.50, 0.00, 0.50, rep(0, ndim)), scale = c(rep(priorSD,3), rep(2.5,ndim)), autoscale=FALSE)
# h1.2 priors: Individual motivation to innovate is u-shaped on intergroup competition
h1.2 <- normal(location = c(-0.50, 0.00, -0.50, rep(0, ndim)), scale = c(rep(priorSD,3), rep(2.5,ndim)), autoscale=FALSE)
# h1.3 priors: Low intergroup competition decreases individual motivation to innovate
h1.3 <- normal(location = c(-0.50, 0.00, 0.00, rep(0, ndim)), scale = c(rep(priorSD,3), rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_1.0 <- bayesGlmer(formula.h1, h1.0)
bridge_1.1 <- bayesGlmer(formula.h1, h1.1)
bridge_1.2 <- bayesGlmer(formula.h1, h1.2)
bridge_1.3 <- bayesGlmer(formula.h1, h1.3)

# Calculate BFs for all comparisons

test_1_2<-bf(bridge_1.1, bridge_1.2)$bf
test_1_3<-bf(bridge_1.1, bridge_1.3)$bf
test_2_3<-bf(bridge_1.2, bridge_1.3)$bf
test_1_0<-bf(bridge_1.1, bridge_1.0)$bf
test_2_0<-bf(bridge_1.2, bridge_1.0)$bf
test_3_0<-bf(bridge_1.3, bridge_1.0)$bf

# Save BFs

BFs <- data.frame(1, test_1_2, test_1_3, test_2_3, test_1_0, test_2_0, test_3_0)
colnames(BFs) <- c("Hypothesis", 
                      "Prediction 1 vs. Prediction 2", 
                      "Prediction 1 vs. Prediction 3", 
                      "Prediction 2 vs. Prediction 3", 
                      "Prediction 1 vs. Null", 
                      "Prediction 2 vs. Null", 
                      "Prediction 3 vs. Null")
write.csv(BFs, paste(od, "BFs1.csv", sep = '/'))                      
