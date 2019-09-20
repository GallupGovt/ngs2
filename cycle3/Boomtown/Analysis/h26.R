## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019
# Formula

formula.h26<-as.formula("innovation~competition+risk+complexity+tools+tolerance+support+centralization+leaderWeight+pressure+framing+density+timeUncertainty+(1|group)") #Formula h26.null

# Priors
# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glmer(formula.h26, data=factorialGroup, family=binomial(link = "logit"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-4

# Baseline priors based on Cycle 2 data: log-odds = 0.5, SD = 0.2
priorSD <- 0.2
# h26.0 priors (null): Competition has no effect on innovation
h26.0 <- normal(location = c(0.00, 0.00, 0.00, rep(0, 18)), scale = c(rep(priorSD,3), rep(2.5,18)), autoscale=FALSE)
# h26.1 priors: Intergroup competition increases group motivation to innovate
h26.1 <- normal(location = c(-0.50, 0.00, 0.50, rep(0, 18)), scale = c(rep(priorSD,3), rep(2.5,18)), autoscale=FALSE)
# h26.2 priors: Group motivation to innovate is u-shaped on intergroup competition
h26.2 <- normal(location = c(-0.50, 0.00, -0.50, rep(0, 18)), scale = c(rep(priorSD,3), rep(2.5,18)), autoscale=FALSE)
# h26.3 priors: Intergroup competition decreases group motivation to innovate
h26.3 <- normal(location = c(0.50, 0.00, -0.50, rep(0, 18)), scale = c(rep(priorSD,3), rep(2.5,18)), autoscale=FALSE)
# h26.4 priors: Low intergroup competition decreases group motivation to innovate
h26.4 <- normal(location = c(-0.50, 0.00, 0.00, rep(0, 18)), scale = c(rep(priorSD,3), rep(2.5,18)), autoscale=FALSE)

# Run models 

bridge_26.0 <- bayesGlmer(formula.h26, h26.0)
bridge_26.1 <- bayesGlmer(formula.h26, h26.1)
bridge_26.2 <- bayesGlmer(formula.h26, h26.2)
bridge_26.3 <- bayesGlmer(formula.h26, h26.3)
bridge_26.4 <- bayesGlmer(formula.h26, h26.4)

# Calculate BFs for all comparisons
test_1_2<-bf(bridge_26.1, bridge_26.2)$bf
test_1_3<-bf(bridge_26.1, bridge_26.3)$bf
test_1_4<-bf(bridge_26.1, bridge_26.4)$bf
test_2_3<-bf(bridge_26.2, bridge_26.3)$bf
test_2_4<-bf(bridge_26.2, bridge_26.4)$bf
test_3_4<-bf(bridge_26.3, bridge_26.4)$bf
test_1_0<-bf(bridge_26.1, bridge_26.0)$bf
test_2_0<-bf(bridge_26.2, bridge_26.0)$bf
test_3_0<-bf(bridge_26.3, bridge_26.0)$bf
test_4_0<-bf(bridge_26.4, bridge_26.0)$bf

BFs26 <- data.frame(26, test_1_2, test_1_3, test_1_4, test_2_3, test_2_4, test_3_4, test_1_0, test_2_0, test_3_0, test_4_0)
colnames(BFs26) <- c("Hypothesis", 
                      "Prediction 1 vs. Prediction 2", 
                      "Prediction 1 vs. Prediction 3", 
                      "Prediction 1 vs. Prediction 4",   
                      "Prediction 2 vs. Prediction 3", 
                      "Prediction 2 vs. Prediction 4", 
                      "Prediction 3 vs. Prediction 4", 
                      "Prediction 1 vs. Null", 
                      "Prediction 2 vs. Null", 
                      "Prediction 3 vs. Null",
                      "Prediction 4 vs. Null")
write.csv(BFs26, paste(od, "BFs26.csv", sep = '/'))                      

