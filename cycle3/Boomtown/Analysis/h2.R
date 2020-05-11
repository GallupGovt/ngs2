## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019

# Formula
formula.h2.1<-as.formula("inmot1~toolsCPT+competition+tolerance+support+structure+pressure+framing+density+timeUncertainty+(1|player)")
formula.h2.2<-as.formula("inmot1~toolsEUT+competition+tolerance+support+structure+pressure+framing+density+timeUncertainty+(1|player)")
formula.h2.3<-as.formula("inmot1~toolsPT+competition+tolerance+support+structure+pressure+framing+density+timeUncertainty+(1|player)")
formula.h2.4<-as.formula("inmot1~toolsCPTEXP+competition+tolerance+support+structure+pressure+framing+density+timeUncertainty+(1|player)")

# Extract number of prior parameters ('ndim') to be declared

fittedGlmer <- stan_glmer(formula.h2.1, data=factorial, family=binomial(link="logit"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-2

# Declare priors
# Baseline priors based on Cycle 2 data: log-odds = 0.5, SD = 0.2
priorSD <- 0.1
# h2.0 priors (null): Prospect value will have no effect on individual motivation to innovate
h2.0 <- normal(location = c(0.00, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h2.1 priors: Prospect value, calculated according to Cumulative Prospect Theory, will determine an individual's motivation to innovate. 
h2.1 <- normal(location = c(0.6, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h2.2 priors: Prospect value, calculated according to Expected Utility Theory, will determine an individual's motivation to innovate. 
h2.2 <- normal(location = c(0.6, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h2.3 priors: Prospect value, calculated according to Probability Theory, will determine an individual's motivation to innovate. 
h2.3 <- normal(location = c(0.6, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h2.4 priors: Prospect value, calculated according to a combination of CPT + Summative Expectancy Theory, will determine an individual's motivation to innovate. 
h2.4 <- normal(location = c(1.35, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_2.1null <- bayesGlmer(formula.h2.1, h2.0)
bridge_2.2null <- bayesGlmer(formula.h2.2, h2.0)
bridge_2.3null <- bayesGlmer(formula.h2.3, h2.0)
bridge_2.4null <- bayesGlmer(formula.h2.4, h2.0)
bridge_2.1 <- bayesGlmer(formula.h2.1, h2.1)
bridge_2.2 <- bayesGlmer(formula.h2.2, h2.2)
bridge_2.3 <- bayesGlmer(formula.h2.3, h2.3)
bridge_2.4 <- bayesGlmer(formula.h2.4, h2.4)

# Calculate BFs for all comparisons
test_1_2<-bf(bridge_2.1, bridge_2.2)$bf
test_1_3<-bf(bridge_2.1, bridge_2.3)$bf
test_1_4<-bf(bridge_2.1, bridge_2.4)$bf
test_2_3<-bf(bridge_2.2, bridge_2.3)$bf
test_2_4<-bf(bridge_2.2, bridge_2.4)$bf
test_3_4<-bf(bridge_2.3, bridge_2.4)$bf
test_1_0<-bf(bridge_2.1, bridge_2.1null)$bf
test_2_0<-bf(bridge_2.2, bridge_2.2null)$bf
test_3_0<-bf(bridge_2.3, bridge_2.3null)$bf
test_4_0<-bf(bridge_2.4, bridge_2.4null)$bf

BFs <- data.frame(2, test_1_2, test_1_3, test_1_4, test_2_3, test_2_4, test_3_4, test_1_0, test_2_0, test_3_0, test_4_0)
colnames(BFs) <- c("Hypothesis", 
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
write.csv(BFs, paste(od, "BFs2.csv", sep = '/'))                      

