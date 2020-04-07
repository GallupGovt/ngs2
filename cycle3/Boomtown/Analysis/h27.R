## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019
## Test of Hypothesis 27 and all associated predictions

# Formula
formula.h27.1<-as.formula("innovation~structureNet+competition+risk+complexity+tools+tolerance+support+pressure+framing+timeUncertainty+(1|group)") #Formula h27.1
formula.h27.2<-as.formula("innovation~structureHie+competition+risk+complexity+tools+tolerance+support+pressure+framing+timeUncertainty+(1|group)") #Formula h27.2
formula.h27.3<-as.formula("innovation~structureCel+competition+risk+complexity+tools+tolerance+support+pressure+framing+timeUncertainty+(1|group)") #Formula h27.3
formula.h27.4<-as.formula("innovation~structureNet+competition+risk+complexity+tools+tolerance+support+pressure+framing+timeUncertainty+(1|group)") #Formula h27.4

fittedGlmer <- stan_glmer(formula.h27.1, data=factorialGroup, family=binomial(link="logit"), iter=3, chains=1)
ndim<-length(fittedGlmer$covmat[1,])-2

# Priors
# Baseline priors based on Cycle 2 data: log-odds = 0.5, SD = 0.2
priorSD <- 0.2
# h27.0 priors (null): Structure has no effect on innovation
h27.0 <- normal(location = c(0.00, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h27.1 priors: Network structure reduces group motivation to innovate
h27.1 <- normal(location = c(-0.50, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h27.2 priors: Hierarchical structure increases group motivation to innovate
h27.2 <- normal(location = c(0.50, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h27.3 priors: Cellular structure increases group motivation to innovate
h27.3 <- normal(location = c(0.50, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)
# h27.4 priors: Network structure increases group motivation to innovate
h27.4 <- normal(location = c(0.50, rep(0, ndim)), scale = c(priorSD, rep(2.5,ndim)), autoscale=FALSE)

# Run models 

bridge_27.1null <- bayesGlmer(formula.h27.1, h27.0, dataset = factorialGroup)
bridge_27.2null <- bayesGlmer(formula.h27.2, h27.0, dataset = factorialGroup)
bridge_27.3null <- bayesGlmer(formula.h27.3, h27.0, dataset = factorialGroup)
bridge_27.1 <- bayesGlmer(formula.h27.1, h27.1, dataset = factorialGroup)
bridge_27.2 <- bayesGlmer(formula.h27.2, h27.2, dataset = factorialGroup)
bridge_27.3 <- bayesGlmer(formula.h27.3, h27.3, dataset = factorialGroup)
bridge_27.4 <- bayesGlmer(formula.h27.4, h27.4, dataset = factorialGroup)

# Calculate BFs for all comparisons
test_1_2<-bf(bridge_27.1, bridge_27.2)$bf
test_1_3<-bf(bridge_27.1, bridge_27.3)$bf
test_1_4<-bf(bridge_27.1, bridge_27.4)$bf
test_2_3<-bf(bridge_27.2, bridge_27.3)$bf
test_2_4<-bf(bridge_27.2, bridge_27.4)$bf
test_3_4<-bf(bridge_27.3, bridge_27.4)$bf
test_1_0<-bf(bridge_27.1, bridge_27.1null)$bf
test_2_0<-bf(bridge_27.2, bridge_27.2null)$bf
test_3_0<-bf(bridge_27.3, bridge_27.3null)$bf
test_4_0<-bf(bridge_27.4, bridge_27.1null)$bf

BFs27 <- data.frame(27, test_1_2, test_1_3, test_1_4, test_2_3, test_2_4, test_3_4, test_1_0, test_2_0, test_3_0, test_4_0)
colnames(BFs27) <- c("Hypothesis", 
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
write.csv(BFs27, paste(od, "BFs27.csv", sep = '/'))                      

