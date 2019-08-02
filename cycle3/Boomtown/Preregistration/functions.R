## Created by Pablo Diego Rosell, PhD, for Gallup inc. in June 2019
# For questions, please email pablo_diego-rosell@gallup.co.uk

glmerBF<-function(formula, priors1, priors2) {
  
  priors1Glmer<- stan_glmer(formula,
                         data=factorial,
                         family = binomial(link = "logit"),
                         prior = priors1,
                         prior_intercept = normal(0, 2.5),
                         chains = 3, iter = nIter,
                         diagnostic_file = "dfPriors1.csv")
  
  bridge_priors1 <- bridge_sampler(priors1Glmer, silent=TRUE)
  
  priors2Glmer<- stan_glmer(formula,
                         data=factorial,
                         family = binomial(link = "logit"),
                         prior = priors2,
                         prior_intercept = normal(0, 2.5),
                         chains = 3, iter = nIter,
                         diagnostic_file = "dfPriors2.csv")
  
  bridge_priors2 <- bridge_sampler(priors2Glmer, silent=TRUE)
  
  BF<-bf(bridge_priors1, bridge_priors2)
  
  return(as.numeric(BF[1]))
  
}

lmerBF<-function(formula, priors1, priors2) {
  
  priors1Glmer<- stan_glmer(formula,
                            data=factorial,
                            family = gaussian(link = "identity"),
                            prior = priors1,
                            prior_intercept = normal(0, 2.5),
                            chains = 3, iter = nIter,
                            diagnostic_file = "dfPriors1.csv")
  
  bridge_priors1 <- bridge_sampler(priors1Glmer, silent=TRUE)
  
  priors2Glmer<- stan_glmer(formula,
                            data=factorial,
                            family = gaussian(link = "identity"),
                            prior = priors2,
                            prior_intercept = normal(0, 2.5),
                            chains = 3, iter = nIter,
                            diagnostic_file = "dfPriors2.csv")
  
  bridge_priors2 <- bridge_sampler(priors2Glmer, silent=TRUE)
  
  BF<-bf(bridge_priors1, bridge_priors2)
  
  return(as.numeric(BF[1]))
  
}

bayesGlmer<-function(formula, priors) {
  set.seed(12345)
  diagnostic<-deparse(substitute(priors))
  fittedGlmer<- stan_glmer(formula,
                           data=factorial,
                           family = binomial(link = "logit"),
                           prior = priors,
                           prior_intercept = normal(0, 2.5),
                           chains = 3, iter = nIter,
                           diagnostic_file = 
                             paste(diagnostic,".csv", sep=""))
  return(fittedGlmer)
}

bayeslmer<-function(formula, priors) {
  set.seed(12345)
  diagnostic<-deparse(substitute(priors))
  fittedGlmer <- stan_glmer(formula,
                            data=factorial,
                            family = gaussian(link = "identity"),
                            prior = priors,
                            prior_intercept = normal(0, 2.5),
                            chains = 3, iter = nIter,
                            diagnostic_file = 
                              paste(diagnostic,".csv", sep=""))
  return(fittedGlmer)
}

bayesPlotter <- function (plotdf, plotBF) {
  frame.posterior<-subset(plotdf, Distribution=="Posterior")
  postPlot<-ggplot(frame.posterior, aes(value, linetype=Distribution)) + 
    geom_density(alpha=0.4) + 
    scale_x_continuous(limits = c(-1.5, 1.5)) + 
    scale_y_continuous(limits = c(0, 2.5))
  bfPlot<-ggplot(plotdf, aes(value, linetype=Distribution)) + 
    geom_density(alpha=0.4) + 
    scale_x_continuous(limits = c(-1.5, 1.5)) + 
    scale_y_continuous(limits = c(0, 2.5)) +
    annotate("text", x=0.75, y=1.7, label = paste("BF (h1.1.1 vs Null) = ", sprintf("%0.2f", plotBF))) +
    geom_vline(xintercept = 0, linetype="dashed")
  return(list(postPlot, bfPlot))
}

bayesPlotter_3curves <- function (model, priors1, priors2, priors3, priorScale, coef1, coef2, coef3, plotBF) {
  plotIters<-nIter*1.5  
  draws <- as.data.frame(model)
  a <- rnorm(plotIters, mean=priors1, sd=priorScale)
  b <- rnorm(plotIters, mean=priors2, sd=priorScale)
  c <- rnorm(plotIters, mean=priors3, sd=priorScale)
  d <- draws[[coef1]]
  e <- draws[[coef2]]
  f <- draws[[coef3]]
  plotdf <- data.frame(value=c(a, b, c, d, e, f), 
                       Distribution=c(rep("Prior", plotIters*3),
                                      rep("Posterior", plotIters*3)), 
                       Level=c(rep(priors1, plotIters),
                               rep(priors2, plotIters),
                               rep(priors3, plotIters), 
                               rep(priors1, plotIters),
                               rep(priors2, plotIters),
                               rep(priors3, plotIters)))
  plots<-bayesPlotter(plotdf, plotBF)
  return(plots)
}
