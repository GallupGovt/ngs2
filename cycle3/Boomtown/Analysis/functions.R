## Created by Pablo Diego Rosell, PhD, for Gallup inc. in June 2019
# For questions, please email pablo_diego-rosell@gallup.co.uk

bayesGlmer<-function(formula, priors, dataset = factorial) {
  set.seed(12345)
  formulatext <- gsub("formula.", "",deparse(substitute(formula)))
  priorstext <- deparse(substitute(priors))
  label<-paste(formulatext, "_", priorstext, sep="")
  diagnostic<-paste("diagnostic_",formulatext, "_", priorstext, ".csv", sep="")
  fittedGlmer<- stan_glmer(formula,
                           data=dataset,
                           family = binomial(link = "logit"),
                           prior = priors,
                           prior_intercept = normal(0, 2.5),
                           chains = 3, iter = nIter,
                           diagnostic_file = diagnostic)
  fittedGlmer$call$diagnostic_file <- diagnostic
  save (fittedGlmer, file = paste("bayesGlmer_",label, sep=""))
  bridge_priors <- bridge_sampler(fittedGlmer, silent=TRUE)
  save (bridge_priors, file = paste("bridge_",label, sep=""))
  return(bridge_priors)
}

bayesLmer<-function(formula, priors, dataset = factorial) {
  set.seed(12345)
  formulatext <- gsub("formula.", "",deparse(substitute(formula)))
  priorstext <- deparse(substitute(priors))
  label<-paste(formulatext, "_", priorstext, sep="")
  diagnostic<-paste("diagnostic_",formulatext, "_", priorstext, ".csv", sep="")
  fittedLmer<- stan_glmer(formula,
                           data=dataset,
                           family = gaussian(link = "identity"),
                           prior = priors,
                           prior_intercept = normal(0, 2.5),
                           chains = 3, iter = nIter,
                           diagnostic_file = diagnostic)
  fittedLmer$call$diagnostic_file <- diagnostic
  save (fittedLmer, file = paste("bayesLmer_",label, sep=""))
  bridge_priors <- bridge_sampler(fittedLmer, silent=TRUE)
  save (bridge_priors, file = paste("bridge_",label, sep=""))
  return(bridge_priors)
}

# Bayesian plotting functions

modelPlotter<- function (model, ivs) {
  posterior <- as.array(model)
  mcmc_areas(posterior, pars = ivs, 
             prob = 0.8, # 80% intervals
             prob_outer = 0.99, # 99%
             point_est = "mean")
}

bayesPlotter <- function (plotdf, plotBF) {
  frame.prior<-subset(plotdf, Distribution=="Prior")
  priorPlot<-ggplot(frame.prior, aes(value, fill=Level, linetype=Distribution)) + 
    geom_density(alpha=0.4) + 
    scale_x_continuous(limits = c(-1.5, 1.5)) + 
    scale_y_continuous(limits = c(0, 5)) +
    geom_vline(xintercept = 0, linetype="dashed")
  frame.posterior<-subset(plotdf, Distribution=="Posterior")
  postPlot<-ggplot(frame.posterior, aes(value, fill=Level, linetype=Distribution)) + 
    geom_density(alpha=0.4) + 
    scale_x_continuous(limits = c(-1.5, 1.5)) + 
    scale_y_continuous(limits = c(0, 5)) +
    geom_vline(xintercept = 0, linetype="dashed")
  bfPlot<-ggplot(plotdf, aes(value, fill=Level, linetype=Distribution)) + 
    geom_density(alpha=0.4) + 
    scale_x_continuous(limits = c(-1.5, 1.5)) + 
    scale_y_continuous(limits = c(0, 5)) +
    annotate("text", x=1.2, y=2, label = paste(deparse(substitute(plotBF)), " = ", sprintf("%0.2f", plotBF))) +
    geom_vline(xintercept = 0, linetype="dashed")
  return(list(priorPlot, postPlot, bfPlot))
}

# Bayesian plotting - frame processing function (Predictions with one coefficient)

bayesPlotter1 <- function (model, priorList, priors1, priorScale, coef1, plotBF) {
  plotIters<-nIter*1.5  
  draws <- as.data.frame(model)
  a <- rnorm(plotIters, mean=priorList[[priors1]], sd=priorScale)
  d <- draws[[coef1]]
  plotdf <- data.frame(value=c(a, d), 
                       Distribution=c(rep("Prior", plotIters),
                                      rep("Posterior", plotIters)), 
                       Level=c(rep(priors1, plotIters),
                               rep(priors1, plotIters)))
  plots<-bayesPlotter(plotdf, plotBF)
  return(plots)
}

# Bayesian plotting - frame processing function (Predictions with three coefficients)

bayesPlotter3 <- function (model, priorList, priors1, priors2, priors3, priorScale, coef1, coef2, coef3, plotBF) {
  plotIters<-nIter*1.5  
  draws <- as.data.frame(model)
  a <- rnorm(plotIters, mean=priorList[[priors1]], sd=priorScale)
  b <- rnorm(plotIters, mean=priorList[[priors2]], sd=priorScale)
  c <- rnorm(plotIters, mean=priorList[[priors3]], sd=priorScale)
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
