## Created by Pablo Diego Rosell, PhD, for Gallup inc. in June 2019
# For questions, please email pablo_diego-rosell@gallup.co.uk

bayesGlmer<-function(formula, priors, dataset = factorial) {
  set.seed(12345)
  formulatext <- gsub("formula.", "",deparse(substitute(formula)))
  priorstext <- deparse(substitute(priors))
  label<-paste("bridge_",formulatext, "_", priorstext, sep="")
  diagnostic<-paste("diagnostic_",formulatext, "_", priorstext, ".csv", sep="")
  fittedGlmer<- stan_glmer(formula,
                           data=dataset,
                           family = binomial(link = "logit"),
                           prior = priors,
                           prior_intercept = normal(0, 2.5),
                           chains = 3, iter = nIter,
                           diagnostic_file = diagnostic)
  fittedGlmer$call$diagnostic_file <- diagnostic
  save (fittedGlmer, file = paste("bayesGlmer.",label, sep=""))
  bridge_priors <- bridge_sampler(fittedGlmer, silent=TRUE)
  save (bridge_priors, file = paste("bridge_",label, sep=""))
  return(bridge_priors)
}

bayesLmer<-function(formula, priors, dataset = factorial) {
  set.seed(12345)
  formulatext <- gsub("formula.", "",deparse(substitute(formula)))
  priorstext <- deparse(substitute(priors))
  label<-paste("bridge_",formulatext, "_", priorstext, sep="")
  diagnostic<-paste("diagnostic_",formulatext, "_", priorstext, ".csv", sep="")
  fittedLmer<- stan_glmer(formula,
                           data=dataset,
                           family = gaussian(link = "identity"),
                           prior = priors,
                           prior_intercept = normal(0, 2.5),
                           chains = 3, iter = nIter,
                           diagnostic_file = diagnostic)
  fittedGlmer$call$diagnostic_file <- diagnostic
  save (fittedLmer, file = paste("bayesLmer.",label, sep=""))
  bridge_priors <- bridge_sampler(fittedLmer, silent=TRUE)
  save (bridge_priors, file = paste("bridge_",label, sep=""))
  return(bridge_priors)
}
