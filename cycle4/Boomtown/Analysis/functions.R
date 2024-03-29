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
                           chains = nChains, 
                           iter = nIter, 
                           seed = 12345,
                           refresh = 10000,
                           diagnostic_file = diagnostic)
  fittedGlmer$call$diagnostic_file <- diagnostic
  save (fittedGlmer, file = paste("bayesGlmer_",label, sep=""))
  bridge_priors <- bridge_sampler(fittedGlmer, silent=TRUE)
  save (bridge_priors, file = paste("bridge_",label, sep=""))
  closeAllConnections()
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
                           chains = nChains, 
                           iter = nIter,
                           seed = 12345,
                           refresh = 10000,
                           diagnostic_file = diagnostic)
  fittedLmer$call$diagnostic_file <- diagnostic
  save (fittedLmer, file = paste("bayesLmer_",label, sep=""))
  bridge_priors <- bridge_sampler(fittedLmer, silent=TRUE)
  save (bridge_priors, file = paste("bridge_",label, sep=""))
  closeAllConnections()
  return(bridge_priors)
}

bayesGlm<-function(formula, priors, dataset = factorial) {
  set.seed(12345)
  formulatext <- gsub("formula.", "",deparse(substitute(formula)))
  priorstext <- deparse(substitute(priors))
  label<-paste(formulatext, "_", priorstext, sep="")
  diagnostic<-paste("diagnostic_",formulatext, "_", priorstext, ".csv", sep="")
  fittedGlm<- stan_glm(formula,
                           data=dataset,
                           family = binomial(link = "logit"),
                           prior = priors,
                           prior_intercept = normal(0, 2.5),
                           chains = nChains, 
                           iter = nIter, 
                           seed = 12345,
                           refresh = 10000,
                           diagnostic_file = diagnostic)
  fittedGlm$call$diagnostic_file <- diagnostic
  save (fittedGlm, file = paste("bayesGlm_",label, sep=""))
  bridge_priors <- bridge_sampler(fittedGlm, silent=TRUE)
  save (bridge_priors, file = paste("bridge_",label, sep=""))
  closeAllConnections()
  return(bridge_priors)
}

bayesLm<-function(formula, priors, dataset = factorial) {
  set.seed(12345)
  formulatext <- gsub("formula.", "",deparse(substitute(formula)))
  priorstext <- deparse(substitute(priors))
  label<-paste(formulatext, "_", priorstext, sep="")
  diagnostic<-paste("diagnostic_",formulatext, "_", priorstext, ".csv", sep="")
  fittedGlm<- stan_glm(formula,
                           data=dataset,
                           family=gaussian(link = "identity"),
                           prior = priors,
                           prior_intercept = normal(0, 2.5),
                           chains = nChains, 
                           iter = nIter, 
                           seed = 12345,
                           refresh = 10000,
                           diagnostic_file = diagnostic)
  fittedGlm$call$diagnostic_file <- diagnostic
  save (fittedGlm, file = paste("bayesGlm_",label, sep=""))
  bridge_priors <- bridge_sampler(fittedGlm, silent=TRUE)
  save (bridge_priors, file = paste("bridge_",label, sep=""))
  closeAllConnections()
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

# Function to summarize models and delete files to release memory

summarize_delete <- function (file_name) {
  closeAllConnections()
  load (file=file_name)
  if (grepl("bayesGlmer_", file_name)){
    file_root <- gsub ("bayesGlmer_", "", file_name)
  } else if (grepl("bayesGlm_", file_name)) {
    file_root <- gsub ("bayesGlm_", "", file_name)
  } else if (grepl("bayesLm_", file_name)) {
    file_root <- gsub ("bayesLm_", "", file_name)
  } else  {
    file_root <- gsub ("bayesLmer_", "", file_name)
  }
  if (grepl("bayesGlmer_", file_name)){
    summary <- summary(fittedGlmer, pars="beta", digits = 3)
  } else if (grepl("bayesGlm_", file_name)) {
    summary <- summary(fittedGlm, pars="beta", digits = 3)
  } else if (grepl("bayesLm_", file_name)) {
    summary <- summary(fittedLm, pars="beta", digits = 3)
  } else  {
    summary <- summary(fittedLmer, pars="beta", digits = 3)
  }
  summary_name <- paste (file_name, "_summary", ".csv", sep = "")
  closeAllConnections()
  lapply(list.files(pattern = file_root), file.remove)
  write.csv(summary, summary_name)
  sink(paste (file_name, "_summary", ".txt", sep = ""))
  print (summary)
  sink()
  pdf(paste (file_name, "_summary", ".pdf", sep = ""))
  if (grepl("bayesGlmer_", file_name)){
    print(plot(fittedGlmer, pars="beta"))
    print(plot(fittedGlmer, "trace", pars="beta"))
  } else if (grepl("bayesGlm_", file_name)) {
    print(plot(fittedGlm, pars="beta"))
    print(plot(fittedGlm, "trace", pars="beta"))
  } else if (grepl("bayesLm_", file_name)) {
    print(plot(fittedLm, pars="beta"))
    print(plot(fittedLm, "trace", pars="beta"))
  } else  {
    print(plot(fittedLmer, pars="beta"))
    print(plot(fittedLmer, "trace", pars="beta"))
  }
  dev.off()
  closeAllConnections()
}

dlScripts <- function (scriptNames) {
  fileHolder <- getURL(paste(githubRepo, scriptNames, sep = "/"), ssl.verifypeer = FALSE)
  fileConn<-file(scriptNames)
  writeLines(fileHolder, fileConn)
  close(fileConn)
}

displayResults <- function (filename) {
  cat(readLines(paste (filename, ".txt", sep = "")), sep = '\n')
  pdf_plots <- image_read_pdf(paste (filename, ".pdf", sep = ""))
  plot(pdf_plots[1])
  plot(pdf_plots[2])
}

Bf_plotter <- function (BF_file) {
  BFs <-read.csv(BF_file)                      
  BFs <- t(BFs[-c(1,2)])
  colnames(BFs) <- "Bayes Factor"
  BFs_2 <- BFs[grep("Null", rownames(BFs)),1]
  par(mar=c(5,12,2,2))
  plot_null <- barplot(rev(BFs_2), horiz = TRUE, col="darkolivegreen3",
                       main="Bayes Factors - Hyp vs. Null",
                       ylab = "", xlab="",
                       names.arg = rownames(BFs_2), las=2)
  plot_null
  BFs_1 <- BFs[grep("Null", rownames(BFs), invert = TRUE),1]
  if (length(BFs_1)>0) {
  par(mar=c(5,12,2,2))
  plot_hyp <- barplot(rev(BFs_1), horiz = TRUE, col="darkolivegreen3",
                      main="Bayes Factors - Hyp vs. Hyp",
                      ylab = "", xlab="",
                      names.arg = rownames(BFs_1), las=2)
  plot_hyp
  }
  BFs
}
