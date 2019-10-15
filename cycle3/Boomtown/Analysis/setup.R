## Created by Pablo Diego Rosell, PhD, for Gallup inc. in October 2019

knitr::opts_chunk$set(echo = FALSE, warning = FALSE, strip.white=TRUE, tidy=TRUE)
# load libraries
if (!require("pacman")) install.packages("pacman")
library ("pacman")
pacman::p_load(rstan, rstanarm, ggplot2, Hmisc, httr, bridgesampling, DT, dplyr, bayesplot, knitr, lme4, RCurl, formatR, caret, pROC, library, formatR, foreach, doParallel)

# download scripts and data
dlScripts <- function (scriptNames) {
  fileHolder <- getURL(paste(githubRepo, scriptNames, sep = "/"), ssl.verifypeer = FALSE)
  fileConn<-file(scriptNames)
  writeLines(fileHolder, fileConn)
  close(fileConn)
}
githubRepo <- "https://raw.githubusercontent.com/GallupGovt/ngs2/master/cycle3/Boomtown/Analysis"
setup <- c("dataprep.R", "functions.R", "analytics.R")
hypotheses <- c("h1.R", "h2.R", "h3.R", "h4.R", "h5.R", "h6.R", "h7.R",
                "h8.R", "h9.R", "h10.R", "h11.R", "h12.R", "h13.R", "h14.R", "h15.R", "h16.R", "h17.R", "h18.R", "h19.R",
                "h20.R", "h21.R", "h22.R", "h25.R", "h26.R", "h27.R")
data <- c("game_data.csv", "survey_data.csv")
lapply(c(setup, hypotheses, data), dlScripts)
