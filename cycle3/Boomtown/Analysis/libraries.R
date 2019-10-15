## Created by Pablo Diego Rosell, PhD, for Gallup inc. in October 2019

knitr::opts_chunk$set(echo = FALSE, warning = FALSE, strip.white=TRUE, tidy=TRUE)
# load libraries
if (!require("pacman")) install.packages("pacman")
library ("pacman")
pacman::p_load(rstan, rstanarm, ggplot2, Hmisc, httr, bridgesampling, DT, dplyr, bayesplot, knitr, lme4, RCurl, formatR, caret, pROC, library, formatR, foreach, doParallel)
