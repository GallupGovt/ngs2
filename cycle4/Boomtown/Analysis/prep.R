rm(list = ls(all = TRUE))
dd <- getwd()
od <- getwd()
knitr::opts_chunk$set(echo=FALSE, warning=TRUE, strip.white=TRUE, tidy=TRUE, message=TRUE)

# load libraries
library("RCurl")
library("bridgesampling")
library("rstanarm")

# Setup Stan parameters and helper functions

options(mc.cores = parallel::detectCores())
nChains <- 3
source("functions.R")

# Prepare data
# Individual-level data

factorial <- read.csv(file="game_data.csv")
factorial2 <- factorial
factorial2[] <- lapply(factorial, factor)
factorial2$nConnected <- as.numeric(factorial$nConnected)
factorial2$nRound <- as.numeric(factorial$nRound)
factorial2$chat_per_round <- as.numeric(factorial$chat_per_round)
factorial2$round <- as.numeric(factorial$round)
factorial2$conformity <- as.numeric(factorial$conformity)
factorial2$grmot1 <- as.numeric(factorial$grmot1)
factorial2$grmot2 <- as.numeric(factorial$grmot2)
factorial2$risk <- as.numeric(factorial$risk)
factorial2$prb <- as.numeric(factorial$prb)
factorial2$density <- as.numeric(factorial$density)
factorial2$settings <- as.numeric(levels(factorial2$settingsNum))[factorial2$settingsNum]
factorial2$framing <-  as.factor(factorial2$framing)
factorial <- factorial2

# Group-level data

factorialGroup <- read.csv(file="game_data_group.csv")
factorialGroup2 <- factorialGroup
factorialGroup2[] <- lapply(factorialGroup, factor)
factorialGroup2$conformity <- as.numeric(factorialGroup$conformity)
factorialGroup2$grmot1 <- as.numeric(factorialGroup$grmot1)
factorialGroup2$grmot2 <- as.numeric(factorialGroup$grmot2)
factorialGroup2$risk <- as.numeric(factorialGroup$risk)
factorialGroup2$density <- as.numeric(factorialGroup$density)
factorialGroup2$centralization <- as.numeric(factorialGroup$centralization)
factorialGroup2$leaderWeight <- as.numeric(factorialGroup$leaderWeight)
factorialGroup2$innovation <- as.numeric(factorialGroup2$innovation)-1
factorialGroup <- factorialGroup2
