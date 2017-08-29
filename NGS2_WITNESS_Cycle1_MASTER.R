#Created by Pablo Diego Rosell, PhD, for Gallup inc. on August 2017

### System information

a=Sys.info()
print(a)
b=sessionInfo(package = NULL)
print(b)

### Set base directory

a = Sys.info()[1]
if( a == "Windows") { setwd("X:/DARPA_NGS2/CONSULTING/Analytics/Cycle1")}
if( a != "Windows") { setwd("/Volumes/Clients/DARPA_NGS2/CONSULTING/Analytics/Cycle1")}
rm(list=ls())

### Load all required packages

if (!require("pacman")) install.packages("pacman")
library ("pacman")
pacman::p_load(multiwayvcov, lmtest, dplyr, reshape2, multiwayvcov, lmtest, Hmisc, corrgram, PerformanceAnalytics, 
               doBy, car, ggplot2, DT, utils, lme4, rstan, rstanarm, igraph)

### Process empanelment data

source ("NGS2_WITNESS_empanelment_prep.R")

### Process breadboard data

source ("NGS2_WITNESS_Cycle1_experiment_prep.R")

### Process coloring game data

# source ("NGS2_WITNESS_Cycle1_coloring_prep.R") ### TO BE ADDED (Pablo)

### Response funnel 

# source ("NGS2_WITNESS_Cycle1_response.R") ### TO BE ADDED (Matt)


### EXPERIMENT 1
### Analyze experiment 1

source ("NGS2_WITNESS_Cycle1_exploratory_exp1.R")
# source ("NGS2_WITNESS_Cycle1_machine_exp1.R")  ### TO BE ADDED (Anu)
source ("NGS2_WITNESS_Cycle1_confirmatory_exp1.R")
source ("NGS2_WITNESS_Cycle1_bayesian_exp1.R")

### Analyze coloring game

# source ("NGS2_WITNESS_Cycle1_coloring.R")  ### TO BE ADDED (Pablo)


### EXPERIMENT 2
### Analyze experiment 2 

source ("NGS2_WITNESS_Cycle1_exploratory_exp1.R")
# source ("NGS2_WITNESS_Cycle1_machine_exp2.R")  ### TO BE ADDED (Anu)
source ("NGS2_WITNESS_Cycle1_confirmatory_exp1.R")
# source ("NGS2_WITNESS_Cycle1_bayesian_exp2.R")  ### TO BE ADDED (Pablo)

