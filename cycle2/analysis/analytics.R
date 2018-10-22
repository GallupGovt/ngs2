## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

rm(list = ls(all = TRUE))
LOCAL <- TRUE # change to FALSE if you want to run from data on the network drive

# load libraries
if (!require("pacman")) install.packages("pacman")
library ("pacman")
pacman::p_load(rstan, rstanarm, ggplot2, Hmisc)

# set directories
if(Sys.info()['sysname'] == "Windows") {
    if(LOCAL) {
        dd <- 'C:/Users/c_pablo_diego-rosell/Desktop/Projects/DARPA/Cycle 2/Analytics/Abductive_loop/pilot'
        od <- 'C:/Users/c_pablo_diego-rosell/Desktop/Projects/DARPA/Cycle 2/Analytics/Abductive_loop/pilot'
    } else {
        dd <- 'W:/DARPA_NGS2/CONSULTING/Analytics/cycle2/data'
        od <- 'W:/DARPA_NGS2/CONSULTING/Analytics/cycle2/output'
    }
} else if(Sys.info()['sysname'] == 'Darwin') {
    if(LOCAL) {
        dd <- '/Users/matt_hoover/git/ngs2/cycle2/data'
        od <- '/Users/matt_hoover/git/ngs2/cycle2/output'
    } else {
        dd <- '/Volumes/dod_clients/DARPA_NGS2/CONSULTING/Analytics/cycle2/data'
        od <- '/Volumes/dod_clients/DARPA_NGS2/CONSULTING/Analytics/cycle2/output'
    }
}
set.seed(12345)

# Read in data
if('gamesData.csv' %in% list.files(paste(dd, '..', sep = '/'))) {
    factorial <- read.csv(file = paste(od, "gamesData.csv", sep = '/'))
} else {
    stop('WARNING - Metadata is missing; download by hand and merge to continue.')
}
factorial<-data.frame(lapply(factorial, factor))

# Controls
# When offered choice given by tools == 9, should always choose "TNTbarrel"
table (subset(factorial, tools == 9)$leaderChoice)
# When offered choice given by tools == 10, 11, or 12, should always choose "SatchelCharge"
table (subset(factorial, tools == 10)$leaderChoice)
table (subset(factorial, tools == 11)$leaderChoice)
table (subset(factorial, tools == 12)$leaderChoice)

# Bayesian modelling

main.formula <- innovation~h1.1+h1.3+h2.1+h3.1+h3.2+h3.3+h3.4+h3.5+tools+(1|matchid)

weak_prior <- cauchy(0, 2.5)

glmmoverall <- stan_glmer(main.formula, data=factorial, family = binomial(link = "logit"),
                          prior = weak_prior, prior_intercept = weak_prior,
                          diagnostic_file = "df1.csv")
glmmoverall
