#Created by Pablo Diego Rosell, PhD, for Gallup inc. in March 2017
a = Sys.info()
if(a['sysname'] == "Windows") {
    setwd(paste0('C:/Users/pablo_diego-rosell/Desktop/Projects/DARPA/Cycle 1/',
                 'Research Protocols/Registration/Experiment 2/Mock data'))
} else {
    if(a['user'] == 'matt_hoover') {
        setwd('~/git/NGS2')
    } else {
        setwd("~/Research/Gallup/GallupAnalytics/RandD/NGS2/NGS2-Experiment")
    }
}

rm(list=ls())

### Install packages that are required for this file
#################################
list.of.packages <- c("pacman", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in%
                                   installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

pacman::p_load(multiwayvcov, lmtest)

# Load data for analysis
exp2_cooperation <- read.csv('NGS2-Cycle1-Experiment2/cooperation.csv',
                             header = TRUE, sep = ',')
exp2_rewire <- read.csv('NGS2-Cycle1-Experiment2/rewire.csv', header = TRUE,
                        sep = ',')

########################################################################################################################
#Hypothesis tests
########################################################################################################################

#1.1.   Individuals will be more likely to form connections with in-group members than with out-group members

logit.1.1 <- glm(connect~ingroup, data = exp2_rewire, fqamily = "binomial")
logit.cluster.1.1 <- cluster.vcov(logit.1.1, cbind(exp2_rewire$sessionnum,
                                                   exp2_rewire$playerid))
coeftest(logit.1.1, logit.cluster.1.1)

#1.2    Overall cooperation level will increase with successive rounds

logit.1.2 <- glm(decision0d1c~round_num, data = exp2_cooperation,
                 family = "binomial")
logit.cluster.1.2 <- cluster.vcov(logit.1.2, cbind(exp2_cooperation$sessionnum,
                                                   exp2_cooperation$playerid))
coeftest(logit.1.2, logit.cluster.1.2)
#Average cooperation by round
plot(aggregate(exp2_cooperation$decision0d1c, list(exp2_cooperation$round_num),
               mean))

#2.1    In-group favoritism will be more likely in the biased pairing condition

logit.2.1 <- glm(decision0d1c~biased*ingroup, data = exp2_cooperation,
                 family = "binomial")
logit.cluster.2.1 <- cluster.vcov(logit.2.1, cbind(exp2_cooperation$sessionnum,
                                                   exp2_cooperation$playerid))
coeftest(logit.2.1, logit.cluster.2.1)

#3.1    Individuals in the 2 avatar condition will be more likely to form connections with in-group members than those in the 4 avatar condition

logit.3.1 <- glm(connect~ingroup*identities, data = exp2_rewire,
                 family = "binomial")
logit.cluster.3.1 <- cluster.vcov(logit.3.1, cbind(exp2_rewire$sessionnum,
                                                   exp2_rewire$playerid))
coeftest(logit.3.1, logit.cluster.3.1)

#3.2    Individuals in the 2 avatar condition will be less likely to cooperate with in-group members than those in the 4 avatar condition

logit.3.2 <- glm(decision0d1c~identities*ingroup, data = exp2_cooperation,
                 family = "binomial")
logit.cluster.3.2 <- cluster.vcov(logit.3.2, cbind(exp2_cooperation$sessionnum,
                                                   exp2_cooperation$playerid))
coeftest(logit.3.2, logit.cluster.3.2)
