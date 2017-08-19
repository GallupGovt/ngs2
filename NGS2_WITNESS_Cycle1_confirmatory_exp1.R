#Created by Pablo Diego Rosell, PhD, for Gallup inc. in March 2017

# Load data for analysis
exp1_cooperation <- read.csv('NGS2-Cycle1-Experiment1/cooperation_exp1.csv',
                             header = TRUE, sep = ',')
exp1_rewire <- read.csv('NGS2-Cycle1-Experiment1/rewire_exp1.csv', header = TRUE,
                        sep = ',')

### Show stopper

print("ATENTION!! Need to make sure the exp1_cooperation dataset includes the following variables: sessionnum, condition, decision..0.D.1.C.")

#Hypothesis 4.1.1: Fixed network compositions reduce exp1_cooperation
#Rand  et al. (2011) coeff = -0.186, p<.000

exp1_cooperation.fixed<-subset(exp1_cooperation, condition=="Static")
logit.fixed <- glm(decision..0.D.1.C.~ round_num, data = exp1_cooperation.fixed, family = "binomial")
logit.multiwayvcov.fixed <- cluster.vcov(logit.fixed, cbind(exp1_cooperation.fixed$sessionnum, exp1_cooperation.fixed$playerid))
coeftest(logit.fixed, logit.multiwayvcov.fixed)

#Hypothesis 4.1.2: Randomly updating network compositions reduce exp1_cooperation
#Rand  et al. (2011) coeff = -0.113, p<.000

exp1_cooperation.random<-subset(exp1_cooperation, condition=="Random")
logit.random <- glm(decision..0.D.1.C.~ round_num, data = exp1_cooperation.random, family = "binomial")
logit.multiwayvcov.random <- cluster.vcov(logit.random, cbind(exp1_cooperation.random$sessionnum, exp1_cooperation.random$playerid))
coeftest(logit.random, logit.multiwayvcov.random)

#Hypothesis 4.1.3: Slowly updating strategic networks reduce exp1_cooperation
#Rand  et al. (2011) coeff = -0.220, p=.013

exp1_cooperation.viscous<-subset(exp1_cooperation, condition=="Viscous")
logit.viscous <- glm(decision..0.D.1.C.~ round_num, data = exp1_cooperation.viscous, family = "binomial")
logit.multiwayvcov.viscous <- cluster.vcov(logit.viscous, cbind(exp1_cooperation.viscous$sessionnum, exp1_cooperation.viscous$playerid))
coeftest(logit.viscous, logit.multiwayvcov.viscous)

#Hypothesis 4.1.4: Rapidly updating strategic networks support exp1_cooperation relative to all other conditions 
#Rand  et al. (2011) coeff = 0.135, p = .006

logit <- glm(decision..0.D.1.C.~ fluid_dummy*round_num, data = exp1_cooperation, family = "binomial")
logit.multiwayvcov <- cluster.vcov(logit, cbind(exp1_cooperation$sessionnum, exp1_cooperation$playerid))
coeftest(logit, logit.multiwayvcov)

#Hypothesis 4.2.1	Rapidly updating strategic networks have greater network heterogeneity

var <-aggregate(exp1_cooperation[,7], by=list(sessionnum=exp1_cooperation$sessionnum, fluid_dummy=exp1_cooperation$fluid_dummy), FUN=var)
fluid.var<-(subset(var$x, var$fluid_dummy==1))
other.var<-(subset(var$x, var$fluid_dummy==0))
wilcox.test(fluid.var, other.var, paired = FALSE, alternative = "two.sided")

#Hypothesis 4.2.2	Links in rapidly updating strategic networks are more stable between cooperators than between a cooperator and a defector
#Rand  et al. (2011) coeff = -2.29, p<.000

exp1_rewire.cd.dc<-subset(exp1_rewire, previouslytie==1 & (state=="CC" | state=="CD" | state=="DC"))
logit.cd.dc <- glm(break.~CC, data = exp1_rewire.cd.dc, family = "binomial")
logit.multiwayvcov.cd.dc <- cluster.vcov(logit.cd.dc, cbind(exp1_rewire.cd.dc$sessionnum, exp1_rewire.cd.dc$playerid))
coeftest(logit.cd.dc, logit.multiwayvcov.cd.dc)

#Hypothesis 4.2.3	Links in rapidly updating strategic networks are more stable between cooperators than between between two defectors
#Rand  et al. (2011) coeff = -2.94, p<.000

exp1_rewire.dd<-subset(exp1_rewire, previouslytie==1 & (state=="CC" | state=="DD"))
logit.dd <- glm(break.~CC, data = exp1_rewire.dd, family = "binomial")
logit.multiwayvcov.dd <- cluster.vcov(logit.dd, cbind(exp1_rewire.dd$sessionnum, exp1_rewire.dd$playerid))
coeftest(logit.dd, logit.multiwayvcov.dd)

#Hypothesis 4.2.4	Cooperators have more connections than defectors in rapidly updating strategic networks
#Rand  et al. (2011) coeff = 0.052, p = .021

exp1_cooperation.fluid<-subset(exp1_cooperation, condition=="Fluid")
logit.fluid <- glm(decision..0.D.1.C.~ num_neighbors, data = exp1_cooperation.fluid, family = "binomial")
logit.multiwayvcov.fluid <- cluster.vcov(logit.fluid, cbind(exp1_cooperation.fluid$sessionnum, exp1_cooperation.fluid$playerid))
coeftest(logit.fluid, logit.multiwayvcov.fluid)
