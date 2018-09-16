#Created by Pablo Diego Rosell, PhD, for Gallup inc. in March 2017


# Load data for analysis

exp1_cooperation <- read.csv("https://raw.githubusercontent.com/gallup/NGS2/master/cooperation_exp1_FINAL.csv")
exp1_rewire <- read.csv("https://raw.githubusercontent.com/gallup/NGS2/master/rewire_exp1_FINAL.csv")

### Show stopper


if(!all(c('session', 'condition', 'action') %in% names(exp1_cooperation))) {

    stop('Attention! `exp1_cooperation` must include `session`, `condition`, and `action` to proceed. Please fix.')

}



### Logit test and summary function



logit_test_and_summary <- function (data, filter.condition) {

  sub <- subset(data, condition == filter.condition)

  logit_result <- glm(action ~ round, data = sub, family = 'binomial')

  logit_vcov <- cluster.vcov(logit_result, cbind(sub$session, sub$pid))

  print(coeftest(logit_result, logit_vcov))

}



#Hypothesis 4.1.1: Fixed network compositions reduce exp1_cooperation

#Rand  et al. (2011) coeff = -0.186, p<.000

Hypothesis.4.1.1<-logit_test_and_summary(exp1_cooperation, "Static")



#Hypothesis 4.1.2: Randomly updating network compositions reduce exp1_cooperation

#Rand  et al. (2011) coeff = -0.113, p<.000



Hypothesis.4.1.2<-logit_test_and_summary(exp1_cooperation, "Random")



#Hypothesis 4.1.3: Slowly updating strategic networks reduce exp1_cooperation

#Rand  et al. (2011) coeff = -0.220, p=.013

sub <- subset(exp1_cooperation, condition == "Viscous")
logit_result <- glm(action ~ round, data = sub, family = 'binomial')
logit_vcov <- cluster.vcov(logit_result, cbind(sub$pid))
Hypothesis.4.1.3<-print(coeftest(logit_result, logit_vcov))


#Hypothesis 4.1.4: Rapidly updating strategic networks support exp1_cooperation relative to all other conditions 

#Rand  et al. (2011) coeff = 0.135, p = .006



logit <- glm(action ~ fluid_dummy*round, data = exp1_cooperation, family = "binomial")

logit.multiwayvcov <- cluster.vcov(logit, cbind(exp1_cooperation$session, exp1_cooperation$pid))

Hypothesis.4.1.4<-coeftest(logit, logit.multiwayvcov)



#Hypothesis 4.2.1	Rapidly updating strategic networks have greater network heterogeneity



network.variance <-aggregate(exp1_cooperation[,c("num_neighbors")], 

                             by=list(session=exp1_cooperation$session, 

                                     fluid_dummy=exp1_cooperation$fluid_dummy), 

                             FUN=var)

fluid.var<-(subset(network.variance$x, 

                   network.variance$fluid_dummy==1)

           )

other.var<-(subset(network.variance$x, 

                   network.variance$fluid_dummy==0)

           )

Hypothesis.4.2.1 <- wilcox.test(fluid.var, 

                                other.var, 

                                paired = FALSE, 

                                alternative = "two.sided")



#Hypothesis 4.2.2	Links in rapidly updating strategic networks are more stable between cooperators than between a cooperator and a defector

#Rand  et al. (2011) coeff = -2.29, p<.000



exp1_rewire.cd.dc<-subset(exp1_rewire, previouslytie==1 & (state=="CC" | state=="CD" | state=="DC"))

logit.cd.dc <- glm(break_tie~CC, data = exp1_rewire.cd.dc, family = "binomial")

logit.multiwayvcov.cd.dc <- cluster.vcov(logit.cd.dc, cbind(exp1_rewire.cd.dc$session, exp1_rewire.cd.dc$playerid))

Hypothesis.4.2.2 <- coeftest(logit.cd.dc, logit.multiwayvcov.cd.dc)



#Hypothesis 4.2.3	Links in rapidly updating strategic networks are more stable between cooperators than between between two defectors

#Rand  et al. (2011) coeff = -2.94, p<.000



exp1_rewire.dd<-subset(exp1_rewire, previouslytie==1 & (state=="CC" | state=="DD"))

logit.dd <- glm(break_tie~CC, data = exp1_rewire.dd, family = "binomial")

logit.multiwayvcov.dd <- cluster.vcov(logit.dd, cbind(exp1_rewire.dd$session, exp1_rewire.dd$playerid))

Hypothesis.4.2.3 <- coeftest(logit.dd, logit.multiwayvcov.dd)



#Hypothesis 4.2.4	Cooperators have more connections than defectors in rapidly updating strategic networks

#Rand  et al. (2011) coeff = 0.052, p = .021



exp1_cooperation.fluid<-subset(exp1_cooperation, condition=="Fluid")

logit.fluid <- glm(action ~ num_neighbors, data = exp1_cooperation.fluid, family = "binomial")

logit.multiwayvcov.fluid <- cluster.vcov(logit.fluid, cbind(exp1_cooperation.fluid$session, exp1_cooperation.fluid$pid))

Hypothesis.4.2.4 <- coeftest(logit.fluid, logit.multiwayvcov.fluid)


