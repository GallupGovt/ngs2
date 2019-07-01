## Created by Pablo Diego Rosell, PhD, for Gallup inc. in June 2019
# For questions, please email pablo_diego-rosell@gallup.co.uk

# Create matrix with 125 groups (per power analysis)

polycraft <- expand.grid(round=seq(1:5), group=seq(1:nGroups))

## Experimental condition

polycraft$h1.1.dummy <- c(rep(0,(nGroups/4)*5),
                          rep(1,(nGroups/4)*5),
                          rep(2,(nGroups/4)*5),
                          rep(3,(nGroups/4)*5))

## Generative model

events<-length(polycraft$group)

# We add random group and individual coefficients following a normal distribution with a mean of 0 and a SD of 0.1
# DEFT=1.2 calculated from Cycle 2 data. 

group.random.effect = 0.5
ind.random.effect = 0.2
group<-data.frame(group=unique(polycraft$group))
group$group_random_effect<-rnorm(length(unique(polycraft$group)),0,group.random.effect)
polycraft<-merge(group, polycraft, by="group")
polycraft$ind_random_effect<-rnorm(length(polycraft$group),0,ind.random.effect)

# Simulate random outcome data:  h1.1.null

log.prob.innovate.null <- runif(events)
polycraft$innovate.null <- rbinom(events,1,log.prob.innovate.null)

# Simulate outcome conforming with predictions of h1.1.1: Innovation inverted u-shaped on competition

polycraft$odds1.1.1[polycraft$h1.1.dummy==0]<- 0 
polycraft$odds1.1.1[polycraft$h1.1.dummy==1]<- -0.50
polycraft$odds1.1.1[polycraft$h1.1.dummy==2]<- 0.50
polycraft$odds1.1.1[polycraft$h1.1.dummy==3]<- -0.50

log.prob.innovate.1.1.1 <- 1/(1+exp(-(polycraft$odds1.1.1
                                      +polycraft$group_random_effect
                                      +polycraft$ind_random_effect)))

polycraft$innovate.1.1.1 <- rbinom(events,1,log.prob.innovate.1.1.1)

# Simulate outcome conforming with predictions of h1.1.2: Competition decreases innovation

polycraft$odds1.1.2[polycraft$h1.1.dummy==0]<- 0 
polycraft$odds1.1.2[polycraft$h1.1.dummy==1]<- 0.50
polycraft$odds1.1.2[polycraft$h1.1.dummy==2]<- 0.00
polycraft$odds1.1.2[polycraft$h1.1.dummy==3]<- -0.50

log.prob.innovate.1.1.2 <- 1/(1+exp(-(polycraft$odds1.1.2
                                      +polycraft$group_random_effect
                                      +polycraft$ind_random_effect)))

polycraft$innovate.1.1.2 <- rbinom(events,1,log.prob.innovate.1.1.2)

# Simulate outcome conforming with predictions of h1.1.3: Competition increases innovation

polycraft$odds1.1.3[polycraft$h1.1.dummy==0]<- 0 
polycraft$odds1.1.3[polycraft$h1.1.dummy==1]<- -0.50
polycraft$odds1.1.3[polycraft$h1.1.dummy==2]<- 0.00
polycraft$odds1.1.3[polycraft$h1.1.dummy==3]<- 0.50

log.prob.innovate.1.1.3 <- 1/(1+exp(-(polycraft$odds1.1.3
                                      +polycraft$group_random_effect
                                      +polycraft$ind_random_effect)))

polycraft$innovate.1.1.3 <- rbinom(events,1,log.prob.innovate.1.1.3)

# Turn dummy variable into factor for modeling purposes

polycraft$h1.1.dummy <- as.factor(polycraft$h1.1.dummy)
