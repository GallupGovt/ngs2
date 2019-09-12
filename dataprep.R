## Created by Pablo Diego Rosell, PhD, for Gallup inc. in July 2019

# For questions, please email pablo_diego-rosell@gallup.co.uk



## Game parameters



nPlayers<-7 # Typically 7 or 10

nBatches<-1 # Number of batches of data collection (96 games per batch, 672 players per game. Viable range = 1 to 8 batches)



# We add random group and individual coefficients following a normal distribution with a mean of 0 and a SD of 0.1

# DEFT=1.2 calculated from Cycle 2 data. 



group.random.effect = 0.5

ind.random.effect = 0.2



## Generate factorial space



factorial <- expand.grid(competition=c(0, 1, 2, 3), 
                         
                         tolerance=c(0,1), 
                         
                         support=c(0,1), 
                         
                         centralization=c(0,1,2), 
                         
                         leaderWeight=seq(0:1), 
                         
                         timeUncertainty=seq(0:1), 
                         
                         round=seq(1:13), 
                         
                         batch=seq(1:nBatches))



#Create group identifiers



factorial$group<- (factorial$competition)+(factorial$tolerance*10)+(factorial$support*100) +
  
  (factorial$centralization*1000) + (factorial$leaderWeight*10000) + (factorial$timeUncertainty*100000)

#length(unique(factorial$group))



# Add Levels for tool complexity



factorial$complexity <- 0

factorial$complexity[factorial$round>9] <- 1



# Add Levels for Time Pressure



factorial$pressure <- 0

factorial$pressure[factorial$round==2 | 
                     
                     factorial$round==4 | 
                     
                     factorial$round==6 | 
                     
                     factorial$round==8 | 
                     
                     factorial$round==10 | 
                     
                     factorial$round==12] <- 1



# Add Levels for availability (framing)



factorial$framing <- 0

factorial$framing[factorial$round==1 | factorial$round==4 | factorial$round==7 | factorial$round==10 | factorial$round==13] <- 1

factorial$framing[factorial$round==2 | factorial$round==5 | factorial$round==8 | factorial$round==11] <- 2



# Add Levels for network density



factorial$density <- 0

factorial$density[factorial$round==1 | factorial$round==4 | factorial$round==7 | factorial$round==10 | factorial$round==13] <- 1

factorial$density[factorial$round==2 | factorial$round==5 | factorial$round==8 | factorial$round==11] <- 2



# Add levels for tool choices



factorial$tools <- 1

factorial$tools[factorial$round==2] <- 2

factorial$tools[factorial$round==4] <- 3

factorial$tools[factorial$round==6] <- 4

factorial$tools[factorial$round==7] <- 5

factorial$tools[factorial$round==8] <- 6

factorial$tools[factorial$round==9] <- 7

factorial$tools[factorial$round==10] <- 8

factorial$tools[factorial$round==11] <- 9

factorial$tools[factorial$round==12] <- 10

factorial$tools[factorial$round==13] <- 11



# Randomize order of levels for repeated measures



groups<-unique(factorial$group)

for(i in 1:length(groups)){
  
  factorial$tools[factorial$group==(groups[i]) & factorial$round<10]<-
    
    sample(factorial$tools[factorial$group==(groups[i]) & factorial$round<10])
  
}



# Generate innovative tool uncertainty (risk) measure, defined as the probability of the "innovative" tool over the probability of the "non-innovative" tool



factorial$risk<-as.numeric(NA)

factorial$risk[factorial$tools==1] <- 0.65

factorial$risk[factorial$tools==2] <- 0.35

factorial$risk[factorial$tools==3] <- 0.65

factorial$risk[factorial$tools==4] <- 0.06

factorial$risk[factorial$tools==5] <- 0.06/0.35

factorial$risk[factorial$tools==6] <- 0.65

factorial$risk[factorial$tools==7] <- 0.80/0.80

factorial$risk[factorial$tools==8] <- 0.70/0.80

factorial$risk[factorial$tools==9] <- 0.43

factorial$risk[factorial$tools==10] <- 0



# Convert 'round' to numeric



factorial$round <- as.numeric(factorial$round)



## Generative model (group level)

# Count of group events



groupEvents<-length(factorial$group)



# Add correlated group-level errors



group<-data.frame(group=unique(factorial$group))

group$group_random_effect<-rnorm(length(unique(factorial$group)),0,group.random.effect)

factorial<-merge(group, factorial, by="group")



# Simulate random outcome data at the group level (null)



log.prob.innovate <- runif(groupEvents)

factorial$innovation <- rbinom(groupEvents,1,log.prob.innovate) # Group Motivation to Innovate



sampleDist = function() { 
  
  sample(x = (seq(0:nPlayers)-1)/nPlayers, 1, replace = T, prob = runif(nPlayers+1))
  
}



for (i in (1:nrow(factorial))){
  
  factorial$gagg1.null[i]<-sampleDist() #Group aggregates T1 (null)
  
  factorial$gagg2.null[i]<-sampleDist() #Group aggregates T1 (null)
  
}



# Simulate random intermediate outcome data at the group level for conformity (null)

# Conformity defined as the rate of copying of the majority option (as in Coultas, 2004)

# With 7 members, copying rates could range from a min of 7/0 (-7)  to a max of 7/4 (+3)

# To normalize the data for varying number of players, we divide frequency change over max range (n2-n1)/(n+1)

# We simulate data by drawing randomly from this range, with a higher probability for positive values



bottom<-(round(nPlayers/2))*-1

top<-bottom+nPlayers



sampleDistConf = function(dist) { 
  
  sample(x = seq(bottom,top)/nPlayers+1, 1, replace = T, prob = dist) 
  
}



distNoMidComp <- rep(1/(nPlayers+1), nPlayers+1)

distLoComp<-c()

for (i in (1:(nPlayers+1))){
  
  distLoComp[i] <-((nPlayers+1)/i)
  
}

distLoComp<-distLoComp/sum(distLoComp)

distHiComp <- sort(distLoComp, decreasing = FALSE)



# Simulate random outcome data at the group level (null effect)



for (i in (1:nrow(factorial))){
  
  factorial$conformity.null[i]<-sampleDistConf(distNoMidComp)
  
}



# Simulate outcome conforming with predictions of h3.1: Intergroup competition increases group conformity

# The intermediate "group conformity" outcome is simulated by manipulating the shape of the beta distribution used by sampleDist



for (i in 1:nrow(factorial)) {
  
  if (factorial$competition[i]== 0 | factorial$competition[i]== 2) {
    
    factorial$conformity.1[i]<-sampleDistConf(distNoMidComp)
    
  } else if (factorial$competition[i]== 1) {
    
    factorial$conformity.1[i]<-sampleDistConf(distLoComp)
    
  } else {
    
    factorial$conformity.1[i]<-sampleDistConf(distHiComp)
    
  }
  
}



## Generative Model (Player events)

# Expand factorial frame to include players



factorial$player <- 1

factorial2 <- factorial

for (i in (2:nPlayers)){
  
  factorial$player <- i
  
  factorial2 <- rbind (factorial, factorial2)
  
}

factorial <- factorial2

factorial$player<- (factorial$player)+(factorial$group*10)

# length(unique(factorial$group)) == length(unique(factorial$player))/7 #Check expansion is correct ("TRUE") 

playerEvents<-length(factorial$player)



# Add individual-level correlated errors



players<-data.frame(player=unique(factorial$player))

players$ind_random_effect<-rnorm(length(players$player),0,ind.random.effect)

factorial<-merge(players, factorial, by="player")



# Simulate individual-level motivation to innovate data conforming with h1.0



log.prob.innovate <- runif(playerEvents)

factorial$inmot1.null <- rbinom(playerEvents,1,log.prob.innovate) # Group Motivation to Innovate



# Simulate individual-level motivation to innovate data conforming with predictions of h1.1



factorial$odds[factorial$competition==0]<- 0 

factorial$odds[factorial$competition==1]<- -0.5

factorial$odds[factorial$competition==2]<- 0

factorial$odds[factorial$competition==3]<- 0.5

log.inmot1.1 <- 1/(1+exp(-(factorial$odds+
                             
                             factorial$ind_random_effect)))

factorial$inmot1 <- rbinom(playerEvents,1,log.inmot1.1)
factorial$inmot2 <- rbinom(playerEvents,1,log.inmot1.1)

# Remove scrap variables



factorial <- subset (factorial, select = -c(odds, ind_random_effect, group_random_effect))

# Recode tools into the predicted choice according to different theories in:
# "NGS2 WITNESS Cycle 3 FINAL.xlsx", "Tools" tab
# EUT predictions

factorial$toolsEUT <- 0
factorial$toolsEUT[factorial$tools==5] <- 1
factorial$toolsEUT[factorial$tools==7] <- 1
factorial$toolsEUT<-factor(factorial$toolsEUT)

# PT predictions

factorial$toolsPT <- 1
factorial$toolsPT[factorial$tools==8] <- 0
factorial$toolsPT<-factor(factorial$toolsPT)

# CPT+Expectancy predictions

factorial$toolsCPTEXP <- 1
factorial$toolsCPTEXP[factorial$tools==1] <- 0
factorial$toolsCPTEXP[factorial$tools==2] <- 0
factorial$toolsCPTEXP[factorial$tools==5] <- 0
factorial$toolsCPTEXP<-factor(factorial$toolsCPTEXP)

# Relative Risk (Difference in variances between "certain" and "innovative" options)

factorial$risk[factorial$tools==1] <- 0.23
factorial$risk[factorial$tools==2] <- 0.23
factorial$risk[factorial$tools==3] <- 0.06
factorial$risk[factorial$tools==4] <- -0.17
factorial$risk[factorial$tools==5] <- 0
factorial$risk[factorial$tools==6] <- 0.05
factorial$risk[factorial$tools==7] <- 0.24
factorial$risk[factorial$tools==8] <- 0

# Ratio of expected values for innovative option according to probability theory

factorial$prb[factorial$tools==1] <- 1.10
factorial$prb[factorial$tools==2] <- 1.10
factorial$prb[factorial$tools==3] <- 1.39
factorial$prb[factorial$tools==4] <- 1.26
factorial$prb[factorial$tools==5] <- 1.03
factorial$prb[factorial$tools==6] <- 1.03
factorial$prb[factorial$tools==7] <- 7.43
factorial$prb[factorial$tools==8] <- 0


# Turn all variables into factors as necessary

factorial2<-factorial
factorial[]<-lapply(factorial, factor)
factorial$risk<-factorial2$risk
factorial$prb<-factorial2$prb
factorial$conformity.null<-factorial2$conformity.null
factorial$conformity.1<-factorial2$conformity.1

#Density will finally be a continuous variable between 0 and 1

factorial$density<-as.numeric(factorial2$density)/max(as.numeric(factorial2$density))

#Structure

factorial$structureHie <- 0
factorial$structureHie[factorial$centralization==0] <- 1
factorial$structureCel <- 0
factorial$structureCel[factorial$centralization==1] <- 1
factorial$structureNet <- 0
factorial$structureNet[factorial$centralization==2] <- 1

# Generate conformity measure

factorial$groupRound <- as.numeric(factorial$group)*100+as.numeric(factorial$round)

factorialNum <- factorial
factorialNum[] <- lapply(factorial, as.numeric)
factorialNum$inmot1 <- factorialNum$inmot1-1
factorialNum$inmot2 <- factorialNum$inmot2-1
factorialNum$grmot1 <- ave(factorialNum$inmot1, factorial$groupRound)
factorialNum$grmot2 <- ave(factorialNum$inmot2, factorial$groupRound)
factorialNum$conformity<-ifelse(
  factorialNum$grmot1>0.5 & factorialNum$inmot1==0 & factorialNum$inmot2==1,1,
  ifelse(
    factorialNum$grmot1<0.5 & factorialNum$inmot1==1 & factorialNum$inmot2==0, 1, 0))

# Generate aggregate dataset

factorialGroup <-aggregate(factorialNum, 
                           by=list(factorialNum$groupRound), 
                           FUN=mean, 
                           na.rm=TRUE)
factorialGroup$nPlayers <- aggregate(player ~ groupRound, data = factorialNum, FUN = length)$player
factorialGroup$unanimity <- (factorialGroup$nPlayers-1)/factorialGroup$nPlayers
factorialGroup$unanimous <-ifelse(factorialGroup$grmot1 == factorialGroup$unanimity,1,0)

factorialGroup2 <- factorialGroup
factorialGroup2[] <- lapply(factorialGroup, factor)
factorialGroup2$conformity <- as.numeric(factorialGroup$conformity)
factorialGroup2$grmot1 <- as.numeric(factorialGroup$grmot1)
factorialGroup2$grmot2 <- as.numeric(factorialGroup$grmot2)
factorialGroup2$risk <- as.numeric(factorialGroup$risk)
factorialGroup2$innovation <- as.numeric(factorialGroup$innovation)-1
factorialGroup <- factorialGroup2


# Merge conformity and group motivation vars back with individual level data

factorial <- merge(factorial, 
                   factorialGroup[c("grmot1", "grmot2", "conformity", "groupRound")], 
                   by="groupRound")

# Remove scrap objects

rm(list = ls()[ls()%in%c('factorial2', 'group', 'players')])




