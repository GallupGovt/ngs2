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

library (dplyr)
factorial2$settings <- as.numeric(levels(factorial2$settingsNum))[factorial2$settingsNum]
factorial2[,"framing"] <- case_when(
  factorial2[,"round"] %in% c(1, 4, 7) ~ 0,
  factorial2[,"settings"] %% 2==1 & 
    factorial2[,"round"] %in% c(2, 6, 8, 11, 12) & 
    (factorial2$toolsLabel == "RDX,Dynamite" |
       factorial2$toolsLabel == "Mine1,BlackPowder" |
       factorial2$toolsLabel == "Mine2,BlackPowder" |
       factorial2$toolsLabel == "Mine3,BlackPowder" |
       factorial2$toolsLabel == "Mine4,BlackPowder") ~ 1, 
  factorial2[,"settings"] %% 2==1 & 
    factorial2[,"round"] %in% c(2, 6, 8, 11, 12) & 
    (factorial2$toolsLabel == "TNTbarrel,SatchelCharge" |
       factorial2$toolsLabel == "BlackPowder,Dynamite" |
       factorial2$toolsLabel == "BlackPowder,RDX" |
       factorial2$toolsLabel == "Mine1,Mine2" |
       factorial2$toolsLabel == "Mine3,Mine4") ~ 2, 
  factorial2[,"settings"] %% 2==1 & 
    factorial2[,"round"] %in% c(3, 5, 9, 10, 13) & 
    (factorial2$toolsLabel == "RDX,Dynamite" |
       factorial2$toolsLabel == "Mine1,BlackPowder" |
       factorial2$toolsLabel == "Mine2,BlackPowder" |
       factorial2$toolsLabel == "Mine3,BlackPowder" |
       factorial2$toolsLabel == "Mine4,BlackPowder") ~ 2, 
  factorial2[,"settings"] %% 2==1 & 
    factorial2[,"round"] %in% c(3, 5, 9, 10, 13) & 
    (factorial2$toolsLabel == "TNTbarrel,SatchelCharge" |
       factorial2$toolsLabel == "BlackPowder,Dynamite" |
       factorial2$toolsLabel == "BlackPowder,RDX" |
       factorial2$toolsLabel == "Mine1,Mine2" |
       factorial2$toolsLabel == "Mine3,Mine4") ~ 1, 
  factorial2[,"settings"] %% 2==0 & 
    factorial2[,"round"] %in% c(3, 5, 9, 13) & 
    (factorial2$toolsLabel == "RDX,Dynamite" |
       factorial2$toolsLabel == "Mine1,BlackPowder" |
       factorial2$toolsLabel == "Mine2,BlackPowder" |
       factorial2$toolsLabel == "Mine3,BlackPowder" |
       factorial2$toolsLabel == "Mine4,BlackPowder") ~ 1, 
  factorial2[,"settings"] %% 2==0 & 
    factorial2[,"round"] %in% c(3, 5, 9, 13) & 
    (factorial2$toolsLabel == "TNTbarrel,SatchelCharge" |
       factorial2$toolsLabel == "BlackPowder,Dynamite" |
       factorial2$toolsLabel == "BlackPowder,RDX" |
       factorial2$toolsLabel == "Mine1,Mine2" |
       factorial2$toolsLabel == "Mine3,Mine4") ~ 2, 
  factorial2[,"settings"] %% 2==0 & 
    factorial2[,"round"] %in% c(2, 6, 8, 10, 11, 12) & 
    (factorial2$toolsLabel == "RDX,Dynamite" |
       factorial2$toolsLabel == "Mine1,BlackPowder" |
       factorial2$toolsLabel == "Mine2,BlackPowder" |
       factorial2$toolsLabel == "Mine3,BlackPowder" |
       factorial2$toolsLabel == "Mine4,BlackPowder") ~ 2, 
  factorial2[,"settings"] %% 2==0 & 
    factorial2[,"round"] %in% c(2, 6, 8, 10, 11, 12) & 
    (factorial2$toolsLabel == "TNTbarrel,SatchelCharge" |
       factorial2$toolsLabel == "BlackPowder,Dynamite" |
       factorial2$toolsLabel == "BlackPowder,RDX" |
       factorial2$toolsLabel == "Mine1,Mine2" |
       factorial2$toolsLabel == "Mine3,Mine4") ~ 1) 


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
factorialGroup2$innovation <- as.numeric(factorialGroup$innovation)-1
factorialGroup <- factorialGroup2
factorialGroup$innovation <- factorialGroup$innovation+2
