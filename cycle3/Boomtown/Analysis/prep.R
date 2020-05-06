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
