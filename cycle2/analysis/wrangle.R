## Experiment processing file, re-factored to batch process multiple games. 
## Created by Pablo Diego Rosell, PhD, for Gallup inc. in October 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Read in logs

filelist <- list.files(pattern = ".*.txt")
datalist <- lapply(filelist, function(x) readLines(x))
nElements <- length(datalist)

# Extract choices in the shop

toolChoices <- lapply(datalist, function(x) x[grep("StartVotation", x)])
nRounds <- length(toolChoices[[1]])

# Extract match id to create group variable

matchid <- lapply(datalist, function(x) x[grep("StartMatch", x)])
matchid <- lapply(matchid, function(x) as.numeric(strsplit(x,',',fixed=TRUE)[[1]][3]))
matchid <- lapply(matchid, function(x) rep(x, nRounds))

# Extract values for block-randomized hypotheses

gameSettings <- lapply(datalist, function(x) x[grep("SetupMatch", x)])

# Extract value of h1.1. 

str1.1 <- c("None", "Weak", "Normal", "Strong")
h1.1 <- lapply(gameSettings, function(x) c(0:3)[sapply(str1.1, grepl, x)])
h1.1 <- lapply(h1.1, function(x) rep(x, nRounds))

# Extract value of h2.1. 

str2.1 <- c("False", "True")
h2.1 <- lapply(gameSettings, function(x) c(0:1)[sapply(str2.1, grepl, x)])
h2.1 <- lapply(h2.1, function(x) rep(x, nRounds))

# Extract value of h3.1. 
###BASED ON FILE FROM JEFF

# Extract value of h3.2.

str3.2 <- c("LowTolerance", "HighTolerance")
h3.2 <- lapply(gameSettings, function(x) c(0:1)[sapply(str3.2, grepl, x)])
h3.2 <- lapply(h3.2, function(x) rep(x, nRounds))

# Extract value of h3.3.

str3.3 <- c("highStatus_highLegitimacy", "highStatus_lowLegitimacy", 
            "lowStatus_highLegitimacy", "lowStatus_lowLegitimacy")
h3.3 <- lapply(gameSettings, function(x) c(0:3)[sapply(str3.3, grepl, x)])
h3.3 <- lapply(h3.3, function(x) rep(x, nRounds))

# Extract value of h3.4.

str3.4 <- c("LowTransformational", "HighTransformational")
h3.4 <- lapply(gameSettings, function(x) c(0:1)[sapply(str3.4, grepl, x)])
h3.4 <- lapply(h3.4, function(x) rep(x, nRounds))

# Assign values of h1.3 (Fixed at Rounds 4 & 9 = 1, Rounds = 5 & 7 = 2, else = 0)

h1.3 <- c(0, 0, 0, 1, 2, 0, 2, 0, 1, 0, 0, 0, 0)
h1.3 <-rep(list(h1.3), nElements)

# Assign values of h3.5 (Fixed at Round 1 = 0, Rounds 6, 8 = 2, else = 1)

h3.5 <- c(0, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1)
h3.5 <- rep(list(h3.5), nElements)

# Extract value of tool choice for each round

card01  <- c("TNTbarrel,SatchelCharge")
card02  <- c("BlackPowder,Dynamite")
card03  <- c("BlackPowder,RDX")
card04  <- c("RDX,Dynamite")
card05  <- c("Mine1,Mine2")
card06  <- c("Mine4,Mine3")
card071  <- c("Mine1,BlackPowder")
card072  <- c("Mine2,BlackPowder")
card081  <- c("Mine3,BlackPowder")
card082  <- c("Mine4,BlackPowder")
card09  <- c("TNTbarrel,Dynamite")
card10  <- c("BlackPowder,SatchelCharge")
card11  <- c("SatchelCharge,RDX")
card12  <- c("Dynamite,SatchelCharge")

tools<-lapply(toolChoices, function(x) ifelse(grepl(card01, x) == TRUE, 1, NA))

for (i in 1:nElements) {
  tools[[i]]<-ifelse(grepl(card02, toolChoices[[i]]) == TRUE, 2, tools[[i]])
  tools[[i]]<-ifelse(grepl(card03, toolChoices[[i]]) == TRUE, 3, tools[[i]])
  tools[[i]]<-ifelse(grepl(card04, toolChoices[[i]]) == TRUE, 4, tools[[i]])
  tools[[i]]<-ifelse(grepl(card05, toolChoices[[i]]) == TRUE, 5, tools[[i]])
  tools[[i]]<-ifelse(grepl(card06, toolChoices[[i]]) == TRUE, 6, tools[[i]])
  tools[[i]]<-ifelse(grepl(card071, toolChoices[[i]]) == TRUE, 7, tools[[i]])
  tools[[i]]<-ifelse(grepl(card072, toolChoices[[i]]) == TRUE, 7, tools[[i]])
  tools[[i]]<-ifelse(grepl(card081, toolChoices[[i]]) == TRUE, 8, tools[[i]])
  tools[[i]]<-ifelse(grepl(card082, toolChoices[[i]]) == TRUE, 8, tools[[i]])
  tools[[i]]<-ifelse(grepl(card09, toolChoices[[i]]) == TRUE, 9, tools[[i]])
  tools[[i]]<-ifelse(grepl(card10, toolChoices[[i]]) == TRUE, 10, tools[[i]])
  tools[[i]]<-ifelse(grepl(card11, toolChoices[[i]]) == TRUE, 11, tools[[i]])
  tools[[i]]<-ifelse(grepl(card12, toolChoices[[i]]) == TRUE, 12, tools[[i]])
  }

# Assign hypothesis and hypothesis level by round, based on tool choices

round <- rep(list(1:nRounds), nElements)
dummyList <- rep(list(rep(NA, nRounds)), nElements)

# THE LOOPS and IF>ELSE STATEMENTS BELOW ARE RECURRENT AND COULD BE SIMPLIFIED WITH A FUNCTION
# h2.2

h2.2<-dummyList
for (j in (1:nElements)) {
  for (i in (1:nRounds)) {
    if (grepl(card01, toolChoices[[j]][i]) == TRUE) {
      h2.2[[j]][i]<-0
      } else if (grepl(card02, toolChoices[[j]][i]) == TRUE) {
        h2.2[[j]][i]<-1
      }
    }
  }

# h2.3

h2.3<-dummyList
for (j in 1:nElements) {
  for (i in (1:nRounds)) {
    if (grepl(card01, toolChoices[[j]][i]) == TRUE) {
      h2.3[[j]][i]<-0
    } else if (grepl(card03, toolChoices[[j]][i]) == TRUE) {
      h2.3[[j]][i]<-1
    }
  }
}

# h2.4

h2.4<-dummyList
for (j in 1:nElements) {
  for (i in (1:nRounds)) {
    if (grepl(card04, toolChoices[[j]][i]) == TRUE) {
      h2.4[[j]][i]<-0
    } else if (grepl(card01, toolChoices[[j]][i]) == TRUE) {
      h2.4[[j]][i]<-1
    }
  }
}

# h2.5

h2.5<-dummyList
for (j in 1:nElements) {
  for (i in (1:nRounds)) {
    if (grepl(card05, toolChoices[[j]][i]) == TRUE) {
      h2.5[[j]][i]<-0
    } else if (grepl(card06, toolChoices[[j]][i]) == TRUE) {
      h2.5[[j]][i]<-1
    }
  }
}


# h2.6

h2.6<-dummyList
for (j in 1:nElements) {
  for (i in (1:nRounds)) {
    if (grepl(card071, toolChoices[[j]][i]) == TRUE | grepl(card072, toolChoices[[j]][i]) == TRUE) {
      h2.6[[j]][i]<-0
    } else if (grepl(card081, toolChoices[[j]][i]) == TRUE | grepl(card082, toolChoices[[j]][i]) == TRUE) {
      h2.6[[j]][i]<-1
    }
  }
}

# Control 1 

control1 <- dummyList
for (j in 1:nElements) {
  for (i in (1:nRounds)) {
    if (grepl(card09, toolChoices[[j]][i]) == TRUE) {
      control1[[j]][i]<-0
    } else if (grepl(card10, toolChoices[[j]][i]) == TRUE) {
      control1[[j]][i]<-1
    }
  }
}

# Control 2

control2 <- dummyList
for (j in 1:nElements) {
  for (i in (1:nRounds)) {
    if (grepl(card11, toolChoices[[j]][i]) == TRUE) {
      control2[[j]][i]<-0
    } else if (grepl(card12, toolChoices[[j]][i]) == TRUE) {
      control2[[j]][i]<-1
    }
  }
}

# Extract innovation outcome, based on tool choice. 

leaderVotes <- lapply(datalist, function(x) x[grep("LeaderSelection", x)])
leaderChoice <- lapply(leaderVotes, function(x) strsplit(x,',',fixed=TRUE))
leaderChoice <- lapply(leaderChoice, function(x) sapply(x, "[[", 3))


innovation <- dummyList

for (j in 1:nElements) {
  for (i in (1:nRounds)) {
    if ((grepl(card01, toolChoices[[j]][i]) == TRUE) & (grepl("SatchelCharge", leaderVotes[[j]][i]) == TRUE)) {
      innovation[[j]][i]<-1
    }
    else if ((grepl(card02, toolChoices[[j]][i]) == TRUE) & (grepl("Dynamite", leaderVotes[[j]][i]) == TRUE)) {
      innovation[[j]][i]<-1
    }
    else if ((grepl(card03, toolChoices[[j]][i]) == TRUE) & (grepl("RDX", leaderVotes[[j]][i]) == TRUE)) {
      innovation[[j]][i]<-1
    }
    else if ((grepl(card04, toolChoices[[j]][i]) == TRUE) & (grepl("RDX", leaderVotes[[j]][i]) == TRUE)) {
      innovation[[j]][i]<-1
    }
    else if ((grepl(card05, toolChoices[[j]][i]) == TRUE) & (grepl("Mine2", leaderVotes[[j]][i]) == TRUE)) {
      innovation[[j]][i]<-1
    }
    else if ((grepl(card06, toolChoices[[j]][i]) == TRUE) & (grepl("Mine4", leaderVotes[[j]][i]) == TRUE)) {
      innovation[[j]][i]<-1
    }
    else if ((grepl(card071, toolChoices[[j]][i]) == TRUE) & (grepl("Mine1", leaderVotes[[j]][i]) == TRUE)) {
      innovation[[j]][i]<-1
    }
    else if ((grepl(card081, toolChoices[[j]][i]) == TRUE) & (grepl("Mine3", leaderVotes[[j]][i]) == TRUE)) {
      innovation[[j]][i]<-1
    }
    else if ((grepl(card072, toolChoices[[j]][i]) == TRUE) & (grepl("Mine2", leaderVotes[[j]][i]) == TRUE)) {
      innovation[[j]][i]<-1
    }
    else if ((grepl(card082, toolChoices[[j]][i]) == TRUE) & (grepl("Mine4", leaderVotes[[j]][i]) == TRUE)) {
      innovation[[j]][i]<-1
    }
    else if (grepl(card09, toolChoices[[j]][i]) == TRUE) {
      innovation[[j]][i]<-NA
    }
    else if (grepl(card10, toolChoices[[j]][i]) == TRUE) {
      innovation[[j]][i]<-NA
    }
    else if (grepl(card11, toolChoices[[j]][i]) == TRUE) {
      innovation[[j]][i]<-NA
    }
    else if (grepl(card12, toolChoices[[j]][i]) == TRUE) {
      innovation[[j]][i]<-NA
    }
    else
      {
        innovation[[j]][i]<-0
      }
  }
}

# Post-game survey

survey <- lapply(datalist, function(x) x[grep("SurveyResponse", x)])
survey <- lapply(survey, function(x) strsplit(x,',',fixed=TRUE))
playerID <- lapply(survey, function(x) sapply(x, "[[", 3))
surveyQuestions<-lapply(survey, function(x) sapply(x, "[[", 4))
surveyResponses<-lapply(survey, function(x) sapply(x, "[[", 5))

CSE <- lapply(surveyResponses, function(x) mean(as.numeric(as.character(x))))
CSE <- lapply(CSE, function(x) rep(x, nRounds))

# Merge all variables into a single frame

gamesList <- list(matchid, round, h1.1, h1.3, h2.1, h2.2, h2.3, h2.4, h2.5, h2.6, 
              h3.2,h3.3,h3.4,h3.5, tools, innovation, CSE, leaderChoice)
gamesData  <-  as.data.frame(matrix(unlist(gamesList), nrow=nRounds*nElements))
colnames(gamesData) <- c("matchid", "round", "h1.1", "h1.3", "h2.1", "h2.2", "h2.3", "h2.4", "h2.5", "h2.6", 
"h3.2","h3.3","h3.4","h3.5", "tools", "innovation", "CSE", "leaderChoice") 

# Load metadata
# Downloads automatically when visting this URL https://volunteerscience.com/gallup/boomtown_metadata/
# Tried to automatize download process using "download.file" command, but not working properly. 
# Copy and paste "boomtown_metadata.csv" from "downloads" folder to working directory, then execute below. 

metadata <- read.csv("boomtown_metadata.csv")
metadata$h3.1[metadata$group..high.low.=="Ambiguity Low"] <- 0
metadata$h3.1[metadata$group..high.low.=="Ambiguity High"] <- 1
metadata <- metadata[c("matchID", "h3.1")]
gamesData  <-  merge(gamesData, metadata, by.x = "matchid", by.y = "matchID", all.x=TRUE)
write.csv(gamesData, file = "gamesData.csv")
rm(list = ls(all = TRUE))
