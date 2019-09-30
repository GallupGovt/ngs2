#####################################################################################
## Experiment processing file, re-factored to batch process multiple games.
## Created by Pablo Diego Rosell and Ying Han, Gallup.Inc, in September 2019
## For any questions, contact pablo_diego-rosell@gallup.co.uk or ying_han@gallup.com
#####################################################################################

# Set up enviroment ----
rm(list = ls())
library(dplyr)

# Define constants -----
# directory to input (storing game logs, metadata, and survey results) and output folder
dd_input  <- "//gallup/dod_clients/DARPA_NGS2/CONSULTING/Ying_Han/Data_Wrangling_Cycle_3/GameLogs_Metadata_SurveyResults" 
dd_output <- "//gallup/dod_clients/DARPA_NGS2/CONSULTING/Ying_Han/Data_Wrangling_Cycle_3" 

# fieldwork start date
field_start <- "01-01-2019"

# game events of interest
events <- c("SetupMatch", 
            "PlayerConnection", 
            "StartMatch", 
            "StartVotation", 
            "PlayerFinalVotation", 
            "FinalItemSelected", 
            "GameSuspended", 
            "EndMatch", 
            "PlayerDisconnection")

# variable excluded in part 6
vars <- c("roundid", "roundid_short", "toolsLabel", "FinalItemSelected", "PlayerVote1",
          "PlayerVote2", "tools", "innovation", "inmot1", "inmot2", "eligible", "framing", "GroupVote1", "GroupVote2", 
          "matchid", "playerid")

# Define functions ----

# function to extract information from one game log file
gamelog_process <- function(data){

  # subset logs related to events of interest
  data <- data[grep(paste(events, collapse ="|"), data)]
  
  # matchid, matchDate
  match_info <- unlist(strsplit(data[grep("StartMatch", data)], ","))
  matchid <- as.numeric(match_info[3])
  matchDate <- as.Date(match_info[2], format = "%m/%d/%Y %I:%M:%S %p")
  
  # competition, timeUncertainty, support
  match_setting <- unlist(strsplit(data[grep("SetupMatch",data)], split=","))
  
  matchSetting1Label <- match_setting[3] 
  timeUncertaintyLabel <- match_setting[4] 
  matchSetting3Label <- match_setting[5]
  competitionLabel <- match_setting[6]
  supportLabel <- match_setting[7]
  
  timeUncertainty <- case_when(timeUncertaintyLabel == "False" ~ 1, 
                               timeUncertaintyLabel == "True" ~ 0,
                               TRUE ~ NA_real_)
  competition <- case_when(competitionLabel == "None" ~ 0,
                           competitionLabel == "Weak" ~ 1,
                           competitionLabel == "Normal" ~ 2,
                           competitionLabel == "Strong" ~ 3,
                           TRUE ~ NA_real_)
  support <- case_when(supportLabel == "HighStatus_HighLegitimacy" ~ 0, 
                       supportLabel == "LowStatus_HighLegitimacy" ~ 1)
  
  # nConnected, playerid
  players_info <- strsplit(data[grep("PlayerConnection", data)], split=",")
  playerid <- unique(unlist(lapply(players_info, function(x){x[3]})))
  nConnected <- length(playerid)
  
  # nRound
  nRound <- length(grep("StartVotation", data))
  
  # set up initial data frame
  df <- data.frame(
    matchid = rep(matchid, nRound * nConnected),
    matchDate = rep(matchDate, nRound * nConnected),
    matchSetting1Label = rep(matchSetting1Label, nRound * nConnected),
    matchSetting3Label = rep(matchSetting3Label, nRound * nConnected),
    competitionLabel = rep(competitionLabel, nRound * nConnected),
    competition = rep(competition, nRound * nConnected),
    timeUncertaintyLabel = rep(timeUncertaintyLabel, nRound * nConnected),
    timeUncertainty = rep(timeUncertainty, nRound * nConnected),
    supportLabel = rep(supportLabel, nRound * nConnected),
    support = rep(support, nRound * nConnected),
    nRound = rep(nRound, nRound * nConnected),
    roundid_short = unlist(lapply(1:nRound, rep, nConnected)),
    roundid = unlist(lapply(matchid*100 + 1:nRound, rep, nConnected)),
    nConnected = rep(nConnected, nRound * nConnected),
    playerid = rep(playerid, nRound)
  )
  
  # toolsLabel, PlayerVote1, PlayerVote2, FinalItemSelected
  roundid <- matchid * 100
  round_info <- strsplit(data[grep("StartVotation|PlayerFinalVotation|FinalItemSelected", data)], split=",")
  
  for (log in round_info){
    if (log[1] == "StartVotation"){
      roundid <- roundid + 1
      players_voted <- NULL
      df[which(df$roundid==roundid),"toolsLabel"] <- paste(log[3:4], collapse = ",")
    }
    
    if (log[1] == "PlayerFinalVotation"){
      playerid <- log[3]
      player_vote <- log[4]
      if(!playerid %in% players_voted){
        df[which(df$roundid == roundid & df$playerid == playerid), "PlayerVote1"] = log[4]
        players_voted <- c(players_voted, playerid)
      }
      else {
        df[which(df$roundid == roundid & df$playerid == playerid), "PlayerVote2"] = log[4]}
    }
    
    if (log[1] == "FinalItemSelected"){
      df[which(df$roundid==roundid),"FinalItemSelected"] <- log[3]
    }
  }
  
  # tools
  df$tools <- case_when(
    df$toolsLabel == "TNTbarrel,SatchelCharge" ~ 1, 
    df$toolsLabel == "BlackPowder,Dynamite" ~ 2, 
    df$toolsLabel == "BlackPowder,RDX" ~ 3, 
    df$toolsLabel == "RDX,Dynamite" ~ 4, 
    df$toolsLabel == "Mine1,Mine2" ~ 5, 
    df$toolsLabel == "Mine4,Mine3" ~ 6, 
    df$toolsLabel == "Mine1,BlackPowder" ~ 7,
    df$toolsLabel == "Mine2,BlackPowder" ~ 7, 
    df$toolsLabel == "Mine3,BlackPowder" ~ 8, 
    df$toolsLabel == "Mine4,BlackPowder" ~ 8, 
    df$toolsLabel == "Dynamite,TNTbarrel" ~ 9, 
    df$toolsLabel == "BlackPowder,SatchelCharge" ~ 10, 
    df$toolsLabel == "SatchelCharge,RDX" ~ 11, 
    df$toolsLabel == "Dynamite,SatchelCharge" ~ 12, 
    TRUE ~ NA_real_
  )
 
  # eligible: at least one round before agem suspend
  suspend <- ifelse(length(grep("GameSuspended", data))==0,length(data), min(grep("GameSuspended", data)))
  vote_1st <- min(grep("StartVotation", data))
  df$eligible <- suspend > vote_1st
  
  return(df)
}

# Part 1: Read data into R ----
gamelogs_files <- list.files(dd_input, "*.txt")
metadata_file  <- list.files(dd_input, "*metadata.csv")
survey_files   <- list.files(dd_input, "^survey")

# Part 2: game logs cleaning ----
gamelogs<- lapply(gamelogs_files, function(file) {readLines(paste(dd_input, file, sep="/"))})
game_data <- bind_rows(lapply(gamelogs, gamelog_process))



# Subset datalist according to the two conditions above:
# 1. Played during fielding period 
# 2. Had at least one "LeaderSelection") event before the game was suspended
game_data <- game_data[game_data$matchDate >= as.Date(field_start, format="%m-%d-%Y") 
                       & game_data$eligible == TRUE,]

# Part 3: metadata cleaning ----
metadata <- read.csv(paste(dd_input, metadata_file, sep="/"), header = T, stringsAsFactors = FALSE)

# tolerance
metadata[,"toleranceLabel"] <- metadata[,"group"]
metadata[,"tolerance"] <- ifelse(metadata[,"toleranceLabel"] == 'Ambiguity Low', 0, 1)

# Part 4: Merging game data and metadata ----
game_data <- merge(game_data, 
                   metadata[,!grepl("group", names(metadata))], 
                   by.x="matchid", by.y="matchID", all.x = TRUE)

# framing
# 1. when settingsNum is even: framing is 0 for rounds 1, 4, 7; 1 for rounds 2, 6, 8, 10, 12, 13; 2 for rounds 3, 5, 9, 11.
# 2. when settingsNum is odd : framing is 0 for rounds 1, 4, 7; 1 for rounds 3, 5, 6, 9, 10, 13; 2 for rounds 2, 6, 8, 11, 12.
game_data[,"framing"] <- case_when(
  game_data[,"settingsNum"] %% 2 == 0 & game_data[,"roundid_short"] %in% c(1, 4, 7) ~ 0,
  game_data[,"settingsNum"]  %% 2 == 0 & game_data[,"roundid_short"] %in% c(2, 6, 8, 10, 12, 13) ~ 1,
  game_data[,"settingsNum"]  %% 2 == 0 & game_data[,"roundid_short"] %in% c(3, 5, 9, 11) ~ 2,
  game_data[,"settingsNum"]  %% 2 == 1 & game_data[,"roundid_short"] %in% c(1, 4, 7) ~ 0,
  game_data[,"settingsNum"]  %% 2 == 1 & game_data[,"roundid_short"] %in% c(3, 5, 9, 10, 13) ~ 1,
  game_data[,"settingsNum"]  %% 2 == 1 & game_data[,"roundid_short"] %in% c(2, 6, 8, 11, 12) ~ 2,
  TRUE ~ NA_real_
)

# group vote
group_vote <- aggregate(x = game_data[, c("PlayerVote1","PlayerVote2")], 
                        by = list(roundid = game_data$roundid), 
                        FUN = function(x){
                          tb = table(x)
                          if(length(tb)>0){
                            tb = formatC(prop.table(table(x)) * 100, format="d")
                            result = paste(paste0(names(tb)," ", tb, "%"), collapse = ", ")
                          } else {result = NA}
                          return(result)
                        })
names(group_vote) <- c("roundid", "GroupVote1", "GroupVote2")
game_data <- merge(game_data, group_vote, by="roundid", all.x=TRUE)

# Back-code 'FinalItemSelected' for first choice between two mines (rounds 10 & 12)

game_data_mines <- game_data[game_data$roundid_short == 11 | game_data$roundid_short == 13,
                             c("roundid", "toolsLabel")]
game_data_mines$FinalItemSelected_2 <- case_when(
  (game_data_mines$toolsLabel == "Mine1,BlackPowder") ~ "Mine1",
  (game_data_mines$toolsLabel == "Mine2,BlackPowder") ~ "Mine2",
  (game_data_mines$toolsLabel == "Mine3,BlackPowder") ~ "Mine3",
  (game_data_mines$toolsLabel == "Mine4,BlackPowder") ~ "Mine4")
game_data_mines$roundid <- game_data_mines$roundid-1
game_data_mines <- game_data_mines[, c("roundid", "FinalItemSelected_2")]
game_data <- merge(game_data,game_data_mines,by="roundid", all = TRUE)

game_data$FinalItemSelected <- ifelse(is.na(game_data$FinalItemSelected), 
                                      game_data$FinalItemSelected_2, 
                                      game_data$FinalItemSelected)
game_data <- subset(game_data, select=-FinalItemSelected_2)

# Motivation to Innovate Outcomes            

motivationCoder <- function (gameData, voteVar) {
  motivationVar <- case_when(
    (gameData$toolsLabel == "TNTbarrel,SatchelCharge" & gameData[voteVar] == "SatchelCharge") | 
      (gameData$toolsLabel == "BlackPowder,Dynamite" & gameData[voteVar] == "Dynamite") |
      (gameData$toolsLabel == "BlackPowder,RDX" & gameData[voteVar] == "RDX") |
      (gameData$toolsLabel == "RDX,Dynamite" & gameData[voteVar] == "RDX") |
      (gameData$toolsLabel == "Mine1,Mine2" & gameData[voteVar] == "Mine2") |
      (gameData$toolsLabel == "Mine4,Mine3" & gameData[voteVar] == "Mine4") |
      (gameData$toolsLabel == "Mine1,BlackPowder" & gameData[voteVar] == "Mine1") |
      (gameData$toolsLabel == "Mine2,BlackPowder" & gameData[voteVar] == "Mine2") |
      (gameData$toolsLabel == "Mine3,BlackPowder" & gameData[voteVar] == "Mine3") |
      (gameData$toolsLabel == "Mine4,BlackPowder" & gameData[voteVar] == "Mine4") ~ 1,
    gameData$toolsLabel == "Dynamite,TNTbarrel" |
      gameData$toolsLabel == "BlackPowder,SatchelCharge" |
      gameData$toolsLabel == "SatchelCharge,RDX" |
      gameData$toolsLabel == "Dynamite,SatchelCharge"  ~ NA_real_,
    TRUE ~ 0
  )
  return(motivationVar)
}

game_data$inmot1 <- motivationCoder (game_data, "PlayerVote1") 
game_data$inmot2 <- motivationCoder (game_data, "PlayerVote2")
game_data$innovation <- motivationCoder (game_data, "FinalItemSelected")

# Organizational structure variables (hard-coded per factorial matrix, pending metadata update from Jeff)

for (i in seq(1, 96, by=3)){ 
  game_data$structure[game_data$settingsNum==i]<- "Hierarchical"
  game_data$centralization[game_data$settingsNum==i]<- "Medium"
  game_data$leaderWeight[game_data$settingsNum==i]<- "High"
  
  game_data$structure[game_data$settingsNum==i+1]<- "Cellular"
  game_data$centralization[game_data$settingsNum==i+1]<- "High"
  game_data$leaderWeight[game_data$settingsNum==i+1]<- "Low"
  
  game_data$structure[game_data$settingsNum==i+2]<- "Network"
  game_data$centralization[game_data$settingsNum==i+2]<- "Low"
}

# Network density variable (hard-coded per json *see "tweaker.R")

roleSampler<- function() {
  nRoles  <- c(0, 1, 2, 3, 4, 5, 6, 7)
  roles  <- c("Engineer", "LeadShotfirer", "LeadHewer", "LeadScout", "Shotfirer", "Hewer", "Scout")
  sampledRoles<-paste (sample (roles, sample(nRoles, 1)), collapse=',')
  sampledRoles<-gsub("(\\w+)", '"\\1"', sampledRoles)
  return(sampledRoles)
}

roleMuted <- data.frame (matrix(ncol = 7, nrow = 96))
roleMuted[,1]<-as.numeric(roleMuted[,1])
for (i in 1:96){
  roleMuted[i,1] <- i
  for (j in 2:7){  
    set.seed((i*10)+j)
    roleMuted[i,j]  <- roleSampler()
  }  
}

colnames(roleMuted) <- c("settingsNum", "Muted1", "Muted2", "Muted3", "Muted4", "Muted5", "Muted6")
game_data <- merge(game_data,roleMuted,by= "settingsNum", all = TRUE)

# format date/time fields
game_data$date.time<-as.POSIXct(game_data$date.time)

# output data
write.csv(game_data, paste(dd_output, 'game_data.csv', sep = '/'), row.names = FALSE)
factorial <- game_data #Assign to object named as used in analytics

# Part 5: survey data cleaning ----
survey_data <- lapply(survey_files, function(file){read.csv(paste(dd_input, file, sep="/"), skip = 1, header = 1, stringsAsFactors = F)})
names(survey_data) <- gsub(".csv", "", survey_files)

# recode PlayerId values in each survey response files
survey_data <- lapply(survey_data, function(x){
  x[,"PlayerId"] <- gsub("boomtown-", "", x[,"PlayerId"])
  return(x)
})

# remove columns with no values
survey_data <- lapply(survey_data, function(x){ x[,-c(which(colSums(!is.na(x)) == 0))]})

# remove the column "Raw Data"
survey_data <- lapply(survey_data, function(x){ x[,!grepl("Raw.Data", names(x))]})

# rename question variables
survey_data <- lapply(1:2, function(i){
  data <- survey_data[[i]]
  start <- min(grep(".", names(data), fixed=T))
  end <- length(names(data))
  names(data)[start:end] <- paste0("Q", 1:length(start:end), "_", i)
  return(data)
})

# merge survey_results_1 and survey_results_2
survey_data <- merge(survey_data[[grep("_1", survey_files)]], 
                     survey_data[[grep("_2", survey_files)]], 
                     by="PlayerId", all.x = T, suffixes = c("_1", "_2"))

# Part 6: merge game data with survey data at the match level ----
game_data_aggr <- aggregate(game_data[,!grepl(paste(vars, collapse = "|"), names(game_data))],
                            by = list(matchid = game_data[,"matchid"], playerid = game_data[,"playerid"]),
                            FUN = unique)

game_survey_data <- merge(game_data_aggr, survey_data, by.x = "playerid", by.y = "PlayerId", all.x = T)

# output data
write.csv(game_survey_data, paste(dd_output, 'survey_data.csv', sep = '/'), row.names = FALSE)
#####################################################################################
## Experiment processing file, re-factored to batch process multiple games.
## Created by Pablo Diego Rosell and Ying Han, Gallup.Inc, in September 2019
## For any questions, contact pablo_diego-rosell@gallup.co.uk or ying_han@gallup.com
#####################################################################################

# Set up enviroment ----
rm(list = ls())
library(dplyr)

# Define constants -----
# directory to input (storing game logs, metadata, and survey results) and output folder
dd_input  <- "//gallup/dod_clients/DARPA_NGS2/CONSULTING/Ying_Han/Data_Wrangling_Cycle_3/GameLogs_Metadata_SurveyResults" 
dd_output <- "//gallup/dod_clients/DARPA_NGS2/CONSULTING/Ying_Han/Data_Wrangling_Cycle_3" 

# fieldwork start date
field_start <- "01-01-2019"

# game events of interest
events <- c("SetupMatch", 
            "PlayerConnection", 
            "StartMatch", 
            "StartVotation", 
            "PlayerFinalVotation", 
            "FinalItemSelected", 
            "GameSuspended", 
            "EndMatch", 
            "PlayerDisconnection")

# variable excluded in part 6
vars <- c("roundid", "roundid_short", "toolsLabel", "FinalItemSelected", "PlayerVote1",
          "PlayerVote2", "tools", "innovation", "inmot1", "inmot2", "eligible", "framing", "GroupVote1", "GroupVote2", 
          "matchid", "playerid")

# Define functions ----

# function to extract information from one game log file
gamelog_process <- function(data){

  # subset logs related to events of interest
  data <- data[grep(paste(events, collapse ="|"), data)]
  
  # matchid, matchDate
  match_info <- unlist(strsplit(data[grep("StartMatch", data)], ","))
  matchid <- as.numeric(match_info[3])
  matchDate <- as.Date(match_info[2], format = "%m/%d/%Y %I:%M:%S %p")
  
  # competition, timeUncertainty, support
  match_setting <- unlist(strsplit(data[grep("SetupMatch",data)], split=","))
  
  matchSetting1Label <- match_setting[3] 
  timeUncertaintyLabel <- match_setting[4] 
  matchSetting3Label <- match_setting[5]
  competitionLabel <- match_setting[6]
  supportLabel <- match_setting[7]
  
  timeUncertainty <- case_when(timeUncertaintyLabel == "False" ~ 1, 
                               timeUncertaintyLabel == "True" ~ 0,
                               TRUE ~ NA_real_)
  competition <- case_when(competitionLabel == "None" ~ 0,
                           competitionLabel == "Weak" ~ 1,
                           competitionLabel == "Normal" ~ 2,
                           competitionLabel == "Strong" ~ 3,
                           TRUE ~ NA_real_)
  support <- case_when(supportLabel == "HighStatus_HighLegitimacy" ~ 0, 
                       supportLabel == "LowStatus_HighLegitimacy" ~ 1)
  
  # nConnected, playerid
  players_info <- strsplit(data[grep("PlayerConnection", data)], split=",")
  playerid <- unique(unlist(lapply(players_info, function(x){x[3]})))
  nConnected <- length(playerid)
  
  # nRound
  nRound <- length(grep("StartVotation", data))
  
  # set up initial data frame
  df <- data.frame(
    matchid = rep(matchid, nRound * nConnected),
    matchDate = rep(matchDate, nRound * nConnected),
    matchSetting1Label = rep(matchSetting1Label, nRound * nConnected),
    matchSetting3Label = rep(matchSetting3Label, nRound * nConnected),
    competitionLabel = rep(competitionLabel, nRound * nConnected),
    competition = rep(competition, nRound * nConnected),
    timeUncertaintyLabel = rep(timeUncertaintyLabel, nRound * nConnected),
    timeUncertainty = rep(timeUncertainty, nRound * nConnected),
    supportLabel = rep(supportLabel, nRound * nConnected),
    support = rep(support, nRound * nConnected),
    nRound = rep(nRound, nRound * nConnected),
    roundid_short = unlist(lapply(1:nRound, rep, nConnected)),
    roundid = unlist(lapply(matchid*100 + 1:nRound, rep, nConnected)),
    nConnected = rep(nConnected, nRound * nConnected),
    playerid = rep(playerid, nRound)
  )
  
  # toolsLabel, PlayerVote1, PlayerVote2, FinalItemSelected
  roundid <- matchid * 100
  round_info <- strsplit(data[grep("StartVotation|PlayerFinalVotation|FinalItemSelected", data)], split=",")
  
  for (log in round_info){
    if (log[1] == "StartVotation"){
      roundid <- roundid + 1
      players_voted <- NULL
      df[which(df$roundid==roundid),"toolsLabel"] <- paste(log[3:4], collapse = ",")
    }
    
    if (log[1] == "PlayerFinalVotation"){
      playerid <- log[3]
      player_vote <- log[4]
      if(!playerid %in% players_voted){
        df[which(df$roundid == roundid & df$playerid == playerid), "PlayerVote1"] = log[4]
        players_voted <- c(players_voted, playerid)
      }
      else {
        df[which(df$roundid == roundid & df$playerid == playerid), "PlayerVote2"] = log[4]}
    }
    
    if (log[1] == "FinalItemSelected"){
      df[which(df$roundid==roundid),"FinalItemSelected"] <- log[3]
    }
  }
  
  # tools
  df$tools <- case_when(
    df$toolsLabel == "TNTbarrel,SatchelCharge" ~ 1, 
    df$toolsLabel == "BlackPowder,Dynamite" ~ 2, 
    df$toolsLabel == "BlackPowder,RDX" ~ 3, 
    df$toolsLabel == "RDX,Dynamite" ~ 4, 
    df$toolsLabel == "Mine1,Mine2" ~ 5, 
    df$toolsLabel == "Mine4,Mine3" ~ 6, 
    df$toolsLabel == "Mine1,BlackPowder" ~ 7,
    df$toolsLabel == "Mine2,BlackPowder" ~ 7, 
    df$toolsLabel == "Mine3,BlackPowder" ~ 8, 
    df$toolsLabel == "Mine4,BlackPowder" ~ 8, 
    df$toolsLabel == "Dynamite,TNTbarrel" ~ 9, 
    df$toolsLabel == "BlackPowder,SatchelCharge" ~ 10, 
    df$toolsLabel == "SatchelCharge,RDX" ~ 11, 
    df$toolsLabel == "Dynamite,SatchelCharge" ~ 12, 
    TRUE ~ NA_real_
  )
 
  # eligible: at least one round before agem suspend
  suspend <- ifelse(length(grep("GameSuspended", data))==0,length(data), min(grep("GameSuspended", data)))
  vote_1st <- min(grep("StartVotation", data))
  df$eligible <- suspend > vote_1st
  
  return(df)
}

# Part 1: Read data into R ----
gamelogs_files <- list.files(dd_input, "*.txt")
metadata_file  <- list.files(dd_input, "*metadata.csv")
survey_files   <- list.files(dd_input, "^survey")

# Part 2: game logs cleaning ----
gamelogs<- lapply(gamelogs_files, function(file) {readLines(paste(dd_input, file, sep="/"))})
game_data <- bind_rows(lapply(gamelogs, gamelog_process))



# Subset datalist according to the two conditions above:
# 1. Played during fielding period 
# 2. Had at least one "LeaderSelection") event before the game was suspended
game_data <- game_data[game_data$matchDate >= as.Date(field_start, format="%m-%d-%Y") 
                       & game_data$eligible == TRUE,]

# Part 3: metadata cleaning ----
metadata <- read.csv(paste(dd_input, metadata_file, sep="/"), header = T, stringsAsFactors = FALSE)

# tolerance
metadata[,"toleranceLabel"] <- metadata[,"group"]
metadata[,"tolerance"] <- ifelse(metadata[,"toleranceLabel"] == 'Ambiguity Low', 0, 1)

# Part 4: Merging game data and metadata ----
game_data <- merge(game_data, 
                   metadata[,!grepl("group", names(metadata))], 
                   by.x="matchid", by.y="matchID", all.x = TRUE)

# framing
# 1. when settingsNum is even: framing is 0 for rounds 1, 4, 7; 1 for rounds 2, 6, 8, 10, 12, 13; 2 for rounds 3, 5, 9, 11.
# 2. when settingsNum is odd : framing is 0 for rounds 1, 4, 7; 1 for rounds 3, 5, 6, 9, 10, 13; 2 for rounds 2, 6, 8, 11, 12.
game_data[,"framing"] <- case_when(
  game_data[,"settingsNum"] %% 2 == 0 & game_data[,"roundid_short"] %in% c(1, 4, 7) ~ 0,
  game_data[,"settingsNum"]  %% 2 == 0 & game_data[,"roundid_short"] %in% c(2, 6, 8, 10, 12, 13) ~ 1,
  game_data[,"settingsNum"]  %% 2 == 0 & game_data[,"roundid_short"] %in% c(3, 5, 9, 11) ~ 2,
  game_data[,"settingsNum"]  %% 2 == 1 & game_data[,"roundid_short"] %in% c(1, 4, 7) ~ 0,
  game_data[,"settingsNum"]  %% 2 == 1 & game_data[,"roundid_short"] %in% c(3, 5, 9, 10, 13) ~ 1,
  game_data[,"settingsNum"]  %% 2 == 1 & game_data[,"roundid_short"] %in% c(2, 6, 8, 11, 12) ~ 2,
  TRUE ~ NA_real_
)

# group vote
group_vote <- aggregate(x = game_data[, c("PlayerVote1","PlayerVote2")], 
                        by = list(roundid = game_data$roundid), 
                        FUN = function(x){
                          tb = table(x)
                          if(length(tb)>0){
                            tb = formatC(prop.table(table(x)) * 100, format="d")
                            result = paste(paste0(names(tb)," ", tb, "%"), collapse = ", ")
                          } else {result = NA}
                          return(result)
                        })
names(group_vote) <- c("roundid", "GroupVote1", "GroupVote2")
game_data <- merge(game_data, group_vote, by="roundid", all.x=TRUE)

# Motivation to Innovate Outcomes            

motivationCoder <- function (gameData, voteVar) {
  motivationVar <- case_when(
    (gameData$toolsLabel == "TNTbarrel,SatchelCharge" & gameData[voteVar] == "SatchelCharge") | 
      (gameData$toolsLabel == "BlackPowder,Dynamite" & gameData[voteVar] == "Dynamite") |
      (gameData$toolsLabel == "BlackPowder,RDX" & gameData[voteVar] == "RDX") |
      (gameData$toolsLabel == "RDX,Dynamite" & gameData[voteVar] == "RDX") |
      (gameData$toolsLabel == "Mine1,Mine2" & gameData[voteVar] == "Mine2") |
      (gameData$toolsLabel == "Mine4,Mine3" & gameData[voteVar] == "Mine4") |
      (gameData$toolsLabel == "Mine1,BlackPowder" & gameData[voteVar] == "Mine1") |
      (gameData$toolsLabel == "Mine2,BlackPowder" & gameData[voteVar] == "Mine2") |
      (gameData$toolsLabel == "Mine3,BlackPowder" & gameData[voteVar] == "Mine3") |
      (gameData$toolsLabel == "Mine4,BlackPowder" & gameData[voteVar] == "Mine4") ~ 1,
    gameData$toolsLabel == "Dynamite,TNTbarrel" |
      gameData$toolsLabel == "BlackPowder,SatchelCharge" |
      gameData$toolsLabel == "SatchelCharge,RDX" |
      gameData$toolsLabel == "Dynamite,SatchelCharge"  ~ NA_real_,
    TRUE ~ 0
  )
  return(motivationVar)
}

game_data$inmot1 <- motivationCoder (game_data, "PlayerVote1") 
game_data$inmot2 <- motivationCoder (game_data, "PlayerVote2")
game_data$innovation <- motivationCoder (game_data, "FinalItemSelected")

# format date/time fields
game_data$date.time<-as.POSIXct(game_data$date.time)

# output data
write.csv(game_data, paste(dd_output, 'game_data.csv', sep = '/'), row.names = FALSE)
factorial <- game_data #Assign to object named as used in analytics

# Part 5: survey data cleaning ----
survey_data <- lapply(survey_files, function(file){read.csv(paste(dd_input, file, sep="/"), skip = 1, header = 1, stringsAsFactors = F)})
names(survey_data) <- gsub(".csv", "", survey_files)

# recode PlayerId values in each survey response files
survey_data <- lapply(survey_data, function(x){
  x[,"PlayerId"] <- gsub("boomtown-", "", x[,"PlayerId"])
  return(x)
})

# remove columns with no values
survey_data <- lapply(survey_data, function(x){ x[,-c(which(colSums(!is.na(x)) == 0))]})

# remove the column "Raw Data"
survey_data <- lapply(survey_data, function(x){ x[,!grepl("Raw.Data", names(x))]})

# rename question variables
survey_data <- lapply(1:2, function(i){
  data <- survey_data[[i]]
  start <- min(grep(".", names(data), fixed=T))
  end <- length(names(data))
  names(data)[start:end] <- paste0("Q", 1:length(start:end), "_", i)
  return(data)
})

# merge survey_results_1 and survey_results_2
survey_data <- merge(survey_data[[grep("_1", survey_files)]], 
                     survey_data[[grep("_2", survey_files)]], 
                     by="PlayerId", all.x = T, suffixes = c("_1", "_2"))

# Part 6: merge game data with survey data at the match level ----
game_data_aggr <- aggregate(game_data[,!grepl(paste(vars, collapse = "|"), names(game_data))],
                            by = list(matchid = game_data[,"matchid"], playerid = game_data[,"playerid"]),
                            FUN = unique)

game_survey_data <- merge(game_data_aggr, survey_data, by.x = "playerid", by.y = "PlayerId", all.x = T)

# output data
write.csv(game_survey_data, paste(dd_output, 'survey_data.csv', sep = '/'), row.names = FALSE)
