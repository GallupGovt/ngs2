#####################################################################################
## Experiment processing file, re-factored to batch process multiple games.
## Created by Pablo Diego Rosell and Ying Han, Gallup.Inc, in September 2019
## For any questions, contact pablo_diego-rosell@gallup.co.uk or ying_han@gallup.com
#####################################################################################

# Set up enviroment ----
rm(list = ls())
if (!require("pacman")) install.packages("pacman")
library ("pacman")
pacman::p_load(dplyr, ggplot2, Hmisc, gplots, car, tidyr)

# Define constants -----
# directory to input (storing game logs, metadata, and survey results) and output folder
dd_input  <- "W:/DARPA_NGS2/CONSULTING/Ying_Han/Data_Wrangling_Cycle_3/GameLogs_Metadata_SurveyResults" 
dd_output <- "W:/DARPA_NGS2/CONSULTING/Ying_Han/Data_Wrangling_Cycle_3" 

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
            "PlayerDisconnection",
            "ChatMessage")

# variable excluded in part 6
vars <- c("roundid", "roundid_short", "toolsLabel", "FinalItemSelected", "PlayerVote1",
          "PlayerVote2", "tools", "innovation", "eligible", "framing", "GroupVote1", "GroupVote2", 
          "matchid", "playerid", "chat_per_round")

# Define functions ----
# function to extract information from one game log file
gamelog_process <- function(data){
  
  # subset logs related to events of interest
  data <- data[grep(paste(events, collapse ="|"), data)]
  
  # nRound
  nRound <- length(grep("StartVotation", data))
  
  if(nRound == 0){
    return(NULL)
  } else {
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
    
    timeUncertainty <- case_when(timeUncertaintyLabel == "False" ~ 0, 
                                 timeUncertaintyLabel == "True" ~ 1,
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
      playerid = rep(playerid, nRound),
      toolsLabel = rep(NA, nRound * nConnected),
      PlayerVote1 = rep(NA, nRound * nConnected),
      PlayerVote2 = rep(NA, nRound * nConnected),
      FinalItemSelected = rep(NA, nRound * nConnected),
      chat_per_round = rep(0, nRound * nConnected), 
      stringsAsFactors = F
    )
    
    # toolsLabel, PlayerVote1, PlayerVote2, FinalItemSelected
    roundid <- matchid * 100
    round_info <- data[grep("StartVotation|PlayerFinalVotation|FinalItemSelected|ChatMessage", data)]
    round_info <- round_info[min(grep("StartVotation", round_info)):length(round_info)]
    round_info <- strsplit(round_info, split=",")
    
    for (log in round_info){
      
      if (log[1] == "StartVotation"){
        chat_per_round <- 0
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
      
      if (log[1] == "ChatMessage"){
        chat_per_round = chat_per_round + 1
      }
      
      if (log[1] == "FinalItemSelected"){
        df[which(df$roundid==roundid),"FinalItemSelected"] <- log[3]
        df[which(df$roundid==roundid),"chat_per_round"] <- chat_per_round
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
      df$toolsLabel == "TNTbarrel,Dynamite" ~ 9, 
      df$toolsLabel == "BlackPowder,SatchelCharge" ~ 10, 
      df$toolsLabel == "SatchelCharge,RDX" ~ 11, 
      df$toolsLabel == "Dynamite,SatchelCharge" ~ 12, 
      TRUE ~ NA_real_
    )
    
    # innovation
    df$innovation <- case_when(
      (df$toolsLabel == "TNTbarrel,SatchelCharge" & df$FinalItemSelected == "SatchelCharge") | 
        (df$toolsLabel == "BlackPowder,Dynamite" & df$FinalItemSelected == "Dynamite") |
        (df$toolsLabel == "BlackPowder,RDX" & df$FinalItemSelected == "RDX") |
        (df$toolsLabel == "RDX,Dynamite" & df$FinalItemSelected == "RDX") |
        (df$toolsLabel == "Mine1,Mine2" & df$FinalItemSelected == "Mine2") |
        (df$toolsLabel == "Mine4,Mine3" & df$FinalItemSelected == "Mine4") |
        (df$toolsLabel == "Mine1,BlackPowder" & df$FinalItemSelected == "Mine1") |
        (df$toolsLabel == "Mine2,BlackPowder" & df$FinalItemSelected == "Mine2") |
        (df$toolsLabel == "Mine3,BlackPowder" & df$FinalItemSelected == "Mine3") |
        (df$toolsLabel == "Mine4,BlackPowder" & df$FinalItemSelected == "Mine4") ~ 1,
      (df$toolsLabel == "TNTbarrel,Dynamite") |
        (df$toolsLabel == "BlackPowder,SatchelCharge") |
        (df$toolsLabel == "SatchelCharge,RDX") |
        (df$toolsLabel == "Dynamite,SatchelCharge") ~ NA_real_,
      TRUE ~ 0
    )
    
    # eligible: at least one round before agem suspend
    suspend <- ifelse(length(grep("GameSuspended", data))==0,length(data), min(grep("GameSuspended", data)))
    vote_1st <- min(grep("StartVotation", data))
    df$eligible <- suspend > vote_1st
    
    return(df)
  }
}

check_range <- function(data){
  vars1 <- c(paste0("Q", 1:12, "_1"), paste0("Q", 1:13, "_2"))
  for (var in vars1){
    if(any(!data[, var] %in% c(NA, 0, 1, 2, 3, 4), na.rm=T)){
      stop(paste(var, "have values other than 0-4."))
    }
  }
  
  vars2 <- paste0("Q", 16:18, "_1")
  for (var in vars2){
    if(any(!data[, var] %in% c(NA,0,1), na.rm=T)){
      stop(paste(var, "have values other than 0-1."))
    }
  }
  
  if(any(!data[, "Q14_1"] %in% c(NA,0,1,2,3), na.rm=T)){
    stop(paste("Q14_1", "have values other than 0-3."))
  }
  
  if(any(!data[, "Q15_1"] %in% c(NA,0,1,2,3,4,5), na.rm=T)){
    stop(paste("Q14_1", "have values other than 0-5."))
  }
  
  print("The values of survey items are in their correct range")
}

survey_clean <- function(filenames){
  # read in data
  data <- lapply(filenames, function(file){read.csv(paste(dd_input, file, sep="/"), skip = 1, header = 1, stringsAsFactors = F)})
  
  # remove columns with no values and the column "Raw Data"
  data <- lapply(data, function(x){ x[,-c(which(colSums(!is.na(x)) == 0))]})
  data <- lapply(data, function(x){ x[,!grepl("Raw.Data", names(x))]})
  
  # rename variable
  data <- lapply(1:2, function(i){
    tmp <- data[[i]]
    start <- min(grep(".", names(tmp), fixed=T))
    end <- length(names(tmp))
    names(tmp)[start:end] <- paste0("Q", 1:length(start:end), "_", i)
    return(tmp)
  })
  
  # check whether there are duplicated playerids
  check_duplicate <- lapply(1:2, function(i){
    
    dup <- duplicated(data[[i]]["PlayerId"])
    
    if (any(dup)) {
      stop(paste("There are duplicated playerids in", filenames[i], 
                    ". Duplicates are removed."))
      }
  })
  
  # merging
  data <- merge(data[[1]], data[[2]], by = "PlayerId", all.x = T, suffixes = c("_1", "_2")) 
  
  # recode variables
  data[, "PlayerId"] <- gsub("boomtown-", "", data[,"PlayerId"])
  
  vars_tmp <- c(paste0("Q", 1:18, "_1"), paste0("Q", 1:12, "_2"))
  for (var in vars_tmp){
    data[, var] <- as.numeric(data[,var])
    data[grep("-1",data[,var]), var] <- NA
  }
  
  return(data)
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

# output data
write.csv(game_data, paste(dd_output, 'game_data.csv', sep = '/'), row.names = FALSE)

# Part 5: survey data cleaning ----
survey_data <- survey_clean(filenames = survey_files)

# Part 6: merge game data with survey data at the match level ----
game_data_aggr <- aggregate(game_data[,!grepl(paste(vars, collapse = "|"), names(game_data))],
                            by = list(matchid = game_data[,"matchid"], playerid = game_data[,"playerid"]),
                            FUN = unique)

game_survey_data <- merge(game_data_aggr, survey_data, by.x = "playerid", by.y = "PlayerId", all.x = T)

# check value ranges for each variable
check_range(data = game_survey_data)

# output data
write.csv(game_survey_data, paste(dd_output, "game_survey_data.csv", sep="/"), row.names = F)
