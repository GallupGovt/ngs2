## Experiment processing file, re-factored to batch process multiple games.
## Created by Ying Han, PhD, Gallup Inc, in August 2019
## For any questions, contact ying_han@gallup.com

# clear workplace
library(dplyr)

# define constants
dd_logs <- "W:/DARPA_NGS2/CONSULTING/Ying_Han/Data_Wrangling_Cycle_3/Sample_Logs"
dd_meta <- "W:/DARPA_NGS2/CONSULTING/Ying_Han/Data_Wrangling_Cycle_3"
dd_output <- "W:/DARPA_NGS2/CONSULTING/Ying_Han/Data_Wrangling_Cycle_3"

events <- c("SetupMatch", "PlayerConnection", "StartMatch", "StartVotation", "PlayerFinalVotation", 
            "FinalItemSelected", "GameSuspended", "EndMatch", "PlayerDisconnection")

field_start <- "01-01-2019"
  
# define functons
gamelog_process <- function(data){
  
  # subset logs related to events of interest
  data <- data[grep(paste(events, collapse ="|"), data)]
  
  # matchid, matchDate
  match_info <- unlist(strsplit(data[grep("StartMatch", data)], ","))
  matchid <- as.numeric(match_info[3])
  matchDate <- as.Date(match_info[2], format = "%m/%d/%Y %H:%M:%S %p")
  
  # competition, timeUncertainty, support
  match_setting <- unlist(strsplit(data[grep("SetupMatch",data)], split=","))
  
  timeUncertaintyLabel <- match_setting[4] 
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
  
  # nRound
  nRound <- length(grep("StartVotation", data))
  
  # set up initial data frame
  df <- data.frame(
    matchid = rep(matchid, nRound * nConnected),
    matchDate = rep(matchDate, nRound * nConnected),
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
    df$toolsLabel == "TNTbarrel,Dynamite" |
      df$toolsLabel == "BlackPowder,SatchelCharge" |
      df$toolsLabel == "SatchelCharge,RDX" |
      df$toolsLabel == "Dynamite,SatchelCharge"  ~ NA_real_,
    TRUE ~ 0
  )
  
  # eligible
  suspend <- ifelse(length(grep("GameSuspended", data))==0,length(data), min(grep("GameSuspended", data)))
  vote_1st <- min(grep("StartVotation", data))
  df$eligible <- suspend > vote_1st
  
  return(df)
}

# read in full game logs and apply the gamelog_process function
files <- list.files(dd_logs, "*.txt")
datalist <- lapply(files, function(file) {readLines(paste(dd_logs, file, sep="/"))})
game_list <- bind_rows(lapply(datalist, gamelog_process))

# read in the metadata
metadata <- read.csv(paste(dd_meta, "boomtown_metadata.csv", sep="/"), 
                     header = T, stringsAsFactors = FALSE)

metadata$toleranceLabel <- metadata$group
metadata$tolerance <- ifelse(metadata$group == 'Ambiguity Low', 0, 1)

# merge processed game logs with meta data
game_data <- merge(game_list, metadata[,c("matchID","toleranceLabel", "tolerance","date.time","replay","consumableKey","settingsNum")], 
                   by.x="matchid", by.y="matchID", all.x = TRUE)

# framing - Double Check with Pablo
# 1. when settingsNum is even: framing is 0 for rounds 1, 4, 7; 1 for rounds 2, 6, 8, 10, 12, 13; 2 for rounds 3, 5, 9, 11.
# 2. when settingsNum is odd : framing is 0 for rounds 1, 4, 7; 1 for rounds 3, 5, 6, 9, 10, 13; 2 for rounds 2, 6, 8, 11, 12.
game_data$framing <- case_when(
  game_data$settingsNum %% 2 == 0 & game_data$roundid_short %in% c(1, 4, 7) ~ 0,
  game_data$settingsNum %% 2 == 0 & game_data$roundid_short %in% c(2, 6, 8, 10, 12, 13) ~ 1,
  game_data$settingsNum %% 2 == 0 & game_data$roundid_short %in% c(3, 5, 9, 11) ~ 2,
  game_data$settingsNum %% 2 == 1 & game_data$roundid_short %in% c(1, 4, 7) ~ 0,
  game_data$settingsNum %% 2 == 1 & game_data$roundid_short %in% c(3, 5, 9, 10, 13) ~ 1,
  game_data$settingsNum %% 2 == 1 & game_data$roundid_short %in% c(2, 6, 8, 11, 12) ~ 2,
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

# Subset datalist according to the two conditions above:
# 1. Played during fielding period (StartMatch date >= 10/25/2018)
# 2. Had at least one "LeaderSelection") event before the game was suspended
game_data <- game_data[game_data$matchDate >= as.Date(field_start, format="%m-%d-%Y") & game_data$eligible == TRUE,]

# output dataset
write.csv(game_data, paste(dd_output, 'game_data.csv', sep = '/'), row.names = FALSE)






