###################################################################
## For poster abstract of 2020 ASA conference
## Created by Ying Han, PhD, for Gallup inc. in Nov, 2019
## Supervised by Pablo Diego Rosell, PhD, for Gallup inc.
###################################################################

# Clear enviroment and load libraries
rm(list = ls())
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(dplyr, hot.deck)

# Define constants
dd_emp <- "W:/DARPA_NGS2/CONSULTING/Analytics/cycle2/empanelment"
dd_log <- "W:/DARPA_NGS2/CONSULTING/Analytics/cycle2/data"
dd_game <- "W:/DARPA_NGS2/CONSULTING/Ying_Han/ASA_Poster_Cycle_2"
# od <- "//gallup/dod_clients/DARPA_NGS2/CONSULTING/Analytics/cycle2/output"

imputation_variables <- 'nid|Q107|Q1[1-8]$|Q2[2-7]|Q29|Q56|Q57|Q59|Q6|Q7[0-9]|Q8[0-9]|q9[0-9]'
events <- c("StartMatch", "LeaderSelection", "NewLeader", "PlayerConnection", "PlayerDisconnection")

# define functions
ambiguity_calc <- function(x) {
  revs <- c(1, 4, 5, 9, 10, 11, 12)
  x[, paste0('Q57_', revs)] <- apply(x[, paste0('Q57_', revs)], 2, function(y) {
    return(abs(y - 5) + 1)
  })
  return(apply(x[, grep('Q57_', names(x))], 1, function(y) {sum(y)}))
}

trans_leadership <- function(x, choice=c('sum', 'mean')) {
  if(choice == 'sum') {
    tmp <- apply(x[, grep('Q107_', names(x))], 1, sum)
  } else {
    tmp <- apply(x[, grep('Q107_', names(x))], 1, mean)
  }
  return(tmp)
}

log_info_abstract <- function(vec){
  # read in a game log and return a data frame with columns Event and ResponseID
  result <- matrix(unlist(lapply(vec, function(x) unlist(strsplit(x, split=","))[c(1,3)] )),
                   ncol = 2, byrow=TRUE, dimnames = list(NULL, c("Event", "ID")))
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  result[, "ID"] <- ifelse(result[,"Event"]=="LeaderSelection", NA, result[, "ID"])
  
  return (result)
}

round_info_summarize <- function(df){
  
  # create an empty data frame based on the max number of players
  players_max = sum(df[,"Event"] == "PlayerConnection")
  Round_Info <- data.frame(matrix(ncol = (3 + players_max), nrow = 0), stringsAsFactors = FALSE)
  names(Round_Info) <- c("MatchID", "Round", "Leader", paste0("Player", 1:players_max))
  
  # set up initial values
  matchid <- 0; round <- 0; leader <- NA; players <- character()
  
  # the loop
  for (i in 1:nrow(df)){
    event <- df[i, "Event"]
    id <- df[i, "ID"]
    
    if (event == "PlayerConnection") {
      players = union(players, id)
    } else if (event == "PlayerDisconnection") {
      players = setdiff(players, id)
    } else if (event == "StartMatch") {
      matchid = id
    } else if (event == "NewLeader"){
      leader = id
    } else {
      round = round + 1
      new_info <- c(matchid, round, leader, players, rep(NA, players_max-length(players)))
      Round_Info[nrow(Round_Info)+1, ] <- new_info
    }
  }
  
  # change the data type of MatchID and Round
  Round_Info[,"MatchID"] = as.numeric(Round_Info[,"MatchID"])
  Round_Info[,"Round"] = as.numeric(Round_Info[,"Round"])
  
  # Remove the columns with all missing vales (often occurs for the last col)
  no_missing <- ifelse(colSums(is.na(Round_Info)) == 0, TRUE, FALSE)
  Round_Info <- Round_Info[, no_missing]
  
  return (Round_Info)
}

#### Section 1: Clean empanelment data
# Read in empanelment data
empanelment <- read.csv(paste(dd_emp, "cycle2_empanelment_28jan2019.csv", sep="/"),header=TRUE, sep=",", stringsAsFactors = FALSE)

# Subset empanelmendt data 
empanelment <- empanelment[empanelment$Status==0 & empanelment$Q3 == 1, ]

# missing data imputation 
empanelment[,"Q15"] <- ifelse(empanelment[,"Q15"]=="37,5", 37.5, empanelment[,"Q15"])
empanelment[,"Q15"] <- as.numeric(empanelment[,"Q15"])

empanelment[empanelment$Q25 %in% c("", ",5", "0.5", "1.5"),"Q25"] <- NA
empanelment[,"Q25"] <- as.numeric(empanelment[,"Q25"])

empanelment[,"Q26"] <- ifelse(empanelment[,"Q26"]==",0", 0, empanelment[,"Q26"])
empanelment[,"Q26"] <- ifelse(empanelment[,"Q26"]==",1", 1, empanelment[,"Q26"])
empanelment[,"Q26"] <- ifelse(empanelment[,"Q26"]==".5", 5, empanelment[,"Q26"]) 
empanelment[,"Q26"] <- as.numeric(empanelment[,"Q26"])

empanelment[,"nid"] <- 1:nrow(empanelment)
hd_values <- hot.deck(empanelment[, grep(imputation_variables, names(empanelment))], method = 'best.cell',
                      cutoff = 1, sdCutoff = 3)
empanelment <- merge(empanelment, hd_values$data[[1]], by='nid')
empanelment <- empanelment[, !grepl('.x', names(empanelment))]
names(empanelment) <- gsub('.y', '', names(empanelment))

# Create new variables 
empanelment[, "tol_ambiguity"] <- ambiguity_calc(empanelment)
empanelment[, "trans_leadership_sum"] <- trans_leadership(empanelment, "sum")
empanelment[, "trans_leadership_mean"] <- trans_leadership(empanelment, "mean")

#### Section 2: Clean Game Logs
# Read in Logs
files <- list.files(dd_log, pattern = "*.txt")
datalist <- lapply(files, function(x) {readLines(paste(dd_log, x, sep = '/'))})

# Filter log information of each match based on the events of interest 
datalist <- lapply(datalist, function(x){x[grep(paste(events, collapse = "|"),x)]})

# Subset matches based on whehter it has at least one round "LeaderSlection" 
nRounds <- unlist(lapply(datalist, function(x){sum(grepl("LeaderSelection", x))}))
datalist <- datalist[which(nRounds > 0)]

# Abstract main information for each log record 
datalist <- lapply(datalist, function(x){log_info_abstract(x)})

# Summarize round informaiton for each match 
datalist <- lapply(datalist, function(x){round_info_summarize(x)})

# Merge round information of all matches together 
match_log <- bind_rows(datalist)

# Read in the gamesData generated by wrangle.R
game_data <- read.csv(paste(dd_game, "gamesData.csv", sep="/"))

# Append leader's respondent id to game_data by merging game_data with match_log using matchid and round
game_data$match_round_id <- game_data$matchid * 100 + game_data$round
match_log$match_round_id <- match_log$MatchID * 100 + match_log$Round

sum(duplicated(game_data$match_round_id)) # game_data has no duplicates
sum(duplicated(match_log$match_round_id)) # match_log has duplicates
unique(match_log$MatchID[duplicated(match_log$match_round_id)]) # The duplicated values all occurs for matches with ID 1234 in match_log
any(game_data$matchid == 1234) # There is no match with ID in game_data
match_log <- match_log[match_log$MatchID != 1234,] # remove matches with ID 1234 from match_log
sum(duplicated(match_log$match_round_id)) # match_log has duplicates

game_data <- merge(game_data, match_log[,c("match_round_id", "Leader")], by = "match_round_id", all.x = T)

#### Section 3: Merge empanelment data with game data by respondent id
game_emp <- merge(game_data, empanelment, by.x = "Leader", by.y = "ResponseID", all.x = T)

write.csv(game_emp, paste(dd_game, "game_empanelment.csv", sep="/"), row.names = F)
