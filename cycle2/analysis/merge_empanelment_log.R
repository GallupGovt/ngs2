
# Load plyr package

pacman::p_load(plyr)

# Define constants
dd_emp <- "//gallup/dod_clients/DARPA_NGS2/CONSULTING/Analytics/cycle2/empanelment"
events <- c("StartMatch", "LeaderSelection", "NewLeader", "PlayerConnection", "PlayerDisconnection")
vars <- c("ResponseID","tol_ambiguity","trans_leadership_sum","trans_leadership_mean" )

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
  round_Info <- data.frame(matrix(ncol = (3 + players_max), nrow = 0), stringsAsFactors = FALSE)
  names(round_Info) <- c("matchid", "round", "Leader", paste0("Player", 1:players_max))
  
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
      round_Info[nrow(round_Info)+1, ] <- new_info
    }
  }
  
  # change the data type of matchid and round
  round_Info[,"matchid"] = as.numeric(round_Info[,"matchid"])
  round_Info[,"round"] = as.numeric(round_Info[,"round"])
  
  # Remove the columns with all missing vales (often occurs for the last col)
  no_missing <- ifelse(colSums(is.na(round_Info)) == 0, TRUE, FALSE)
  round_Info <- round_Info[, no_missing]
  
  return (round_Info)
}

merge_process <- function(){
  merged_data <- match_log
  indices <- grep(paste("Leader", "Player", sep="|"), names(match_log))
  
  for (i in indices){
    merged_data <- merge(merged_data, empanelment, 
                         by.x = names(match_log)[i], by.y = "ResponseID", all.x = TRUE, 
                         sort = FALSE, imcomparables = NA)
    
    for (var in vars){
      loc <- which(names(merged_data) == var)
      names(merged_data)[loc] <- paste(names(merged_data)[loc], names(match_log)[i], sep="_")
    }
  }
  
  return(merged_data)
}

#### Section 1: Clean empanelment data
# Read in empanelment data
empanelment <- read.csv(paste(dd_emp, "cycle2_empanelment_26nov2018.csv", sep="/"),header=TRUE, sep=",", stringsAsFactors = FALSE)

# Subset empanelmendt data 
empanelment <- empanelment[empanelment$Status==0 & empanelment$Q3 == 1, ]

# Create new variables 
empanelment[, "tol_ambiguity"] <- ambiguity_calc(empanelment)
empanelment[, "trans_leadership_sum"] <- trans_leadership(empanelment, "sum")
empanelment[, "trans_leadership_mean"] <- trans_leadership(empanelment, "mean")

# Select variables of interest
empanelment <- empanelment[,vars]

#### Section 2: Clean Game Logs
# Read in Logs
files <- list.files(dd, pattern = "*.txt")
datalist <- lapply(files, function(x) {readLines(paste(dd, x, sep = '/'))})

# Filter log information of each match based on the events of interest 
datalist <- lapply(datalist, function(x){x[grep(paste(events, collapse = "|"),x)]})

# Subset matches based on whehter it has at least one round "LeaderSlection" 
nrounds <- unlist(lapply(datalist, function(x){sum(grepl("LeaderSelection", x))}))
datalist <- datalist[which(nrounds > 0)]

# Abstract main information for each log record 
datalist <- lapply(datalist, function(x){log_info_abstract(x)})

# Summarize round informaiton for each match 
datalist <- lapply(datalist, function(x){round_info_summarize(x)})

# Merge round information of all matches together 
match_log <- rbind.fill(datalist)
nPlayersMax  <- sum(grepl("Player", names(match_log)))

#### Section 3: Merge empanelmanet data with log data, and output
merged_data <- merge_process()

# order rows and columns
col_order <- unlist(lapply(c("matchid", "round", "Leader", as.character(1:nPlayersMax)),
                           function(x) grep(x, names(merged_data))))
row_order <- order(merged_data[,"matchid"],merged_data[,"round"])
merged_data <- merged_data[row_order, col_order]

# output merged_data into a .csv file
write.csv(empanelment, paste(od, "empanelment.csv", sep="/"), row.names = FALSE)
write.csv(match_log, paste(od, "game_logs.csv", sep="/"), row.names = FALSE)
write.csv(merged_data, paste(od, "empanelment_and_log.csv", sep="/"), row.names = FALSE)
