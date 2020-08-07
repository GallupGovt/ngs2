#####################################################################################
## Experiment processing file, re-factored to batch process multiple games.
## Created by Pablo Diego Rosell and Ying Han, Gallup.Inc, in September 2019
## For any questions, contact pablo_diego-rosell@gallup.co.uk or ying_han@gallup.com
#####################################################################################

# PRELIMINARY STEPS
# Save all game logs and metadata to the dd_input folder before running script. 
## Link to metadata: https://volunteerscience.com/gallup/boomtown_metadata
# Run "Wrangle.R"
# Copy "game_data.csv" and "survey_data.csv" from \\gallup\dod_clients\DARPA_NGS2\CONSULTING\Ying_Han\Data_Wrangling_Cycle_3
# Upload both csv files above to github https://github.com/GallupGovt/ngs2/edit/master/cycle3/Boomtown/Analysis/

# Set up enviroment ----
rm(list = ls())
if (!require("pacman")) install.packages("pacman")
library ("pacman")
pacman::p_load(dplyr, Hmisc, car, tidyr, RCurl)

# Define constants -----
# directory to input (storing game logs, metadata, and survey results) and output folder
dd_input  <- "//gallup/dod_clients/DARPA_NGS2/CONSULTING/Ying_Han/Data_Wrangling_Cycle_4/GameLogs_Metadata_SurveyResults" 
dd_output <- "//gallup/dod_clients/DARPA_NGS2/CONSULTING/Ying_Han/Data_Wrangling_Cycle_4" 

# fieldwork start date
field_start <- "08-05-2020"

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

# columns to keep in survey_results_1
selected_columns_1 <- c(
  PlayerId = "PlayerId", 
  Q1_1 = "I.avoid.settings.where.people.don.t.share.my.values.",
  Q2_1 = "I.can.enjoy.being.with.people.whose.values.are.very.different.from.mine.",                                                          
  Q3_1 = "I.would.like.to.live.in.a.foreign.country.for.a.while.",                                                                           
  Q4_1 = "I.like.to.surround.myself.with.things.that.are.familiar.to.me.",                                                                     
  Q5_1 = "The.sooner.we.all.acquire.similar.values.and.ideals.the.better.",                                                                  
  Q6_1 = "I.can.be.comfortable.with.nearly.all.kinds.of.people.",                                                                   
  Q7_1 = "If.given.a.choice..I.will.usually.visit.a.foreign.country.rather.than.vacation.at.home.",                                            
  Q8_1 = "A.good.teacher.is.one.who.makes.you.wonder.about.your.way.of.looking.at.things.",                                                    
  Q9_1 = "A.good.job.is.one.where.what.is.to.be.done.and.how.it.is.to.be.done.are.always.clear." ,                                             
  Q10_1 = "A.person.who.leads.an.even..regular.life.in.which.few.surprises.or.unexpected.happenings.arise.really.has.a.lot.to.be.grateful.for.",
  Q11_1 = "What.we.are.used.to.is.always.preferable.to.what.is.unfamiliar.",                                                                    
  Q12_1 = "I.like.parties.where.I.know.most.of.the.people.more.than.ones.where.all.or.most.of.the.people.are.complete.strangers.",            
  Q13_1 = "Age",                                                                                                                               
  Q14_1 = "Gender",                                                                                                                            
  Q15_1 = "What.is.the.highest.level.of.education.you.completed.",                                                                             
  Q16_1 = "Do.you.currently.have.a.job.",                                                                                                      
  Q17_1 = "Have.you.ever.participated.in.an.on.line.research.experiment.before.",                                                              
  Q18_1 = "Do.we.have.permission.to.occasionally.send.survey.invitations.and.reminders.to.your.mobile.phone.number.using.text.messages.",
  ToA_Calculated = "ToA.Calculated"
)

# columns to keep in survey_results_2
selected_columns_2 <- c(
  PlayerId = "PlayerId", 
  Q1_2 = "A.few.questions.before.you.go..Please.indicate.how.much.you.agree.or.disagree.with.the.following.statements.",                            
  Q2_2 = "The.opposing.teams.were.stronger.than.my.team.",                                                                                         
  Q3_2 = "The.relationship.between.my.team.and.the.other.teams.was.competitive.",                                                                   
  Q4_2 = "During.quick.voting.rounds..I.felt.a.great.amount.of.time.pressure.when.voting.for.an.item.",                                             
  Q5_2 = "During.quick.voting.rounds..I.had.to.make.my.decisions.very.fast.",                                                                   
  Q6_2 = "During.slow.voting.rounds..I.felt.a.great.amount.of.time.pressure.when.voting.for.an.item.",                                            
  Q7_2 = "During.slow.voting.rounds..I.had.to.make.my.decisions.very.fast.",                                                                       
  Q8_2 = "When.I.had.to.choose.between.different.types.of.explosives..I.felt.like.I.had.to.process.too.much.information.",                          
  Q9_2 = "When.I.had.to.choose.between.different.types.of.mines..I.felt.like.I.had.to.process.too.much.information.",                               
  Q10_2 = "When.I.was.playing.the.game.I.was.certain.about.the.number.of.rounds.I.had.to.play.",  
  MatchId = "MatchId" 
)

# variable excluded in part 6

vars <- c("roundid", "roundid_short", "toolsLabel", "FinalItemSelected", "PlayerVote1",
          "PlayerVote2", "tools", "innovation", "eligible", "framing", "GroupVote1", "GroupVote2", 
          "matchid", "playerid", "chat_per_round", "conformity", "unanimous", "playernum", 
          "compStrong", "round", "group", "player", "structure", "pressure", "toolsCPT", "toolsEUT", 
          "toolsSPT", "toolsCPTEXP", "risk", "prb", "structureHie","structureCel","structureNet",
          "centralization", "density", "leaderWeight", "compStrong", "complexity", "playernum", 
          "grmot1", "grmot2", "inmot1", "inmot2")

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
      df$toolsLabel == "Mine3,Mine4" ~ 6, 
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
        (df$toolsLabel == "Mine3,Mine4" & df$FinalItemSelected == "Mine4") |
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
    
    # Player Vote 1
    df$inmot1 <- case_when(
      (df$toolsLabel == "TNTbarrel,SatchelCharge" & df$PlayerVote1 == "SatchelCharge") | 
        (df$toolsLabel == "BlackPowder,Dynamite" & df$PlayerVote1 == "Dynamite") |
        (df$toolsLabel == "BlackPowder,RDX" & df$PlayerVote1 == "RDX") |
        (df$toolsLabel == "RDX,Dynamite" & df$PlayerVote1 == "RDX") |
        (df$toolsLabel == "Mine1,Mine2" & df$PlayerVote1 == "Mine2") |
        (df$toolsLabel == "Mine3,Mine4" & df$PlayerVote1 == "Mine4") |
        (df$toolsLabel == "Mine1,BlackPowder" & df$PlayerVote1 == "Mine1") |
        (df$toolsLabel == "Mine2,BlackPowder" & df$PlayerVote1 == "Mine2") |
        (df$toolsLabel == "Mine3,BlackPowder" & df$PlayerVote1 == "Mine3") |
        (df$toolsLabel == "Mine4,BlackPowder" & df$PlayerVote1 == "Mine4") ~ 1,
      (df$toolsLabel == "TNTbarrel,Dynamite") |
        (df$toolsLabel == "BlackPowder,SatchelCharge") |
        (df$toolsLabel == "SatchelCharge,RDX") |
        (df$toolsLabel == "Dynamite,SatchelCharge") ~ NA_real_,
      TRUE ~ 0
    )
    
    # Player Vote 2
    
    df$inmot2 <- case_when(
      (df$toolsLabel == "TNTbarrel,SatchelCharge" & df$PlayerVote2 == "SatchelCharge") | 
        (df$toolsLabel == "BlackPowder,Dynamite" & df$PlayerVote2 == "Dynamite") |
        (df$toolsLabel == "BlackPowder,RDX" & df$PlayerVote2 == "RDX") |
        (df$toolsLabel == "RDX,Dynamite" & df$PlayerVote2 == "RDX") |
        (df$toolsLabel == "Mine1,Mine2" & df$PlayerVote2 == "Mine2") |
        (df$toolsLabel == "Mine3,Mine4" & df$PlayerVote2 == "Mine4") |
        (df$toolsLabel == "Mine1,BlackPowder" & df$PlayerVote2 == "Mine1") |
        (df$toolsLabel == "Mine2,BlackPowder" & df$PlayerVote2 == "Mine2") |
        (df$toolsLabel == "Mine3,BlackPowder" & df$PlayerVote2 == "Mine3") |
        (df$toolsLabel == "Mine4,BlackPowder" & df$PlayerVote2 == "Mine4") ~ 1,
      (df$toolsLabel == "TNTbarrel,Dynamite") |
        (df$toolsLabel == "BlackPowder,SatchelCharge") |
        (df$toolsLabel == "SatchelCharge,RDX") |
        (df$toolsLabel == "Dynamite,SatchelCharge") ~ NA_real_,
      TRUE ~ 0
    )
    
    # eligible: at least one round before game suspend
    suspend <- ifelse(length(grep("GameSuspended", data))==0,length(data), min(grep("GameSuspended", data)))
    vote_1st <- min(grep("StartVotation", data))
    df$eligible <- suspend > vote_1st
    
    return(df)
  }
}

check_range <- function(data){
  vars1 <- vars1 <- c(paste0("Q", 1:12, "_1"), paste0("Q", 1:10, "_2"))
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

## Part 1: Read data into R ----
gamelogs_files <- list.files(dd_input, "*.txt")
metadata_file  <- list.files(dd_input, "*metadata.csv")

## Part 2: game logs cleaning ----
gamelogs<- lapply(gamelogs_files, function(file) {readLines(paste(dd_input, file, sep="/"))})
game_data <- bind_rows(lapply(gamelogs, gamelog_process))

# Subset datalist according to the two conditions above:
# 1. Played during fielding period 
# 2. Had at least one "LeaderSelection") event before the game was suspended
game_data <- game_data[game_data$matchDate >= as.Date(field_start, format="%m-%d-%Y") 
                       & game_data$eligible == TRUE,]

## Part 3: metadata cleaning ----
metadata <- read.csv(paste(dd_input, metadata_file, sep="/"), header = T, stringsAsFactors = FALSE)

# tolerance
metadata[,"toleranceLabel"] <- metadata[,"group"]
metadata[,"tolerance"] <- ifelse(metadata[,"toleranceLabel"] == 'Ambiguity Low', 0, 1)

## Part 4: Merging game data and metadata ----
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

# Variable name changes to conform with analysis

game_data$round <- game_data$roundid_short  
game_data$group <- game_data$matchid
game_data$player <- game_data$playerid
game_data$structure <- game_data$organizationalStructure
game_data$pressure <- cut(game_data$roundid_short,
                          breaks=c(0, 3, 6, 9, 11, 13),
                          labels=c("low","high","low", "high", "low"))

# There is no 'FinalItemSelected' event for rounds 10 or 12, as mines are selected over two rounds 
# We still need to define the choice on each round as innovative or not.
# We can define the 'FinalItemSelected' in rounds 10 or 12, as it will be the same as the mine shown in 'toolsLabel' for rounds 11 & 13. 
# Create subset with data for rounds 11 and 13
game_data_r11_13 <- game_data [game_data$round==11 | game_data$round==13,]
# Assign innovative choice in rounds 10 or 12 according to mine choice available in round 11 or 13
game_data_r11_13$innovation2 <- case_when(
 (game_data_r11_13$toolsLabel == "Mine2,BlackPowder" |
    game_data_r11_13$toolsLabel == "Mine4,BlackPowder") ~ 1, 
  TRUE ~ 0)
game_data_r11_13$roundid <- game_data_r11_13$roundid-1
game_data_r11_13 <- unique(subset (game_data_r11_13, select = c(innovation2, roundid)))
# Merge innovative choice in rounds 10 and 12. 
game_data <- merge(game_data, game_data_r11_13, by="roundid", all.x=TRUE)
filter <- game_data$round==10 | game_data$round==12
game_data$innovation[filter] <- game_data$innovation2[filter]
# Drop 'innovation2' var 
game_data <- subset(game_data, select = -c(innovation2))

# CPT predictions

game_data$toolsCPT <- 1
game_data$toolsCPT[game_data$tools==1] <- 0
game_data$toolsCPT[game_data$tools==2] <- 0
game_data$toolsCPT[game_data$tools==8] <- 0
game_data$toolsCPT<-factor(game_data$toolsCPT)

# EUT predictions

game_data$toolsEUT <- 0
game_data$toolsEUT[game_data$tools==5] <- 1
game_data$toolsEUT[game_data$tools==7] <- 1
game_data$toolsEUT<-factor(game_data$toolsEUT)

# PT predictions

game_data$toolsPT <- 1
game_data$toolsPT[game_data$tools==8] <- 0
game_data$toolsPT<-factor(game_data$toolsPT)

# CPT+Expectancy predictions

game_data$toolsCPTEXP <- 1
game_data$toolsCPTEXP[game_data$tools==1] <- 0
game_data$toolsCPTEXP[game_data$tools==2] <- 0
game_data$toolsCPTEXP[game_data$tools==5] <- 0
game_data$toolsCPTEXP<-factor(game_data$toolsCPTEXP)

# Risk

game_data$risk[game_data$tools==1] <- 0.23
game_data$risk[game_data$tools==2] <- 0.23
game_data$risk[game_data$tools==3] <- 0.06
game_data$risk[game_data$tools==4] <- -0.17
game_data$risk[game_data$tools==5] <- 0
game_data$risk[game_data$tools==6] <- 0.05
game_data$risk[game_data$tools==7] <- 0.24
game_data$risk[game_data$tools==8] <- 0

# Probability

game_data$prb[game_data$tools==1] <- 1.10
game_data$prb[game_data$tools==2] <- 1.10
game_data$prb[game_data$tools==3] <- 1.39
game_data$prb[game_data$tools==4] <- 1.26
game_data$prb[game_data$tools==5] <- 1.03
game_data$prb[game_data$tools==6] <- 1.03
game_data$prb[game_data$tools==7] <- 7.43
game_data$prb[game_data$tools==8] <- 0

# Structure dummies

game_data$structureHie <- 0
game_data$structureHie[game_data$structure=="Hierarchical"] <- 1
game_data$structureCel <- 0
game_data$structureCel[game_data$structure=="Cellular"] <- 1
game_data$structureNet <- 0
game_data$structureNet[game_data$structure=="Network"] <- 1

# Centralization scores (see pre-registration)

game_data$centralization <- NA
game_data$centralization[game_data$structure=="Hierarchical"] <- 0.42
game_data$centralization[game_data$structure=="Cellular"] <- 0
game_data$centralization[game_data$structure=="Network"] <- 0.83

# Leader Weight scores (see pre-registration)

game_data$leaderWeight <- NA
game_data$leaderWeight[game_data$structure=="Hierarchical"] <- 0.45
game_data$leaderWeight[game_data$structure=="Cellular"] <- 0.2
game_data$leaderWeight[game_data$structure=="Network"] <- 0

# Competition dummy

game_data$compStrong <- 0
game_data$compStrong [game_data$competitionLabel=="Strong"] <- 1

# Tool complexity dummy

game_data$complexity <- 0
game_data$complexity [game_data$tools==5 | 
                      game_data$tools==6 |
                      game_data$tools==7 |
                      game_data$tools==8 ] <- 1

# Network density 
# Densities could not be reproduced in different platforms (even using the same random seeds)
# So densities were pre-calculated on the basis of network structure and role disconnections in each round, per game settings
# Find density estimator here: https://github.com/GallupGovt/ngs2/blob/master/cycle3/Boomtown/Analysis/density.R

githubRepo <- "https://raw.githubusercontent.com/GallupGovt/ngs2/master/cycle3/Boomtown/Analysis"
fileHolder <- getURL(paste(githubRepo, "densities.csv", sep = "/"), ssl.verifypeer = FALSE)
fileConn<-file("densities.csv")
writeLines(fileHolder, fileConn)
close(fileConn)
densities <- read.csv(file="densities.csv")

game_data$round2[game_data$roundid_short == 1 | 
                  game_data$roundid_short == 2 | 
                  game_data$roundid_short == 3] <- 1 
game_data$round2[game_data$roundid_short == 4 | 
                   game_data$roundid_short == 5 | 
                   game_data$roundid_short == 6] <- 2
game_data$round2[game_data$roundid_short == 7 | 
                   game_data$roundid_short == 8 | 
                   game_data$roundid_short == 9] <- 3
game_data$round2[game_data$roundid_short == 10 | 
                   game_data$roundid_short == 11] <- 4
game_data$round2[game_data$roundid_short == 12 | 
                   game_data$roundid_short == 13] <- 5

game_data <- merge(game_data, densities, by= c("settingsNum","round2"), all=TRUE)

# Conformity measures (aggregate level)

game_data$groupRound <- as.numeric(game_data$group)*100+as.numeric(game_data$round)
game_data$playernum <- as.integer(factor(game_data$player))

game_dataNum <- game_data
game_dataNum[] <- lapply(game_dataNum, as.numeric)
game_dataNum$grmot1 <- ave(game_dataNum$inmot1, game_data$groupRound)
game_dataNum$grmot2 <- ave(game_dataNum$inmot2, game_data$groupRound)
game_dataNum$conformity<-ifelse(
  game_dataNum$grmot1>0.5 & game_dataNum$inmot1==0 & game_dataNum$inmot2==1,1,
  ifelse(
    game_dataNum$grmot1<0.5 & game_dataNum$inmot1==1 & game_dataNum$inmot2==0, 1, 0))

# Generate aggregate dataset

game_dataGroup <-aggregate(game_dataNum, 
                           by=list(game_dataNum$groupRound), FUN=mean, 
                           na.rm=TRUE)
game_dataGroup$nPlayers <- aggregate(playernum ~ groupRound, 
                                     data = game_dataNum, FUN = length)$playernum
game_dataGroup$unanimity <- (game_dataGroup$nPlayers-1)/game_dataGroup$nPlayers
game_dataGroup$unanimous <-ifelse(game_dataGroup$grmot1 == game_dataGroup$unanimity,1,0)
game_dataGroup2 <- game_dataGroup
game_dataGroup2[] <- lapply(game_dataGroup, factor)
game_dataGroup2$conformity <- as.numeric(game_dataGroup$conformity)
game_dataGroup2$grmot1 <- as.numeric(game_dataGroup$grmot1)
game_dataGroup2$grmot2 <- as.numeric(game_dataGroup$grmot2)
game_dataGroup2$risk <- as.numeric(game_dataGroup$risk)
game_dataGroup2$innovation <- as.numeric(game_dataGroup$innovation)-1
game_dataGroup <- game_dataGroup2

# Merge conformity and group motivation vars back with individual level data

game_data <- merge(game_data, 
                   game_dataGroup[c("grmot1", "grmot2", "conformity", "unanimous", "groupRound")], 
                   by="groupRound")

## Part 5: survey data cleaning ----
# read in data
# Need to remove duplicated rows in "survey_results_2" to makes sure the match_player_id is unique.
survey_1 = read.csv(paste(dd_input, "survey_results_1.csv", sep="/"), skip = 1, header = 1, stringsAsFactors = F)
survey_2 = read.csv(paste(dd_input, "survey_results_2_cleaned.csv", sep="/"), header = 1, stringsAsFactors = F)

# cleaning survey_results_1
survey_1 <- survey_1 %>%
  select(selected_columns_1) %>%
  mutate(PlayerId = gsub("boomtown-", "", PlayerId))

for (var in names(survey_1)[grepl("_1", names(survey_1))]){
  survey_1[, var] <-  gsub("-1", "", survey_1[, var])
  survey_1[, var] <- as.numeric(survey_1[,var])
}

if (any(duplicated(survey_1["PlayerId"]))) {
  stop(paste("There are duplicated playerids in survey_results_1.csv. Remove duplicates before proceeding."))
}

# cleaning survey_results_2
survey_2 <- survey_2 %>%
  select(selected_columns_2) %>%
  mutate(PlayerId = gsub("boomtown-", "", PlayerId), 
         MatchId_PlayerId = paste(MatchId, PlayerId, sep = "-")) %>%
  select(-c("MatchId", "PlayerId"))

for (var in names(survey_2)[grepl("_2", names(survey_2))]){
  survey_2[, var] <-  gsub("-1", "", survey_2[, var])
  survey_2[, var] <- as.numeric(survey_2[,var])
}

if (any(duplicated(survey_2["MatchId_PlayerId"]))) {
  stop(paste("There are duplicated match_player_ids in survey_results_2.csv. Remove duplicates before proceeding."))
}

## Part 6: merge game data with survey data at the match level ----

game_data_aggr <- aggregate(game_data[,!grepl(paste(vars, collapse = "|"), names(game_data))],
                            by = list(matchid = game_data[,"matchid"], playerid = game_data[,"playerid"]),
                            FUN = unique)
game_data_aggr <- game_data_aggr %>% mutate(matchid_playerid = paste(matchid, playerid, sep = "-"))

# merge game_data_aggr with survey_1 using palyer id first, and then merge with survey_2 using matchid_playerid
game_survey_data <- merge(game_data_aggr, survey_1, by.x = "playerid", by.y = "PlayerId", all.x = T)
game_survey_data <- merge(game_survey_data, survey_2, by.x = "matchid_playerid", by.y = "MatchId_PlayerId", all.x = T)

# check value ranges for each variable
check_range(data = game_survey_data)

# Drop scrap variables

dropvars <- c("groupRound", 
              "round2", 
              "matchSetting1Label", 
              "roundid_short", 
              "playerid", 
              "competitive.level", 
              "consumableKey")

game_data <- game_data[ , -which(names(game_data) %in% dropvars)]
game_dataGroup <- game_dataGroup[ , -which(names(game_dataGroup) %in% dropvars)]

# output all data (individual and group level)

write.csv(game_data, paste(dd_output, 'game_data.csv', sep = '/'), row.names = FALSE)
write.csv(game_dataGroup, paste(dd_output, 'game_data_group.csv', sep = '/'), row.names = FALSE)
write.csv(game_survey_data, paste(dd_output, "game_survey_data.csv", sep="/"), row.names = F)
