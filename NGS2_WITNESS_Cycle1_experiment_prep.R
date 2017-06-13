#Created by Pablo Diego Rosell, PhD, for Gallup inc. in March 2017
a = Sys.info()
if(a['sysname'] == "Windows") {
    setwd(paste0('C:/Users/pablo_diego-rosell/Desktop/Projects/DARPA/Cycle 1/',
                 'Research Protocols/Registration/Experiment 2/Mock data'))
} else {
    if(a['user'] == 'matt_hoover') {
        setwd('~/git/NGS2')
    } else {
        setwd("~/Research/Gallup/GallupAnalytics/RandD/NGS2/NGS2-Experiment")
    }
}
rm(list=ls())

# Define constants
CYCLE <- 1
EXPERIMENTS <- 1:2

# Install packages that are required for this file
list.of.packages <- c("pacman", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in%
                                   installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

pacman::p_load(multiwayvcov, lmtest)

# Load raw breadboard data
FILES <- c()
FILENAMES <- c()
for(i in EXPERIMENTS) {
    FILES <- c(FILES,
              paste(paste0('NGS2-Cycle', CYCLE, '-Experiment', i),
                  paste0('data/experiment-', i, '_test_2017-03-17.csv'),
                  sep = '/'))
    FILENAMES <- c(FILENAMES, paste0('exp', i))
}
bb <- lapply(FILES, read.csv, header = TRUE, sep = ',', stringsAsFactors = FALSE)
names(bb) <- FILENAMES

# Experiment 1


# Experiment 2
###############################################################################
#Restructure raw data output from Breadboard into 'cooperation' dataset
###############################################################################
#Subset cooperation decisions, drop "datetime" and "event fields"
coop <- subset(bb$exp2, event == 'CooperationDecision',
               select = c('id', 'data.name', 'data.value'))

#Reshape long to wide on cooperation events
coop.events <- reshape(coop, timevar = "data.name", idvar = c("id"),
                       direction = "wide")

#Declare `data.value` fields as numerics so events can be sorted logically
coop.events[, 2:ncol(coop.events)] <- apply(coop.events[, 2:ncol(coop.events)], 2,
    function(x) {
        return(ifelse(grepl('^_', x), as.numeric(gsub('_', '', x)), as.numeric(x)))
    }
)

#Sort cooperation events by player id ('data.value.pid') and round
#('data.value.curRound')
coop.events <- coop.events[order(coop.events$data.value.pid,
                                 coop.events$data.value.curRound),]

#Create list of group selections by each individual ('pid') at the "ChooseGroup"
#and "ChangeGroup" events
group <- subset(bb$exp2, event %in% c('ChooseGroup', 'ChangeGroup') &
                data.name %in% c('group', 'pid', 'curRound'),
                select = c('id', 'data.name', 'data.value'))
group.events <- reshape(group, timevar = "data.name", idvar = c("id"),
                        direction = "wide")[,-(1)]
group.events <- data.frame(apply(group.events, 2, function(x) {
    return(ifelse(grepl('^_', x), as.numeric(gsub('_', '', x)), as.numeric(x)))
}))

group.events$link <- (group.events$data.value.pid * 1000 +
                      group.events$data.value.curRound)
group.events<-group.events[,c(2,4)]
colnames(group.events)[1] <- c("group")

#Since group changes only happen at rounds 1, 4, 8, 12 and so on, need to
#recode id links at the coop.events level to match this sequence
#CHECK THAT THIS IS INDEED THE GROUP CHANGE SEQUENCE. (ALSO FIND LESS
#EMBARRASSING WAY TO CREATE THIS 'recround' VARIABLE!)
#WILL NEED TO ADD MORE 'recround' values if there are more than 24 rounds in a given game

## Comment by Anu: I changed your code to following to reflect the same logic
coop.events$recround <- cut(coop.events$data.value.curRound,
                            breaks = c(seq(1,24, 4), 24), right = FALSE,
                            include.lowest = TRUE, labels = c(1, 4, 8, 12, 16, 20))

#Create id link for coop.events data frame to merge the group change data
## Comment by Anu: We need to change the recround to be numeric to do the operation
coop.events$link<-coop.events$data.value.pid*1000 + as.numeric(coop.events$recround)
cooperation <- merge(coop.events, group.events,by="link")

#Add condition
#CHECK HOW THIS WORKS WITH MULTIPLE SESSIONS/CONDITIONS!!)
condition1<- as.character(bb$exp2$data.value[bb$exp2$data.name == "M"])
condition2<- as.numeric(as.character(subset(breadboard, data.name=="sameGroupConnectivity")[,5]))
condition3<- as.numeric(as.character(subset(breadboard, data.name=="diffGroupConnectivity")[,5]))
if (condition1=="2" & condition2>condition3) {
  cooperation$condition<-"Biased-2"
} else if (condition1=="4" & condition2>condition3) {
  cooperation$condition<-"Biased-4"
} else if (condition1=="2" & condition2==condition3) {
  cooperation$condition<-"Unbiased-2"
} else if (condition1=="4" & condition2==condition3)
  cooperation$condition<-"Unbiased-4"
#'condition3'>'condition2' (biased connections towards outgroup)is not contemplated in the current experiment
#If this situation would happen by mistake, the value of 'condition' will show as "".
#cooperation$condition
#Add session number
#CHECK HOW THIS WORKS WITH MULTIPLE SESSIONS/CONDITIONS!!)
cooperation$session<- "1"
#Drop unused variables
cooperation<-cooperation [,c(3:6,8:10)]
#Change variable names to shorter, clearer names
colnames(cooperation) <- c("playerid", "round_num", "group_coop", "decision0d1c", "group", "condition", "session")
#Remove factor levels for 'group_coop' and 'decision0d1c'
cooperation$group_coop<-as.numeric(as.character(cooperation$group_coop))
cooperation$decision0d1c<-as.numeric(as.character(cooperation$decision0d1c))
#Add in-group vs. out-group dummy
cooperation$ingroup <- ifelse(cooperation$group_coop==cooperation$group, 1, 0)
#Add biased vs. unbiased dummy
cooperation$biased <- ifelse(cooperation$condition=="Biased-2" | cooperation$condition=="Biased-4", 1, 0)
cooperation$identities <- ifelse(cooperation$condition=="Biased-4" | cooperation$condition=="Unbiased-4", 1, 0)
write.csv(cooperation, file = "NGS2-Cycle1-Experiment2/cooperation.csv",
          row.names = FALSE)


########################################################################################################################
#Restructure raw data output from Breadboard into 'rewire' dataset
########################################################################################################################

#Subset change decisions, drop "datetime" and "event fields"
changegroup <- subset(breadboard, event=="ChangeGroup" & (data.name=="group" | data.name=="pid" | data.name=="curRound"))[,-(2:3)]
changegroup.events <- reshape(changegroup, timevar = "data.name", idvar = c("id"), direction = "wide")[,-(1)]
changegroup.events$data.value.pid<-as.numeric(changegroup.events$data.value.pid)
changegroup.events$data.value.group<-as.numeric(as.character(changegroup.events$data.value.group))
changegroup.events$data.value.curRound<-as.numeric(as.character(changegroup.events$data.value.curRound))
changegroup.events$link<-changegroup.events$data.value.pid*1000+changegroup.events$data.value.curRound
colnames(changegroup.events)[1] <- "playerid"
colnames(changegroup.events)[2] <- "group"
colnames(changegroup.events)[3] <- "round_num"
#Subset rewire decisions, drop "datetime" and "event fields"
rewire <- subset(breadboard, event=="RewiringDecision")[,-(2:3)]
rewire.events<-reshape(rewire, timevar = "data.name", idvar = c("id"), direction = "wide")
rewire.events$data.value.pid<-as.numeric(rewire.events$data.value.pid)
rewire.events$data.value.group<-as.numeric(as.character(rewire.events$data.value.group))
rewire.events$data.value.curRound<-as.numeric(as.character(rewire.events$data.value.curRound))
rewire.events$link<-rewire.events$data.value.pid*1000+rewire.events$data.value.curRound
rewire.events<-rewire.events[,c(4:6)]
colnames(rewire.events)[1] <- "group_connect"
colnames(rewire.events)[2] <- "connect"
rewire <- merge(changegroup.events, rewire.events,by="link")
#Add condition
#CHECK HOW THIS WORKS WITH MULTIPLE SESSIONS/CONDITIONS!!)
if (condition1=="2" & condition2>condition3) {
  rewire$condition<-"Biased-2"
} else if (condition1=="4" & condition2>condition3) {
  rewire$condition<-"Biased-4"
} else if (condition1=="2" & condition2==condition3) {
  rewire$condition<-"Unbiased-2"
} else if (condition1=="4" & condition2==condition3)
  rewire$condition<-"Unbiased-4"
#Add session number
#CHECK HOW THIS WORKS WITH MULTIPLE SESSIONS/CONDITIONS!!)
rewire$sessionnum<- "1"
#Drop unused link variable
rewire<-rewire[,-1]
#Remove factor levels for 'group_coop' and 'decision0d1c'
rewire$connect<-as.numeric(as.character(rewire$connect))
#Add in-group vs. out-group dummy
rewire$ingroup <- ifelse(rewire$group_connect==rewire$group, 1, 0)
#Add biased vs. unbiased dummy
rewire$biased <- ifelse(rewire$condition=="Biased-2" | rewire$condition=="Biased-4", 1, 0)
rewire$identities <- ifelse(rewire$condition=="Biased-4" | rewire$condition=="Unbiased-4", 1, 0)
write.csv(rewire, file = "NGS2-Cycle1-Experiment2/rewire.csv", row.names = FALSE)


