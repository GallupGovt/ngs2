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

# Install packages that are required for this file
list.of.packages <- c("dplyr", "pacman", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in%
                                   installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

pacman::p_load(multiwayvcov, lmtest)

# Define functions
add_actions <- function(data, actions, focus = c('player', 'alter')) {
    # add respondent actions to `rewire` dataset
    if(focus == 'player') {
        data <- merge(
            data,
            actions[, c('pid', 'round', 'action')],
            by = c('round', 'pid')
        )
        data$action <- ifelse(data$action == 'cooperate', 'C', 'D')
        names(data)[grep('^action$', names(data))] <- 'playeraction'
    } else {
        data <- merge(
            data,
            actions[, c('pid', 'round', 'action')],
            by.x = c('round', 'nid'),
            by.y = c('round', 'pid')
        )
        data$action <- ifelse(data$action == 'cooperate', 'C', 'D')
        names(data)[grep('^action$', names(data))] <- 'alteraction'
    }
    return(data)
}

add_ties <- function(rewire, connections, rd_changes) {
    # add current and previous ties for actors
    rewire$previouslytie <- NA
    rewire$nowtie <- NA
    for(i in 1:nrow(rewire)) {
        if(rewire$round[i] == 1) {
            id1 <- rewire[i, 'pid']
            id2 <- rewire[i, 'nid']
            conn_id <- which(
                (connections$playerId1 == id1 & connections$playerId2 == id2) |
                (connections$playerId1 == id2 & connections$playerId2 == id1)
            )
            rewire$previouslytie[i] <- ifelse(length(conn_id) == 0, 0, 1)
            rewire$nowtie[i] <- ifelse(
                (rewire$previouslytie[i] == 1 & rewire$break_tie[i] != 1) |
                (rewire$previouslytie[i] == 0 & rewire$make_tie[i] == 1), 1, 0
            )
        } else {
            rd <- rewire$round[i] - 1
            id1 <- rewire[i, 'pid']
            id2 <- rewire[i, 'nid']
            conn_id <- which(
                (rd_changes[[rd]]$playerId1 == id1 & rd_changes[[rd]]$playerId2 == id2) |
                (rd_changes[[rd]]$playerId1 == id2 & rd_changes[[rd]]$playerId2 == id1)
            )
            rewire$previouslytie[i] <- ifelse(length(conn_id) == 0, 0, 1)
            rewire$nowtie[i] <- ifelse(
                (rewire$previouslytie[i] == 1 & rewire$break_tie[i] != 1) |
                (rewire$previouslytie[i] == 0 & rewire$make_tie[i] == 1), 1, 0
            )
        }
    }
    names(rewire)[grep('pid', names(rewire))] <- 'playerid'
    names(rewire)[grep('nid', names(rewire))] <- 'alterid'

    return(rewire)
}

build_cooperation <- function(actions) {
    # start `cooperation` that will build on `actions` and `connections`
    tmp <- merge(
        actions,
        aggregate(actions$pid, by = list(actions$round), length),
        by.x = 'round',
        by.y = 'Group.1'
    )
    names(tmp)[names(tmp) == 'x'] <- 'group_size'

    tmp$previous_decision <- NA
    for(i in 1:nrow(tmp)) {
        if(tmp[i, 'round'] > 1) {
            id <- tmp$pid[i]
            rd <- tmp$round[i] - 1
            tmp$previous_decision[i] <- tmp$action[tmp$pid == id & tmp$round == rd]
        }
    }

    tmp[, c('action', 'previous_decision')] <- apply(
        tmp[, c('action', 'previous_decision')], 2, function(x) {
            ifelse(x == 'cooperate', 1, 0)
        }
    )

    return(tmp[, -grep('^id', names(tmp))])
}

build_rewire <- function(changes) {
    # start building the `rewire` output file
    tmp <- changes[
        order(changes$round, changes$pid, changes$nid),
        c('round', 'pid', 'nid', 'action_num')
    ]

    tmp$break_tie <- ifelse(tmp$action_num == -1, 1, 0)
    tmp$make_tie <- ifelse(tmp$action_num == 1, 1, 0)
    tmp$action_num <- ifelse(tmp$action != 0, 1, tmp$action_num)

    return(tmp)
}

calculate_connections <- function(rounds) {
    # count up number of connections by id by round
    tmp <- do.call(rbind, mapply(function(x, rd) {
        tmp1 <- aggregate(x$playerId2, by = list(x$playerId1), length)
        tmp2 <- aggregate(x$playerId1, by = list(x$playerId2), length)
        tmp <- merge(tmp1, tmp2, by = 'Group.1', all = TRUE)
        tmp$num_neighbors <- apply(tmp[, grep('^x', names(tmp))], 1, function(x) {
            sum(x, na.rm = TRUE)
        })
        tmp$round <- rd
        tmp <- tmp[, c('round', 'Group.1', 'num_neighbors')]
        names(tmp)[2] <- 'pid'
        return(tmp)
    }, rounds, 1:length(rounds), SIMPLIFY = FALSE))
    return(tmp)
}

changes_by_round <- function(changes, connections) {
    # identify changes by id by round
    round_changes <- list()
    connections$change_event <- 0
    for(j in 1:max(changes$round)) {
        tmp <- subset(changes, changes$round == j)
        tmp <- tmp[order(tmp$id), ]
        for(i in 1:nrow(tmp)) {
            id1 <- tmp[i, 'nid']
            id2 <- tmp[i, 'pid']
            conn_id <- which((connections$playerId1 == id1 & connections$playerId2 == id2) |
                             (connections$playerId1 == id2 & connections$playerId2 == id1))
            if(length(conn_id) == 0) {
                if(tmp[i, 'action'] == 'makeConnection') {
                    connections <- rbind(connections,
                                  c(tmp[i, 'id'],
                                    id1,
                                    id2,
                                    1)
                    )
                }
            } else {
                connections$change_event[conn_id] <- ifelse(
                    is.na(connections$change_event[conn_id]),
                    tmp[i, 'action_num'],
                    connections$change_event[conn_id] + tmp[i, 'action_num']
                )
            }
        }
        connections <- connections[connections$change_event >= 0, ]
        connections$change_event <- 0
        round_changes[[j]] <- connections[, c('id', 'playerId1', 'playerId2')]
        names(round_changes)[j] <- paste0('r', j)
    }
    return(round_changes)
}

create_subsets <- function(data, action, scrubs) {
    # create data subsets
    tmp <- dcast(subset(data, event == action), id ~ data.name,
                 value.var = 'data.value')
    tmp[, grep(scrubs, names(tmp))] <- apply(
        tmp[, grep(scrubs, names(tmp))], 2, function(x) {
            as.numeric(gsub('_', '', x))
        }
    )
    return(tmp[complete.cases(tmp), ])
}

strip_chars <- function(d) {
    return(
        data.frame(apply(d, 2, function(x) {
            return(
                ifelse(grepl('^_', x),
                       as.numeric(gsub('_', '', x)),
                       as.numeric(x))
            )
        }))
    )
}

# Load raw breadboard data
exp1 <- read.csv('NGS2-Cycle1-Experiment1/data/experiment-1_test_2017-03-17.csv',
                 header = TRUE, sep = ',', stringsAsFactors = FALSE)
exp2 <- read.csv('NGS2-Cycle1-Experiment2/data/experiment-2_test_2017-03-17.csv',
                 header = TRUE, sep = ',', stringsAsFactors = FALSE)

# EXPERIMENT 1
# cooperation dataset
# create subsets for manipulation
conn <- create_subsets(exp1, 'Connected', 'player')
act <- create_subsets(exp1, 'cooperationEvent', 'pid|round')
chng <- create_subsets(exp1, 'rewiringEvent', 'pid|nid|round')
chng$action_num <- ifelse(chng$action == 'maintainConnection', 0,
                          ifelse(chng$action == 'breakConnection', -1, 1))

# start building the `cooperation` output file
coop <- build_cooperation(act)

# calculate actor moves by round
round_changes <- changes_by_round(chng, conn)
nbr_connections_round <- calculate_connections(round_changes)

# finish off the `cooperation` dataset
coop <- merge(coop, nbr_connections_round, by = c('round', 'pid'))

# output dataset to disk
write.csv(coop[order(coop$round, coop$pid), ],
          file = 'NGS2-Cycle1-Experiment1/cooperation_exp1.csv',
          row.names = FALSE, na='')

# rewire dataset
# start building `rewire` output file
rewire <- build_rewire(chng)

# add respondent actions to `rewire` dataset
rewire <- add_actions(rewire, act, focus = 'player')
rewire <- add_actions(rewire, act, focus = 'alter')

rewire$state <- apply(rewire[, grep('action$', names(rewire))], 1, function(x) {
    paste0(x, collapse = '')
})
rewire$CC <- ifelse(rewire$state == 'CC', 1, 0)
rewire$DD <- ifelse(rewire$state == 'DD', 1, 0)

# add in previous and current ties
rewire <- add_ties(rewire, conn, round_changes)

# output dataset to disk
var_order <- c(
    'session',
    'round',
    'playerid',
    'alterid',
    'previouslytie',
    'nowtie',
    'playeraction',
    'alteraction',
    'otherD',
    'action_num',
    'break_tie',
    'make_tie',
    'state',
    'CC',
    'DD'
)
rewire$session <- 1


write.csv(rewire[order(rewire$round, rewire$playerid), var_order],
          file = 'NGS2-Cycle1-Experiment1/rewire_exp1.csv',
          row.names = FALSE, na = '')

# Experiment 2
###############################################################################
#Restructure raw data output from Breadboard into 'cooperation' dataset
###############################################################################
#Subset cooperation decisions, drop "datetime" and "event fields"
coop <- subset(exp2, event == 'CooperationDecision',
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
group <- subset(exp2, event %in% c('ChooseGroup', 'ChangeGroup') &
                data.name %in% c('group', 'pid', 'curRound'),
                select = c('id', 'data.name', 'data.value'))
group.events <- reshape(group, timevar = "data.name", idvar = c("id"),
                        direction = "wide")[,-(1)]
group.events <- strip_chars(group.events)

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
coop.events$link<-coop.events$data.value.pid*1000 + as.numeric(as.character(coop.events$recround))
cooperation <- merge(coop.events, group.events,by="link")

#Add condition
#CHECK HOW THIS WORKS WITH MULTIPLE SESSIONS/CONDITIONS!!)
condition1<- as.character(exp2$data.value[exp2$data.name == "M"])
condition2<- as.numeric(as.character(subset(exp2, data.name=="sameGroupConnectivity")[,5]))
condition3<- as.numeric(as.character(subset(exp2, data.name=="diffGroupConnectivity")[,5]))
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

write.csv(cooperation, file = "NGS2-Cycle1-Experiment2/cooperation_exp2.csv",
          row.names = FALSE)

########################################################################################################################
#Restructure raw data output from Breadboard into 'rewire' dataset
########################################################################################################################

#Subset change decisions, drop "datetime" and "event fields"
changegroup <- subset(exp2,
    event=="ChangeGroup" &
    (data.name=="group" | data.name=="pid" | data.name=="curRound")
)[,-(2:3)]
changegroup.events <- reshape(changegroup, timevar = "data.name", idvar = c("id"), direction = "wide")[,-(1)]
changegroup.events <- strip_chars(changegroup.events)
changegroup.events$link<-changegroup.events$data.value.pid*1000+changegroup.events$data.value.curRound
colnames(changegroup.events)[1] <- "playerid"
colnames(changegroup.events)[2] <- "group"
colnames(changegroup.events)[3] <- "round_num"

#Subset rewire decisions, drop "datetime" and "event fields"
rewire <- subset(exp2, event=="RewiringDecision")[,-(2:3)]
rewire.events<-reshape(rewire, timevar = "data.name", idvar = c("id"), direction = "wide")
rewire.events <- strip_chars(rewire.events)
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

write.csv(rewire, file = "NGS2-Cycle1-Experiment2/rewire_exp2.csv", row.names = FALSE)
