#Created by Pablo Diego Rosell, PhD, for Gallup inc. in March 2017

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

identify_condition <- function(data, condition) {
    # takes a specified initial starting condition and returns its values
    tmp <- data$data.value[data$data.name == condition]
    if(length(grep('^[A-Za-z]', tmp)) == 0) {
        return(as.numeric(tmp))
    } else {
        return(as.logical(toupper(tmp)))
    }
}

match_empanelment_bb_ids <- function(emp, id_dict) {
    # reads in files to match up empanelment and breadboard ids
    comps <- read.csv(paste('data', emp, sep = '/'), header = TRUE,
                      sep = ',', stringsAsFactors = FALSE)
    bb_ids <- read.csv(paste('data', id_dict, sep = '/'), header = TRUE,
                       sep = ',', stringsAsFactors = FALSE)
    emp_bb_ids <- merge(
        comps[, c('RecipientLastName', 'RecipientFirstName', 'ExternalReference')],
        bb_ids[, c('LastName', 'FirstName', 'USERID')],
        by.x = c('RecipientLastName', 'RecipientFirstName'),
        by.y = c('LastName', 'FirstName')
    )
    names(emp_bb_ids)[3:4] <- c('empanel_id', 'bb_id')
    if(nrow(emp_bb_ids) == nrow(bb_ids)) {
        return(emp_bb_ids[, c('empanel_id', 'bb_id')])
    } else {
        stop(paste('There is a problem -- some BreadBoard IDs do not have an',
                   'empanelment match. Please fix...'))
    }
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
exp1 <- read.csv('NGS2-Cycle1-Experiment1/data/ngs2_e1_pilot_2017-07-12-01_9222.csv',
                 header = TRUE, sep = ',', stringsAsFactors = FALSE)
exp2 <- read.csv('NGS2-Cycle1-Experiment2/data/ngs2_e2_pilot_2017-07-12-01_10381.csv',
                 header = TRUE, sep = ',', stringsAsFactors = FALSE)

# gather empanelment information to add in ids for experiments below
emp_bb_id_matches <- match_empanelment_bb_ids(
    'ngs2_empanelment_pilot_completes.csv',
    'empanelment_breadboard_ids.csv'
)

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

# add in static parameters from dataset for completeness
# MAH: placeholder for now -- haven't received a file with multiple sessions
#      in it, so difficult to programmatically determine how to identify
#      session... but will update as soon as we do know how
coop$session <- 1
coop$condition <- ifelse(
    identify_condition(exp1, 'connectivity') == .1, 'Viscous',
    ifelse(identify_condition(exp1, 'connectivity') == .3, 'Fluid',
    # MAH: need to check this is a valid way to identify the random/static
    #      conditions
    ifelse(identify_condition(exp1, 'k') > 0, 'Random', 'Static')))

# add in empanelment id for non-bots
coop <- merge(coop, emp_bb_id_matches, by.x = 'pid', by.y = 'bb_id',
              all.x = TRUE)

# output dataset to disk
var_order <- c(
    'round',
    'pid',
    'action',
    'group_size',
    'previous_decision',
    'num_neighbors',
    'session',
    'condition',
    'empanel_id'
)
write.csv(coop[order(coop$round, coop$pid), var_order],
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
rewire$other_d <- ifelse(rewire$alteraction == 'D', 1, 0)

# add in previous and current ties
rewire <- add_ties(rewire, conn, round_changes)

# add in static parameters from dataset for completeness
# MAH: placeholder for now -- haven't received a file with multiple sessions
#      in it, so difficult to programmatically determine how to identify
#      session... but will update as soon as we do know how
rewire$session <- 1

# add in empanelment id for non-bots
rewire <- merge(rewire, emp_bb_id_matches, by.x = 'playerid', by.y = 'bb_id',
                all.x = TRUE)
names(rewire)[names(rewire) == 'empanel_id'] <- 'player_empanel_id'
rewire <- merge(rewire, emp_bb_id_matches, by.x = 'alterid', by.y = 'bb_id',
                all.x = TRUE)
names(rewire)[names(rewire) == 'empanel_id'] <- 'alter_empanel_id'

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
    'other_d',
    'action_num',
    'break_tie',
    'make_tie',
    'state',
    'CC',
    'DD',
    'player_empanel_id',
    'alter_empanel_id'
)

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

# add in empanelment id for non-bots
cooperation <- merge(cooperation, emp_bb_id_matches, by.x = 'playerid',
                     by.y = 'bb_id', all.x = TRUE)

write.csv(
    cooperation[order(cooperation$round_num, cooperation$playerid), ],
    file = "NGS2-Cycle1-Experiment2/cooperation_exp2.csv",
    row.names = FALSE,
    na = ''
)

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

# add in empanelment id for non-bots
rewire <- merge(rewire, emp_bb_id_matches, by.x = 'playerid', by.y = 'bb_id',
                all.x = TRUE)

write.csv(
    rewire[order(rewire$round_num, rewire$playerid), ],
    file = "NGS2-Cycle1-Experiment2/rewire_exp2.csv",
    row.names = FALSE,
    na = ''
)

####################################################################################################
# Coloring Game
####################################################################################################

# Load raw breadboard data
coloring <- read.csv('Coloring game/ngs2_e3_pilot_2017-07-12-01_9409.csv',
                 header = TRUE, sep = ',', stringsAsFactors = FALSE)
players<- as.data.frame(subset(coloring$data.value, coloring$event=='PlayerReady'))
colnames(players)[1]<-"pid"

### Compute performance metrics
# Graph completion time

### There are multiple end times, apparently 2 players went on to a different game. 
### Take the first end time as valid for the 5 players that completed the game.  
time.start <- as.POSIXct(subset(coloring$datetime, coloring$event=='GameStart'))
time.end <- as.POSIXct(subset(coloring$datetime, coloring$event=='GameEnd')[1])
players$completion.time<-time.end-time.start

# Individual probability of cooperation

av.cooperation.exp1 <-aggregate(exp1_cooperation$action, 
                               by=list(exp1_cooperation$pid), 
                               FUN=mean, 
                               na.rm=TRUE)
colnames(av.cooperation.exp1)<-c("pid", "prob.cooperate")

### Network structure metrics (per Marcus Alexander) 

connections.coloring <- subset(coloring, 
                      event=="Connected", 
                      select=c("id", 
                               "data.name", 
                               "data.value")
)

connections.coloring.dcast <- dcast(connections.coloring, 
                           id~ data.name, 
                           value.var="data.value")

connections.from.to.coloring <- cbind(connections.coloring.dcast$playerId1,
                                      connections.coloring.dcast$playerId2)
nodes.coloring <- unique(
  append(
    (unique(connections.from.to.coloring[,1])), 
    (unique(connections.from.to.coloring[,2]))
    )
  )

net.coloring <- graph_from_data_frame(d=connections.from.to.coloring, 
                             vertices=nodes.coloring, 
                             directed=F)

# Network metrics

net.edge_density.coloring <- edge_density(net.coloring, loops=F)
net.reciprocity.coloring <- reciprocity(net.coloring)
net.transitivity.coloring <- transitivity(net.coloring, type="global") # net is treated as an undirected network
net.diameter.coloring <- diameter(net.coloring, directed=F, weights=NA)
net.assortativity_degree.coloring <- assortativity_degree(net.coloring, directed=F)

# Overall probability of cooperation in final round

av.cooperation.round.exp1 <-aggregate(
  as.integer(exp1_cooperation$action), 
  by=list(exp1_cooperation$round), 
  FUN=mean,
  na.rm=TRUE)

colnames(av.cooperation.round.exp1) <- c("finalRound", "av.coop")

av.cooperation.round.final.exp1 <- subset(av.cooperation.round.exp1, 
                                          finalRound==max(finalRound),
                                          select=c("av.coop")
                                          )

# Node metrics

# Degree

deg.coloring<-degree(net.coloring, mode="all")
deg.frame.coloring<-cbind(names(deg.coloring), deg.coloring)
colnames(deg.frame.coloring)<- c("pid", "degree")

# Centrality (eg eigenvalue)

eigen<-eigen_centrality(net.coloring, directed=T, weights=NA)
eigen.vector.coloring<- eigen$vector

# Plot 

plot(net.coloring, 
     edge.arrow.size=.2, 
     edge.curved=0,
     vertex.color="orange", 
     vertex.frame.color="#555555",
     vertex.label=V(net.coloring)$pid, 
     vertex.label.color="black",
     vertex.label.cex=.7, 
     vertex.size=(deg.coloring)*3)

# Merge all data into one analytical dataset and save

coloring.1 <- merge (players, av.cooperation.exp1, by = "pid")
coloring.2 <- data.frame(coloring.1, 
                         av.coop.final.exp1 = av.cooperation.round.final.exp1,
                         edge_density = net.edge_density.coloring,
                         reciprocity = net.reciprocity.coloring,
                         transitivity = net.transitivity.coloring, 
                         diameter = net.diameter.coloring,
                         assortativity_degree = net.assortativity_degree.coloring
                         )

coloring.3 <- merge (coloring.2, deg.frame, by = "pid")
coloring.save <- merge (coloring.3, eigen.frame, by = "pid")

write.csv(coloring.save, 
          "NGS2-Cycle1-Experiment2/coloring.exp1.csv")

####################################################################################################
# DIFI
####################################################################################################

# Breadboard extract function by event type

breadboard.extract <- function (eventType) {
  breadboardFields <- c("id", 
                        "data.name", 
                        "data.value")
  data.subset <- subset(exp2, 
                        event==eventType, 
                        select=breadboardFields)
  data.subset.dcast <- dcast(data.subset, 
                             id ~ data.name, 
                             value.var="data.value")
  if(c('pid') %in% names(data.subset.dcast)) 
    {
    data.subset.dcast$pid <- as.integer(gsub("_", 
                                             "",   
                                             data.subset.dcast$pid)
                                        )
  }
return(data.subset.dcast)
}

# DIFI scores

DIFI.final<-breadboard.extract ("DIFI")

# Individual probability of cooperation

av.cooperation.exp2 <-aggregate(exp2_cooperation$decision0d1c, 
                               by=list(exp2_cooperation$playerid), 
                               FUN=mean, 
                               na.rm=TRUE)
colnames(av.cooperation.exp2)<-c("pid", "prob.cooperate")

### Create frame of nodes in network, with their final score and final group. 

# Final score

score.exp2 <- breadboard.extract ("FinalScore")

# Final group

group.data.exp2<-breadboard.extract ("ChangeGroup")
group.data.exp2$finalGroupRound <- as.integer(group.data.exp2$curRound)
group.data.exp2$finalGroup <- as.integer(group.data.exp2$group)
group.data.exp2.final  <- subset(group.data.exp2, 
                            finalGroupRound==max(finalGroupRound), 
                            select=c("pid", 
                                     "finalGroup", 
                                     "finalGroupRound")
                            )

### Add network properties (cooperative structure)

connections.exp2 <- subset(exp2, 
                      event=="Connected" | event=="Disconnected", 
                      select=c("id", 
                               "event", 
                               "data.name", 
                               "data.value")
                      )

connections.exp2$data.value <- as.integer(gsub("_", 
                                         "",   
                                         connections.exp2$data.value)
                                     )

connections.exp2.dcast <- dcast(connections.exp2, 
                           id+event ~ data.name, 
                           value.var="data.value")

connections.exp2.final <- connections.exp2.dcast[0,]

for (i in 1:length(connections.exp2.dcast$id)) 
  {
  if 
  (connections.exp2.dcast$event[i]=='Connected'){
    connections.exp2.final[i,] <- rbind(connections.exp2.dcast[i,])
    } 
  else if 
  (connections.exp2.dcast$event[i]=='Disconnected'){
    connections.exp2.final <- 
        subset(connections.exp2.final, !(
          (playerId1 == connections.exp2.dcast[i, c("playerId1")] & 
             playerId2 == connections.exp2.dcast[i, c("playerId2")]) |
          (playerId1 == connections.exp2.dcast[i, c("playerId2")] & 
             playerId2 == connections.exp2.dcast[i, c("playerId1")]) 
                                    )
               )
      }
  else {
    NULL
    }
  }

connections.from.to.exp2 <- cbind(connections.final$playerId1,connections.final$playerId2)
connections.from.to.exp2.final <- connections.from.to.exp2[complete.cases(connections.from.to.exp2), ]

net.exp2 <- graph_from_data_frame(d=connections.from.to.exp2.final, 
                             vertices=DIFI.1, 
                             directed=F)

# Network metrics

net.edge_density.exp2 <- edge_density(net.exp2, loops=F)
net.reciprocity.exp2 <- reciprocity(net.exp2)
net.transitivity.exp2 <- transitivity(net.exp2, type="global") # net is treated as an undirected network
net.diameter.exp2 <- diameter(net.exp2, directed=F, weights=NA)
net.assortativity_degree.exp2 <- assortativity_degree(net.exp2, directed=F)

# Overall probability of cooperation in final round

av.cooperation.round.exp2 <-aggregate(
  as.integer(exp2_cooperation$decision0d1c), 
  by=list(exp2_cooperation$round_num), 
  FUN=mean,
  na.rm=TRUE)

colnames(av.cooperation.round.exp2) <- c("finalRound", "av.coop")

av.cooperation.round.exp2.final <- subset(av.cooperation.round.exp2, 
                                     finalRound==max(finalRound), 
                                     select=c("av.coop"))

# Node metrics

# Degree

deg.exp2<-degree(net.exp2, mode="all")
deg.frame.exp2<-cbind(names(deg.exp2), deg.exp2)
colnames(deg.frame.exp2)<- c("pid", "degree.exp2")

# Centrality (eg eigenvalue)

eigen.exp2<-eigen_centrality(net.exp2, directed=T, weights=NA)
eigen.vector.exp2<- eigen.exp2$vector
eigen.frame.exp2<-cbind(names(eigen.vector.exp2), eigen.vector.exp2)
colnames(eigen.frame.exp2)<- c("pid", "eigen.centrality.exp2")

# Plot final network

plot(net.exp2, 
     edge.arrow.size=.2, 
     edge.curved=0,
     vertex.color="orange", 
     vertex.frame.color="#555555",
     vertex.label=V(net)$pid, 
     vertex.label.color="black",
     vertex.label.cex=.7, 
     vertex.size=(deg.exp2*3))

# Merge all data into one analytical dataset and save

DIFI.1 <- merge (av.cooperation.exp2, score.exp2, by = "pid")
DIFI.2 <- merge (group.data.exp2.final, DIFI.final, by = "pid")
DIFI.3 <- merge (DIFI.1, DIFI.2, by = "pid")
DIFI.4 <- data.frame(DIFI.3, 
                        av.coop.final.exp2 = av.cooperation.round.exp2.final,
                        edge_density.exp2 = net.edge_density.exp2,
                        reciprocity.exp2 = net.reciprocity.exp2,
                        transitivity.exp2 = net.transitivity.exp2, 
                        diameter.exp2 = net.diameter.exp2,
                        assortativity_degree.exp2 = net.assortativity_degree.exp2
                        )
DIFI.5 <- merge (DIFI.4, deg.frame.exp2, by = "pid")
DIFI.save <- merge (DIFI.5, eigen.frame.exp2, by = "pid")

write.csv(DIFI.save, 
          "NGS2-Cycle1-Experiment2/DIFI.exp2.csv")
