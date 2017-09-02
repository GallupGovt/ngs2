#Created by Pablo Diego Rosell, PhD, for Gallup inc. in March 2017

# Define constants for use in programmatically working through new data
CYCLE <- 1 # should be numeric; the cycle number being processed
EXP3 <- FALSE # boolean; change to TRUE if there is experiment 3 data

# Define functions
add_actions <- function(data, actions, focus = c('player', 'alter')) {
    # add respondent actions to `rewire` dataset
    res <- mapply(function(x, y) {
        if(nrow(x) > 0) {
            if(focus == 'player') {
                x <- merge(
                    x,
                    y[, c('pid', 'round', 'action')],
                    by = c('round', 'pid')
                )
                x$action <- ifelse(x$action == 'cooperate', 'C', 'D')
                names(x)[grep('^action$', names(x))] <- 'playeraction'
            } else {
                x <- merge(
                    x,
                    y[, c('pid', 'round', 'action')],
                    by.x = c('round', 'nid'),
                    by.y = c('round', 'pid')
                )
                x$action <- ifelse(x$action == 'cooperate', 'C', 'D')
                names(x)[grep('^action$', names(x))] <- 'alteraction'
            }
            return(x)
        } else {
            return(data.frame())
        }
    }, data, actions, SIMPLIFY = FALSE)
    return(res)
}

add_ties <- function(rewire, connections, rd_changes) {
    # add current and previous ties for actors
    res <- mapply(function(x, y, z) {
        if(nrow(x) > 0) {
            x$previouslytie <- NA
            x$nowtie <- NA
            for(i in 1:nrow(x)) {
                if(x$round[i] == 1) {
                    id1 <- x[i, 'pid']
                    id2 <- x[i, 'nid']
                    conn_id <- which(
                        (y$playerId1 == id1 & y$playerId2 == id2) |
                        (y$playerId1 == id2 & y$playerId2 == id1)
                    )
                    x$previouslytie[i] <- ifelse(length(conn_id) == 0, 0, 1)
                    x$nowtie[i] <- ifelse(
                        (x$previouslytie[i] == 1 & x$break_tie[i] != 1) |
                        (x$previouslytie[i] == 0 & x$make_tie[i] == 1), 1, 0
                    )
                } else {
                    rd <- x$round[i] - 1
                    id1 <- x[i, 'pid']
                    id2 <- x[i, 'nid']
                    conn_id <- which(
                        (z[[rd]]$playerId1 == id1 & z[[rd]]$playerId2 == id2) |
                        (z[[rd]]$playerId1 == id2 & z[[rd]]$playerId2 == id1)
                    )
                    x$previouslytie[i] <- ifelse(length(conn_id) == 0, 0, 1)
                    x$nowtie[i] <- ifelse(
                        (x$previouslytie[i] == 1 & x$break_tie[i] != 1) |
                        (x$previouslytie[i] == 0 & x$make_tie[i] == 1), 1, 0
                    )
                }
            }
            names(x)[grep('pid', names(x))] <- 'playerid'
            names(x)[grep('nid', names(x))] <- 'alterid'

            return(x)
        } else {
            return(data.frame())
        }
    }, rewire, connections, rd_changes, SIMPLIFY = FALSE)
    return(res)
}

build_cooperation <- function(actions) {
    # start `cooperation` that will build on `actions` and `connections`
    return(lapply(actions, function(x) {
        tmp <- merge(
            x,
            aggregate(x$pid, by = list(x$round), length),
            by.x = 'round',
            by.y = 'Group.1'
        )
        names(tmp)[names(tmp) == 'x'] <- 'group_size'

        tmp$previous_decision <- NA
        for(i in 1:nrow(tmp)) {
            if(tmp[i, 'round'] > 1) {
                id <- tmp$pid[i]
                rd <- tmp$round[i] - 1
                if(length(which(tmp$pid == id & tmp$round == rd)) == 1) {
                    tmp$previous_decision[i] <- tmp$action[tmp$pid == id &
                                                           tmp$round == rd]
                }
            }
        }

        tmp[, c('action', 'previous_decision')] <- apply(
            tmp[, c('action', 'previous_decision')], 2, function(y) {
                ifelse(y == 'cooperate', 1, 0)
            }
        )

        return(tmp[, -grep('^id', names(tmp))])
    }))
}

build_empanelment_bb_xwalk <- function(file) {
    # reads in data to determine breadboard/empanelment crosswalk and outputs a
    # clean file
    bb_ids <- read.table(paste('data', file, sep = '/'), header = TRUE,
                         sep = '\t', stringsAsFactors = FALSE)
    bb_ids$bbid <- do.call(c, lapply(strsplit(bb_ids$ROUTER_URL, '/'), function(x) {
        return(x[length(x)])
    }))
    names(bb_ids)[grep('^EMPLOYEE', names(bb_ids))] <- 'empanel_id'
    return(bb_ids[, c('empanel_id', 'bbid')])
}

build_rewire <- function(changes) {
    # start building the `rewire` output file
    res <- lapply(changes, function(x) {
        if(nrow(x) > 0) {
            tmp <- x[
                order(x$round, x$pid, x$nid),
                c('round', 'pid', 'nid', 'action_num')
            ]

            tmp$break_tie <- ifelse(tmp$action_num == -1, 1, 0)
            tmp$make_tie <- ifelse(tmp$action_num == 1, 1, 0)
            tmp$action_num <- ifelse(tmp$action != 0, 1, tmp$action_num)

            return(tmp)
        } else {
            return(data.frame())
        }
    })
    return(res)
}

calculate_connections <- function(rounds) {
    # count up number of connections by id by round
    return(lapply(rounds, function(x) {
        if(length(x) > 0) {
            tmp <- do.call(rbind, mapply(function(y, rd) {
                tmp1 <- aggregate(y$playerId2, by = list(y$playerId1), length)
                tmp2 <- aggregate(y$playerId1, by = list(y$playerId2), length)
                tmp <- merge(tmp1, tmp2, by = 'Group.1', all = TRUE)
                tmp$num_neighbors <- apply(tmp[, grep('^x', names(tmp))], 1, function(z) {
                    sum(z, na.rm = TRUE)
                })
                tmp$round <- rd
                tmp <- tmp[, c('round', 'Group.1', 'num_neighbors')]
                names(tmp)[2] <- 'pid'
                return(tmp)
            }, x, 1:length(x), SIMPLIFY = FALSE))
            return(tmp)
        } else {
            return(data.frame())
        }
    }))
}

changes_by_round <- function(changes, connections) {
    # identify changes by id by round
    return(mapply(function(x, y) {
        round_changes <- list()
        if(nrow(x) > 0) {
            y$change_event <- 0
            for(j in 1:max(x$round)) {
                tmp <- subset(x, x$round == j)
                tmp <- tmp[order(tmp$id), ]
                for(i in 1:nrow(tmp)) {
                    id1 <- tmp[i, 'nid']
                    id2 <- tmp[i, 'pid']
                    conn_id <- which((y$playerId1 == id1 & y$playerId2 == id2) |
                                     (y$playerId1 == id2 & y$playerId2 == id1))
                    if(length(conn_id) == 0) {
                        if(tmp[i, 'action'] == 'makeConnection') {
                            y <- rbind(y,
                                       data.frame(id = tmp[i, 'id'],
                                                  playerId1 = id1,
                                                  playerId2 = id2,
                                                  change_event = 1)
                            )
                        }
                    } else {
                        y$change_event[conn_id] <- ifelse(
                            is.na(y$change_event[conn_id]),
                            tmp[i, 'action_num'],
                            y$change_event[conn_id] + tmp[i, 'action_num']
                        )
                    }
                }
                y <- y[y$change_event >= 0, ]
                y$change_event <- 0
                round_changes[[j]] <- y[, c('id', 'playerId1', 'playerId2')]
                names(round_changes)[j] <- paste0('r', j)
            }
        }
        return(round_changes)
    }, changes, connections, SIMPLIFY = FALSE))
}

create_subsets <- function(data, action, scrubs, strip_practice=TRUE) {
    # create data subsets
    return(lapply(data, function(x) {
        if(action %in% x$event) {
            tmp <- dcast(subset(x, event == action), id ~ data.name,
                         value.var = 'data.value')
            if(strip_practice) {
                tmp$round <- as.numeric(tmp$round)
                tmp <- subset(tmp, !is.na(round))
            }
            return(tmp[complete.cases(tmp), ])
        } else {
            return(data.frame())
        }
    }))
}

get_neighbor_number <- function(d, cnx, rd) {
    ids <- d$pid[d$round == rd]
    tmp <- cnx[cnx$playerId1 %in% ids & cnx$playerId2 %in% ids, ]
    tmp_df <- merge(
        data.frame(table(tmp$playerId1)),
        data.frame(table(tmp$playerId2)),
        by = 'Var1',
        all = TRUE
    )
    tmp_df[is.na(tmp_df)] <- 0
    for(id in ids) {
        d$num_neighbors[d$round == rd & d$pid == id] <- sum(
            tmp_df$Freq.x[tmp_df$Var1 == id],
            tmp_df$Freq.y[tmp_df$Var1 == id]
        )
    }
    return(d)
}

read_input_files <- function(dir) {
    # identifies all csv files in a directory, reads then into r, and appends them
    res <- list()
    files <- list.files(dir, pattern = '.csv$')
    sources <- strsplit(files, split = '_')
    for(i in 1:length(files)) {
        tmp <- read.csv(paste(dir, files[i], sep = '/'), header = TRUE,
                        sep = ',', stringsAsFactors = FALSE)
        tmp$source <- paste(sources[[i]][1:2], collapse = '_')
        res[[i]] <- tmp
        names(res)[i] <- paste(sources[[i]][1:2], collapse = '_')
    }
    return(res)
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
exp1 <- read_input_files(paste0('NGS2-Cycle', CYCLE, '-Experiment1/data'))
exp2 <- read_input_files(paste0('NGS2-Cycle', CYCLE, '-Experiment2/data'))
if(EXP3) {
    exp3 <- read_input_files(paste0('NGS2-Cycle', CYCLE, '-Experiment3/data'))
}

# gather empanelment/breadboard crosswalk information
empanel_bb_xwalk <- build_empanelment_bb_xwalk('oms_url_upload_20170821_1145.txt')

# EXPERIMENT 1
# cooperation dataset
# create subsets for manipulation
conn <- create_subsets(exp1, 'Connected', 'player', strip_practice = FALSE)
act <- create_subsets(exp1, 'cooperationEvent', 'pid|round',
                      strip_practice = TRUE)
chng <- create_subsets(exp1, 'rewiringEvent', 'pid|nid|round',
                       strip_practice = TRUE)
chng <- lapply(chng, function(x) {
    if(nrow(x) > 0) {
        x$action_num <- ifelse(x$action == 'maintainConnection', 0,
                               ifelse(x$action == 'breakConnection', -1, 1))
    }
    return(x)
})

# start building the `cooperation` output file
coop <- build_cooperation(act)

# calculate actor moves by round
round_changes <- changes_by_round(chng, conn)
nbr_connections_round <- calculate_connections(round_changes)

# finish off the `cooperation` dataset
coop <- mapply(function(x, y, exp) {
    if(nrow(y) > 0) {
        return(merge(x, y, by = c('round', 'pid')))
    } else {
        conns <- dcast(subset(exp, event == 'Connected'), id ~ data.name,
                       value.var = 'data.value')
        x$num_neighbors <- NA
        for(i in 1:max(x$round)) {
            x <- get_neighbor_number(x, conns, i)
        }
        return(x[, c('round', 'pid', 'action', 'group_size',
                     'previous_decision', 'num_neighbors')])
    }
}, coop, nbr_connections_round, exp1, SIMPLIFY = FALSE)

# add in parameters from dataset for completeness
coop <- do.call(rbind, mapply(function(x, name, exp) {
    exp_cond <- as.numeric(exp$data.value[exp$event == 'initParameters' &
                                          exp$data.name == 'k'])
    x$session <- name
    x$condition <- ifelse(exp_cond == 0, 'Static',
                          ifelse(exp_cond == .1, 'Viscous',
                          ifelse(exp_cond == .3, 'Fluid',
                          ifelse(exp_cond == 1, 'Random', 'Other'))))
    return(x)
}, coop, as.list(names(coop)), exp1, SIMPLIFY = FALSE))

# add in empanelment id for non-bots
coop <- merge(coop, empanel_bb_xwalk, by.x = 'pid', by.y = 'bbid',
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
write.csv(coop[order(coop$session, coop$round, coop$pid), var_order],
          file = 'NGS2-Cycle1-Experiment1/cooperation_exp1.csv',
          row.names = FALSE, na='')

# rewire dataset
# start building `rewire` output file
rewire <- build_rewire(chng)

# add respondent actions to `rewire` dataset
rewire <- add_actions(rewire, act, focus = 'player')
rewire <- add_actions(rewire, act, focus = 'alter')

rewire <- lapply(rewire, function(x) {
    if(nrow(x) > 0) {
        x$state <- apply(x[, grep('action$', names(x))], 1, function(y) {
            paste0(y, collapse = '')
        })
        x$CC <- ifelse(x$state == 'CC', 1, 0)
        x$DD <- ifelse(x$state == 'DD', 1, 0)
        x$other_d <- ifelse(x$alteraction == 'D', 1, 0)
        return(x)
    } else {
        return(data.frame())
    }
})

# add in previous and current ties
rewire <- add_ties(rewire, conn, round_changes)

# add oin parameters from dataset for completeness
rewire <- do.call(rbind, mapply(function(x, name) {
    if(nrow(x) > 0) {
        x$session <- name
    }
    return(x)
}, rewire, as.list(names(rewire)), SIMPLIFY = FALSE))

# add in empanelment id for non-bots
rewire <- merge(rewire, empanel_bb_xwalk, by.x = 'playerid', by.y = 'bbid',
                all.x = TRUE)
names(rewire)[names(rewire) == 'empanel_id'] <- 'player_empanel_id'
rewire <- merge(rewire, empanel_bb_xwalk, by.x = 'alterid', by.y = 'bbid',
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
# Restructure raw data output from Breadboard into 'cooperation' dataset
###############################################################################
# Subset cooperation decisions, drop "datetime" and "event fields"
coop <- lapply(exp2, function(x) {
    return(subset(x, event == 'CooperationDecision',
                  select = c('id', 'data.name', 'data.value')))
})

#Reshape long to wide on cooperation events
coop.events <- lapply(coop, function(x) {
    tmp <- reshape(x, timevar = "data.name", idvar = c("id"), direction = "wide")

    # Declare some `data.value` fields as numerics so events can be sorted logically
    tmp[, grep('oup|ion|und$', names(tmp))] <- apply(
        tmp[, grep('oup|ion|und$', names(tmp))], 2, function(y) {
            return(ifelse(grepl('^_', y), as.numeric(gsub('_', '', y)), as.numeric(y)))
        }
    )
    return(tmp)
})

# Create list of group selections by each individual ('pid') at the "ChooseGroup"
# and "ChangeGroup" events
group <- lapply(exp2, function(x) {
    return(subset(x, event %in% c('ChooseGroup', 'ChangeGroup') &
           data.name %in% c('group', 'pid', 'curRound'),
           select = c('id', 'data.name', 'data.value')))
})
group.events <- lapply(group, function(x) {
    tmp <- reshape(x, timevar = "data.name", idvar = c("id"), direction = "wide")[, -1]
    tmp$link <- (as.numeric(as.factor(tmp$data.value.pid)) * 1000 +
                 as.numeric(tmp$data.value.curRound))
    tmp <- tmp[, grep('pid|group|link$', names(tmp))]
    names(tmp) <- gsub('data.value.', '', names(tmp))
    return(tmp)
})

# Since group changes only happen at rounds 1, 4, 8, 12 and so on, need to
# recode id links at the coop.events level to match this sequence
coop.events <- lapply(coop.events, function(x) {
    x$recround <- cut(
        x$data.value.curRound,
        breaks = c(seq(1,24, 4), 24),
        right = FALSE,
        include.lowest = TRUE,
        labels = c(1, 4, 8, 12, 16, 20)
    )
    # Create id link for coop.events data frame to merge the group change data
    x$link <- (as.numeric(as.factor(x$data.value.pid)) * 1000 +
               as.numeric(as.character(x$recround)))
    return(x)
})

cooperation <- mapply(function(x, y) {
    merge(x, y, by = 'link')
}, coop.events, group.events, SIMPLIFY = FALSE)

# Add condition
cooperation <- do.call(rbind, mapply(function(x, y, name) {
    c1 <- as.numeric(y$data.value[y$data.name == 'M'])
    c2 <- as.numeric(subset(y, data.name == 'sameGroupConnectivity')[, 'data.value'])
    c3 <- as.numeric(subset(y, data.name == 'diffGroupConnectivity')[, 'data.value'])

    if (c1 == 2 & c2 > c3) {
        x$condition <- 'Biased-2'
    } else if (c1 == 4 & c2 > c3) {
        x$condition <- 'Biased-4'
    } else if (c1 == 2 & c2 == c3) {
        x$condition <- 'Unbiased-2'
    } else if (c1 == 4 & c2 == c3){
        x$condition <- 'Unbiased-4'
    # 'c3' > 'c2' (biased connections towards outgroup) is not contemplated in
    # the current experiment. If this situation would happen by mistake, the
    # value of 'condition' will show as 'Other'.
    } else {
        x$condition <- 'Other'
    }

    # Add session number
    x$session <- name

    return(x)
}, cooperation, exp2, as.list(names(cooperation)), SIMPLIFY = FALSE))

# Drop unused variables
KEEP_VARS <- c(
    'data.value.pid',
    'data.value.curRound',
    'data.value.group',
    'data.value.cooperation',
    'group',
    'condition',
    'session'
)
cooperation <- cooperation[, KEEP_VARS]

#Change variable names to shorter, clearer names
names(cooperation) <- c(
    'playerid',
    'round_num',
    'group_coop',
    'decision0d1c',
    'group',
    'condition',
    'session'
)

#Add in-group vs. out-group dummy
cooperation$ingroup <- ifelse(cooperation$group_coop == cooperation$group, 1, 0)

#Add biased vs. unbiased dummy
cooperation$biased <- ifelse(cooperation$condition == 'Biased-2' |
                             cooperation$condition == 'Biased-4', 1, 0)
cooperation$identities <- ifelse(cooperation$condition == 'Biased-4' |
                                 cooperation$condition == 'Unbiased-4', 1, 0)

# add in empanelment id for non-bots
cooperation <- merge(cooperation, empanel_bb_xwalk, by.x = 'playerid',
                     by.y = 'bbid', all.x = TRUE)

write.csv(
    cooperation[order(cooperation$round_num, cooperation$playerid), ],
    file = "NGS2-Cycle1-Experiment2/cooperation_exp2.csv",
    row.names = FALSE,
    na = ''
)

###############################################################################
#Restructure raw data output from Breadboard into 'rewire' dataset
###############################################################################
# Subset change decisions, drop "datetime" and "event fields"
changegroup.events <- lapply(exp2, function(x) {
    tmp <- subset(x, event == 'ChangeGroup' & data.name != 'changedGroup',
                  select = c('id', 'data.name', 'data.value'))
    tmp <- dcast(tmp, id ~ data.name, value.var = 'data.value')
    tmp[, grep('^cur|gro', names(tmp))] <- apply(
        tmp[, grep('^cur|gro', names(tmp))], 2, function(y) {
            return(as.numeric(y))
        }
    )
    tmp$link <- as.numeric(as.factor(tmp$pid)) * 1000 + tmp$curRound
    tmp <- tmp[, c('pid', 'group', 'curRound', 'link')]
    names(tmp) <- c('playerid', 'group', 'round_num', 'link')
    return(tmp)
})

#Subset rewire decisions, drop "datetime" and "event fields"
rewire.events <- lapply(exp2, function(x) {
    tmp <- dcast(subset(x, event == 'RewiringDecision'), id ~ data.name,
                 value.var = 'data.value')
    tmp[, grep('^cur|dec|gro', names(tmp))] <- apply(
        tmp[, grep('^cur|dec|gro', names(tmp))], 2, function(y) {
            return(as.numeric(y))
        }
    )
    tmp$link <- as.numeric(as.factor(tmp$pid)) * 1000 + tmp$curRound
    tmp <- tmp[, c('group', 'decision', 'link')]
    names(tmp) <- c('group_connect', 'connect', 'link')
    return(tmp)
})

rewire <- mapply(function(x, y) {
    return(merge(x, y, by = 'link'))
}, changegroup.events, rewire.events, SIMPLIFY = FALSE)

#Add condition
rewire <- do.call(rbind, mapply(function(x, y, name) {
    c1 <- as.numeric(y$data.value[y$data.name == 'M'])
    c2 <- as.numeric(subset(y, data.name == 'sameGroupConnectivity')[, 'data.value'])
    c3 <- as.numeric(subset(y, data.name == 'diffGroupConnectivity')[, 'data.value'])

    if (c1 == 2 & c2 > c3) {
        x$condition <- 'Biased-2'
    } else if (c1 == 4 & c2 > c3) {
        x$condition <- 'Biased-4'
    } else if (c1 == 2 & c2 == c3) {
        x$condition <- 'Unbiased-2'
    } else if (c1 == 4 & c2 == c3){
        x$condition <- 'Unbiased-4'
    # 'c3' > 'c2' (biased connections towards outgroup) is not contemplated in
    # the current experiment. If this situation would happen by mistake, the
    # value of 'condition' will show as 'Other'.
    } else {
        x$condition <- 'Other'
    }

    # Add session number
    x$session <- name

    return(x)
}, rewire, exp2, as.list(names(rewire)), SIMPLIFY = FALSE))

#Drop unused link variable
rewire <- rewire[, -which(names(rewire) %in% 'link')]

#Add in-group vs. out-group dummy
rewire$ingroup <- ifelse(rewire$group_connect == rewire$group, 1, 0)

#Add biased vs. unbiased dummy
rewire$biased <- ifelse(rewire$condition == 'Biased-2' |
                        rewire$condition == 'Biased-4', 1, 0)
rewire$identities <- ifelse(rewire$condition == 'Biased-4' |
                            rewire$condition == 'Unbiased-4', 1, 0)

# add in empanelment id for non-bots
rewire <- merge(rewire, empanel_bb_xwalk, by.x = 'playerid', by.y = 'bbid',
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
