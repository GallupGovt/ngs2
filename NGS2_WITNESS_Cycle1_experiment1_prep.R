# Created by Matt Hoover (matt_hoover@gallup.com) for Gallup in September 2017
# determine system environment
if(Sys.info()['sysname'] == 'Darwin') {
    rm(list=ls())

    # Install packages that are required for this file
    list.of.packages <- c('reshape2')
    new.packages <- list.of.packages[!(list.of.packages %in%
                                       installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    lapply(list.of.packages, require, character.only = TRUE)
}

# Define constants for use in programmatically working through new data
CYCLE <- 1 # should be numeric; the cycle number being processed
COOP_VAR_ORDER <- c(
    'round',
    'pid',
    'action',
    'group_size',
    'previous_decision',
    'num_neighbors',
    'session',
    'condition',
    'fluid_dummy',
    'empanel_id'
)
REWIRE_VAR_ORDER <- c(
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

experiments_to_drop <- function(d) {
    bad_cond <- unique(d$session[d$condition == 'Other'])
    no_ppl <- aggregate(d$pid[d$round == 1], by = list(d$session[d$round == 1]),
                        length)
    no_ppl <- no_ppl$Group.1[no_ppl$x < 8]
    no_rds <- aggregate(d$round, by = list(d$session), function(x) {
        length(unique(x))
    })
    no_rds <- no_rds$Group.1[no_rds$x < 2]
    return(unique(c(bad_cond, no_ppl, no_rds)))
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

merge_with_empanelment <- function(data, empanel, merge_var, rename = FALSE,
                                   rename_var = NULL) {
    tmp <- merge(data, empanel, by.x = merge_var, by.y = 'bbid', all.x = TRUE)
    if(rename) {
        names(tmp)[which(names(tmp) %in% 'empanel_id')] <- rename_var
    }
    return(tmp)
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

# Load raw breadboard data
exp1 <- read_input_files(paste0('NGS2-Cycle', CYCLE, '-Experiment1/data'))

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
    x$fluid_dummy <- ifelse(x$condition == 'Fluid', 1, 0)
    return(x)
}, coop, as.list(names(coop)), exp1, SIMPLIFY = FALSE))

# add in empanelment id for non-bots
coop <- merge_with_empanelment(coop, empanel_bb_xwalk, 'pid')

# drop ridiculous experiments
# conditions:
#   1. `condition` = Other; this indicates bad paramter configurations
#   2. less than 2 rounds
#   3. less than 8 participants
experiment_drops <- experiments_to_drop(coop)
coop <- subset(coop, !(session %in% experiment_drops))

# output dataset to disk
write.csv(coop[order(coop$session, coop$round, coop$pid), COOP_VAR_ORDER],
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
rewire <- merge_with_empanelment(rewire, empanel_bb_xwalk, 'playerid',
                                 rename = TRUE, rename_var = 'player_empanel_id')
rewire <- merge_with_empanelment(rewire, empanel_bb_xwalk, 'alterid',
                                 rename = TRUE, rename_var = 'alter_empanel_id')

# drop ridiculous experiments
rewire <- subset(rewire, !(session %in% experiment_drops))

# output dataset to disk
write.csv(rewire[order(rewire$round, rewire$playerid), REWIRE_VAR_ORDER],
          file = 'NGS2-Cycle1-Experiment1/rewire_exp1.csv',
          row.names = FALSE, na = '')
