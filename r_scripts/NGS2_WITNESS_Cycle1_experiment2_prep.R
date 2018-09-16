# Created by Pablo Diego Rosell, PhD, for Gallup inc. in March 2017
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
KEEP_VARS <- c(
    'pid',
    'curRound',
    'group.x',
    'cooperation',
    'group.y',
    'condition',
    'session'
)

# Define functions
add_experiment_condition <- function(data, raw) {
    # figures out the experimental condition for study and adds variable to
    # dataset. then, outputs the `flattened` experiment lists to a data frame.
    tmp <- do.call(rbind, mapply(function(x, y, name) {
        c1 <- as.numeric(y$data.value[y$data.name == 'M'])
        c2 <- as.numeric(
            subset(y, data.name == 'sameGroupConnectivity')[, 'data.value']
        )
        c3 <- as.numeric(
            subset(y, data.name == 'diffGroupConnectivity')[, 'data.value']
        )

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
    }, data, raw, as.list(names(data)), SIMPLIFY = FALSE))
    return(tmp)
}

build_empanelment_bb_xwalk <- function(file) {
    # reads in data to determine breadboard/empanelment crosswalk and outputs a
    # clean file
    bb_ids <- read.csv(paste('data', file, sep = '/'), header = TRUE,
                         sep = ',', stringsAsFactors = FALSE)
    bb_ids$bbid <- do.call(c, lapply(strsplit(bb_ids$ROUTER_URL, '/'), function(x) {
        return(x[length(x)])
    }))
    names(bb_ids)[grep('^EMPLOYEE', names(bb_ids))] <- 'empanel_id'
    return(bb_ids[, c('empanel_id', 'bbid')])
}

experiments_to_drop <- function(d) {
    no_ppl <- aggregate(
        d$playerid,
        by = list(d$session),
        function(x) {
            length(unique(x))
        }
    )
    no_ppl <- no_ppl$Group.1[no_ppl$x < 8]
    return(no_ppl)
}

read_input_files <- function(dir) {
    # identifies all csv files in a directory, reads then into r, and appends them
    res <- list()
    files <- list.files(dir, pattern = '^e.*[0-9]\\.csv$')
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
exp2 <- read_input_files(paste0('NGS2-Cycle', CYCLE, '-Experiment2/data'))

# gather empanelment/breadboard crosswalk information
empanel_bb_xwalk <- build_empanelment_bb_xwalk('all_ids_24oct2017.csv')

# Experiment 2
###############################################################################
# Restructure raw data output from Breadboard into 'cooperation' dataset
###############################################################################
# Subset cooperation decisions, drop "datetime" and "event fields"
coop <- lapply(exp2, function(x) {
    return(subset(x, event == 'CooperationDecision',
                  select = c('id', 'data.name', 'data.value')))
})

# Reshape long to wide on cooperation events
coop.events <- lapply(coop, function(x) {
    tmp <- dcast(x, id ~ data.name, value.var = 'data.value')

    # Declare `data.value` fields as numerics so events can be sorted logically
    tmp[, grep('oup|ion|und$', names(tmp))] <- apply(
        tmp[, grep('oup|ion|und$', names(tmp))], 2, function(y) {
            return(ifelse(grepl('^_', y), as.numeric(gsub('_', '', y)),
                          as.numeric(y)))
        }
    )
    return(tmp)
})

# Create list of group selections by each individual ('pid') at the "ChooseGroup"
# and "ChangeGroup" events
group <- lapply(exp2, function(x) {
    return(subset(x, event %in% c('ChooseGroup', 'ChangeGroup') &
           data.name %in% c('group', 'pid', 'curRound'),
           select = c('id', 'event', 'data.name', 'data.value')))
})
group.events <- mapply(function(x, y) {
    tmp <- dcast(x, id + event ~ data.name, value.var = 'data.value')
    tmp <- tmp[tmp$pid %in% y$pid, ]
    tmp$link <- ifelse(tmp$event == 'ChooseGroup',
        as.numeric(as.factor(tmp$pid)) * 1000 + as.numeric(tmp$curRound),
        as.numeric(as.factor(tmp$pid)) * 1000 + (as.numeric(tmp$curRound) + 1))
    tmp <- tmp[, grep('pid|event|group|Round|link$', names(tmp))]

    return(tmp)
}, group, coop.events, SIMPLIFY = FALSE)

# Develop change event sequence by session
change.events <- lapply(exp2, function(x) {
    roll <- subset(x, event == 'GroupChangeRoll', select = 'data.value')
    return(which(roll$data.value < .25))
})

# Group changes happen at random rounds (p ~ 0.25) so need to recode id links
# at the coop.events level to match this sequence
coop.events <- mapply(function(x, rd) {
    x$recround <- cut(
        x$curRound,
        breaks = c(rd, max(x$curRound) + 1),
        right = TRUE,
        include.lowest = FALSE,
        labels = rd + 1
    )
    x$recround <- as.numeric(as.character(x$recround))
    x$recround[is.na(x$recround)] <- 1

    x$link <- as.numeric(as.factor(x$pid)) * 1000 + x$recround

    return(x)
}, coop.events, change.events, SIMPLIFY = FALSE)

cooperation <- mapply(function(x, y) {
    merge(x[, c('cooperation', 'curRound', 'group', 'pid', 'link')],
          y[, c('link', 'group')],
          by = 'link')
}, coop.events, group.events, SIMPLIFY = FALSE)

# Add condition
cooperation <- add_experiment_condition(cooperation, exp2)

# Drop unused variables
cooperation <- cooperation[, KEEP_VARS]

# Change variable names to shorter, clearer names
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

# drop ridiculous experiments
# conditions:
#   1. less than 8 participants
experiment_drops <- experiments_to_drop(cooperation)
cooperation <- subset(cooperation, !(session %in% experiment_drops))

# write data to disk
write.csv(
    cooperation[order(cooperation$round_num, cooperation$playerid), ],
    file = "NGS2-Cycle1-Experiment2/cooperation_exp2.csv",
    row.names = FALSE,
    na = ''
)

###############################################################################
# Restructure raw data output from Breadboard into 'rewire' dataset
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

# Subset rewire decisions, drop "datetime" and "event fields"
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
rewire <- add_experiment_condition(rewire, exp2)

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

# subset out ridiculous experiments
rewire <- subset(rewire, !(session %in% experiment_drops))

# write data to disk
write.csv(
    rewire[order(rewire$round_num, rewire$playerid), ],
    file = "NGS2-Cycle1-Experiment2/rewire_exp2.csv",
    row.names = FALSE,
    na = ''
)
