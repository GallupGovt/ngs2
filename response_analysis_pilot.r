# Empanelment stitching for attrition analysis within NGS2
# Author: Matt Hoover <matt_hoover@gallup.com>

# directory setup
if(Sys.info()['sysname'] == "Windows") {
    setwd(paste0('C:/Users/pablo_diego-rosell/Desktop/Projects/DARPA/Cycle 1/',
                 'Research Protocols/Registration/Experiment 2/Mock data'))
} else {
    if(Sys.info()['user'] == 'matt_hoover') {
        setwd('~/git/NGS2')
    } else {
        setwd("~/Research/Gallup/GallupAnalytics/RandD/NGS2/NGS2-Experiment")
    }
}
rm(list=ls())

# libraries
library(foreign)
library(reshape2)

# renaming dictionaries
EMPANEL_NUMERIC_VARS <- c(
    'Q11_age',
    'Q15_total_hours',
    'Q25_adult_hh_num',
    'Q26_child_hh_num',
    'Q28_friends_num',
    'Q34_internet_hours_day',
    'Q36_social_media_people_num',
    'Q37_social_media_hours_day'
)
EMPANEL_YESNO_VARS <- c(
    'Q14_job',
    'Q29_born_in_country',
    'Q30_move_last5',
    'Q35_social_networks',
    'Q38_online_research',
    'Q39_send_survey_invites'
)
NAME_MATCHES <- c(
    'StartDate',
    'EndDate',
    'Status',
    'IPAddress',
    'Progress',
    'Duration..in.seconds.',
    'Finished',
    'RecordedDate',
    'ResponseId',
    'RecipientLastName',
    'RecipientFirstName',
    'RecipientEmail',
    'ExternalReference',
    'LocationLatitude',
    'LocationLongitude',
    'DistributionChannel',
    'UserLanguage',
    'Q0_intro_screen',
    'Q0_yins_screen',
    'Q0_contact_screen',
    'Q3_consent',
    'Q6_1_extravert',
    'Q6_2_critical',
    'Q6_3_dependable',
    'Q6_4_anxious',
    'Q6_5_open',
    'Q6_6_reserved',
    'Q6_7_sympathetic',
    'Q6_8_disorganized',
    'Q6_9_calm',
    'Q6_10_conventional',
    'Q7_1_group_inferiority',
    'Q7_2_use_force',
    'Q7_3_more_chances',
    'Q7_4_step_on',
    'Q7_5_stay_place',
    'Q7_6_top_bottom',
    'Q7_7_inferior',
    'Q7_8_kept_place',
    'Q7_9_equal',
    'Q7_10_equality_ideal',
    'Q7_11_equal_chance',
    'Q7_12_equalize_conditions',
    'Q7_13_social_equality',
    'Q7_14_fewer_problems',
    'Q7_15_incomes_equal',
    'Q7_16_no_dominate',
    'Q8_1_neglect_needs',
    'Q8_2_others_needs',
    'Q8_3_sensitive_feelings',
    'Q8_4_not_helpful',
    'Q8_5_be_helpful',
    'Q8_6_no_aid',
    'Q8_7_be_responsive',
    'Q8_8_help_others',
    'Q8_9_no_involvement',
    'Q8_10_no_help_others',
    'Q8_11_turn_to_others',
    'Q8_12_emotion_avoid',
    'Q8_13_trouble_themselves',
    'Q8_14_ignored_hurt',
    'Q9_1_depend_on_self',
    'Q9_2_rely_on_self',
    'Q9_3_do_own_thing',
    'Q9_4_personal_identity_important',
    'Q9_5_do_job_better',
    'Q9_6_winning_everything',
    'Q9_7_competition_nature',
    'Q9_8_others_better_tense',
    'Q9_9_coworker_prize_proud',
    'Q9_10_coworker_wellbeing',
    'Q9_11_spend_time_others',
    'Q9_12_good_cooperate_others',
    'Q9_13_family_together',
    'Q9_14_care_family_duty',
    'Q9_15_family_stick_together',
    'Q9_16_respect_group_decision',
    'Q11_age',
    'Q12_gender',
    'Q13_education',
    'Q14_job',
    'Q15_total_hours',
    'Q16_employer_selfemployed',
    'Q17_occupation',
    'Q18_employ_situation',
    'Q18_employ_situation_text',
    'Q19_income_US',
    'Q20_income_MA',
    'Q21_income_PH',
    'Q22_income_feeling',
    'Q23_religion',
    'Q23_religion_text',
    'Q24_religion_freq',
    'Q25_adult_hh_num',
    'Q26_child_hh_num',
    'Q27_marital_status',
    'Q28_friends_num',
    'Q29_born_in_country',
    'Q30_move_last5',
    'Q31_country_origin',
    'Q32_1_smartphone',
    'Q32_2_computer',
    'Q32_3_tablet',
    'Q33_internet_where',
    'Q34_internet_hours_day',
    'Q35_social_networks',
    'Q36_social_media_people_num',
    'Q37_social_media_hours_day',
    'Q38_online_research',
    'Q39_send_survey_invites',
    'Q40_mobile_number'
)
PANEL_VARNAMES <- c(
    'Employee_Key_Value',
    'DEMO_GENDER',
    'DEMO_AGE',
    'demo_division',
    'demo_region',
    'DEMO_EDUCATION_NEW',
    'DEMO_EMPLOYMENT_STATUS',
    'MEMBERSHIP_START_DATE',
    'MEMBERSHIP_STATUS'
)
Q6_ANSWERS <- list(
    'Disagree strongly' = 1,
    'Disagree moderately' = 2,
    'Disagree a little' = 3,
    'Neither agree nor disagree' = 4,
    'Agree a little' = 5,
    'Agree moderately' = 6,
    'Agree strongly' = 7
)
Q7_ANSWERS <- list(
    'Very negative' = 1,
    'Negative' = 2,
    'Slightly negative' = 3,
    'Neither positive nor negative' = 4,
    'Slightly positive' = 5,
    'Positive' = 6,
    'Very positive' = 7
)
Q8_ANSWERS <- list(
    'Extremely uncharacteristic of me (1)' = 1,
    '2' = 2,
    '3' = 3,
    '4' = 4,
    '5' = 5,
    '6' = 6,
    'Extremely characteristic of me (7)' = 7
)
Q9_ANSWERS <- list(
    'Never or definitely no (1)' = 1,
    '2' = 2,
    '3' = 3,
    '4' = 4,
    '5' = 5,
    '6' = 6,
    '7' = 7,
    '8' = 8,
    'Always or definitely yes (9)' = 9
)
Q16_ANSWERS <- list(
    '1' = 'Self-employed',
    '2' = 'Work for an employer'
)
Q17_ANSWERS <- list(
    '1' = 'Professional',
    '2' = 'Managerial',
    '3' = 'Secretarial of clerical',
    '4' = 'Service or labor',
    '5' = 'Sales or retail',
    '6' = 'Farmer or rancher',
    '7' = 'Military',
    '8' = 'Other'
)
Q18_ANSWERS <- list(
    '1' = 'Student',
    '2' = 'Homemaker (including stay-at-home parents)',
    '3' = 'Retired',
    '4' = 'Disabled',
    '5' = 'Unemployed',
    '6' = 'Other'
)
Q19_ANSWERS <- list(
    '1' = 'Less than $6,000 per year',
    '2' = '$6,000-$11,999 per year',
    '3' = '$12,000-$23,999 per year',
    '4' = '$24,000-$35,999 per year',
    '5' = '$36,000-$47,999 per year',
    '6' = '$48,000-$59,999 per year',
    '7' = '$60,000-$89,999 per year',
    '8' = '$90,000-$119,999 per year',
    '9' = '$120,000-$179,999 per year',
    '10' = '$180,000-$239,999 per year',
    '11' = '$240,000 or more per year',
    '12' = 'Don\'t Know'
)
Q22_ANSWERS <- list(
    '1' = 'Living comfortably on present income',
    '2' = 'Getting by on present income',
    '3' = 'Finding it difficult on present income',
    '4' = 'Finding it very difficult on present income'
)
Q23_ANSWERS <- list(
    '1' = 'Protestant',
    '2' = 'Roman Catholic',
    '3' = 'Mormon/Latter-Day Saints',
    '4' = 'Other Christian Religion',
    '5' = 'Jewish',
    '6' = 'Muslim/Islam',
    '7' = 'Islam/Muslim (Shiite)',
    '8' = 'Islam/Muslim (Sunni)',
    '9' = 'Hinduism',
    '10' = 'Buddhism',
    '11' = 'Sikhism',
    '12' = 'Primal-indigenous/African Traditional and Diasporic/Animist/Nature Worship/Paganism',
    '13' = 'Chinese Traditional Religion/Confucianism',
    '14' = 'Spiritism',
    '15' = 'Other Non-Christian Religion',
    '16' = 'No Religion/Atheist/Agnostic',
    '17' = 'Don\'t know',
    '18' = 'Other (Write in:)'
)
Q24_ANSWERS <- list(
    '1' = 'Weekly (at least once a week)',
    '2' = 'Monthly (at least once a month)',
    '3' = 'Annually (at least once a year)',
    '4' = 'Do not regularly attend religious services'
)
Q32_ANSWERS <- list(
    '1' = '7 days',
    '2' = '6 days',
    '3' = '5 days',
    '4' = '4 days',
    '5' = '3 days',
    '6' = '2 days',
    '7' = '1 day',
    '8' = 'Less than once a week',
    '9' = 'Never'
)
Q33_ANSWERS <- list(
    '1' = 'On a mobile device',
    '2' = 'On a computer or tablet at home',
    '3' = 'On a computer or tablet at work',
    '4' = 'On a computer or tablet at school',
    '5' = 'On a computer or tablet at a public place (e.g., library)',
    '6' = 'Other'
)
TIMING_VARNAMES <- c(
    'StartDate',
    'EndDate',
    'Duration',
    'RecordedDate',
    'ResponseId',
    'ExternalReference',
    'Q1',
    'Q2_Browser',
    'Q2_Version',
    'Q2_Operating.System',
    'Q2_Resolution'
)

# function definitions
bb_decision_timing <- function(d, exp1 = FALSE) {
    # calculate the average time (across rounds) it takes a player to make a
    # decision
    if(exp1) {
        tmp <- dcast(subset(d, event == 'cooperationEvent'),
                     id + datetime ~ data.name, value.var = 'data.value')
    } else {
        tmp <- dcast(subset(d, event == 'CooperationDecision'),
                     id + datetime ~ data.name, value.var = 'data.value')
        names(tmp)[which(names(tmp) == 'curRound')] <- 'round'
    }
    tmp <- tmp[grepl('^[0-9]', tmp$round), ]
    tmp$round <- as.numeric(tmp$round)
    res <- do.call(rbind, lapply(split(tmp, tmp$round), function(x) {
        start <- min(x$datetime)
        x <- subset(x, grepl('^[0-9]{4}', pid))
        if(exp1) {
            return(data.frame(round = unique(x$round), pid = as.numeric(x$pid),
                              secs = x$datetime - start))
        } else {
            return(do.call(rbind, lapply(split(x, x$pid), function(y) {
                return(data.frame(round = unique(y$round),
                                  pid = unique(as.numeric(y$pid)),
                                  secs = unique(y$datetime - start)))
            })))
        }
    }))
    res <- aggregate(res$secs, by = list(res$pid), mean, na.rm = TRUE)
    names(res) <- c('pid', 'secs_to_decision')
    return(res)
}

bb_end_scores <- function(d, recast = FALSE) {
    # take a breadboard dataset and determine final score by participant
    tmp <- subset(d, event == 'FinalScore')
    if(recast) {
        tmp <- dcast(tmp, id + datetime ~ data.name, value.var = 'data.value')
        tmp <- tmp[grep('^[0-9]', tmp$pid), c('id', 'datetime', 'pid', 'score')]
        names(tmp)[3] <- 'playerid'
    } else {
        tmp <- tmp[grep('^[0-9]', tmp$data.name),
                   c('id', 'datetime', 'data.name', 'data.value')]
        names(tmp)[3:4] <- c('playerid', 'score')
    }
    return(data.frame(pid = as.numeric(tmp$playerid),
                      end_score = as.numeric(tmp$score)))
}

bb_gameplay <- function(d, exp1 = FALSE) {
    # determines rounds played per person
    if(exp1) {
        tmp <- dcast(subset(d, event == 'cooperationEvent'),
                     id + datetime ~ data.name, value.var = 'data.value')
        tmp <- tmp[!is.na(as.numeric(tmp$pid)) & !is.na(as.numeric(tmp$round)), ]
        tmp[, c('pid', 'round')] <- apply(tmp[, c('pid', 'round')], 2, function(x) {
            as.numeric(x)
        })
        tmp <- dcast(tmp, pid ~ round, fun.aggregate = length)
        names(tmp)[2:ncol(tmp)] <- paste0('r', names(tmp)[2:ncol(tmp)])
    } else {
        tmp <- dcast(subset(d, event == 'CooperationDecision'),
                     id + datetime ~ data.name, value.var = 'data.value')
        tmp <- tmp[!is.na(as.numeric(tmp$pid)), ]
        names(tmp)[which(names(tmp) == 'curRound')] <- 'round'
        tmp[, c('pid', 'round')] <- apply(tmp[, c('pid', 'round')], 2, function(x) {
            as.numeric(x)
        })
        tmp <- dcast(tmp, pid ~ round, fun.aggregate = length)
        tmp[, 2:ncol(tmp)] <- apply(tmp[, 2:ncol(tmp)], 2, function(x) {
            ifelse(x > 0, 1, 0)
        })
        names(tmp)[2:ncol(tmp)] <- paste0('r', names(tmp)[2:ncol(tmp)])
    }
    return(tmp)
}

bb_login <- function(d, end_time) {
    # create unique logins for players, using the last loging prior to final
    # scores being recorded for a game
    tmp <- dcast(subset(d, event == 'clientLogIn'),
                 id + datetime ~ data.name, value.var = 'data.value')
    tmp <- tmp[grepl('^[0-9]', tmp$clientId), ]
    tmp <- tmp[order(tmp$datetime, decreasing = TRUE), ]
    tmp <- tmp[tmp$datetime <= end_time, ]
    tmp <- tmp[!duplicated(tmp$clientId), ]
    return(data.frame(pid = as.numeric(tmp$clientId),
                      ip_address = tmp$ipAddress, logged_in = 1))
}

bb_passed_training <- function(d, experiment = c(1, 2, 3)) {
    # calculate which players passed training
    if(experiment == 1) {
        tmp <- dcast(subset(d, event == 'cooperationEvent'),
                     id + datetime ~ data.name, value.var = 'data.value')
        tmp <- subset(tmp, grepl('^[0-9]{4}', pid) & round == 'p1')
        return(data.frame(pid = as.numeric(tmp$pid), passed_training = 1))
    } else if(experiment == 2) {
        qcast <- dcast(subset(d, grepl('^q', event)),
                       id + datetime ~ data.name, value.var = 'data.value')
        q_success <- do.call(rbind, lapply(split(qcast, qcast$pid), function(x) {
            res <- apply(x[, 3:ncol(x)], 2, function(y) {
                return(y[!is.na(y)])
            })
            res <- sum(res %in% c('randomly', 'net_total', 'leave_neighborhood',
                                  'pay_100_gain_100')) / 4
            return(data.frame(pid = unique(x$pid), frac_q_correct = res))
        }))
        tmp <- dcast(subset(d, event == 'ChooseGroup'),
                     id + datetime ~ data.name, value.var = 'data.value')
        tmp <- subset(tmp, grepl('^[0-9]{4}', pid))
        return(merge(data.frame(pid = as.numeric(tmp$pid), passed_training = 1),
                     q_success, on = 'pid'))
    } else {
        tmp <- subset(d, event == 'PlayerWaiting')
        return(data.frame(pid = as.numeric(tmp$data.value), passed_training = 1))
    }
}

bb_data_merge <- function(login, training, play = NULL, decision = NULL,
                          scores = NULL, exp3 = FALSE, prefix = 'bb') {
    # merge various breadboard summary pieces together
    if(exp3) {
        tmp <- merge(login, training, on = 'pid', all = TRUE)
        names(tmp) <- paste(prefix, names(tmp), sep = '_')
    } else {
        tmp <- merge(login, training, on = 'pid', all = TRUE)
        tmp <- merge(tmp , play, on = 'pid', all = TRUE)
        tmp <- merge(tmp, decision, on = 'pid', all = TRUE)
        tmp <- merge(tmp, scores, on = 'pid', all = TRUE)
        names(tmp) <- paste(prefix, names(tmp), sep = '_')
    }
    return(tmp)
}

convert_time <- function(d) {
    # convert a vector of breadboard date strings to datetime
    return(strptime(d, '%Y-%m-%d %H:%M:%S'))
}

drop_extra_rows <- function(d) {
    # takes data from qualtrics and drops extraneous rows that don't provide
    # any actual data, but just meta-information
    extra_rows <- apply(d, 2, function(x) {grep('^\\{', x)})
    stopifnot(length(unique(extra_rows)) == 1)
    d <- d[-(1:unique(extra_rows)), ]
    return(d)
}

relabel_values <- function(d, regex, dict, single = FALSE) {
    # takes a data frame (d) and uses a regular expression (regex) to identify
    # relevant variables to apply a value-redefining dictionary (dict) to make
    # variable values numeric
    if(single) {
        tmp <- d[, grep(regex, names(d))]
        return(do.call(c, ifelse(tmp %in% names(dict), dict[tmp], NA)))
    } else {
        return(apply(d[, grep(regex, names(d))], 2, function(x) {
            do.call(c, ifelse(x %in% names(dict), dict[x], NA))
        }))
    }
}

reverse_code <- function(var, max) {
    # reverse-codes a numeric value, given a scale maximum (max)
    return(abs(var - max) + 1)
}

tipi_scale <- function(var1, var2) {
    # takes two variables and calculates the row mean
    return(apply(cbind(var1, var2), 1, mean))
}

variable_rename <- function(d, dict) {
    # takes data and dictionary and renames variables
    for(i in 1:length(names(d))) {
        names(d)[i] <- ifelse(names(d)[i] %in% dict$label,
            dict$verbose[which(names(d)[i] == dict$label)],
            names(d)[i])
    }
    return(d)
}

# load all data
comps <- read.csv('data/ngs2_empanelment_pilot_completes.csv', header = TRUE,
                  sep = ',', stringsAsFactors = FALSE)
parts <- read.csv('data/ngs2_empanelment_pilot_partials.csv', header = TRUE,
                  sep = ',', stringsAsFactors = FALSE)
bb_ids <- read.csv('data/empanelment_breadboard_ids.csv', header = TRUE,
                   sep = ',', stringsAsFactors = FALSE)
dist1k <- read.csv('data/ngs2_empanelment_distribution_additional_1k.csv',
                   header = TRUE, sep = ',', stringsAsFactors = FALSE)
times <- read.csv('data/ngs2_pilot_timing_completes.csv', header = TRUE,
                  sep = ',', stringsAsFactors = FALSE)
panel <- read.spss('data/ngs2_pilot_panel.sav', to.data.frame = TRUE)
bb1 <- read.csv('NGS2-Cycle1-Experiment1/data/ngs2_e1_pilot_2017-07-12-01_9222.csv',
                header = TRUE, sep = ',', stringsAsFactors = FALSE)
bb2 <- read.csv('NGS2-Cycle1-Experiment2/data/ngs2_e2_pilot_2017-07-12-01_10381.csv',
                header = TRUE, sep = ',', stringsAsFactors = FALSE)
bb3 <- read.csv('NGS2-Cycle1-Experiment3/data/ngs2_e3_pilot_2017-07-12-01_9409.csv',
                header = TRUE, sep = ',', stringsAsFactors = FALSE)
cdict <- read.csv('data/empanelment_dictionary.csv', header = TRUE, sep = ',',
                  stringsAsFactors = FALSE)
pdict <- read.csv('data/empanelment_partial_dictionary.csv', header = TRUE,
                  sep = ',', stringsAsFactors = FALSE)

# 1. deal with survey responses, both `completes` and `partials`
# rename variables for both `completes` and `partials`
comps <- variable_rename(comps, cdict)
parts <- variable_rename(parts, pdict)

# drop extraneous rows of redundant information
# note: the hard-coding sucks for `partials`, but not a great way around it
comps <- drop_extra_rows(comps)
parts <- parts[2:nrow(parts), ]

# make answer values numerics for `completes`
# note: qualtrics export for `partials` already outputs values as numerics,
#       though due to the weird header rows, they do need to be made numeric
comps[, grep('^Q6_', names(comps))] <- relabel_values(comps, '^Q6_', Q6_ANSWERS)
comps[, grep('^Q7_', names(comps))] <- relabel_values(comps, '^Q7_', Q7_ANSWERS)
comps[, grep('^Q8_', names(comps))] <- relabel_values(comps, '^Q8_', Q8_ANSWERS)
comps[, grep('^Q9_', names(comps))] <- relabel_values(comps, '^Q9_', Q9_ANSWERS)
comps[, EMPANEL_NUMERIC_VARS] <- apply(comps[, EMPANEL_NUMERIC_VARS], 2,
    function(x) {as.numeric(x)}
)
# comps[, EMPANEL_YESNO_VARS] <- apply(comps[, EMPANEL_YESNO_VARS], 2,
#     function(x) {ifelse(x == '', NA, ifelse(grepl('Yes', x), 1, 0))}
# )

# convert values in `partials` to numeric
parts[, grep('^Q[0|6-9]_', names(parts))] <- apply(
    parts[, grep('^Q[0|6-9]_', names(parts))], 2, function(x) {as.numeric(x)}
)
parts[, c('Status', 'Finished', 'Q3_consent')] <- apply(
    parts[, c('Status', 'Finished', 'Q3_consent')], 2, function(x) {
        as.numeric(x)
    }
)
parts[, EMPANEL_NUMERIC_VARS] <- apply(parts[, EMPANEL_NUMERIC_VARS], 2,
    function(x) {as.numeric(x)}
)
# parts[, EMPANEL_YESNO_VARS] <- apply(parts[, EMPANEL_YESNO_VARS], 2,
#     function(x) {ifelse(x == '', NA, ifelse(grepl('Yes', x), 1, 0))}
# )

# convert `partials` to same format as the `completes`
parts$Status <- NA
parts$Finished <- ifelse(parts$Finished == 1, TRUE, FALSE)
parts$Q3_consent <- ifelse(parts$Q3_consent == 1,
                           'I consent to participate in this experiment.', NA)
parts[, grep('^Q16_', names(parts))] <- relabel_values(parts, '^Q16_',
                                                       Q16_ANSWERS, TRUE)
parts[, grep('^Q17_', names(parts))] <- relabel_values(parts, '^Q17_',
                                                       Q17_ANSWERS, TRUE)
parts[, grep('^Q18_.*n$', names(parts))] <- relabel_values(parts, '^Q18_.*n$',
                                                       Q18_ANSWERS, TRUE)
parts[, grep('^Q19_', names(parts))] <- relabel_values(parts, '^Q19_',
                                                       Q19_ANSWERS, TRUE)
parts[, grep('^Q22_', names(parts))] <- relabel_values(parts, '^Q22_',
                                                       Q22_ANSWERS, TRUE)
parts[, grep('^Q23_.*n$', names(parts))] <- relabel_values(parts, '^Q23_.*n$',
                                                       Q23_ANSWERS, TRUE)
parts[, grep('^Q24_', names(parts))] <- relabel_values(parts, '^Q24_',
                                                       Q24_ANSWERS, TRUE)
parts[, grep('^Q32_', names(parts))] <- relabel_values(parts, '^Q32_', Q32_ANSWERS)
parts[, grep('^Q33_', names(parts))] <- relabel_values(parts, '^Q33_',
                                                       Q33_ANSWERS, TRUE)

# split `partials` name variable into first and last names
parts$RecipientFirstName <- sapply(strsplit(parts$Name, ','), function(x) {
    trimws(x[2])
})
parts$RecipientLastName <- sapply(strsplit(parts$Name, ','), function(x) {
    trimws(x[1])
})

# create variables in `partials` for parity with `completes`
parts$Progress <- NA
parts$Duration..in.seconds. <- NA
parts$DistributionChannel <- NA
parts$UserLanguage <- NA

# create variables in `completes` for parity with `partials`
comps$Q0_intro_screen <- NA
comps$Q0_yins_screen <- NA
comps$Q0_contact_screen <- NA

# order `partials` and `completes` for appending
parts <- parts[, NAME_MATCHES]

qstart <- min(grep('^Q', names(comps)))
q0start <- min(grep('^Q0', names(comps)))
comps <- comps[, names(comps)[c(1:(qstart - 1), q0start:ncol(comps),
                                qstart:(q0start - 1))]]
parts$source <- 'partial'
comps$source <- 'complete'

# append `completes` and `partials`
d <- rbind(comps, parts)

# create scales
# tipi
# http://gosling.psy.utexas.edu/scales-weve-developed/ten-item-personality-measure-tipi/
# directions:
#   1. Reverse-code items (2, 4, 6, 8, 10)
#   2. Take average of item pairs ('R' reverse-scored):
#       A. Extraversion: [1, 6R]
#       B. Agreeableness: [2R, 7]
#       C. Conscientiousness: [3, 8R]
#       D. Emotional Stability: [4R, 9]
#       E. Openness to Experiences: [5, 10R]
d$Q6_2_critical_REV <- reverse_code(d$Q6_2_critical, 7)
d$Q6_4_anxious_REV <- reverse_code(d$Q6_4_anxious, 7)
d$Q6_6_reserved_REV <- reverse_code(d$Q6_6_reserved, 7)
d$Q6_8_disorganized_REV <- reverse_code(d$Q6_8_disorganized, 7)
d$Q6_10_conventional_REV <- reverse_code(d$Q6_10_conventional, 7)
d$tipi_extraversion <- rowMeans(
    d[, c('Q6_1_extravert', 'Q6_6_reserved_REV')]
)
d$tipi_agreeableness <- rowMeans(
    d[, c('Q6_7_sympathetic', 'Q6_2_critical_REV')]
)
d$tipi_conscientiousness <- rowMeans(
    d[, c('Q6_3_dependable', 'Q6_8_disorganized_REV')]
)
d$tipi_emot_stability <- rowMeans(
    d[, c('Q6_9_calm', 'Q6_4_anxious_REV')]
)
d$tipi_open_experiences <- rowMeans(
    d[, c('Q6_5_open', 'Q6_10_conventional_REV')]
)

# social dominance orientation
# https://dash.harvard.edu/bitstream/handle/1/3207711/Sidanius_SocialDominanceOrientation.pdf
# directions
#   1. Reverse-code items (9 through 16)
#   2. Take average
d$Q7_9_equal_REV <- reverse_code(d$Q7_9_equal, 7)
d$Q7_10_equality_ideal_REV <- reverse_code(d$Q7_10_equality_ideal, 7)
d$Q7_11_equal_chance_REV <- reverse_code(d$Q7_11_equal_chance, 7)
d$Q7_12_equalize_conditions_REV <- reverse_code(d$Q7_12_equalize_conditions, 7)
d$Q7_13_social_equality_REV <- reverse_code(d$Q7_13_social_equality, 7)
d$Q7_14_fewer_problems_REV <- reverse_code(d$Q7_14_fewer_problems, 7)
d$Q7_15_incomes_equal_REV <- reverse_code(d$Q7_15_incomes_equal, 7)
d$Q7_16_no_dominate_REV <- reverse_code(d$Q7_16_no_dominate, 7)
d$soc_dom_orient <- rowMeans(
    d[, grep('Q7_[1-8]_|Q7_.*REV$', names(d))]
)

# communal orientation scale
# http://fetzer.org/sites/default/files/images/stories/pdf/selfmeasures/CollectiveOrientation.pdf
# directions
#   1. Reverse-code items (3, 4, 6, 9, 10, 12, 13)
#   2. Take average
d$Q8_3_sensitive_feelings_REV <- reverse_code(d$Q8_3_sensitive_feelings, 7)
d$Q8_4_not_helpful_REV <- reverse_code(d$Q8_4_not_helpful, 7)
d$Q8_6_no_aid_REV <- reverse_code(d$Q8_6_no_aid, 7)
d$Q8_9_no_involvement_REV <- reverse_code(d$Q8_9_no_involvement, 7)
d$Q8_10_no_help_others_REV <- reverse_code(d$Q8_10_no_help_others, 7)
d$Q8_12_emotion_avoid_REV <- reverse_code(d$Q8_12_emotion_avoid, 7)
d$Q8_13_trouble_themselves_REV <- reverse_code(d$Q8_13_trouble_themselves, 7)
d$comm_orient_scale <- rowSums(
    d[, grep('^Q8_[12578]([14]|_)|Q8_.*_REV$', names(d))]
)

# cultural orientation scales
# http://fetzer.org/sites/default/files/images/stories/pdf/selfmeasures/CollectiveOrientation.pdf
# directions
#   1. Sum scores:
#       A. Horizontal individualism: [1, 2, 3, 4]
#       B. Vertical individualism: [5, 6, 7, 8]
#       C. Horizontal collectivism: [9, 10, 11, 12]
#       D. Vertical collectivism: [13, 14, 15, 16]
d$horiz_indiv <- rowSums(
    d[, grep('Q9_[1234]_', names(d))]
)
d$vert_indiv <- rowSums(
    d[, grep('Q9_[5678]_', names(d))]
)
d$horiz_collect <- rowSums(
    d[, grep('Q9_(9|1[012])_', names(d))]
)
d$vert_collect <- rowSums(
    d[, grep('Q9_1[3456]_', names(d))]
)

# 2. bring in breadboard ids for those that went from empanelment to experiment
# merge in breadboard ids
d <- merge(
    d,
    bb_ids[, c('LastName', 'FirstName', 'USERID')],
    by.x = c('RecipientLastName', 'RecipientFirstName'),
    by.y = c('LastName', 'FirstName'),
    all.x = TRUE
)
names(d)[grep('USERID', names(d))] <- 'bb_id'
d$bb_id <- as.numeric(d$bb_id)

# 3. deal with the experiment timing data
# drop extraneous rows of redundant information
times <- drop_extra_rows(times)

# convert variables to numeric
times$Duration..in.seconds. <- as.numeric(times$Duration..in.seconds.)
times$Finished <- as.logical(times$Finished)

# keep only valid responses
times <- times[times$ExternalReference != '', ]

# rename survey duration variable
names(times)[grep('^Duration', names(times))] <- 'Duration'
times <- times[, TIMING_VARNAMES]
names(times) <- paste('pilot', names(times), sep = '_')

# merge timing data into empanelment
d <- merge(
    d,
    times,
    by.x = 'ExternalReference',
    by.y = 'pilot_ExternalReference',
    all.x = TRUE
)

# 4. add in additional 1000 panel members contacted
names(dist1k) <- paste('dist1k', names(dist1k), sep = '_')
d <- merge(
    d,
    dist1k[, c('dist1k_Response.Id', 'dist1k_External.Data.Reference',
               'dist1k_Status', 'dist1k_End.Date')],
    by.x = 'ExternalReference',
    by.y = 'dist1k_External.Data.Reference',
    all = TRUE
)
d$source <- ifelse(is.na(d$source), 'dist1k', d$source)

# 5. add in panel demographics
panel <- panel[, PANEL_VARNAMES]
names(panel) <- paste('panel', names(panel), sep = '_')

# redo datetime variable from numeric
panel$panel_MEMBERSHIP_START_DATE <- as.POSIXct(panel$panel_MEMBERSHIP_START_DATE,
                                                origin = '1582-10-14')

# merge panel in with existing data
d <- merge(
    d,
    panel,
    by.x = 'ExternalReference',
    by.y = 'panel_Employee_Key_Value',
    all = TRUE
)
d$source <- ifelse(is.na(d$source), 'panel', d$source)

# 6. bring in breadboard data
# create datetimes from timestamps
bb1$datetime <- convert_time(bb1$datetime)
bb2$datetime <- convert_time(bb2$datetime)
bb3$datetime <- convert_time(bb3$datetime)

# identify logins
bb1_login <- bb_login(bb1, bb1$datetime[bb1$event == 'initStart'])
bb2_login <- bb_login(bb2, bb2$datetime[bb2$event == 'StepStart' &
                                        bb2$data.value == 'initStep'])
bb3_login <- bb_login(bb3, bb3$datetime[bb3$event == 'GameStart'])

# determine who passed training
bb1_training <- bb_passed_training(bb1, experiment = 1)
bb2_training <- bb_passed_training(bb2, experiment = 2)
bb3_training <- bb_passed_training(bb3, experiment = 3)

# identify game play through rounds
bb1_play <- bb_gameplay(bb1, exp1 = TRUE)
bb2_play <- bb_gameplay(bb2, exp1 = FALSE)

# identify time for player decisions across rounds
bb1_decision <- bb_decision_timing(bb1, exp1 = TRUE)
bb2_decision <- bb_decision_timing(bb2, exp1 = FALSE)

# identify ending scores
bb1_scores <- bb_end_scores(bb1, recast = FALSE)
bb2_scores <- bb_end_scores(bb2, recast = TRUE)

# create final breadboard datasets
bb1_summary <- bb_data_merge(bb1_login, bb1_training, bb1_play,
                             bb1_decision, bb1_scores, exp3 = FALSE,
                             prefix = 'bb1')
bb2_summary <- bb_data_merge(bb2_login, bb2_training, bb2_play,
                             bb2_decision, bb2_scores, exp3 = FALSE,
                             prefix = 'bb2')
bb3_summary <- bb_data_merge(bb3_login, bb3_training, exp3 = TRUE,
                             prefix = 'bb3')

# merge breadboard summaries to data
d <- merge(d, bb1_summary, by.x = 'bb_id', by.y = 'bb1_pid', all = TRUE)
d <- merge(d, bb2_summary, by.x = 'bb_id', by.y = 'bb2_pid', all = TRUE)
d <- merge(d, bb3_summary, by.x = 'bb_id', by.y = 'bb3_pid', all = TRUE)
d$source <- ifelse(is.na(d$source), 'bb', d$source)

# 7. derive variables for analysis
d$sample_allocation <- ifelse(d$ExternalReference %in%
                              dist1k$dist1k_External.Data.Reference, 1, 2)
d$days_with_panel <- as.numeric(
    as.POSIXct('2017-07-12') -
    d$panel_MEMBERSHIP_START_DATE
)
d$began_empanelment <- ifelse(d$source == 'complete' | d$source == 'partial', 1, 0)
d$stop_prior_to_consent <- ifelse(d$began_empanelment == 1 & is.na(d$Q3_consent), 1,
                                  ifelse(d$began_empanelment == 1, 0, NA))
d$stop_at_consent <- ifelse(d$stop_prior_to_consent == 0 & is.na(d$Q6_1_extravert),
                            1, ifelse(d$stop_prior_to_consent == 0, 0, NA))
start_stop <- c(which(names(d) == 'Q6_1_extravert'),
                which(names(d) == 'Q38_online_research'))
d$stop_at_other_point <- apply(d[, start_stop[1]:start_stop[2]], 1, function(x) {
    ifelse(is.na(x[1]), NA,
           ifelse(length(x[!is.na(x)]) / (start_stop[2] - start_stop[1] + 1) < .95,
                  1, 0))
})
d$completed_empanelment <- ifelse(
    !is.na(d$Q39_send_survey_invites), 1,
    ifelse(d$source == 'partial' | d$source == 'complete', 0, NA)
)

d$completed_experiment_survey <- ifelse(d$source != 'complete', NA,
                                        ifelse(d$pilot_Q1 != '', 1, 0))
d$agreed_exp1 <- ifelse(d$completed_experiment_survey == 1,
                        ifelse(grepl('8pm', d$pilot_Q1), 1, 0), NA)
d$agreed_exp2 <- ifelse(d$completed_experiment_survey == 1,
                        ifelse(grepl('9pm', d$pilot_Q1), 1, 0), NA)
d$agreed_exp3 <- ifelse(d$completed_experiment_survey == 1,
                        ifelse(grepl('10pm', d$pilot_Q1), 1, 0), NA)

# write data to disk
write.csv(d, file = 'pilot_data_merged.csv', row.names = FALSE, na = '')
