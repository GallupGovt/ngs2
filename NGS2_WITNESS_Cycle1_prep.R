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

# Define renaming dictionaries
Q1_ANSWERS <- list(
    'Disagree strongly' = 1,
    'Disagree moderately' = 2,
    'Disagree a little' = 3,
    'Neither agree nor disagree' = 4,
    'Agree a little' = 5,
    'Agree moderately' = 6,
    'Agree strongly' = 7
)
Q2_ANSWERS <- list(
    'Very negative' = 1,
    'Negative' = 2,
    'Slightly negative' = 3,
    'Neither positive nor negative' = 4,
    'Slightly positive' = 5,
    'Positive' = 6,
    'Very positive' = 7
)
Q3_ANSWERS <- list(
    'Extremely uncharacteristic of me (1)' = 1,
    '2' = 2,
    '3' = 3,
    '4' = 4,
    '5' = 5,
    '6' = 6,
    'Extremely characteristic of me (7)' = 7
)
Q4_ANSWERS <- list(
    'Never or definitely no (1)' = 1,
    '2' = 2,
    '3' = 3,
    '4' = 4,
    '5' = 5,
    '6' = 6,
    '7' = 7,
    '8' = 8,
    'Always of definitely yes (9)' = 9
)
EMPANEL_NUMERIC_VARS <- c(
    'Q11_age',
    'Q15_total_hours',
    'Q22_adult_hh_num',
    'Q23_child_hh_num',
    'Q25_friends_num',
    'Q29_internet_hours_day',
    'Q31_social_media_people_num',
    'Q32_social_media_hours_day'
)
EMPANEL_YESNO_VARS <- c(
    'Q14_job',
    'Q26_born_in_country',
    'Q30_social_networks',
    'Q31_social_media_people_num',
    'Q33_online_research',
    'Q35_send_survey_invites'
)

# Define functions
relabel_values <- function(d, regex, dict) {
    # takes a data frame (d) and uses a regular expression (regex) to identify
    # relevant variables to apply a value-redefining dictionary (dict) to make
    # variable values numeric
    return(apply(d[, grep(regex, names(d))], 2, function(x) {
        do.call(c, ifelse(x %in% names(dict), dict[x], NA))
    }))
}

reverse_code <- function(var, max) {
    # reverse-codes a numeric value, given a scale maximum (max)
    return(abs(var - max) + 1)
}

tipi_scale <- function(var1, var2) {
    # takes two variables and calculates the row mean
    return(apply(cbind(var1, var2), 1, mean))
}


### Install packages that are required for this file
#################################
list.of.packages <- c("pacman", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in%
                                   installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

pacman::p_load(multiwayvcov, lmtest)

#Load raw breadboard data
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


###############################################################################
# Prepare empanelment data
###############################################################################
empanel <- read.csv('data/empanelment_demo_24may2017.csv', header = TRUE,
                    sep = ',', stringsAsFactors = FALSE)
empanel_dict <- read.csv('data/empanelment_dictionary.csv', header = TRUE,
                         sep = ',', stringsAsFactors = FALSE)

# rename variables
for(i in 1:length(names(empanel))) {
    names(empanel)[i] <- ifelse(names(empanel)[i] %in% empanel_dict$label,
        empanel_dict$verbose[which(names(empanel)[i] == empanel_dict$label)],
        names(empanel)[i])
}

# drop extraneous rows of redundant information
extra_rows <- apply(empanel, 2, function(x) {grep('^\\{', x)})
stopifnot(length(unique(extra_rows)) == 1)
empanel <- empanel[-(1:unique(extra_rows)), ]

# make answer values numerics
empanel[, grep('^Q1_', names(empanel))] <- relabel_values(empanel, '^Q1_',
                                                          Q1_ANSWERS)
empanel[, grep('^Q2_', names(empanel))] <- relabel_values(empanel, '^Q2_',
                                                          Q2_ANSWERS)
empanel[, grep('^Q3_', names(empanel))] <- relabel_values(empanel, '^Q3_',
                                                          Q3_ANSWERS)
empanel[, grep('^Q4_', names(empanel))] <- relabel_values(empanel, '^Q4_',
                                                          Q4_ANSWERS)
empanel[, EMPANEL_NUMERIC_VARS] <- apply(empanel[, EMPANEL_NUMERIC_VARS], 2,
    function(x) {as.numeric(x)}
)
empanel[, EMPANEL_YESNO_VARS] <- apply(empanel[, EMPANEL_YESNO_VARS], 2,
    function(x) {ifelse(x == '', NA, ifelse(grepl('Yes', x), 1, 0))}
)
# note to self (mh): could turn Q27 variables into numeric - need input on
# scheme though

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
empanel$Q1_2_critical_REV <- reverse_code(empanel$Q1_2_critical, 7)
empanel$Q1_4_anxious_REV <- reverse_code(empanel$Q1_4_anxious, 7)
empanel$Q1_6_reserved_REV <- reverse_code(empanel$Q1_6_reserved, 7)
empanel$Q1_8_disorganized_REV <- reverse_code(empanel$Q1_8_disorganized, 7)
empanel$Q1_10_conventional_REV <- reverse_code(empanel$Q1_10_conventional, 7)
empanel$tipi_extraversion <- rowMeans(
    empanel[, c('Q1_1_extravert', 'Q1_6_reserved_REV')]
)
empanel$tipi_agreeableness <- rowMeans(
    empanel[, c('Q1_7_sympathetic', 'Q1_2_critical_REV')]
)
empanel$tipi_conscientiousness <- rowMeans(
    empanel[, c('Q1_3_dependable', 'Q1_8_disorganized_REV')]
)
empanel$tipi_emot_stability <- rowMeans(
    empanel[, c('Q1_9_calm', 'Q1_4_anxious_REV')]
)
empanel$tipi_open_experiences <- rowMeans(
    empanel[, c('Q1_5_open', 'Q1_10_conventional_REV')]
)

# social dominance orientation
# https://dash.harvard.edu/bitstream/handle/1/3207711/Sidanius_SocialDominanceOrientation.pdf
# directions
#   1. Reverse-code items (9 through 16)
#   2. Take average
empanel$Q2_9_equal_REV <- reverse_code(empanel$Q2_9_equal, 7)
empanel$Q2_10_equality_ideal_REV <- reverse_code(empanel$Q2_10_equality_ideal, 7)
empanel$Q2_11_equal_chance_REV <- reverse_code(empanel$Q2_11_equal_chance, 7)
empanel$Q2_12_equalize_conditions_REV <- reverse_code(empanel$Q2_12_equalize_conditions, 7)
empanel$Q2_13_social_equality_REV <- reverse_code(empanel$Q2_13_social_equality, 7)
empanel$Q2_14_fewer_problems_REV <- reverse_code(empanel$Q2_14_fewer_problems, 7)
empanel$Q2_15_incomes_equal_REV <- reverse_code(empanel$Q2_15_incomes_equal, 7)
empanel$Q2_16_no_dominate_REV <- reverse_code(empanel$Q2_16_no_dominate, 7)
empanel$soc_dom_orient <- rowMeans(
    empanel[, grep('Q2_[1-8]_|Q2_.*REV$', names(empanel))]
)

# communal orientation scale
# http://fetzer.org/sites/default/files/images/stories/pdf/selfmeasures/CollectiveOrientation.pdf
# directions
#   1. Reverse-code items (3, 4, 6, 9, 10, 12, 13)
#   2. Take average
empanel$Q3_3_sensitive_feelings_REV <- reverse_code(empanel$Q3_3_sensitive_feelings, 7)
empanel$Q3_4_not_helpful_REV <- reverse_code(empanel$Q3_4_not_helpful, 7)
empanel$Q3_6_no_aid_REV <- reverse_code(empanel$Q3_6_no_aid, 7)
empanel$Q3_9_no_involvement_REV <- reverse_code(empanel$Q3_9_no_involvement, 7)
empanel$Q3_10_no_help_others_REV <- reverse_code(empanel$Q3_10_no_help_others, 7)
empanel$Q3_12_emotion_avoid_REV <- reverse_code(empanel$Q3_12_emotion_avoid, 7)
empanel$Q3_13_trouble_themselves_REV <- reverse_code(empanel$Q3_13_trouble_themselves, 7)
empanel$comm_orient_scale <- rowMeans(
    empanel[, grep('^Q3_[12578]([14]|_)|Q3_.*_REV$', names(empanel))]
)

# cultural orientation scales
# http://fetzer.org/sites/default/files/images/stories/pdf/selfmeasures/CollectiveOrientation.pdf
# directions
#   1. Sum scores:
#       A. Horizontal individualism: [1, 2, 3, 4]
#       B. Vertical individualism: [5, 6, 7, 8]
#       C. Horizontal collectivism: [9, 10, 11, 12]
#       D. Vertical collectivism: [13, 14, 15, 16]
empanel$horiz_indiv <- rowSums(
    empanel[, grep('Q4_[1234]_', names(empanel))]
)
empanel$vert_indiv <- rowSums(
    empanel[, grep('Q4_[5678]_', names(empanel))]
)
empanel$horiz_collect <- rowSums(
    empanel[, grep('Q4_(9|1[012])_', names(empanel))]
)
empanel$vert_collect <- rowSums(
    empanel[, grep('Q4_1[3456]_', names(empanel))]
)

# write data to disk
write.csv(empanel, file = 'empanelment_cleaned.csv', row.names = FALSE)
