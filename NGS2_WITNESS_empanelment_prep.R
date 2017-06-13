# Empanelment prep for NGS2
# Author: Matt Hoover <matt_hoover@gallup.com>
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

# Define renaming dictionaries
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

# read in empanelment data
empanel <- read.csv('data/empanelment_demo_08jun2017.csv', header = TRUE,
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
empanel[, grep('^Q6_', names(empanel))] <- relabel_values(empanel, '^Q6_',
                                                          Q6_ANSWERS)
empanel[, grep('^Q7_', names(empanel))] <- relabel_values(empanel, '^Q7_',
                                                          Q7_ANSWERS)
empanel[, grep('^Q8_', names(empanel))] <- relabel_values(empanel, '^Q8_',
                                                          Q8_ANSWERS)
empanel[, grep('^Q9_', names(empanel))] <- relabel_values(empanel, '^Q9_',
                                                          Q9_ANSWERS)
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
empanel$Q6_2_critical_REV <- reverse_code(empanel$Q6_2_critical, 7)
empanel$Q6_4_anxious_REV <- reverse_code(empanel$Q6_4_anxious, 7)
empanel$Q6_6_reserved_REV <- reverse_code(empanel$Q6_6_reserved, 7)
empanel$Q6_8_disorganized_REV <- reverse_code(empanel$Q6_8_disorganized, 7)
empanel$Q6_10_conventional_REV <- reverse_code(empanel$Q6_10_conventional, 7)
empanel$tipi_extraversion <- rowMeans(
    empanel[, c('Q6_1_extravert', 'Q6_6_reserved_REV')]
)
empanel$tipi_agreeableness <- rowMeans(
    empanel[, c('Q6_7_sympathetic', 'Q6_2_critical_REV')]
)
empanel$tipi_conscientiousness <- rowMeans(
    empanel[, c('Q6_3_dependable', 'Q6_8_disorganized_REV')]
)
empanel$tipi_emot_stability <- rowMeans(
    empanel[, c('Q6_9_calm', 'Q6_4_anxious_REV')]
)
empanel$tipi_open_experiences <- rowMeans(
    empanel[, c('Q6_5_open', 'Q6_10_conventional_REV')]
)

# social dominance orientation
# https://dash.harvard.edu/bitstream/handle/1/3207711/Sidanius_SocialDominanceOrientation.pdf
# directions
#   1. Reverse-code items (9 through 16)
#   2. Take average
empanel$Q7_9_equal_REV <- reverse_code(empanel$Q7_9_equal, 7)
empanel$Q7_10_equality_ideal_REV <- reverse_code(empanel$Q7_10_equality_ideal, 7)
empanel$Q7_11_equal_chance_REV <- reverse_code(empanel$Q7_11_equal_chance, 7)
empanel$Q7_12_equalize_conditions_REV <- reverse_code(empanel$Q7_12_equalize_conditions, 7)
empanel$Q7_13_social_equality_REV <- reverse_code(empanel$Q7_13_social_equality, 7)
empanel$Q7_14_fewer_problems_REV <- reverse_code(empanel$Q7_14_fewer_problems, 7)
empanel$Q7_15_incomes_equal_REV <- reverse_code(empanel$Q7_15_incomes_equal, 7)
empanel$Q7_16_no_dominate_REV <- reverse_code(empanel$Q7_16_no_dominate, 7)
empanel$soc_dom_orient <- rowMeans(
    empanel[, grep('Q7_[1-8]_|Q7_.*REV$', names(empanel))]
)

# communal orientation scale
# http://fetzer.org/sites/default/files/images/stories/pdf/selfmeasures/CollectiveOrientation.pdf
# directions
#   1. Reverse-code items (3, 4, 6, 9, 10, 12, 13)
#   2. Take average
empanel$Q8_3_sensitive_feelings_REV <- reverse_code(empanel$Q8_3_sensitive_feelings, 7)
empanel$Q8_4_not_helpful_REV <- reverse_code(empanel$Q8_4_not_helpful, 7)
empanel$Q8_6_no_aid_REV <- reverse_code(empanel$Q8_6_no_aid, 7)
empanel$Q8_9_no_involvement_REV <- reverse_code(empanel$Q8_9_no_involvement, 7)
empanel$Q8_10_no_help_others_REV <- reverse_code(empanel$Q8_10_no_help_others, 7)
empanel$Q8_12_emotion_avoid_REV <- reverse_code(empanel$Q8_12_emotion_avoid, 7)
empanel$Q8_13_trouble_themselves_REV <- reverse_code(empanel$Q8_13_trouble_themselves, 7)
empanel$comm_orient_scale <- rowSums(
    empanel[, grep('^Q8_[12578]([14]|_)|Q8_.*_REV$', names(empanel))]
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
    empanel[, grep('Q9_[1234]_', names(empanel))]
)
empanel$vert_indiv <- rowSums(
    empanel[, grep('Q9_[5678]_', names(empanel))]
)
empanel$horiz_collect <- rowSums(
    empanel[, grep('Q9_(9|1[012])_', names(empanel))]
)
empanel$vert_collect <- rowSums(
    empanel[, grep('Q9_1[3456]_', names(empanel))]
)

# write data to disk
write.csv(empanel, file = 'empanelment_cleaned.csv', row.names = FALSE, na = '')
