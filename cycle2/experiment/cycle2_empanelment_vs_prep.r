#!/usr/local/bin/R
# script to process cycle2 empanelment data for volunteer science upload

# instructions
#   first name >> RecipientFirstName
#   last name >> RecipientLastName
#   username >> RecipientEmail before @ symbol
#   id (your internal identification for future needs) >> ResponseID
#   email >> RecipientEmail and/or Q54
#   phone number >> Q40
#   transformational leadership value >> Q107_{1-8} (note, there is no 7...)
#   tolerance of ambiguity value >> Q57_{1-12}
#    1, 4, 5, 9, 10, 11, 12 >> reverse code

# libraries and constants
library(dplyr)
library(hot.deck)

# grab command-line arguments
args <- commandArgs(trailingOnly = TRUE)

complete_vars <- c(
    'RecipientFirstName',
    'RecipientLastName',
    'RecipientEmail',
    'ResponseID',
    'Q40',
    'Q57_1',
    'Q57_2',
    'Q57_3',
    'Q57_4',
    'Q57_5',
    'Q57_6',
    'Q57_7',
    'Q57_8',
    'Q57_9',
    'Q57_10',
    'Q57_11',
    'Q57_12',
    'Q107_1',
    'Q107_2',
    'Q107_3',
    'Q107_4',
    'Q107_5',
    'Q107_6',
    'Q107_8'
)
imputation_variables <- 'nid|Q107|Q1[1-8]$|Q2[2-7]|Q29|Q56|Q57|Q59|Q6|Q7[0-9]|Q8[0-9]|q9[0-9]'

# define functions
ambiguity_calc <- function(x) {
    revs <- c(1, 4, 5, 9, 10, 11, 12)
    x[, paste0('Q57_', revs)] <- apply(x[, paste0('Q57_', revs)], 2, function(y) {
        return(abs(y - 5) + 1)
    })
    return(apply(x[, grep('Q57_', names(x))], 1, function(y) {sum(y)}))
}

create_game_id <- function(len=60) {
    return(paste0(sample(c(letters, 0:9), len, replace = TRUE), collapse = ''))
}

randomize_data <- function(x) {
    half <- floor(nrow(x) / 2)
    x <- x[order(x$tol_ambiguity), ]
    lh <- x[1:half, ]
    uh <- x[(half + 1):nrow(x), ]
    lh_switch <- sample(1:nrow(lh), floor(half * .2))
    uh_switch <- sample(1:nrow(uh), floor(half * .2))
    lh <- rbind(lh, uh[uh_switch, ])
    uh <- rbind(uh, lh[lh_switch, ])
    lh <- lh[-lh_switch, ]
    uh <- uh[-uh_switch, ]
    return(list('low_tol' = lh, 'high_tol' = uh))
}

trans_leadership <- function(x, choice=c('sum', 'mean')) {
    if(choice == 'sum') {
        tmp <- apply(x[, grep('Q107_', names(x))], 1, sum)
    } else {
        tmp <- apply(x[, grep('Q107_', names(x))], 1, mean)
    }
    return(tmp)
}

# load data
set.seed(as.numeric(gsub('-', '', Sys.Date())))

c <- read.csv(args[1], sep = ',', header = TRUE, stringsAsFactors = FALSE)
c <- c[c$Status == 0 & c$Q3 == 1, ]
gid <- read.csv('cycle2/data/vs_game_ids.csv', header = TRUE, sep = ',',
                stringsAsFactors = FALSE)

# impute missing data for scale values
c$Q26 <- ifelse(c$Q26 == ',0', 0, c$Q26)
c$Q26 <- as.numeric(c$Q26)

c$nid <- 1:nrow(c)
hd_values <- hot.deck(c[, grep(imputation_variables, names(c))], method = 'best.cell',
                      cutoff = 1, sdCutoff = 3)
c <- merge(c, hd_values$data[[1]], by='nid')
c <- c[, !grepl('.x', names(c))]
names(c) <- gsub('.y', '', names(c))

# prep the dataset
c$email <- ifelse(grepl('[Ff]ake', c$RecipientEmail), c$Q54, c$RecipientEmail)
c$email <- gsub(' ', '', c$email)
c$user_name <- do.call(rbind, lapply(strsplit(c$email, '@'), function(x) {
    return(trimws(x[1]))
}))
c <- c %>% rename(id = ResponseID,
                  phone_number = Q40,
                  `First Name` = RecipientFirstName,
                  `Last Name` = RecipientLastName,
                  `Email Address` = email)
c$phone_number <- gsub('-| ', '', c$phone_number)
c[, grep('Q57_', names(c))] <- apply(c[, grep('Q57_', names(c))], 2, function(x) {
    return(as.numeric(x))
})

# calculate scales
c$tol_ambiguity <- ambiguity_calc(c)
c$trans_leadership_sum <- trans_leadership(c, 'sum')

# combine data
d <- c[, c('First Name', 'Last Name', 'user_name', 'id', 'Email Address',
           'phone_number', 'trans_leadership_sum', 'tol_ambiguity')]

# merge in any old game ids
d <- merge(d, gid, by.x = 'id', by.y = 'qualtrics_id', all.x = TRUE)
d$game_id <- sapply(d$game_id, function(x) {
    ifelse(is.na(x), create_game_id(), x)
})
d$`Login Link` <- paste0('https://volunteerscience.com/worldlab/start/', d$game_id, '/')

# randomize to game conditions
conditions <- randomize_data(d)

# write data to disk
fpath = strsplit(args[2], '\\.')
mapply(function(x, name) {
    write.csv(x, file = paste0(fpath[[1]][1], '_', name, '.', fpath[[1]][2]),
              row.names = FALSE)
}, conditions, list('low', 'high'))
