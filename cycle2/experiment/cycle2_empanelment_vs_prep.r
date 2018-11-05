#!/usr/local/bin/R
# script to process cycle2 empanelment data for volunteer science upload

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

# instructions
#   first name >> RecipientFirstName
#   last name >> RecipientLastName
#   username >> RecipientEmail before @ symbol
#   id (your internal identification for future needs) >> ResponseID
#   email >> RecipientEmail
#   phone number >> Q40
#   transformational leadership value >> Q107_{1-8} (note, there is no 7...)
#   tolerance of ambiguity value >> Q57_{1-12}
#    1, 4, 5, 9, 10, 11, 12 >> reverse code

# load data
c <- read.csv(args[1], sep = ',', header = TRUE, stringsAsFactors = FALSE)
c <- c[c$Status == 0 & c$Q3 == 1, ]

# impute missing data for scale values
c$nid <- 1:nrow(c)
hd_values <- hot.deck(c[, grep(imputation_variables, names(c))], method = 'best.cell',
                      cutoff = 1, sdCutoff = 3)
c <- merge(c, hd_values$data[[1]], by='nid')
c <- c[, !grepl('.x', names(c))]
names(c) <- gsub('.y', '', names(c))

# prep the dataset
c$user_name <- do.call(rbind, lapply(strsplit(c$RecipientEmail, '@'), function(x) {
    return(trimws(x[1]))
}))
c <- c %>% rename(id = ResponseID, email = RecipientEmail, phone_number = Q40,
                  first_name = RecipientFirstName, last_name = RecipientLastName)
c$phone_number <- gsub('-', '', c$phone_number)
c[, grep('Q57_', names(c))] <- apply(c[, grep('Q57_', names(c))], 2, function(x) {
    return(as.numeric(x))
})

# calculate scales
c$tol_ambiguity <- ambiguity_calc(c)
c$trans_leadership_sum <- trans_leadership(c, 'sum')
c$trans_leadership_mean <- trans_leadership(c, 'mean')

# combine data
d <- c[, c('first_name', 'last_name', 'user_name', 'id', 'email',
           'phone_number', 'trans_leadership_sum', 'trans_leadership_mean',
           'tol_ambiguity')]

# randomize to game conditions
conditions <- randomize_data(d)

# write data to disk
fpath = strsplit(args[2], '\\.')
mapply(function(x, name) {
    write.csv(x, file = paste0(fpath[[1]][1], '_', name, '.', fpath[[1]][2]),
              row.names = FALSE)
}, conditions, list('low', 'high'))
