# ngs2 cycle 2 empanelment lists
# for questions, contact: matt hoover (matt_hoover@gallup.com)

# clean environment and install packages that are required
rm(list = ls())
local <- TRUE # change to FALSE if getting data from the dod_clients drive

# determine system environment and define constants for use
if(Sys.info()['sysname'] == "Windows") {
    dd <- "//gallup/dod_clients/DARPA_NGS2/CONSULTING/Analytics/cycle2/data"
    od <- "//gallup/dod_clients//DARPA_NGS2/CONSULTING/Analytics/cycle2/output"
} else if(Sys.info()['sysname'] == 'Darwin') {
    if(local) {
        dd <- 'data'
        od <- 'cycle2/empanelment'
    } else {
        dd <- '/Volumes/dod_clients/DARPA_NGS2/CONSULTING/Analytics/cycle2/data'
        od <- '/Volumes/dod_clients/DARPA_NGS2/CONSULTING/Analytics/cycle2/output'
    }
}

# define functions
# split and output files by week
filemaker <- function(data, name) {
    lapply(split(data, data$wave), function(x) {
        write.csv(
            x,
            file =
                paste(od,
                paste0('cycle2_', name, '_week', unique(x$wave), '.csv'),
                sep = '/'),
            row.names = FALSE
        )
    })
}

# add random identifier and split
randomize <- function(data, cuts) {
    rn <- runif(nrow(data))
    data <- data[order(rn), ]
    groups <- rep(1:cuts, each = floor(nrow(data) / cuts))
    if(length(groups) != nrow(data)) {
        groups <- c(groups, rep(1:cuts, length.out = nrow(data) - length(groups)))
    }
    data$wave <- groups
    return(data)
}

# main work
# gather data
panels <- list.files(dd, pattern = 'cycle2_panel')
panel <- do.call(rbind, lapply(panels, function(x) {
    return(
        read.csv(paste(dd, x, sep = '/'), sep = ',', stringsAsFactors = FALSE,
                 header = TRUE)
    )
}))

wp <- read.csv(paste(dd, 'cycle2_wp_recontacts_19sep2018.csv', sep = '/'),
               header = TRUE, sep = ',', stringsAsFactors = FALSE)
g1k <- read.csv(paste(dd, 'cycle2_g1k_2015_recontacts_06jun2018.csv', sep = '/'),
                header = TRUE, sep = ',', stringsAsFactors = FALSE)

# munge data sources
wl_cycle1 <- subset(panel, EMPANEL_PREV_FINISHED == 1)
panel <- subset(panel, is.na(EMPANEL_PREV_FINISHED))

wp <- wp[, 2:ncol(wp)]
wp$WP12415 <- trimws(wp$WP12415)
wp_cohort <- ifelse(grepl('11', wp$filename), 0, 1)
wp_new <- subset(wp, wp_cohort == 1)
wp_old <- subset(wp, wp_cohort == 0)
wp_new <- wp_new[!duplicated(wp_new$WP12355), ]
wp_old <- wp_old[!duplicated(wp_old$WP12355), ]

# assert all id's are unique
stopifnot(length(wl_cycle1$EXTERNALDATAREFERENCE) ==
          length(unique(wl_cycle1$EXTERNALDATAREFERENCE)))
stopifnot(length(panel$EXTERNALDATAREFERENCE) ==
          length(unique(panel$EXTERNALDATAREFERENCE)))
stopifnot(length(wp_new$WP12355) == length(unique(wp_new$WP12355)))
stopifnot(length(wp_old$WP12355) == length(unique(wp_old$WP12355)))
stopifnot(length(g1k$G1K_ENTITY_ID) == length(unique(g1k$G1K_ENTITY_ID)))

# identify waves
set.seed(072619798)
wl_cycle1 <- randomize(wl_cycle1, 4)
panel <- randomize(panel, 4)
wp_old <- randomize(wp_old, 2)
wp_new <- randomize(wp_new, 3)
g1k <- randomize(g1k, 2)

# output files
filemaker(wl_cycle1, 'world_lab')
filemaker(panel, 'panel')
filemaker(wp_old, 'wp_old')
filemaker(wp_new, 'wp_new')
filemaker(g1k, 'g1k')
