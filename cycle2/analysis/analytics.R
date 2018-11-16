## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2018
## For any questions, contact pablo_diego-rosell@gallup.co.uk

# Read in data
if('gamesData.csv' %in% list.files(paste(od, sep = '/'))) {
    factorial <- read.csv(file = paste(od, "gamesData.csv", sep = '/'))
} else {
    stop('WARNING - Metadata is missing; download by hand and merge to continue.')
}
factorial<-data.frame(lapply(factorial, factor))

