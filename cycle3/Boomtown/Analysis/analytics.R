## Created by Pablo Diego Rosell, PhD, for Gallup inc. in September 2019
## Bayesian hypothesis testing (Individual-level outcomes)

# Number of valid games
factorial$group <- factorial$matchid
nGames<-length(unique(factorial$group))

# Number of players connected
nPlayers<-length(unique(factorial$player))

# Number of unique experimental conditions played
nConditions<-length(unique(factorial$settingsNum))

# Game dates
factorial$matchDate <- as.POSIXct(factorial$matchDate,format="%Y-%m-%d")
dates<-aggregate(matchDate ~ matchid, data=factorial, mean)

# Game times
factorial$date.time <- as.POSIXct(factorial$date.time,format="%Y-%m-%d %H:%M:%S")
times<-aggregate(date.time ~ matchid, data=factorial, mean)

# Games by hour

times$hour <- strftime(times$date.time, format="%Y-%m-%d %H")
times$hour2 <- substr(times$hour, nchar(times$hour) - 2 + 1, nchar(times$hour))
times$day <- strftime(times$date.time, format="%Y-%m-%d")

hourly_plot <- ggplot(data=times, mapping=aes(x=hour2)) + geom_bar() + 
facet_grid(facets = day ~ ., margins = FALSE) + theme_bw() + 
labs(title="Number of games by hour and day", x="Hour of the Day", y="Number of Games Played")
