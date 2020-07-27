# Network Analytics (network density)

rm(list = ls(all = TRUE))
#install.packages("statnet", dependencies = TRUE) 
library(statnet)

# First setup the three organizational structures

roleNames <- c("Engineer", "LeadShotfirer", "LeadHewer", "LeadScout", "Shotfirer", "Hewer", "Scout")

# Hierarchical network

Hierarchical <- cbind(c(0,1,1,1,0,0,0),
                   c(1,0,1,1,1,0,0),
                   c(1,1,0,1,0,1,0),
                   c(1,0,1,1,0,0,1),
                   c(0,1,0,0,0,1,1),
                   c(0,0,1,0,1,0,1),
                   c(0,0,0,1,1,1,0))

hieNet <- as.network(x = Hierarchical, directed = FALSE, loops = FALSE, matrix.type = "adjacency")
network.vertex.names(hieNet) <- roleNames

# Cellular network

Cellular <- cbind(c(0,1,1,1,0,0,0),
                   c(1,0,0,0,1,0,0),
                   c(1,0,0,0,0,1,0),
                   c(1,0,0,0,0,0,1),
                   c(0,1,0,0,0,0,0),
                   c(0,0,1,0,0,0,0),
                   c(0,0,0,1,0,0,0))

celNet <- as.network(x = Cellular, directed = FALSE, loops = FALSE, matrix.type = "adjacency")
network.vertex.names(celNet) <- roleNames

# Network-network

Network <- matrix(1, nrow = 6, ncol = 6)
netNet <- as.network(x = Network, directed = FALSE, loops = FALSE, matrix.type = "adjacency")
network.vertex.names(netNet) <- c("LeadShotfirer", "LeadHewer", "LeadScout", "Shotfirer", "Hewer", "Scout") # No engineer

# Calculate density

gden(netNet)
gden(celNet)
gden(hieNet)

# Plot networks along with their densities

plot.network(hieNet, displaylabels = T, label.pos = 5)
plot.network(celNet, displaylabels = T, label.pos = 5)
plot.network(netNet, displaylabels = T, label.pos = 5)

# Function to calculate network density after disconnections

densityCalc <- function (orgMatrix, rolesMuted) {
  orgMatrixMuted <- eval(parse(text=orgMatrix))
  if (orgMatrix == "Network") {
    roleNames.df <- data.frame(roleNames[2:7], 1:6)
  } else {
    roleNames.df <- data.frame(roleNames, 1:7)
  }
  colnames(roleNames.df) <- "roleNames"
  rolesMuted.df <- data.frame(roleNames = rolesMuted)
  rolesMutednum <- merge(roleNames.df , rolesMuted.df, by = "roleNames")[,2]
  for (i in unique (rolesMutednum)) {
    orgMatrixMuted[,i]<- 0
    orgMatrixMuted[i,]<- 0  
  }
  mutedNet <- as.network(x = orgMatrixMuted, directed = FALSE, loops = FALSE, matrix.type = "adjacency")
  return(gden(mutedNet))
}

# Tests

rolesMuted <- ""
densityCalc("Network", rolesMuted) # Density should be = 1.00
densityCalc("Hierarchical", rolesMuted) # Density should be = 0.57
densityCalc("Cellular", rolesMuted) # Density should be = 0.29

rolesMuted <- roleNames
densityCalc("Network", rolesMuted) # Density should be = 0.00
densityCalc("Hierarchical", rolesMuted) # Density should be = 0.00
densityCalc("Cellular", rolesMuted) # Density should be = 0.00 

# Generate frame of densities based on Boomtown Json generation loop

roleSampler<- function() {
  nRoles  <- c(0,0,0,1,1,1,1,1,1,1,2,2,3,3,4)
  roles  <- c("Engineer", "LeadShotfirer", "LeadHewer", "LeadScout", "Shotfirer", "Hewer", "Scout")
  sampledRoles<-paste (sample (roles, sample(nRoles, 1)), collapse=',')
  return(sampledRoles)
}

gameSettings2 <- matrix (NA, 96, 14)
for (row in seq(1, 96, by=3)){
  gameSettings2[row,7]<- "Hierarchical"
  gameSettings2[row+1,7]<- "Cellular"
  gameSettings2[row+2,7]<- "Network"
}
for (row in 1:96){
  gameSettings2[row,14]<- row
  for (col in 1:6){
    set.seed((row*10)+col+1)
    gameSettings2[row,col]<- roleSampler()
    gameSettings2[row,col+7]<- densityCalc(paste(gameSettings2[row,7]), 
                                           strsplit(gameSettings2[row,col], ",")[[1]])
  }
}
head(gameSettings2)
gameSettings2 <- as.data.frame(gameSettings2)
colnames(gameSettings2)[14] <- c("settingsNum")
write.csv(gameSettings2, 'densities2.csv', row.names = FALSE)

densities <- read.csv('densities2.csv', stringsAsFactors = FALSE)
densities <- (densities[,8:14])
densities <- reshape(densities, varying=c("V8","V9","V10","V11","V12","V13"),
                     direction="long", v.names="density", idvar=c("settingsNum"), 
                     timevar="round2")

write.csv(densities, 'densities.csv', row.names = FALSE)
