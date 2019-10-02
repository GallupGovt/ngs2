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


