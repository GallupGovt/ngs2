####################################################################################################
# DIFI
####################################################################################################

# Breadboard extract function by event type

breadboard.extract <- function (eventType) {
  breadboard.list <- lapply(exp2, function(x) {
    roll <- subset(x, 
                   event == eventType, 
                   select = c('id', 
                              'data.name', 
                              'data.value')
                   )
    return(roll)
    })
  breadboard.list2 <- mapply(`[<-`, 
                             breadboard.list, 
                             'session', 
                             value = names(breadboard.list), 
                             SIMPLIFY = FALSE)
  breadboard.events<-do.call(rbind,
                             lapply(breadboard.list2,data.frame)
                             )
  breadboard.frame<-dcast(breadboard.events, 
                          id + session ~ data.name, 
                          value.var = 'data.value')
  return(breadboard.frame)
}

# Extract DIFI scores

DIFI.final<-breadboard.extract ("DIFI")

# Compute individual probability of cooperation

av.cooperation.exp2 <-aggregate(exp2_cooperation$decision0d1c,
                                by=list(exp2_cooperation$playerid),
                                FUN=mean,
                                na.rm=TRUE)
colnames(av.cooperation.exp2)<-c("pid", "prob.cooperate")

### Create frame of nodes in network, with their final score and final group.

# Final score

score.exp2 <- breadboard.extract ("FinalScore")

# Final group

exp2_cooperation <- read.csv('NGS2-Cycle1-Experiment2/cooperation_exp2.csv',
                             header = TRUE, sep = ',')

final.group <- unique(subset (exp2_cooperation, 
                       round_num == 15, 
                       select = c('playerid', 'group')))

colnames(final.group) <- c('pid', 'group')

### Add network properties (cooperative structure)

connections.list <- lapply(exp2, function(x) {
  roll <- subset(x, 
                 event == "Connected" | event == "Disconnected" , 
                 select = c('id',
                            "event",
                            'data.name', 
                            'data.value')
  )
  return(roll)
})

connections.list2 <- mapply(`[<-`, 
                            connections.list, 
                           'session', 
                           value = names(breadboard.list), 
                           SIMPLIFY = FALSE)
connection.events<-do.call(rbind,
                           lapply(connections.list2,data.frame)
)
connections.exp2.dcast <- dcast(connection.events,
                                id+session+event ~ data.name,
                                value.var="data.value")

connections.exp2.final <- connections.exp2.dcast[0,]

for (i in 1:length(connections.exp2.dcast$id))
{
  if
  (connections.exp2.dcast$event[i]=='Connected'){
    connections.exp2.final[i,] <- rbind(connections.exp2.dcast[i,])
  }
  else if
  (connections.exp2.dcast$event[i]=='Disconnected'){
    connections.exp2.final <-
      subset(connections.exp2.final, !(
        (playerId1 == connections.exp2.dcast[i, c("playerId1")] &
           playerId2 == connections.exp2.dcast[i, c("playerId2")]) |
          (playerId1 == connections.exp2.dcast[i, c("playerId2")] &
             playerId2 == connections.exp2.dcast[i, c("playerId1")])
      )
      )
  }
  else {
    NULL
  }
}

connections.from.to.exp2 <- cbind(connections.exp2.final$playerId1,
                                  connections.exp2.final$playerId2, 
                                  connections.exp2.final$session)


connections.from.to.exp2.final <- connections.from.to.exp2[complete.cases(connections.from.to.exp2), ]
DIFI.1 <- merge (av.cooperation.exp2, score.exp2, by = "pid")

DIFI.1.subset<-subset(DIFI.1, session == i)

### Loop over each session to compute network metrics and graphs

network.frame <- data.frame(matrix(ncol= 6))
network.frame.names <-c("session", 
                        "edge.density", 
                        "reciprocity", 
                        "transitivity", 
                        "diameter", 
                        "assortativity")
colnames(network.frame) <- network.frame.names


for(i in unique(connections.from.to.exp2.final[,3])) {
  
  connection.subset <- connections.from.to.exp2.final[connections.from.to.exp2.final[,3] == i,
                                                      1:2]
  DIFI.1.subset<-subset(DIFI.1, 
                        session == i)
  
  net.exp2 <- graph_from_data_frame(d=connection.subset,
                                    vertices=DIFI.1.subset,
                                    directed=F)
  
  net.edge_density.exp2 <- edge_density(net.exp2, loops=F)
  net.reciprocity.exp2 <- reciprocity(net.exp2)
  net.transitivity.exp2 <- transitivity(net.exp2, type="global") # net is treated as an undirected network
  net.diameter.exp2 <- diameter(net.exp2, directed=F, weights=NA)
  net.assortativity_degree.exp2 <- assortativity_degree(net.exp2, directed=F)
  network.row<-data.frame(cbind(i,
                     net.edge_density.exp2, 
                     net.reciprocity.exp2, 
                     net.transitivity.exp2, 
                     net.diameter.exp2, 
                     net.assortativity_degree.exp2))
  colnames(network.row) <- network.frame.names
  network.frame<-rbind(network.frame, network.row)
  
  # Plot final network
  
  jpeg(file = paste('netgraph.', i, '.jpeg', sep = ''))
  plot(net.exp2,
       edge.arrow.size=.2,
       edge.curved=0,
       vertex.color="orange",
       vertex.frame.color="#555555",
       vertex.label=V(net.exp2)$pid,
       vertex.label.color="black",
       vertex.label.cex=.7, 
       vertex.size=(deg.exp2*3), 
       main=i)
  dev.off()
  }

network.frame <- subset(network.frame, !is.na(network.frame$session))

# Overall probability of cooperation in final round

av.cooperation.round.exp2 <-aggregate(
  as.integer(exp2_cooperation$decision0d1c),
  by=list(exp2_cooperation$round_num),
  FUN=mean,
  na.rm=TRUE)

colnames(av.cooperation.round.exp2) <- c("finalRound", "av.coop")

av.cooperation.round.exp2.final <- subset(av.cooperation.round.exp2,
                                          finalRound==max(finalRound),
                                          select=c("av.coop"))

# Node metrics

net.exp2 <- graph_from_data_frame(d=connections.from.to.exp2.final,
                                  vertices=DIFI.1,
                                  directed=F)

# Degree

deg.exp2<-degree(net.exp2, mode="all")
deg.frame.exp2<-cbind(names(deg.exp2), deg.exp2)
colnames(deg.frame.exp2)<- c("pid", "degree.exp2")

# Centrality (eg eigenvalue)

eigen.exp2<-eigen_centrality(net.exp2, directed=T, weights=NA)
eigen.vector.exp2<- eigen.exp2$vector
eigen.frame.exp2<-cbind(names(eigen.vector.exp2), eigen.vector.exp2)
colnames(eigen.frame.exp2)<- c("pid", "eigen.centrality.exp2")


# Merge all data into one analytical dataset and save

DIFI.2 <- merge (final.group, DIFI.final, by = "pid")
DIFI.3 <- merge (DIFI.1, DIFI.2, by = "pid")
names(DIFI.3)[names(DIFI.3) == 'session.x'] <- 'session'
DIFI.4 <- merge (network.frame, DIFI.3, by =  "session")
DIFI.5 <- merge (DIFI.4, deg.frame.exp2, by = "pid")
DIFI.save <- merge (DIFI.5, eigen.frame.exp2, by = "pid")

write.csv(DIFI.save,
          "NGS2-Cycle1-Experiment2/DIFI.exp2.csv")