####################################################################################################
# Coloring Game
####################################################################################################

# Load raw breadboard data
coloring <- read.csv('Coloring game/ngs2_e3_pilot_2017-07-12-01_9409.csv',
                 header = TRUE, sep = ',', stringsAsFactors = FALSE)
players<- as.data.frame(subset(coloring$data.value, coloring$event=='PlayerReady'))
colnames(players)[1]<-"pid"

### Compute performance metrics
# Graph completion time

### There are multiple end times, apparently 2 players went on to a different game.
### Take the first end time as valid for the 5 players that completed the game.
time.start <- as.POSIXct(subset(coloring$datetime, coloring$event=='GameStart'))
time.end <- as.POSIXct(subset(coloring$datetime, coloring$event=='GameEnd')[1])
players$completion.time<-time.end-time.start

# Individual probability of cooperation

av.cooperation.exp1 <-aggregate(exp1_cooperation$action,
                               by=list(exp1_cooperation$pid),
                               FUN=mean,
                               na.rm=TRUE)
colnames(av.cooperation.exp1)<-c("pid", "prob.cooperate")

### Network structure metrics (per Marcus Alexander)

connections.coloring <- subset(coloring,
                      event=="Connected",
                      select=c("id",
                               "data.name",
                               "data.value")
)

connections.coloring.dcast <- dcast(connections.coloring,
                           id~ data.name,
                           value.var="data.value")

connections.from.to.coloring <- cbind(connections.coloring.dcast$playerId1,
                                      connections.coloring.dcast$playerId2)
nodes.coloring <- unique(
  append(
    (unique(connections.from.to.coloring[,1])),
    (unique(connections.from.to.coloring[,2]))
    )
  )

net.coloring <- graph_from_data_frame(d=connections.from.to.coloring,
                             vertices=nodes.coloring,
                             directed=F)

# Network metrics

net.edge_density.coloring <- edge_density(net.coloring, loops=F)
net.reciprocity.coloring <- reciprocity(net.coloring)
net.transitivity.coloring <- transitivity(net.coloring, type="global") # net is treated as an undirected network
net.diameter.coloring <- diameter(net.coloring, directed=F, weights=NA)
net.assortativity_degree.coloring <- assortativity_degree(net.coloring, directed=F)

# Overall probability of cooperation in final round

av.cooperation.round.exp1 <-aggregate(
  as.integer(exp1_cooperation$action),
  by=list(exp1_cooperation$round),
  FUN=mean,
  na.rm=TRUE)

colnames(av.cooperation.round.exp1) <- c("finalRound", "av.coop")

av.cooperation.round.final.exp1 <- subset(av.cooperation.round.exp1,
                                          finalRound==max(finalRound),
                                          select=c("av.coop")
                                          )

# Node metrics

# Degree

deg.coloring<-degree(net.coloring, mode="all")
deg.frame.coloring<-cbind(names(deg.coloring), deg.coloring)
colnames(deg.frame.coloring)<- c("pid", "degree")

# Centrality (eg eigenvalue)

eigen<-eigen_centrality(net.coloring, directed=T, weights=NA)
eigen.vector.coloring<- eigen$vector

# Plot

plot(net.coloring,
     edge.arrow.size=.2,
     edge.curved=0,
     vertex.color="orange",
     vertex.frame.color="#555555",
     vertex.label=V(net.coloring)$pid,
     vertex.label.color="black",
     vertex.label.cex=.7,
     vertex.size=(deg.coloring)*3)

# Merge all data into one analytical dataset and save

coloring.1 <- merge (players, av.cooperation.exp1, by = "pid")
coloring.2 <- data.frame(coloring.1,
                         av.coop.final.exp1 = av.cooperation.round.final.exp1,
                         edge_density = net.edge_density.coloring,
                         reciprocity = net.reciprocity.coloring,
                         transitivity = net.transitivity.coloring,
                         diameter = net.diameter.coloring,
                         assortativity_degree = net.assortativity_degree.coloring
                         )

coloring.3 <- merge (coloring.2, deg.frame, by = "pid")
coloring.save <- merge (coloring.3, eigen.frame, by = "pid")

write.csv(coloring.save,
          "NGS2-Cycle1-Experiment2/coloring.exp1.csv")

####################################################################################################
# DIFI
####################################################################################################

# Breadboard extract function by event type

breadboard.extract <- function (eventType) {
  breadboardFields <- c("id",
                        "data.name",
                        "data.value")
  data.subset <- subset(exp2,
                        event==eventType,
                        select=breadboardFields)
  data.subset.dcast <- dcast(data.subset,
                             id ~ data.name,
                             value.var="data.value")
  if(c('pid') %in% names(data.subset.dcast))
    {
    data.subset.dcast$pid <- as.integer(gsub("_",
                                             "",
                                             data.subset.dcast$pid)
                                        )
  }
return(data.subset.dcast)
}

# DIFI scores

DIFI.final<-breadboard.extract ("DIFI")

# Individual probability of cooperation

av.cooperation.exp2 <-aggregate(exp2_cooperation$decision0d1c,
                               by=list(exp2_cooperation$playerid),
                               FUN=mean,
                               na.rm=TRUE)
colnames(av.cooperation.exp2)<-c("pid", "prob.cooperate")

### Create frame of nodes in network, with their final score and final group.

# Final score

score.exp2 <- breadboard.extract ("FinalScore")

# Final group

group.data.exp2<-breadboard.extract ("ChangeGroup")
group.data.exp2$finalGroupRound <- as.integer(group.data.exp2$curRound)
group.data.exp2$finalGroup <- as.integer(group.data.exp2$group)
group.data.exp2.final  <- subset(group.data.exp2,
                            finalGroupRound==max(finalGroupRound),
                            select=c("pid",
                                     "finalGroup",
                                     "finalGroupRound")
                            )

### Add network properties (cooperative structure)

connections.exp2 <- subset(exp2,
                      event=="Connected" | event=="Disconnected",
                      select=c("id",
                               "event",
                               "data.name",
                               "data.value")
                      )

connections.exp2$data.value <- as.integer(gsub("_",
                                         "",
                                         connections.exp2$data.value)
                                     )

connections.exp2.dcast <- dcast(connections.exp2,
                           id+event ~ data.name,
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

connections.from.to.exp2 <- cbind(connections.final$playerId1,connections.final$playerId2)
connections.from.to.exp2.final <- connections.from.to.exp2[complete.cases(connections.from.to.exp2), ]

net.exp2 <- graph_from_data_frame(d=connections.from.to.exp2.final,
                             vertices=DIFI.1,
                             directed=F)

# Network metrics

net.edge_density.exp2 <- edge_density(net.exp2, loops=F)
net.reciprocity.exp2 <- reciprocity(net.exp2)
net.transitivity.exp2 <- transitivity(net.exp2, type="global") # net is treated as an undirected network
net.diameter.exp2 <- diameter(net.exp2, directed=F, weights=NA)
net.assortativity_degree.exp2 <- assortativity_degree(net.exp2, directed=F)

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

# Degree

deg.exp2<-degree(net.exp2, mode="all")
deg.frame.exp2<-cbind(names(deg.exp2), deg.exp2)
colnames(deg.frame.exp2)<- c("pid", "degree.exp2")

# Centrality (eg eigenvalue)

eigen.exp2<-eigen_centrality(net.exp2, directed=T, weights=NA)
eigen.vector.exp2<- eigen.exp2$vector
eigen.frame.exp2<-cbind(names(eigen.vector.exp2), eigen.vector.exp2)
colnames(eigen.frame.exp2)<- c("pid", "eigen.centrality.exp2")

# Plot final network

plot(net.exp2,
     edge.arrow.size=.2,
     edge.curved=0,
     vertex.color="orange",
     vertex.frame.color="#555555",
     vertex.label=V(net)$pid,
     vertex.label.color="black",
     vertex.label.cex=.7,
     vertex.size=(deg.exp2*3))

# Merge all data into one analytical dataset and save

DIFI.1 <- merge (av.cooperation.exp2, score.exp2, by = "pid")
DIFI.2 <- merge (group.data.exp2.final, DIFI.final, by = "pid")
DIFI.3 <- merge (DIFI.1, DIFI.2, by = "pid")
DIFI.4 <- data.frame(DIFI.3,
                        av.coop.final.exp2 = av.cooperation.round.exp2.final,
                        edge_density.exp2 = net.edge_density.exp2,
                        reciprocity.exp2 = net.reciprocity.exp2,
                        transitivity.exp2 = net.transitivity.exp2,
                        diameter.exp2 = net.diameter.exp2,
                        assortativity_degree.exp2 = net.assortativity_degree.exp2
                        )
DIFI.5 <- merge (DIFI.4, deg.frame.exp2, by = "pid")
DIFI.save <- merge (DIFI.5, eigen.frame.exp2, by = "pid")

write.csv(DIFI.save,
          "NGS2-Cycle1-Experiment2/DIFI.exp2.csv")
