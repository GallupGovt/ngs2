########################################################################################################################
# DIFI Analytics - To be added to "NGS2_WITNESS_Cycle1_exploratory_exp2.R"
########################################################################################################################

DIFI <- read.csv('NGS2-Cycle1-Experiment2/DIFI.exp2.csv',
                     header = TRUE, sep = ',')

# Select analytical variables
# (Exclude "reciprocity". Since these are non-directed networks, it does not apply)

VarList <- c("distanceScale",
             "overlapScale",
             "prob.cooperate",
             "score",
             "av.coop",
             "edge.density",
             "transitivity",
             "diameter",
             "assortativity",
             "degree.exp2",
             "eigen.centrality.exp2")
col.num <- which(colnames(DIFI) %in% VarList)
cont.vars <- DIFI[,col.num]

# DIFI descriptives

summary(DIFI$distanceScale)
hist(DIFI$distanceScale)
summary(DIFI$overlapScale)
hist(DIFI$overlapScale)

# Association tests with distance & overlap

pdf(file = paste('DIFI.correlations.pdf', sep = ''))
corrgram (cont.vars)
chart.Correlation(cont.vars)
dev.off()

# Univariate test function for predictors using glm

lm.tester <- function(dep.var, ind.var) {
  vars<-paste(c(ind.var))
  formula.lm<-as.formula(paste(dep.var, "~", vars, sep=""))
  lm.test <- lm(formula.lm, data = cont.vars)
  lm.multiwayvcov.test <- cluster.vcov(lm.test, 
                                       cbind(DIFI$pid, 
                                             DIFI$session)
                                       )
  output<-coeftest(lm.test, lm.multiwayvcov.test)
  return(output)
}

#Loop over all variables. 
#It will throw an error ("undefined columns selected") until session is  added to "coloring". 

# Distance scale report

lm.distanceScale.frame = list()
for (i in VarList) {
  report.lm<-lm.tester("distanceScale", i)
  report.rows<-length(report.lm[,1])
  lm.distanceScale.frame[[i]]<-as.data.frame(report.lm[,1:4])[2:report.rows,]
}

report.lm.distanceScale<-do.call(rbind, lm.distanceScale.frame)
write.csv(report.lm.distanceScale, file = 'lm_DIFIdistance_exp2.csv', row.names = FALSE, na = '')

# Overlap scale report

lm.overlapScale.frame = list()
for (i in VarList) {
  report.lm<-lm.tester("overlapScale", i)
  report.rows<-length(report.lm[,1])
  lm.overlapScale.frame[[i]]<-as.data.frame(report.lm[,1:4])[2:report.rows,]
}

report.lm.overlapScale<-do.call(rbind, lm.overlapScale.frame)
write.csv(report.lm.overlapScale, file = 'lm_DIFIoverlap_exp2.csv', row.names = FALSE, na = '')
