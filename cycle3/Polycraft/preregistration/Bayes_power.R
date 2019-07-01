### Created by Pablo Diego Rosell, PhD, for Gallup inc. in June 2019
# For questions, please email pablo_diego-rosell@gallup.co.uk

### Power Analysis for Bayesian hypothesis testing. 
# Conduct 100 simulations under priors implied by different hypotheses for ROM empirical power 
# Power for  h1.1.1 vs null

output = matrix(data=NA, nrow=400, ncol=2)
row<-1
for(j in seq(80, 200, 40)){
  nGroups<-j
  for(i in 1:100){
    source("dataprep.R")
    output[row,1]<-j
    output[row,2] = glmerBF (formula.h1.1.1, h1.1.1, h1.1.null)
    print(row)
    print(output[row,])
    row<-row+1
  }
}

write.csv(output, "power_h1.1.1.csv")

# Power for null vs h1.1.1

output = matrix(data=NA, nrow=400, ncol=2)
row<-1
for(j in seq(80, 200, 40)){
  nGroups<-j
  for(i in 1:100){
    source("dataprep.R")
    output[row,1] = j
    output[row,2] =  glmerBF(formula.null, h1.1.null, h1.1.1)
    print(output[row,])
    print(row)
    row<-row+1
  }
}

write.csv(output, "power_h1.1.null.csv")

# Analyze results

power.true<- read.csv("power_h1.1.1.csv")
power.true$Hypothesis <- "H1.1.1 = True"
power.null<- read.csv("power_h1.1.null.csv")
power.null$Hypothesis <- "H1.1.Null = True"
str(power.true)
power<-rbind (power.true, power.null)

# Compare distributions for power simulations with 80, 120, 160, 200 groups

# plot densities 
ggplot(power, aes(x=BF)) + xlim(-100, 500) +
  geom_density()+facet_grid(nGroups+Hypothesis ~ .)

power$BF10 <- power$BF>=10
df1<-as.data.frame(power [power$BF10=="TRUE",] %>% group_by(nGroups, Hypothesis) %>% count(BF10))
ggplot(data=df1, aes(x=nGroups, y=n/100, group=Hypothesis)) +
  geom_line(aes(color=Hypothesis))+
  geom_hline(yintercept=0.80, color='darkgray', size=1, linetype="dashed")+
  geom_point(aes(color=Hypothesis)) +
  theme(legend.position="right") +
  annotate("text", 150, 0.80, vjust = -1, label = "80% Power", color='darkgray') +
  labs(x = "Number of Groups", y = "Power") +
  scale_x_continuous(breaks=c(80,120,160,200)) +
  scale_y_continuous(breaks=c(0.70, 0.75, 0.80, 0.85, 0.90, 0.95))
