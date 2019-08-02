### Created by Pablo Diego Rosell, PhD, for Gallup inc. in July 2019
# For questions, please email pablo_diego-rosell@gallup.co.uk

### Power Analysis for Bayesian hypothesis testing. 
# Conduct 100 simulations under priors implied by different hypotheses for ROM empirical power 
# Use one group-level hypothesis of a small effect (h12: Competition -> Conformity) as most data-hungry condition


# Power for  h12.1 vs null

output = matrix(data=NA, nrow=700, ncol=2)
row<-1
for(j in seq(2:8)){
  nBatches<-j
  for(i in 1:100){
    source("dataprep.R")
    output[row,1]<-j
    output[row,2] = lmerBF (formula.h12.1, h12.1, h12.null)
    print(row)
    print(output[row,])
    row<-row+1
  }
}

write.csv(output, "power_h12.csv")
