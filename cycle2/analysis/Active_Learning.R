## Created by John Paisley and Ghazal Fazelnia, Columbia University, in October 2018
## Generate all of the possible combinations of variables, we call that test.data

test.data1 <- expand.grid(h1.1=0, 
                          h2.1=c(0,1), 
                          h3.1=c(0,1), 
                          h3.2=c(0,1), 
                          h3.3=9,
                          h3.4=c(0,1),
                          h3.5 = c(0,1,2),
                          tools = seq(1:8))

test.data2 <- expand.grid(h1.1=c(1, 2, 3), 
                          h2.1=c(0,1), 
                          h3.1=c(0,1), 
                          h3.2=c(0,1), 
                          h3.3=seq(0:3), 
                          h3.4=c(0,1),
                          h3.5 = c(0,1,2),
                          tools = seq(1:8))
test.data<-rbind(test.data1, test.data2)

test.data$settingsNum <- rep(seq(1:(208)),24) # we set all matchid values to be the same so it would not have any effects

test.data$h1.3<-0

test.data <- data.frame(lapply(test.data, factor))

levels(test.data$h1.1) <- levels(factorial$h1.1)
levels(test.data$h3.3) <- levels(factorial$h3.3)
levels(test.data$tools) <- levels(factorial$tools)
# 4*2*2*2*5*2*3
sum(table(factorial$h1.1))
test.data$matchid = 1

##### Now we are ready to run active learning process:
################################################################################
## Active Learning Criterion Number 1: Choosing based on Variance of Posterior

S = 100 # number of Monte Carlo draws from p(w|D) where D is the initial set of observed data

x = test.data
ppdx = posterior_linpred(glmmoverall,newdata = x,transform = TRUE)[1:S,]

prob = (1/S * colSums(ppdx))*(1-1/S * colSums(ppdx))
rank.var <- order(prob,decreasing = TRUE)
x.ranked.var = x[rank.var,]

## matchid assignments:

print("here are the top 20 suggestions based on the first active learning process")
print(x.ranked.var[1:20,1:9])

################################################################################
## Active Learning Criterion Number 2: Choosing based on entropy of posterior


P0.mat = posterior_linpred(glmmoverall,newdata = x,transform = TRUE)[1:S,] # ppdx
term1=rep(0,ncol(P0.mat))
term2=rep(0,ncol(P0.mat))

for (i in 1:ncol(P0.mat)){
  term1[i] = -(1/S)*(sum(P0.mat[,i]*log(P0.mat[,i]))+sum((1-P0.mat[,i])*log(1-P0.mat[,i])))
  term2[i] = log(1/S *sum(P0.mat[,i]))*(1/S *sum(P0.mat[,i])) + log(1/S *sum(1-P0.mat[,i]))*(1/S *sum(1-P0.mat[,i]))
}

post.entropy = term1 + term2

rank.post.entropy <- order(post.entropy,decreasing = FALSE)
x.ranked.post.entropy = x[rank.post.entropy,]

print("here are the top 20 suggestions based on the second active learning process")
print(x.ranked.post.entropy[1:20,1:9])
