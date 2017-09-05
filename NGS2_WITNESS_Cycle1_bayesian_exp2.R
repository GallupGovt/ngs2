#Created by Pablo Diego Rosell, PhD, for Gallup inc. in August 2017

# Summary function

bayesian.summary <- function (model) {
  prior.summary<-prior_summary(model)
  print(prior.summary)
  result.summary<-summary(model, digits =3)
  print(result.summary)
  return(result.summary)
  plot(model, plotfun = "areas", prob = 0.9)
}

###################################################### ##################################################################
#Hypothesis tests
########################################################################################################################
#1.1.	Individuals will be more likely to form connections with in-group members than with out-group members

glmm1.1.default <- stan_glmer(connect~ingroup + (1|playerid), 
                              data = exp2_rewire, 
                              family = binomial(link = "logit"), 
                              chains = 3, iter = 3000)
hypothesis.1.1.default <- bayesian.summary(glmm1.1.default)

#Prior of OR = 1.5 (expressed in log odds, as log(1.5)) from expected effect size (see pre-registration)
glmm1.1.inform <- stan_glmer(connect~ingroup + (1|playerid), 
                              data = exp2_rewire, 
                              family = binomial(link = "logit"), 
                              prior = student_t(log(1.5), df = 7),
                              chains = 3, iter = 3000)
hypothesis.1.1.inform <- bayesian.summary(glmm1.1.inform)

#1.2	Overall cooperation level will increase with successive rounds

glmm1.2.default <- stan_glmer(decision0d1c~round_num + (1|playerid), 
                              data = exp2_cooperation, 
                              family = binomial(link = "logit"), 
                              chains = 3, iter = 3000)
hypothesis.1.2.default <- bayesian.summary(glmm1.2.default)

#Prior of OR = 1.5 over 20 rounds, (expressed in log odds, as log(1.5^(1/20)) from expected effect size (see pre-registration)
glmm1.2.inform <- stan_glmer(decision0d1c~round_num + (1|playerid), 
                              data = exp2_cooperation, 
                              family = binomial(link = "logit"), 
                              prior = student_t(log(1.5^(1/20)), df = 7),
                              chains = 3, iter = 3000)
hypothesis.1.2.inform <- bayesian.summary(glmm1.2.inform)

#Average cooperation by round
#plot(aggregate(cooperation$decision0d1c, list(cooperation$round_num), mean))

#2.1	In-group favoritism will be more likely in the biased pairing condition

glmm2.1.default <- stan_glmer(decision0d1c~ingroup*biased + (1|playerid), 
                              data = exp2_cooperation, 
                              family = binomial(link = "logit"), 
                              chains = 3, iter = 3000)
hypothesis.2.1.default <- bayesian.summary(glmm2.1.default)

#Prior of OR = 1.25 for interaction term from expected effect size (see pre-registration). Other parameters set at 0. 
priors.2.1 <- student_t(c(0, 0, log(1.25)), df=7)
glmm2.1.inform <- stan_glmer(decision0d1c~ingroup*biased + (1|playerid), 
                              data = exp2_cooperation, 
                              family = binomial(link = "logit"), 
                              prior = priors.2.1,
                              chains = 3, iter = 3000)
hypothesis.2.1.inform <- bayesian.summary(glmm2.1.inform)

#3.1	Individuals in the 2 avatar condition will be more likely to form connections with in-group members than those in the 4 avatar condition

glmm3.1.default <- stan_glmer(connect~ingroup*identities + (1|playerid), 
                              data = exp2_rewire, 
                              family = binomial(link = "logit"), 
                              chains = 3, iter = 3000)
hypothesis.3.1.default <- bayesian.summary(glmm3.1.default)

#Prior of OR = 1.5 for interaction term from expected effect size (see pre-registration). Other parameters set at 0. 
priors.3.1 <- student_t(c(0, 0, log(1.5)), df=7)
glmm3.1.inform <- stan_glmer(connect~ingroup*identities + (1|playerid), 
                              data = exp2_rewire, 
                              family = binomial(link = "logit"), 
                              prior = priors.3.1,
                              chains = 3, iter = 3000)
hypothesis.3.1.inform <- bayesian.summary(glmm3.1.inform)

#3.2	Individuals in the 2 avatar condition will be less likely to cooperate with in-group members than those in the 4 avatar condition

glmm3.2.default <- stan_glmer(decision0d1c~ingroup*identities + (1|playerid), 
                              data = exp2_cooperation, 
                              family = binomial(link = "logit"), 
                              chains = 3, iter = 3000)
hypothesis.3.2.default <- bayesian.summary(glmm3.2.default)

#Prior of OR = 1.25 for interaction term from expected effect size (see pre-registration). Other parameters set at 0. 
priors.3.2 <- student_t(c(0, 0, log(1.25)), df=7)
glmm3.2.inform <- stan_glmer(decision0d1c~ingroup*identities + (1|playerid), 
                              data = exp2_cooperation, 
                              family = binomial(link = "logit"), 
                              prior = priors.3.2,
                              chains = 3, iter = 3000)
hypothesis.3.2.inform <- bayesian.summary(glmm3.2.inform)
