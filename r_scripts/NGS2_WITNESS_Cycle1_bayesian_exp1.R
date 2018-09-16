#Created by Pablo Diego Rosell, PhD, for Gallup inc. in August 2017

#Summary Function

bayesian.summary <- function (model) {
  prior.summary<-prior_summary(model)
  print(prior.summary)
  result.summary<-summary(model, digits =3)
  print(result.summary)
  return(result.summary)
  plot(model, plotfun = "areas", prob = 0.9)
  }

#Hypothesis 4.1.1: Fixed network compositions reduce cooperation
#Rand  et al. (2011) coeff = -0.186, SE = 0.036, p<.000

glm.out.4.1.1.default=stan_glm(action~round, 
                               data=exp1_cooperation.fixed,
                               family = binomial(link = "logit"), 
                               chains = 3, iter = 3000)
Hypothesis.4.1.1.default <- bayesian.summary(glm.out.4.1.1.default)
#launch_shinystan(glm.out.4.1.1.default)

glm.out.4.1.1.inform=stan_glm(action~round, 
                              data=exp1_cooperation.fixed,
                              family = binomial(link = "logit"), 
                              prior = student_t(-0.186, 0.036, df = 7),
                              prior_intercept = student_t(0.662, 0.196, df = 7),
                              chains = 3, iter = 3000)
Hypothesis.4.1.1.inform <- bayesian.summary(glm.out.4.1.1.inform)
#launch_shinystan(glm.out.4.1.1.inform)

#Hypothesis 4.1.2: Randomly updating network compositions reduce cooperation
#Rand  et al. (2011) coeff = -0.113, p<.000 

exp1_cooperation.random<-subset(exp1_cooperation, condition=="Random")
glm.out.4.1.2.default=stan_glm(action~round, 
                       data=exp1_cooperation.random,
                       family = binomial(link = "logit"), 
                       chains = 3, iter = 3000)
Hypothesis.4.1.2.default <- bayesian.summary(glm.out.4.1.2.default)
#launch_shinystan(glm.out.4.1.2.default)

glm.out.4.1.2.inform=stan_glm(action~round, 
                              data=exp1_cooperation.random,
                              family = binomial(link = "logit"), 
                              prior = student_t(-0.113, 0.025, df = 7),
                              prior_intercept = student_t(0.455, 0.140, df = 7),
                               chains = 3, iter = 3000)
Hypothesis.4.1.2.inform <- bayesian.summary(glm.out.4.1.2.inform)
#launch_shinystan(glm.out.4.1.2.inform)

#Hypothesis 4.1.3: Slowly updating strategic networks reduce cooperation
#Rand  et al. (2011) coeff = -0.220, p=.013

exp1_cooperation.viscous<-subset(exp1_cooperation, condition=="Viscous")
glm.out.4.1.3.default=stan_glm(action~round, 
                       data=exp1_cooperation.viscous,
                       family = binomial(link = "logit"), 
                       chains = 3, iter = 3000)
Hypothesis.4.1.3.default <- bayesian.summary(glm.out.4.1.3.default)
#launch_shinystan(glm.out.4.1.3.default)

glm.out.4.1.3.inform=stan_glm(action~round, 
                              data=exp1_cooperation.viscous,
                              family = binomial(link = "logit"), 
                              prior = student_t(-0.220, 0.088, df = 7),
                              prior_intercept = student_t(1.044, 0.303, df = 7),
                              chains = 3, iter = 3000)
Hypothesis.4.1.3.inform <- bayesian.summary(glm.out.4.1.3.inform)
#launch_shinystan(glm.out.4.1.3.inform)

#Hypothesis 4.1.4: Rapidly updating strategic networks support cooperation relative to all other conditions 
#Rand  et al. (2011) coeff = 0.135, p = .006

glm.out.4.1.4.default=stan_glm(action~fluid_dummy+round+
                   fluid_dummy*round, 
                 data=exp1_cooperation,
                 family = binomial(link = "logit"), 
                 chains = 3, iter = 3000)
Hypothesis.4.1.4.default <- bayesian.summary(glm.out.4.1.4.default)
#launch_shinystan(glm.out.4.1.4)

priors.4.1.4 <- student_t(c(-0.161, -0.171, 0.135), c(0.230, 0.031, 0.050), df=7)
glm.out.4.1.4.inform=stan_glm(action~fluid_dummy+round+
                                 fluid_dummy*round, 
                               data=exp1_cooperation,
                               family = binomial(link = "logit"), 
                               prior = priors.4.1.4,
                               prior_intercept = student_t(0.700, 0.122, df = 7),
                               chains = 3, iter = 3000)
Hypothesis.4.1.4.inform <- bayesian.summary(glm.out.4.1.4.inform)

#Hypothesis 4.2.1	Rapidly updating strategic networks have greater network heterogeneity

network.variance <-aggregate(exp1_cooperation[,c("num_neighbors")], 
                             by=list(session=exp1_cooperation$session, 
                                     fluid_dummy=exp1_cooperation$fluid_dummy), 
                             FUN=var)
fluid.var<-(subset(network.variance$x, 
                   network.variance$fluid_dummy==1)
           )
other.var<-(subset(network.variance$x, 
                   network.variance$fluid_dummy==0)
Hypothesis.4.2.1.freq <- wilcox.test(fluid.var, other.var, paired = FALSE, alternative = "two.sided")
### Consider approximate Bayesian alternative, eg. http://andrewgelman.com/2015/07/13/dont-do-the-wilcoxon/

#Hypothesis 4.2.2	Links in rapidly updating strategic networks are more stable between cooperators than between a cooperator and a defector
#Rand  et al. (2011) coeff = -2.29, p<.000

exp1_rewire.cd.dc<-subset(exp1_rewire, previouslytie==1 & (state=="CC" | state=="CD" | state=="DC"))
glm.out.4.2.2.default=stan_glm(break_tie~CC, 
                       data=exp1_rewire.cd.dc,
                       family = binomial(link = "logit"), 
                       chains = 3, iter = 3000)
Hypothesis.4.2.2.default <- bayesian.summary(glm.out.4.2.2.default)
#launch_shinystan(glm.out.4.2.2.default)

glm.out.4.2.2.inform=stan_glm(break_tie~CC, 
                              data=exp1_rewire.cd.dc,
                              family = binomial(link = "logit"), 
                              prior = student_t(-2.295, 0.276, df = 7),
                              prior_intercept = student_t(-0.287, 0.085, df = 7),
                              chains = 3, iter = 3000)
Hypothesis.4.2.2.inform <- bayesian.summary(glm.out.4.2.2.inform)
#launch_shinystan(glm.out.4.2.2.inform)

#Hypothesis 4.2.3	Links in rapidly updating strategic networks are more stable between cooperators than between between two defectors
#Rand  et al. (2011) coeff = -2.94, p<.000

exp1_rewire.dd<-subset(exp1_rewire, previouslytie==1 & (state=="CC" | state=="DD"))

glm.out.4.2.3.default=stan_glm(break_tie~CC, 
                       data=exp1_rewire.dd,
                       family = binomial(link = "logit"), 
                       chains = 3, iter = 3000)
Hypothesis.4.2.3.default <- bayesian.summary(glm.out.4.2.3.default)
#launch_shinystan(glm.out.4.2.3.default)

glm.out.4.2.3.inform=stan_glm(break_tie~CC, 
                              data=exp1_rewire.dd,
                              family = binomial(link = "logit"), 
                              prior = student_t(-2.943, 0.499, df = 7),
                              prior_intercept = student_t(0.362, 0.173, df = 7),
                              chains = 3, iter = 3000)
Hypothesis.4.2.3.inform <- bayesian.summary(glm.out.4.2.3.inform)
#launch_shinystan(glm.out.4.2.3.inform)

#Hypothesis 4.2.4	Cooperators have more connections than defectors in rapidly updating strategic networks
#Rand  et al. (2011) coeff = 0.052, p = .021

exp1_cooperation.fluid<-subset(exp1_cooperation, condition=="Fluid")
glm.out.4.2.4.default=stan_glm(action~num_neighbors, 
                       data=exp1_cooperation.fluid,
                       family = binomial(link = "logit"), 
                       chains = 3, iter = 3000)
Hypothesis.4.2.4.default <- bayesian.summary(glm.out.4.2.4.default)
#launch_shinystan(glm.out.4.2.4.default)

glm.out.4.2.4.inform=stan_glm(action~num_neighbors, 
                              data=exp1_cooperation.fluid,
                              family = binomial(link = "logit"), 
                              prior = student_t(0.053, 0.023, df = 7),
                              prior_intercept = student_t(-0.017, 0.265, df = 7),
                              chains = 3, iter = 3000)
Hypothesis.4.2.4.inform <- bayesian.summary(glm.out.4.2.4.inform)
#launch_shinystan(glm.out.4.2.4.inform)
