#Created by Pablo Diego Rosell, PhD, for Gallup inc. in April 2018

# Small, medium, large per Cohen's (1987), and Polanin & Snilstveit (2016)

log.odds.small<-0.2/(sqrt(3)/pi)
log.odds.medium<-0.5/(sqrt(3)/pi)
log.odds.large<-0.8/(sqrt(3)/pi)

odds.small<-exp(log.odds.small)
odds.medium<-exp(log.odds.medium)
odds.large<-exp(log.odds.large)

prob.small<-1/(1+exp(-log.odds.small))
prob.medium<-1/(1+exp(-log.odds.medium))
prob.large<-1/(1+exp(-log.odds.large))

# Expected variable levels and effect sizes
# (expressed as probability of innovation for each level)

h1.1.nocomp<-0.5
h1.1.locomp<-1-prob.large
h1.1.medcomp<-prob.large
h1.1.hicomp<-1-prob.large

h1.3.posprime<-prob.large
h1.3.noprime<-0.5
h1.3.negprime<-1-prob.large

h2.1.uncert<-prob.small
h2.1.cert<-0.5

h2.2<-1-prob.large

h2.3<-prob.large

h2.4<-1-prob.large

h2.5<-1-prob.large

h2.6<-1-prob.large

h3.1.hitol<-prob.medium
h3.1.lotol<-0.5

h3.2.hitol<-prob.large
h3.2.lotol<-0.5

h3.3.losta.hileg<-0.4
h3.3.losta.loleg<-0.7
h3.3.hista.hileg<-0.6
h3.3.hista.loleg<-0.5

h3.4.transfor<-prob.large
h3.4.transact<-0.5

h3.5.nocom<-0.5
h3.5.com<-prob.medium
h3.5.comval<-prob.medium

# Transform to odds

eff.names<-ls(pattern="^h[[:digit:]]\\.[[:digit:]]")
eff.list<-mget(eff.names, envir = globalenv()) 

oddify<-function (effect.probs) {
  effect.odds<-effect.probs/(1-effect.probs)
  return (effect.odds)
  }

logoddify<-function (effect.probs) {
  effect.logodds<-log(effect.probs/(1-effect.probs))
  return (effect.logodds)
}

odds<-lapply(eff.list,oddify)
logodds<-lapply(eff.list,logoddify)

