# -------------------------------- #
#          Bayesian model          #
#         Mozzie phase 1           #
#             Aim 1B               #
#          June 5, 2020            #
#           K. Sumner              #
# -------------------------------- #

# will call repeated infections: "reinfection"

#### ------- load libraries -------- ####
library(R2OpenBUGS)
library(rjags)
library(coda)
library(MCMCvis)



#### ------ tutorial: logit model for rjags ----- ####

#  http://stephenrho.github.io/rjags-model.html

### make data
nTrials = 12
nSubs = 30

fake <- expand.grid(trial = 1:nTrials, ppt = 1:nSubs, J = c('J1', 'J2'), K = c('K1', 'K2'), stringsAsFactors = T)

B0 <- 1.5 # grand mean
B1 <- .5 # main effect of J
B2 <- -.3 # main effect of K
s <- rnorm(nSubs, 0, 1)

# there are two true main effects and no interaction
logitSuccess <- with(fake, B0 + c(B1, -B1)[J] + c(B2, -B2)[K] + s[ppt])

logistic <- function(x){
  1/(1 + exp(-x))
}

fake$y <- rbinom(n = length(logitSuccess), size = 1, prob = logistic(logitSuccess))

with(aggregate(y ~ J + K, data = fake, FUN = mean), barplot(y, names.arg = paste(J,K, sep = '-')))

# rename fake data
dataf= fake


### set up the model
modelString = "
  model {
    for (i in 1:n){
      y[i] ~ dbern(y.hat[i])
      y.hat[i] <- max(0, min(1,P[i]))
      logit(P[i]) <- B0 + inprod(B, X[i,]) + s[id[i]]
    }
    # grand mean
    B0 ~ dt(0, 1/2.5^2, 1)
    # deflections from grand mean (fixed effects)
    for (b in 1:nEff){
      B[b] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008)
    }
    # participant random effect
    for (ppt in 1:S){
      s[ppt] ~ dnorm(0, sTau)
    }
    sTau <- 1/pow(sSD, 2)
    sSD ~ dgamma(1.01005, 0.1005012) # mode = .1, SD = 10 (v. vague)
  }
"


### set up the parameters
params = c('B0', 'B', 's', 'sSD')


### now run the model

# here we use sum-to-zero effects coding
options(contrasts=c('contr.sum', 'contr.sum'))

contrasts(dataf$J)
contrasts(dataf$K)

# design matrix
X <- model.matrix(~ J*K, data = dataf)[,2:4] # don't include intercept (dealt with via B0 parameter)

# put data in a list
datal <- list(
  y = dataf$y,
  n = nrow(dataf),
  X = X,
  nEff = ncol(X),
  id = dataf$ppt,
  S = length(unique(dataf$ppt))
)

# settings
nAdapt = 1000
nBurn = 1000
nChains = 4 # as JAGS has 4 random number generators
nSave = 10^4 # may want to increase
nThin = 1
nIter = ceiling((nSave*nThin)/nChains)

# create JAGS model
mod = jags.model(textConnection(modelString), data = datal, n.chains = nChains, n.adapt = nAdapt)

# burn in
update(mod, n.iter = nBurn)

# MCMC samples
samp = coda.samples(mod, variable.names=params, n.iter=nIter, thin=nThin)

# check posterior
gelman.diag(samp)
effectiveSize(samp) # ideally should be > 10000 (see Kruschke, 2015)

# summary of posterior
summary(samp)

# extract to matrix
mcmc <- as.matrix(samp, chains = T)
