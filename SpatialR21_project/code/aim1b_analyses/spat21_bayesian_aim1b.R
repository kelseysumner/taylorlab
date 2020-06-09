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

# example code
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