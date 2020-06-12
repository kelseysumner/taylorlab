# -------------------------------- #
#          Bayesian model          #
#         Mozzie phase 1           #
#             Aim 1B               #
#          June 5, 2020            #
#           K. Sumner              #
# -------------------------------- #


#### ------- load libraries -------- ####
library(R2OpenBUGS)
library(rjags)
library(coda)
library(MCMCvis)
library(readr)


### ------ load in the data sets ------ ####

# for csp only right now
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/without_first_infection_csp_data_spat21_aim1b_11JUN2020.rds")


#### ------ now try the logit model for rjags ----- ####

#  http://stephenrho.github.io/rjags-model.html

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


### create a variable for whether or not the person had a new haplotype or not
csp_data$new_haplotype_present = ifelse(csp_data$count_new_haplotypes > 0,"yes","no")
table(csp_data$new_haplotype_present,csp_data$count_new_haplotypes,useNA = "always")
csp_data$new_haplotype_present = as.factor(csp_data$new_haplotype_present)

# make age count baseline a factor
csp_data$age_cat_baseline = as.factor(csp_data$age_cat_baseline)
levels(csp_data$age_cat_baseline)

# make number of prior infections a factor
csp_data$add_cat_number_prior_infections = ifelse(csp_data$number_prior_infections < 4,"3 infections or less","more than 3 infections")
table(csp_data$number_prior_infections,csp_data$add_cat_number_prior_infections)
csp_data$add_cat_number_prior_infections = as.factor(csp_data$add_cat_number_prior_infections)

# make mosquito week count a factor
csp_data$mosquito_week_count_cat_add = ifelse(csp_data$mosquito_week_count <= 50,"50 or less mosquitoes","more than 50 mosquitoes")
table(csp_data$mosquito_week_count,csp_data$mosquito_week_count_cat_add)
csp_data$mosquito_week_count_cat_add = as.factor(csp_data$mosquito_week_count_cat_add)

# here we use sum-to-zero effects coding to set up contrasts
options(contrasts=c('contr.sum', 'contr.sum'))
contrasts(csp_data$new_haplotype_present)
contrasts(csp_data$age_cat_baseline)
contrasts(csp_data$add_cat_number_prior_infections)
contrasts(csp_data$mosquito_week_count_cat_add)

### create the design matrix - take out age for now because three levels
X <- model.matrix(~new_haplotype_present*add_cat_number_prior_infections*mosquito_week_count_cat_add,data = csp_data)[,2:4] # don't include intercept (dealt with via B0 parameter)

# put data in a list
datal <- list(
  y = csp_data$symptomatic_status,
  n = nrow(csp_data),
  X = X,
  nEff = ncol(X),
  id = csp_data$unq_memID,
  S = length(unique(csp_data$unq_memID))
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








