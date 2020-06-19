# -------------------------------- #
#          Bayesian model          #
#         Mozzie phase 1           #
#             Aim 1B               #
#          June 5, 2020            #
#           K. Sumner              #
# -------------------------------- #


#### ------- load libraries -------- ####
# load the library
library(rstanarm)
library(rstan)
library(tidyverse)


### ------ load in the data sets ------ ####

# for csp only right now
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/without_first_infection_csp_data_spat21_aim1b_11JUN2020.rds")



#### ------ set up the bayesian model ------- ####

# link to rstanarm tutorial: http://biostat.mc.vanderbilt.edu/wiki/pub/Main/StatisticalComputingSeries/bayes_reg_rstanarm.html

# this option uses multiple cores if they're available - can do but slows down my computer
# options(mc.cores = parallel::detectCores())

# set up the data
csp_data$symptomatic_status = ifelse(csp_data$symptomatic_status=="asymptomatic infection",0,1)

# set up the covariates
# number prior infections
csp_data$add_cat_number_prior_infections = ifelse(csp_data$number_prior_infections < 4,"3 infections or less","more than 3 infections")
table(csp_data$number_prior_infections,csp_data$add_cat_number_prior_infections)
csp_data$add_cat_number_prior_infections = as.factor(csp_data$add_cat_number_prior_infections)
# number mosquitoes collected
csp_data$mosquito_week_count_cat_add = ifelse(csp_data$mosquito_week_count <= 50,"50 or less mosquitoes","more than 50 mosquitoes")
table(csp_data$mosquito_week_count,csp_data$mosquito_week_count_cat_add)
csp_data$mosquito_week_count_cat_add = as.factor(csp_data$mosquito_week_count_cat_add)

# take out the infections with persistent haplotypes
no_persistent_data = csp_data[which(!(str_detect(csp_data$haplotype_category,"persistent"))),]
table(no_persistent_data$haplotype_category, useNA = "always")
no_persistent_data$haplotype_category = as.character(no_persistent_data$haplotype_category)
no_persistent_data$haplotype_category = as.factor(no_persistent_data$haplotype_category)
levels(no_persistent_data$haplotype_category)
no_persistent_data$haplotype_category = relevel(no_persistent_data$haplotype_category,ref="all recurrent")

# set up the priors
t_prior <- student_t(df = 7, location = 0, scale = 2.5)

# run the bayesian model
glm_pos1 = stan_glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + 
                      mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link="logit"),prior = t_prior,prior_intercept = t_prior,
                    data=no_persistent_data)
summary(glm_pos1)

# evaluate the model by making trace plots
stan_trace(glm_pos1)
stan_plot(glm_pos1)

# check the output
pp_check(glm_pos1)

# make a histogram
stan_hist(glm_pos1)
stan_dens(glm_pos1)

# check the priors
prior_summary(glm_pos1)

# juxtapose the posterior and prior distributions to see how the observed data has changed
posterior_vs_prior(glm_pos1, group_by_parameter = TRUE)

# you can compare models using an apporximation to leave-one-out (loo) cross-validation
# method for estimating out of sample predictive performance
# library(loo)
# compare_models(glm_pos1,glm_pos2)



