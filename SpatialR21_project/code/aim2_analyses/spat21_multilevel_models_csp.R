# ----------------------------------------- #
#     Create aim 2 multi-level models       #
#             Mozzie Phase 1                #
#                CSP data                   #
#            October 14, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(lme4)



#### ---------- read in the data sets ---------- ####

# read in the merged csp abdomen edgelist
csp_abdomens = read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_edgelist_abdomen_08OCT2019.rds")

# read in the merged csp head edgelist
csp_heads = read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_edgelist_head_08OCT2019.rds")



#### --------- run multi-level model for csp_abdomens ---------- ####

# look at the column names
colnames(csp_abdomens)

# make sure the exposure and outcome are coded correctly
str(csp_abdomens$aim2_exposure)
csp_abdomens$aim2_exposure = as.factor(csp_abdomens$aim2_exposure)
str(csp_abdomens$haps_shared)
str(csp_abdomens$village_name)
csp_abdomens$village_name = as.factor(csp_abdomens$village_name)
str(csp_abdomens$total_num_mosq_in_hh)
str(csp_abdomens$age_cat_baseline)
csp_abdomens$age_cat_baseline = as.factor(csp_abdomens$age_cat_baseline)

# first start out with a null model
# now fit the model
null_model <- lmer(haps_shared~1 +(1|HH_ID/sample_id_human), data = csp_abdomens)
summary(null_model)

# then fit a random-intercept model with no covariates (just exposure and outcome)
random_intercept_model <- lmer(haps_shared~aim2_exposure +(1|HH_ID/sample_id_human), data = csp_abdomens)
summary(random_intercept_model)

# then fit a random-intercept model with covariates 
random_intercept_model_covariates <- lmer(haps_shared~aim2_exposure+total_num_mosq_in_hh+village_name +(1|HH_ID/sample_id_human), data = csp_abdomens)
summary(random_intercept_model_covariates)






