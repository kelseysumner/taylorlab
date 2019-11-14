# ----------------------------------------- #
#     Create aim 2 multi-level models       #
#             Mozzie Phase 1                #
#                AMA data                   #
#           November 14, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(lme4)
library(MASS)
library(ggplot2)
library(dplyr)


#### ---------- read in the data sets ---------- ####

# read in the merged ama abdomen edgelist ready for the multilevel models
ama_abdomens = read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_ama_edgelist_abdomen_29OCT2019.rds")

# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
ama_haplotypes <- read_csv("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.csv")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")


#### ----- merge moi into the data set ------ ####

# subset the csp_haplotpes data set to sample id and moi
ama_haplotypes = ama_haplotypes %>% 
  select(sample_name_dbs,haplotype_number) %>%
  rename("sample_id_human" = "sample_name_dbs","moi"="haplotype_number")

# merge the csp_abdomens and csp_haplotypes data sets to get moi
ama_abdomens = left_join(ama_abdomens,ama_haplotypes,by="sample_id_human")
length(which(is.na(ama_abdomens$moi)))
str(ama_abdomens$moi)


#### --------- set up the data ---------- ####

# look at the column names
colnames(ama_abdomens)

# make sure the exposure and outcome are coded correctly
str(ama_abdomens$aim2_exposure)
ama_abdomens$aim2_exposure = as.factor(ama_abdomens$aim2_exposure)
str(ama_abdomens$haps_shared)
str(ama_abdomens$village_name)
ama_abdomens$village_name = as.factor(ama_abdomens$village_name)
str(ama_abdomens$total_num_mosq_in_hh)
str(ama_abdomens$age_cat_baseline)
ama_abdomens$age_cat_baseline = as.factor(ama_abdomens$age_cat_baseline)
ama_abdomens$aim2_exposure = relevel(ama_abdomens$aim2_exposure,"symptomatic infection")
ama_abdomens$X1 <- NULL
table(ama_abdomens$village_name, useNA = "always")
# remove sitabicha
ama_abdomens = ama_abdomens %>%
  filter(village_name != "Sitabicha")
ama_abdomens$village_name = as.factor(ifelse(ama_abdomens$village_name == "Kinesamo","Kinesamo","Sitabicha"))
table(ama_abdomens$village_name)
str(ama_abdomens$village_name)
# make pfr364Q_std_combined_cat
ama_abdomens$pfr364Q_std_combined_cat = ifelse(ama_abdomens$pfr364Q_std_combined<100,"under_rdt_detection",
                                               "over_rdt_detection")
table(ama_abdomens$pfr364Q_std_combined_cat,ama_abdomens$pfr364Q_std_combined, useNA = "always")
ama_abdomens$pfr364Q_std_combined_cat = as.factor(ama_abdomens$pfr364Q_std_combined_cat)



#### ----- work with the regular models ------ ####

# create a regular poisson model
regular_poisson = glm(haps_shared ~ aim2_exposure + age_cat_baseline + moi, family=c("poisson"), data=ama_abdomens)
summary(regular_poisson)

# test if data is overdisperse by running a quasipoisson model and seeing if results differ
quasi_poisson = glm(haps_shared ~ aim2_exposure + age_cat_baseline + moi, family=c("quasipoisson"), data=ama_abdomens)
summary(quasi_poisson)
# standard errors went down a little bit so could have mild overdispersion

# try a negative binomial model that accounts for overdispersion
neg_binomial = glm.nb(haps_shared ~ aim2_exposure + age_cat_baseline + moi, data=ama_abdomens)
summary(neg_binomial)
# test if good model fit
1-pchisq(deviance(neg_binomial),df.residual(neg_binomial)) # p>0.05 so good model fit
# looks like negative binomial not really better because has slightly higher AIC


#### ------ make the multi-level models ------- ####

# make a random-intercept poisson model with covariates but use glmer function instead
model1 <- glmer(haps_shared~aim2_exposure+moi+age_cat_baseline+aim2_exposure*age_cat_baseline+village_name + (1|HH_ID/unq_memID),family=poisson, data = ama_abdomens)
summary(model1)
1-pchisq(deviance(model1),df.residual(model1)) # p > 0.05 so good model fit
# looks like no interaction between main exposure and age

# make a random-intercept poisson model with covariates but remove interaction term
model2 <- glmer(haps_shared~aim2_exposure+moi+age_cat_baseline+village_name + (1|HH_ID/unq_memID),family=poisson, data = ama_abdomens)
summary(model2)
1-pchisq(deviance(model2),df.residual(model2)) # p > 0.05 so good model fit
# still singular

# make a random-intercept poisson model with covariates but remove village_name
model3 <- glmer(haps_shared~aim2_exposure+moi+age_cat_baseline + (1|HH_ID/unq_memID),family=poisson, data = ama_abdomens)
summary(model3)
1-pchisq(deviance(model3),df.residual(model3)) # p > 0.05 so good model fit
# still very singular
anova(model3,model4)

# make a random-intercept poisson model with covariates but remove age
model4 <- glmer(haps_shared~aim2_exposure+moi + (1|HH_ID/unq_memID),family=poisson, data = ama_abdomens)
summary(model4)
1-pchisq(deviance(model4),df.residual(model4)) # p > 0.05 so good model fit
anova(model3,model4)
# still very singular

# make a random-intercept poisson model with no covariates
model5 <- glmer(haps_shared~aim2_exposure + (1|HH_ID/unq_memID),family=poisson, data = ama_abdomens)
summary(model5)
1-pchisq(deviance(model5),df.residual(model5)) # p > 0.05 so good model fit
# still very singular
anova(model4,model5)
# model 4 better