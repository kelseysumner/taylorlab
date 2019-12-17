# -------------------------------------- #
#           Spat21/Mozzie Study          #
#       Build computational model        #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#          December 3, 2019              #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)
library(betareg)
library(lme4)



#### ----- read in the data sets ----- ####

# read in the csp data set for mosquito abdomens
edgelist_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/spat21_aim2_merged_data_with_weights_16DEC2019.rds")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")





### ------ check to see who had antimalarials recently ------- ####

# remove malaria infections that occurred within 14 days of symptomatic infection where received treatment in study

# tally up the people that had antimalarials at a monthly visit at least 14 days before their infection
had_antimalarials_recently = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" & mal_illness == "yes" & ill_med == "yes" &
           (medicine_ACT_hum_monthly_data == "yes" | medicine_Qui_hum_monthly_data == "yes" | medicine_SP_hum_monthly_data == "yes" | 
              medicine_OACT_hum_monthly_data == "yes" | medicine_AMO_hum_monthly_data == "yes" | medicine_SPT_hum_monthly_data == "yes" |
              medicine_CIP_hum_monthly_data == "yes" | medicine_PAN_hum_monthly_data == "yes" | medicine_DNT_hum_monthly_data == "yes" |
              medicine_OTH_hum_monthly_data == "yes")) %>%
  select(main_exposure_primary_case_def,med_date,recover,sample_id_date,ill_med,sample_name_dbs) %>%
  mutate(time_since_antimalarials_taken = sample_id_date - med_date) %>%
  filter((time_since_antimalarials_taken < 15 & time_since_antimalarials_taken >= 0) | is.na(time_since_antimalarials_taken)) %>%
  rename(sample_id_human = sample_name_dbs)

# see if any of these asymptomatic infections are in the paired data set
test = left_join(edgelist_data,had_antimalarials_recently,by="sample_id_human")
test = test %>%
  filter(ill_med=="yes")
# the self-reported use of antimalarials isn't that reputable

# let's see how many people had a positive RDT at a symptomatic visit and received an antimalarial within 14 days of the asymptomatic infection
# first reorder the data set
final_data = final_data[with(final_data, order(final_data$unq_memID, final_data$sample_id_date)),]
final_data %>%
  select(sample_name_final,sample_id_date,unq_memID) %>%
  View()

# check to see if there are any instances where you have two symptomatic infections within 14 days of each other
received_antimalarial = rep(NA,nrow(final_data))
date_diff = rep(NA,nrow(final_data))
for (i in 1:nrow(final_data)){
  if (final_data$unq_memID[i] == final_data$unq_memID[i+1] & final_data$prescription[i] == "prescribed" & !(is.na(final_data$prescription[i])) & i != nrow(final_data) &
      final_data$main_outcome_primary_case_def[i+1] == "symptomatic infection" & !(is.na(final_data$main_outcome_primary_case_def[i+1]))){
    date_diff[i] = final_data$sample_id_date[i+1] - final_data$sample_id_date[i]
    if (date_diff[i] < 15){
      received_antimalarial[i+1] = "yes"
    }
  } 
}
final_data$date_diff = date_diff
final_data$received_antimalarial = received_antimalarial
final_data %>%
  select(visit_type,sample_name_final,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,date_diff,received_antimalarial) %>%
  View()
final_data %>%
  select(visit_type,sample_name_final,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,date_diff,received_antimalarial) %>%
  filter(received_antimalarial == "yes") %>%
  View()
# found 1 sample that should be removed: K05-230518-4-R


# then do a for loop to see if any asymptomatic infections occurred within 14 days of a symptomatic infection where antimalarials prescribed
received_antimalarial = rep(NA,nrow(final_data))
date_diff = rep(NA,nrow(final_data))
for (i in 1:nrow(final_data)){
  if (final_data$unq_memID[i] == final_data$unq_memID[i+1] & final_data$prescription[i] == "prescribed" & !(is.na(final_data$prescription[i])) & i != nrow(final_data) &
      final_data$main_exposure_primary_case_def[i+1] == "asymptomatic infection" & !(is.na(final_data$main_exposure_primary_case_def[i+1]))){
    date_diff[i] = final_data$sample_id_date[i+1] - final_data$sample_id_date[i]
    if (date_diff[i] < 15){
      received_antimalarial[i+1] = "yes"
    }
  } 
}
final_data$date_diff = date_diff
final_data$received_antimalarial = received_antimalarial
final_data %>%
  select(visit_type,sample_name_final,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,date_diff,received_antimalarial) %>%
  View()
final_data %>%
  select(visit_type,sample_name_final,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,date_diff,received_antimalarial) %>%
  filter(received_antimalarial == "yes") %>%
  View()
# looks like there are 26 asymptomatic infections that should be censored
ppts_to_censor = final_data %>%
  select(visit_type,sample_name_final,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,date_diff,received_antimalarial) %>%
  filter(received_antimalarial == "yes")
test = final_data %>%
  select(age_all_baseline,unq_memID,sample_id_date,sample_name_final,pf_pcr_infection_status,rdt_rst,main_exposure_primary_case_def,main_outcome_primary_case_def,prescription,taken_al)

## final decision:
# Participants who had a symptomatic malaria infection and were prescribed antimalarials by the study team had asymptomatic
# infections that occurred within 14 days of the symptomatic infections removed from the data set. 

# censor these participants 
setdiff(ppts_to_censor$sample_name_final,edgelist_data$sample_name_final)
length(unique(edgelist_data$sample_name_final))
length(unique(edgelist_data$sample_id_human))
length(which(edgelist_data$sample_name_final %in% ppts_to_censor$sample_name_final))
edgelist_data = edgelist_data[-which(edgelist_data$sample_name_final %in% ppts_to_censor$sample_name_final),]
edgelist_data = edgelist_data[-which(edgelist_data$sample_name_final == "K05-230518-4-R"),]
length(unique(edgelist_data$sample_name_final))
length(unique(edgelist_data$sample_id_human))
# 6 weren't removed - 4 didn't pass genotyping and 2 were censored out in processing


#### --------- set up the data ---------- ####

# look at the column names
colnames(edgelist_data)

# make sure the exposure and outcome are coded correctly
str(edgelist_data$aim2_exposure)
edgelist_data$aim2_exposure = as.factor(edgelist_data$aim2_exposure)
str(edgelist_data$csp_haps_shared)
str(edgelist_data$ama_haps_shared)
str(edgelist_data$village_name)
edgelist_data$village_name = as.factor(edgelist_data$village_name)
str(edgelist_data$total_num_mosq_in_hh)
str(edgelist_data$age_cat_baseline)
edgelist_data$age_cat_baseline = as.factor(edgelist_data$age_cat_baseline)
str(edgelist_data$pfr364Q_std_combined)
edgelist_data$aim2_exposure = relevel(edgelist_data$aim2_exposure,"symptomatic infection")
str(edgelist_data$p_te_all)

# create a categorical variable for parasite density
length(which(is.na(edgelist_data$pfr364Q_std_combined))) # 0 missing
edgelist_data$pfr364Q_std_combined_cat = ifelse(edgelist_data$pfr364Q_std_combined < 100,"less than 100 p/uL","greater than or equal to 100 p/uL")
table(edgelist_data$pfr364Q_std_combined_cat,edgelist_data$pfr364Q_std_combined, useNA = "always")
table(edgelist_data$pfr364Q_std_combined_cat, useNA = "always")
edgelist_data$pfr364Q_std_combined_cat = as.factor(edgelist_data$pfr364Q_std_combined_cat)
edgelist_data$pfr364Q_std_combined_cat = relevel(edgelist_data$pfr364Q_std_combined_cat,ref="less than 100 p/uL")

# subset the data set to only look at observations where the probability of transmission > 0 (P(TEall) > 0)
edgelist_data = edgelist_data %>%
  filter(p_te_all > 0)
summary(edgelist_data$p_te_all)

# export the data set
# write_csv(edgelist_data,"Desktop/spat21_aim2_computational_model_subset_data_17DEC2019.csv")
# write_rds(edgelist_data,"Desktop/spat21_aim2_computational_model_subset_data_17DEC2019.rds")


#### ----- work with the regular models ------ ####

# look at distribution of p_te_all
hist(edgelist_data$p_te_all)
# looks very right-skewed
# could be a beta distribution
# options for models: 1. log binomial model or 2. beta model

# try a log-binomial model
regular_log_binomial = glm(p_te_all ~ aim2_exposure + age_cat_baseline + pfr364Q_std_combined_cat + village_name + aim2_exposure*age_cat_baseline, family=binomial(link="log"), data=edgelist_data)
summary(regular_log_binomial)

# try a logistic regression model
regular_logistic = glm(p_te_all ~ aim2_exposure + age_cat_baseline + pfr364Q_std_combined_cat + village_name + aim2_exposure*age_cat_baseline, family=binomial(link="logit"), data=edgelist_data)
summary(regular_logistic)

# try a poisson model
regular_poisson = glm(p_te_all ~ aim2_exposure + age_cat_baseline + pfr364Q_std_combined_cat + village_name + aim2_exposure*age_cat_baseline, family=c("poisson"), data=edgelist_data)
summary(regular_poisson)

# try a beta model
regular_beta = betareg(p_te_all ~ aim2_exposure + age_cat_baseline + pfr364Q_std_combined_cat + village_name + aim2_exposure*age_cat_baseline, data=edgelist_data)
summary(regular_beta)
# note: not an easy way to do multi-level beta models



#### ------- measure the outcome variable in different ways -------- ####

# look at a summary of the outcome variable
summary(edgelist_data$p_te_all)
length(which(is.na(edgelist_data$p_te_all)))

# make a binary variable for <0.05 or >= 0.05 
edgelist_data$outcome_binary_lessthan0.05 = ifelse(edgelist_data$p_te_all < 0.05,"less than 0.05","greater than 0.05")
table(edgelist_data$outcome_binary_lessthan0.05,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.05, useNA = "always")
edgelist_data$outcome_binary_lessthan0.05 = factor(edgelist_data$outcome_binary_lessthan0.05)
levels(edgelist_data$outcome_binary_lessthan0.05)
edgelist_data$outcome_binary_lessthan0.05 = relevel(edgelist_data$outcome_binary_lessthan0.05,ref = "less than 0.05")

# make a binary variable for <0.025 or >= 0.025
edgelist_data$outcome_binary_lessthan0.025 = ifelse(edgelist_data$p_te_all < 0.025,"less than 0.025","greater than 0.025")
table(edgelist_data$outcome_binary_lessthan0.025,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.025, useNA = "always")
edgelist_data$outcome_binary_lessthan0.025 = factor(edgelist_data$outcome_binary_lessthan0.025)
levels(edgelist_data$outcome_binary_lessthan0.025)
edgelist_data$outcome_binary_lessthan0.025 = relevel(edgelist_data$outcome_binary_lessthan0.025,ref = "less than 0.025")

# make a categorical variable for the outcome
edgelist_data$outcome_categorical = ifelse(edgelist_data$p_te_all < 0.02,"less than 0.02",
                                           ifelse(edgelist_data$p_te_all >= 0.02 & edgelist_data$p_te_all < 0.04,"0.02 to <0.04",
                                                  ifelse(edgelist_data$p_te_all >= 0.04 & edgelist_data$p_te_all < 0.06, "0.04 to <0.06",
                                                         ifelse(edgelist_data$p_te_all >= 0.06 & edgelist_data$p_te_all < 0.08, "0.06 to <0.08",
                                                                "0.08 to 0.10"))))
table(edgelist_data$outcome_categorical, useNA = "always")
edgelist_data$outcome_categorical = factor(edgelist_data$outcome_categorical)
levels(edgelist_data$outcome_categorical)
edgelist_data$outcome_categorical = relevel(edgelist_data$outcome_categorical,"less than 0.02")

# make a binary variable for <0.01 or >= 0.01
edgelist_data$outcome_binary_lessthan0.01 = ifelse(edgelist_data$p_te_all < 0.01,"less than 0.01","greater than 0.01")
table(edgelist_data$outcome_binary_lessthan0.01,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.01, useNA = "always")
edgelist_data$outcome_binary_lessthan0.01 = factor(edgelist_data$outcome_binary_lessthan0.01)
levels(edgelist_data$outcome_binary_lessthan0.01)
edgelist_data$outcome_binary_lessthan0.01 = relevel(edgelist_data$outcome_binary_lessthan0.01,ref = "less than 0.01")


# make a binary variable for <0.015 or >= 0.015
edgelist_data$outcome_binary_lessthan0.015 = ifelse(edgelist_data$p_te_all < 0.015,"less than 0.015","greater than 0.015")
table(edgelist_data$outcome_binary_lessthan0.015,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.015, useNA = "always")
edgelist_data$outcome_binary_lessthan0.015 = factor(edgelist_data$outcome_binary_lessthan0.015)
levels(edgelist_data$outcome_binary_lessthan0.015)
edgelist_data$outcome_binary_lessthan0.015 = relevel(edgelist_data$outcome_binary_lessthan0.015,ref = "less than 0.015")


# make a binary variable for <0.02 or >= 0.02
edgelist_data$outcome_binary_lessthan0.02 = ifelse(edgelist_data$p_te_all < 0.02,"less than 0.02","greater than 0.02")
table(edgelist_data$outcome_binary_lessthan0.02,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.02, useNA = "always")
edgelist_data$outcome_binary_lessthan0.02 = factor(edgelist_data$outcome_binary_lessthan0.02)
levels(edgelist_data$outcome_binary_lessthan0.02)
edgelist_data$outcome_binary_lessthan0.02 = relevel(edgelist_data$outcome_binary_lessthan0.02,ref = "less than 0.02")


# make a binary variable for <0.03 or >= 0.03
edgelist_data$outcome_binary_lessthan0.03 = ifelse(edgelist_data$p_te_all < 0.03,"less than 0.03","greater than 0.03")
table(edgelist_data$outcome_binary_lessthan0.03,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.03, useNA = "always")
edgelist_data$outcome_binary_lessthan0.03 = factor(edgelist_data$outcome_binary_lessthan0.03)
levels(edgelist_data$outcome_binary_lessthan0.03)
edgelist_data$outcome_binary_lessthan0.03 = relevel(edgelist_data$outcome_binary_lessthan0.03,ref = "less than 0.03")


# make a binary variable for <0.035 or >= 0.035
edgelist_data$outcome_binary_lessthan0.035 = ifelse(edgelist_data$p_te_all < 0.035,"less than 0.035","greater than 0.035")
table(edgelist_data$outcome_binary_lessthan0.03,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.03, useNA = "always")
edgelist_data$outcome_binary_lessthan0.03 = factor(edgelist_data$outcome_binary_lessthan0.03)
levels(edgelist_data$outcome_binary_lessthan0.03)
edgelist_data$outcome_binary_lessthan0.03 = relevel(edgelist_data$outcome_binary_lessthan0.03,ref = "less than 0.03")






#### -------- run some multi-level models that these different outcomes ------ ####

## first run all the models with the full set of covariates

# continuous proportion outcome with a logistic model
model1 <- glmer(p_te_all~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model1)
# did not work - singular fit

# binary outcome <0.05 with a logistic model
model.05 <- glmer(outcome_binary_lessthan0.05~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model.05)
# converged

# binary outcome <0.025 with a logistic model
model.025 <- glmer(outcome_binary_lessthan0.025~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model.025)
# converged

# categorical outcome with a logistic model
modelcat <- glmer(outcome_categorical~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(modelcat)
# converged


