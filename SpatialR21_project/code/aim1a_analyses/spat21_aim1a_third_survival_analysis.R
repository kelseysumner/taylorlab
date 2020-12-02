# -------------------------------------- #
#           Spat21/Mozzie Study          #
#           Third survival analysis      #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#            December 1, 2020            #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)
library(survminer)
library(survival)
library(ggalluvial)
library(gridExtra)
library(coxme)


#### ------- read in the data sets -------- ####

# read in the primary data set
survival_data_primary = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_19NOV2020.rds")

# read in the secondary stringent data set
survival_data_secondary_stringent = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_stringent_survival_format_19NOV2020.rds")

# read in the secondary permissive data set
survival_data_secondary_permissive = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_permissive_survival_format_19NOV2020.rds")





#### ------- now subset the data set to remove all monthly visits up to 14 days before the symptomatic infection --------- ####

# remove monthly visits up to 14 days before the symptomatic infection
# primary data set
remove = rep(NA,nrow(survival_data_primary))
for (i in 1:nrow(survival_data_primary)){
  if (survival_data_primary$days_until_event[i] < 15 & survival_data_primary$event_indicator[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_primary$days_until_event < 15 & survival_data_primary$status == "symptomatic infection"))
survival_data_primary$remove = remove
survival_data_primary = survival_data_primary %>% filter(is.na(remove))
survival_data_primary$remove <- NULL


# remove monthly visits up to 14 days before the symptomatic infection
# secondary stringent data set
remove = rep(NA,nrow(survival_data_secondary_stringent))
for (i in 1:nrow(survival_data_secondary_stringent)){
  if (survival_data_secondary_stringent$days_until_event[i] < 15 & survival_data_secondary_stringent$event_indicator[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_secondary_stringent$days_until_event < 15 & survival_data_secondary_stringent$status == "symptomatic infection"))
survival_data_secondary_stringent$remove = remove
survival_data_secondary_stringent = survival_data_secondary_stringent %>% filter(is.na(remove))
survival_data_secondary_stringent$remove <- NULL


# remove monthly visits up to 14 days before the symptomatic infection
# secondary permissive data set
remove = rep(NA,nrow(survival_data_secondary_permissive))
for (i in 1:nrow(survival_data_secondary_permissive)){
  if (survival_data_secondary_permissive$days_until_event[i] < 15 & survival_data_secondary_permissive$event_indicator[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_secondary_permissive$days_until_event < 15 & survival_data_secondary_permissive$status == "symptomatic infection"))
survival_data_secondary_permissive$remove = remove
survival_data_secondary_permissive = survival_data_secondary_permissive %>% filter(is.na(remove))
survival_data_secondary_permissive$remove <- NULL





#### --------- now rerun the cox porportional hazards model ---------- ####

# run a model using the primary case definition
fit.coxph.primary <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                   data = survival_data_primary)
fit.coxph.primary
exp(confint(fit.coxph.primary))


# run a model using the secondary stringent case definition
fit.coxph.secondarystringent <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_stringent_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                   data = survival_data_secondary_stringent)
fit.coxph.secondarystringent
exp(confint(fit.coxph.secondarystringent))


# run a model using the secondary permissive case definition
fit.coxph.secondarypermissive <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                                      data = survival_data_secondary_permissive)
fit.coxph.secondarypermissive
exp(confint(fit.coxph.secondarypermissive))


