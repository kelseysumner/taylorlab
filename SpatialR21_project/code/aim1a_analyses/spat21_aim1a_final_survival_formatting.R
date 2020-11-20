# -------------------------------------- #
#           Spat21/Mozzie Study          #
# Put data set in final survival format  #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#           November 19, 2020            #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)



#### ------- read in the data sets -------- ####

# read in the primary data set
survival_data_primary = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival_data_primary_final_data_19NOV2020.rds")

# read in the secondary stringent data set
survival_data_secondary_stringent = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival_data_secondary_stringent_final_data_19NOV2020.rds")

# read in the secondary permissive data set
survival_data_secondary_permissive = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival_data_secondary_permissive_final_data_19NOV2020.rds")


#### ------- take out the end of follow-up entries (when days_until_event = 0) -------- ####

# for the primary data set
survival_data_primary = survival_data_primary %>% filter(days_until_event > 0)

# for the secondary stringent data set
survival_data_secondary_stringent = survival_data_secondary_stringent %>% filter(days_until_event > 0)

# for the secondary permissive data set
survival_data_secondary_permissive = survival_data_secondary_permissive %>% filter(days_until_event > 0)


#### -------- now recreate the event indicator ------- ####

# make symptomatic events = 1, all censored events (LTFU and study ended) = 0
# for the primary data set
survival_data_primary$event_indicator = ifelse(survival_data_primary$status=="symptomatic infection",1,0)
table(survival_data_primary$event_indicator,survival_data_primary$status,useNA="always")

# for the secondary stringent data set
survival_data_secondary_stringent$event_indicator = ifelse(survival_data_secondary_stringent$status=="symptomatic infection",1,0)
table(survival_data_secondary_stringent$event_indicator,survival_data_secondary_stringent$status,useNA="always")

# for the secondary permissive data set
survival_data_secondary_permissive$event_indicator = ifelse(survival_data_secondary_permissive$status=="symptomatic infection",1,0)
table(survival_data_secondary_permissive$event_indicator,survival_data_secondary_permissive$status,useNA="always")

# clean up the data set
# for the primary data set
survival_data_primary$end_type <- NULL
survival_data_primary$main_outcome_primary_case_def <- NULL
survival_data_primary$end_follow_up <- NULL
survival_data_primary$starter_infections <- NULL
# for the secondary stringent data set
survival_data_secondary_stringent$end_type <- NULL
survival_data_secondary_stringent$main_outcome_primary_case_def <- NULL
survival_data_secondary_stringent$end_follow_up <- NULL
survival_data_secondary_stringent$starter_infections <- NULL
# for the secondary permissive data set
survival_data_secondary_permissive$end_type <- NULL
survival_data_secondary_permissive$main_outcome_primary_case_def <- NULL
survival_data_secondary_permissive$end_follow_up <- NULL
survival_data_secondary_permissive$starter_infections <- NULL

# make the days until event variable numeric
survival_data_primary$days_until_event = as.numeric(survival_data_primary$days_until_event)
survival_data_secondary_stringent$days_until_event = as.numeric(survival_data_secondary_stringent$days_until_event)
survival_data_secondary_permissive$days_until_event = as.numeric(survival_data_secondary_permissive$days_until_event)

# make the age cat variable
# primary data
survival_data_primary$age_cat_baseline = ifelse(survival_data_primary$age_all_baseline < 5,"<5 years",
                                                ifelse(survival_data_primary$age_all_baseline >= 5 & survival_data_primary$age_all_baseline <= 15,"5-15 years",">15 years"))
table(survival_data_primary$age_all_baseline,survival_data_primary$age_cat_baseline,useNA="always")
survival_data_primary$age_cat_baseline = as.factor(survival_data_primary$age_cat_baseline)
levels(survival_data_primary$age_cat_baseline)
# secondary stringent data
survival_data_secondary_stringent$age_cat_baseline = ifelse(survival_data_secondary_stringent$age_all_baseline < 5,"<5 years",
                                                ifelse(survival_data_secondary_stringent$age_all_baseline >= 5 & survival_data_secondary_stringent$age_all_baseline <= 15,"5-15 years",">15 years"))
table(survival_data_secondary_stringent$age_all_baseline,survival_data_secondary_stringent$age_cat_baseline,useNA="always")
survival_data_secondary_stringent$age_cat_baseline = as.factor(survival_data_secondary_stringent$age_cat_baseline)
levels(survival_data_secondary_stringent$age_cat_baseline)
# secondary permissive data
survival_data_secondary_permissive$age_cat_baseline = ifelse(survival_data_secondary_permissive$age_all_baseline < 5,"<5 years",
                                                            ifelse(survival_data_secondary_permissive$age_all_baseline >= 5 & survival_data_secondary_permissive$age_all_baseline <= 15,"5-15 years",">15 years"))
table(survival_data_secondary_permissive$age_all_baseline,survival_data_secondary_permissive$age_cat_baseline,useNA="always")
survival_data_secondary_permissive$age_cat_baseline = as.factor(survival_data_secondary_permissive$age_cat_baseline)
levels(survival_data_secondary_permissive$age_cat_baseline)

# make village name a factor
# primary data
survival_data_primary$village_name = as.factor(survival_data_primary$village_name)
# secondary stringent
survival_data_secondary_stringent$village_name = as.factor(survival_data_secondary_stringent$village_name)
# secondary permissive
survival_data_secondary_permissive$village_name = as.factor(survival_data_secondary_permissive$village_name)

# make slept under net regularly a factor
# primary data
survival_data_primary$slept_under_net_regularly = as.factor(survival_data_primary$slept_under_net_regularly)
# secondary stringent
survival_data_secondary_stringent$slept_under_net_regularly = as.factor(survival_data_secondary_stringent$slept_under_net_regularly)
# secondary permissive
survival_data_secondary_permissive$slept_under_net_regularly = as.factor(survival_data_secondary_permissive$slept_under_net_regularly)

# make status a factor
# primary data
survival_data_primary$status = as.factor(survival_data_primary$status)
# secondary stringent
survival_data_secondary_stringent$status = as.factor(survival_data_secondary_stringent$status)
# secondary permissive
survival_data_secondary_permissive$status = as.factor(survival_data_secondary_permissive$status)

# make main exposure a factor
# primary data
survival_data_primary$main_exposure_primary_case_def = as.factor(survival_data_primary$main_exposure_primary_case_def)
survival_data_primary$main_exposure_primary_case_def = relevel(survival_data_primary$main_exposure_primary_case_def,ref="no infection")
# secondary stringent
survival_data_secondary_stringent$main_exposure_secondary_stringent_case_def = as.factor(survival_data_secondary_stringent$main_exposure_secondary_stringent_case_def)
survival_data_secondary_stringent$main_exposure_secondary_stringent_case_def = relevel(survival_data_secondary_stringent$main_exposure_secondary_stringent_case_def,ref="no infection")
# secondary permissive
survival_data_secondary_permissive$main_exposure_secondary_permissive_case_def = as.factor(survival_data_secondary_permissive$main_exposure_secondary_permissive_case_def)
survival_data_secondary_permissive$main_exposure_secondary_permissive_case_def = relevel(survival_data_secondary_permissive$main_exposure_secondary_permissive_case_def,ref="no infection")

# make gender a factor
# primary data
survival_data_primary$gender = as.factor(survival_data_primary$gender)
# secondary stringent
survival_data_secondary_stringent$gender = as.factor(survival_data_secondary_stringent$gender)
# secondary permissive
survival_data_secondary_permissive$gender = as.factor(survival_data_secondary_permissive$gender)



#### ----- now export the data sets ------ ####

# primary data
write_rds(survival_data_primary,"Desktop/survival_data_primary_survival_format_19NOV2020.rds")
write_csv(survival_data_primary,"Desktop/survival_data_primary_survival_format_19NOV2020.csv")
# secondary stringent
write_rds(survival_data_secondary_stringent,"Desktop/survival_data_secondary_stringent_survival_format_19NOV2020.rds")
write_csv(survival_data_secondary_stringent,"Desktop/survival_data_secondary_stringent_survival_format_19NOV2020.csv")
# secondary permissive
write_rds(survival_data_secondary_permissive,"Desktop/survival_data_secondary_permissive_survival_format_19NOV2020.rds")
write_csv(survival_data_secondary_permissive,"Desktop/survival_data_secondary_permissive_survival_format_19NOV2020.csv")
