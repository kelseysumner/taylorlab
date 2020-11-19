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

#### FINISH BY MAKING REST OF VARIABLES FACTORS AND IN CORRECT FORMAT


#### ----- now export the data sets ------ ####



