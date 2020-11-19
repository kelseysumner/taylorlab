# -------------------------------------- #
#           Spat21/Mozzie Study          #
#       General follow-up descriptives   #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#           November 19, 2020            #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)
library(survminer)
library(survival)


#### ------- read in the data sets -------- ####

# read in the primary data set
survival_data_primary = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival_data_primary_final_data_19NOV2020.rds")

# read in the secondary stringent data set
survival_data_secondary_stringent = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival_data_secondary_stringent_final_data_19NOV2020.rds")

# read in the secondary permissive data set
survival_data_secondary_permissive = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival_data_secondary_permissive_final_data_19NOV2020.rds")



#### ------ look at basic descriptives of follow-up time ---------- ####

## ------ for survival data primary

# first just look at general summary of days until event
survival_data_primary$days_until_event = as.numeric(survival_data_primary$days_until_event)
summary(survival_data_primary$days_until_event)

# look at the distribution of follow-up times
follow_up_plot 


#### ---- look at some general survival curves ----- ####

# KM curve not stratified
ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ 1, data = survival_data_primary), 
           xlab = "Days", 
           ylab = "Overall survival probability")

