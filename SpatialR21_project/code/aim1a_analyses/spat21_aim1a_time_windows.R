# -------------------------------------- #
#           Spat21/Mozzie Study          #
#           Look at time windows         #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#             February 9, 2021           #
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


# change the age category coding
# for primary data
table(survival_data_primary$age_cat_baseline)
survival_data_primary$new_age_cat_baseline = ifelse(survival_data_primary$age_cat_baseline== ">15 years",">15 years","15 years or less")
table(survival_data_primary$new_age_cat_baseline,useNA = "always")
survival_data_primary$new_age_cat_baseline = as.factor(survival_data_primary$new_age_cat_baseline)
survival_data_primary$new_age_cat_baseline = relevel(survival_data_primary$new_age_cat_baseline,ref="15 years or less")
# for secondary stringent data
table(survival_data_primary$age_cat_baseline)
survival_data_secondary_stringent$new_age_cat_baseline = ifelse(survival_data_secondary_stringent$age_cat_baseline== ">15 years",">15 years","15 years or less")
table(survival_data_secondary_stringent$new_age_cat_baseline,useNA = "always")
survival_data_secondary_stringent$new_age_cat_baseline = as.factor(survival_data_secondary_stringent$new_age_cat_baseline)
survival_data_secondary_stringent$new_age_cat_baseline = relevel(survival_data_secondary_stringent$new_age_cat_baseline,ref="15 years or less")
# for secondary permissive data
table(survival_data_secondary_permissive$age_cat_baseline)
survival_data_secondary_permissive$new_age_cat_baseline = ifelse(survival_data_secondary_permissive$age_cat_baseline== ">15 years",">15 years","15 years or less")
table(survival_data_secondary_permissive$new_age_cat_baseline,useNA = "always")
survival_data_secondary_permissive$new_age_cat_baseline = as.factor(survival_data_secondary_permissive$new_age_cat_baseline)
survival_data_secondary_permissive$new_age_cat_baseline = relevel(survival_data_secondary_permissive$new_age_cat_baseline,ref="15 years or less")




#### ------- recode the survival data primary to just look T when people first leave the study --------- ####

##  primary data set

# look when each person first lost to follow-up










