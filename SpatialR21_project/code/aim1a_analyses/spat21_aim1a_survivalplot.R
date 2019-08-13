# -------------------------------------- #
#           Spat21/Mozzie Study          #
# Make plot of survival for participants #
#                 Aim 1A                 #
#               Human Data               #
#                K. Sumner               #
#             July 16, 2019              #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)


#### ------- read in the data sets -------- ####

# read in the full data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_12AUG2019.rds")

# read in the consecutive monthly follow-up data set
followup_data = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/Consecutive Follow-up Tables/aim1a_consecutive_follow_up_order_df_after_censoring.csv")


#### ------- set up the data sets to be in survival format -------- ####








