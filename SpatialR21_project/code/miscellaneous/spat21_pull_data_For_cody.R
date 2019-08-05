# ----------------------------------------- #
#       Spat21 Pull Samples for Cody        #
#              July 30, 2019                #
#                K. Sumner                  #
# ----------------------------------------- #


#### --------- load packages ----------------- ####
library(tidyverse)


#### -------- read in the data sets ----------- ####

# read in the mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the human data set
human_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_16JUL2019.rds")



#### ----- pull out summary statistics for the data ------- ####

# calculate how many mosquitoes are Pf negative but Hb positive in the abdomen and have a 
# head that is Pf positive 
# note: we will not have extracts for the head left
length(which(anoph_merged_data$pf_pcr_infection_status_sample_level_a == "negative" & 
               anoph_merged_data$hb_status_sample_level_a == "positive" &
               anoph_merged_data$pf_pcr_infection_status_sample_level_h == "positive"))
# 23 available
met_criteria_mosq_data = anoph_merged_data %>%
  filter(anoph_merged_data$pf_pcr_infection_status_sample_level_a == "negative" & 
           anoph_merged_data$hb_status_sample_level_a == "positive" &
           anoph_merged_data$pf_pcr_infection_status_sample_level_h == "positive")

# look at what households these 23 mosquitoes were found within
table(met_criteria_mosq_data$HH_ID, useNA="always")
length(unique(met_criteria_mosq_data$HH_ID, useNA="always"))

# look at the month and year these 23 mosquites were collected
table(met_criteria_mosq_data$collection_month_year_combo, useNA="always")

# match these mosquitoes that fit the criteria to the human samples that are Pf positive in that household during the same month
merged_human_data = human_data %>%
  filter(human_data$pf_pcr_infection_status == "positive" &
           human_data$HH_ID %in% anoph_merged_data$HH_ID &
           human_data$month_year_combo_monthly_data %in% anoph_merged_data$collection_month_year_combo)







