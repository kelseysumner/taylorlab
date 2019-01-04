# ----------------------------------------- #
#        Spat21 Data Set Merging            #
#              Mosquito Data                #
#             January 4, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


#### --------- read in mosquito data ----------------- ####

# read in the anopheles mosquito data sets
# first the cleaned descriptive data set
anoph_descriptive_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/spat21_mosquito_anopheles_descriptive_long_data_4JAN2019_v2.RDS")
# next the cleaned qpcr data set
anoph_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/spat21_mosquito_qpcr_data_wide_3JAN2019.RDS")

# look at the new data sets
summary(anoph_descriptive_data)
summary(anoph_qpcr_data)


#### ------- merge the anopheles mosquito data together by sample_id_mosquito ---------- ####

# first make sure all the sample_id_mosquito variables in both data set sare in character format
anoph_descriptive_data$sample_id_mosquito = as.character(anoph_descriptive_data$sample_id_mosquito)
anoph_qpcr_data$sample_id_mosquito = as.character(anoph_qpcr_data$sample_id_mosquito)

# left join the qpcr data with the descriptive data
anoph_merged_data = left_join(anoph_descriptive_data, anoph_qpcr_data, by = "sample_id_mosquito")

# check the merge
summary(anoph_merged_data)
summary(anoph_descriptive_data)
summary(anoph_qpcr_data)

# looks like a few IDs did not merge correctly
# check which IDs did not merge
unmerged_ids_descriptive = anoph_descriptive_data[-which(anoph_merged_data$sample_id_mosquito %in% anoph_descriptive_data$sample_id_mosquito),]

# all the descriptive data IDs merged correctly
unmerged_ids_qpcr = anoph_qpcr_data[-which(anoph_qpcr_data$sample_id_mosquito %in% anoph_merged_data$sample_id_mosquito),]
# 3 mosquito sample IDs didn't merge from the qpcr data

# create tabulations of the mosquito data
table(anoph_merged_data$abdominal_status, anoph_merged_data$village, useNA = "always")
table(anoph_merged_data$species_type, anoph_merged_data$village, useNA = "always")
table(anoph_merged_data$village, useNA = "always")
average_mosquitoes = anoph_merged_data %>%
  group_by(village, HH_ID) %>%
  summarize(total_n = sum(total_num_mosq_in_hh, na.rm = T)) %>%
  group_by(village) %>%
  summarize(avg_hh = mean(total_n, na.rm = T), lower_bound = min(total_n), upper_bound = max(total_n))
summary(anoph_merged_data$total_num_mosq_in_hh)




