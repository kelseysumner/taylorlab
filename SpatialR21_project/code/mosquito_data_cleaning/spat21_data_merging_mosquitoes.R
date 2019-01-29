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
anoph_descriptive_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/spat21_mosquito_anopheles_descriptive_long_data_18JAN2019.RDS")
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
unmerged_ids_descriptive = anoph_merged_data[which(is.na(anoph_merged_data$pf_infection_status_mosquito_level)),]
  
# all the descriptive data IDs merged correctly
unmerged_ids_qpcr = anoph_qpcr_data[-which(anoph_qpcr_data$sample_id_mosquito %in% anoph_merged_data$sample_id_mosquito),]

# test out the ids with a right join
anoph_merged_data_right = right_join(anoph_descriptive_data, anoph_qpcr_data, by = "sample_id_mosquito")
unmerged_ids_qpcr_right = anoph_merged_data_right[which(is.na(anoph_merged_data_right$total_num_mosq_in_hh)),]
unmerged_ids_descriptive_right = anoph_descriptive_data[-which(anoph_descriptive_data$sample_id_mosquito %in% anoph_merged_data_right$sample_id_mosquito),]

# take out the the sample_id_head.x and sample_id_abdoomen.x columns
anoph_merged_data$sample_id_head.x <- NULL
anoph_merged_data$sample_id_abdomen.x <- NULL

# rename some of the variable names
anoph_merged_data = rename(anoph_merged_data, "sample_id_abdomen" = "sample_id_abdomen.y", "sample_id_head" = "sample_id_head.y")
colnames(anoph_merged_data)

# export the merged data for now
write_csv(anoph_merged_data, "spat21_mosquito_anopheles_merged_data_18JAN2019.csv")
write_rds(anoph_merged_data, "spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# create tabulations of the mosquito data
# create table 2
table(anoph_merged_data$abdominal_status, anoph_merged_data$village, useNA = "always")
table(anoph_merged_data$species_type, anoph_merged_data$village, useNA = "always")
table(anoph_merged_data$village, useNA = "always")
average_mosquitoes = anoph_merged_data %>%
  group_by(village,HH_ID,repeat_instance) %>%
  summarize(total_n = max(total_num_mosq_in_hh, na.rm = T)) %>%
  group_by(village,HH_ID) %>%
  summarize(total_n_hh = sum(total_n)) %>%
  group_by(village) %>%
  summarize(avg_hh = mean(total_n_hh, na.rm = T), lower_bound = min(total_n_hh), upper_bound = max(total_n_hh))
average_mosquitoes_total = anoph_merged_data %>%
  group_by(village,HH_ID,repeat_instance) %>%
  summarize(total_n = max(total_num_mosq_in_hh, na.rm = T)) %>%
  group_by(village,HH_ID) %>%
  summarize(total_n_hh = sum(total_n))
summary(average_mosquitoes_total)

# create table 3
table3 = table(anoph_merged_data$species_type, anoph_merged_data$abdominal_status, useNA = "always")
table3 = table3[,1:3]
table(anoph_merged_data$species_type, anoph_merged_data$hb_status_mosquito_level, useNA = "always")
table(anoph_merged_data$species_type, anoph_merged_data$pf_infection_status_mosquito_level, useNA = "always")
table(anoph_merged_data$species_type, anoph_merged_data$pf_pcr_infection_status_sample_level_h, useNA = "always")
table(anoph_merged_data$species_type, anoph_merged_data$pf_pcr_infection_status_sample_level_a, useNA = "always")



