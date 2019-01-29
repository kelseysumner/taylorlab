# ----------------------------------------- #
#     Comparing Merged Mosquito Data        #
#            January 17, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)



#### ---------- read in the data sets ------------- ####

# now the merged data (my data set)
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in Sam's merged data
sam_merged_data = read_rds("/Users/kelseysumner/Desktop/Sam Update 17JAN2018/merged_mosquito_data.rds")


#### -------- look at differences -------- ####

# differences in total number of mosquitoes
# pull out the list of mosquitoes ids 
kels_mosq_ids = anoph_merged_data$sample_id_mosquito
sam_mosq_ids = sam_merged_data$sample.id
# see which ones are not in each other's list
not_in_sam = kels_mosq_ids[-(which(kels_mosq_ids %in% sam_mosq_ids))]
not_in_kels = sam_mosq_ids[-(which(sam_mosq_ids %in% kels_mosq_ids))]
# looks like the difference is M16 00035

# check table 2
# create table 2 for kelsey's data
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


# create table 2 for sam's data
table(sam_merged_data$abdominal.status, sam_merged_data$village, useNA = "always")
table(sam_merged_data$species.type, sam_merged_data$village, useNA = "always")
table(sam_merged_data$village, useNA = "always")
# create table 3
table3s = table(sam_merged_data$species.type, sam_merged_data$abdominal.status, useNA = "always")
table3s = table3s[,1:3]
table(anoph_merged_data$species_type, anoph_merged_data$hb_status_mosquito_level, useNA = "always")
table(anoph_merged_data$species_type, anoph_merged_data$pf_infection_status_mosquito_level, useNA = "always")
table(anoph_merged_data$species_type, anoph_merged_data$pf_pcr_infection_status_sample_level_h, useNA = "always")
table(anoph_merged_data$species_type, anoph_merged_data$pf_pcr_infection_status_sample_level_a, useNA = "always")










