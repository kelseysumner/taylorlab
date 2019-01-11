# ----------------------------------------- #
#  Spat21 Preliminary Analyses for Paper 1  #
#            December 19, 2018              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(tableone)


#### --------- read in the data sets ----------------- ####

# human data sets
hum_ann_household_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_ann_household_data_19DEC2018.RDS")
hum_monthly_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_monthly_data_19DEC2018.RDS")
hum_sick_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_sick_data_19DEC2018.RDS")
hum_sleeping_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_sleeping_data_19DEC2018.RDS")
hum_table_household_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_table_household_data_19DEC2018.RDS")

# read in the anopheles mosquito data sets
# first the cleaned descriptive data set
anoph_descriptive_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/spat21_mosquito_anopheles_descriptive_long_data_4JAN2019_v2.RDS")
# next the cleaned qpcr data set
anoph_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/spat21_mosquito_qpcr_data_wide_3JAN2019.RDS")


#### --------- create table 1 based on the cohort data ----------------- ####

##  start out using the hum_table_household_data

# for this data set, create a variable that is the age categories:
# 1-5 years is 1, 6-10 years is 2, 11-17 years is 3, 18 and above is 4
hum_table_household_data$age_cat = ifelse(hum_table_household_data$age_y >= 1 & hum_table_household_data$age_y <= 5,1,
                                          ifelse(hum_table_household_data$age_y >= 6 & hum_table_household_data$age_y <= 10,2,
                                                 ifelse(hum_table_household_data$age_y >= 11 & hum_table_household_data$age_y <= 17,3,
                                                        ifelse(hum_table_household_data$age_y >=18,4,NA))))
# make age_cat a factor
hum_table_household_data$age_cat = factor(hum_table_household_data$age_cat,levels = c(1,2,3,4), labels = c("1-5","6-10","11-17","18+"))
# check the variable coding
table(hum_table_household_data$age_cat,hum_table_household_data$age_y)

# check for duplicates and missingness in hum_monthly_data
hum_monthly_dups = duplicated(hum_monthly_data)
length(which(hum_monthly_dups == T))
count_table = table(hum_monthly_data$unq_memID, useNA = "always")
dups_table = count_table[which(count_table > 1)] # many instances of an ID in the data set

# make sure that the unq_memIDs are read as characters for both data sets before the merge
hum_monthly_data$unq_memID = as.character(hum_monthly_data$unq_memID)
hum_table_household_data$unq_memID = as.character(hum_table_household_data$unq_memID)

# remove unq_memID "S12_1" from hum_monthly_data because not in hum_table_household_data and no longer in study
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$unq_memID == "S12_1"),]

# merge the hum_monthly_data and hum_table_household_data data sets
hum_merged_data = full_join(hum_monthly_data,hum_table_household_data,by="unq_memID")
# check the merge
summary(hum_merged_data)
summary(hum_monthly_data)
summary(hum_table_household_data)
length(which(is.na(hum_merged_data$unq_memID)))
table(hum_merged_data$unq_memID)
table(hum_monthly_data$unq_memID)
table(hum_table_household_data$unq_memID)

# figure out what values are not merging properly
# create a function for not in
'%ni%' <- Negate('%in%')
different_unq_memIDs = hum_table_household_data$unq_memID[unique(hum_table_household_data$unq_memID) %ni% unique(hum_merged_data$unq_memID)]
same_unq_memIDs = hum_table_household_data$unq_memID[unique(hum_table_household_data$unq_memID) %in% unique(hum_merged_data$unq_memID)]

# calculate new village variable
# first split the Household ID variable to have the letters separate
split_HH = sapply(strsplit(hum_merged_data$HH_ID.y,""),head,1)
hum_merged_data$split_household_id = as.character(split_HH)

# summarize variables across villages
village_summary = hum_merged_data %>%
  group_by(split_household_id) %>%
  summarize(n_person_months = n(), n_households = n_distinct(HH_ID.x),
            n_participants = n_distinct(unq_memID))

# subset the data set for each village
k_data = hum_merged_data[which(hum_merged_data$split_household_id == "K"),]
m_data = hum_merged_data[which(hum_merged_data$split_household_id == "M"),]
s_data = hum_merged_data[which(hum_merged_data$split_household_id == "S"),]

# calculate the number of households
length(unique(k_data$HH_ID.x)) # 13
length(unique(m_data$HH_ID.x)) # 14
length(unique(s_data$HH_ID.x)) # 14

# calculate the number of participants
length(unique(k_data$unq_memID)) # 89
length(unique(m_data$unq_memID)) # 82
length(unique(s_data$unq_memID)) # 97
# check this with how many participants were in the hum_table_household_data
length(unique(hum_table_household_data$unq_memID)) # 268
length(unique(hum_monthly_data$unq_memID)) # 253

# calculate the average household size
householdsize_data = hum_table_household_data %>%
  group_by(HH_ID) %>%
  summarize(n=n())
householdsize_data$village = as.character(sapply(strsplit(householdsize_data$HH_ID,""),head,1))
k_data = householdsize_data[which(householdsize_data$village == "K"),]
m_data = householdsize_data[which(householdsize_data$village == "M"),]
s_data = householdsize_data[which(householdsize_data$village == "S"),]
mean(k_data$n)
sd(k_data$n)
mean(m_data$n)
mean(s_data$n)
mean(householdsize_data$n)

# look at gender
gendertable = table(hum_merged_data$gender_hum_monthly_data,hum_merged_data$split_household_id)
prop.table(gendertable,2)
# another way that's at the participant level
participant_data = hum_merged_data %>%
  group_by(unq_memID) %>%
  summarize(n=n(), n_gender = n_distinct(gender_hum_monthly_data))
# look at those where there are two gender entries
twoentries = participant_data$unq_memID[participant_data$n_gender>1]
twogenders = hum_merged_data[which(hum_merged_data$unq_memID %in% twoentries),]


#### --------- create table 2 based on the mosquito data ----------------- ####

# look at anopheles only data

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
unmerged_ids_descriptive = anoph_descriptive_data[-which(anoph_descriptive_data$sample_id_mosquito %in% anoph_merged_data$sample_id_mosquito),]

# all the descriptive data IDs merged correctly
unmerged_ids_qpcr = anoph_qpcr_data[-which(anoph_qpcr_data$sample_id_mosquito %in% anoph_merged_data$sample_id_mosquito),]
# 3 mosquito sample IDs didn't merge from the qpcr data

# export the merged data for now
# write_csv(anoph_merged_data, "spat21_mosquito_anopheles_merged_data_4JAN2019.csv")
# write_rds(anoph_merged_data, "spat21_mosquito_anopheles_merged_data_4JAN2019.RDS")

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


