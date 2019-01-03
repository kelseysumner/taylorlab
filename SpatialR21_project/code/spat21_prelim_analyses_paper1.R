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

# mosquito data sets
allspecies_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/raw data/MOZZIECollectionSummary_June2017_July2018.csv")
anopheles_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/raw data/MOZZIEFemaleAnophele_June2017_July2018.csv")
qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/spat21_mosquito_qpcr_data_22DEC2018.RDS")


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

# clean the anopheles data, switch from wide to long format
names(anopheles_data)
anopheles_data_long = gather_(data = anopheles_data, key_col = "Collection Number", value_col = "sample_ID_head", c("Sample ID Head #1","Sample ID Head #2","Sample ID Head #3","Sample ID Head #4","Sample ID Head #5","Sample ID Head #6","Sample ID Head #7","Sample ID Head #8","Sample ID Head #9","Sample ID Head #10","Sample ID Head #11","Sample ID Head #12","Sample ID Head #13","Sample ID Head #14","Sample ID Head #15","Sample ID Head #16"))

# take out the variables that aren't needed

# another way to try to switch from wide to long format
new_data = short_data %>%
  gather(key="collection_number", value="sample_ID_head", -"Collection Date",-"Household ID",-"Repeat Instance") %>%
  unite(col, key, times)





# summarize variables across villages
village_summary_mos = anopheles_data %>%
  group_by(Village) %>%
  summarize(total_female_anophs = sum(`Total Number of mosquitos in the household`, na.rm=T),
            avg_per_hh = mean(total_female_anophs, na.rm=T))

# summarize qPCR data
table(qpcr_data$pf_pcr_infection_status_sample_level, useNA = "always")
table(qpcr_data$hb_status_sample_level, useNA = "always")

