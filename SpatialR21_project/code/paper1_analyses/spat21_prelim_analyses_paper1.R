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

# read in the merged human monthly and table data set
human_monthly_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/hum_monthly_merged_with_table_data_4FEB2019.RDS")

# read in the merged anpopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the human sick data
human_sick_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_sick_data_19DEC2018.RDS")

# read in the preliminary qpcr data
human_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/spat21_qpcr_data_clean_human_dbs_16JAN2019.RDS")

# read in the data set (preliminary)
prelim_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/human_merged_all_data_final_6MAR2019.rds")



#### ----- decide who to exclude from longitudinal analyses ----- ####

# look at summaries of the people that will be excluded from longitudinal analyses

# create a household summary
household_summary = human_monthly_merged_data %>%
  group_by(HH_ID) %>%
  summarize(n_person_months = n(), n_households = n_distinct(HH_ID),
            n_participants = n_distinct(unq_memID))
# look at just a few households
m15 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="M15"),]
m16 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="M16"),]
k13 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="K13"),]
k14 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="K14"),]
s12 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="S12"),]
s13 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="S13"),]
# S13 and M16 entered study late
# look for how many people were <1
under1 = human_monthly_merged_data[which(!(is.na(human_monthly_merged_data$age_m))),]
length(unique(under1$unq_memID)) # 11
undernames = unique(under1$unq_memID)
# look for how many participants did not have monthly follow-up and weren't 1
nofolloup = human_monthly_merged_data[which(is.na(human_monthly_merged_data$gender_hum_monthly_data) & is.na(human_monthly_merged_data$age_m)),]
length(unique(nofolloup$unq_memID)) # 10
nofollowuptime = nofolloup$unq_memID
# look for how many participants had < 2 months follow-up
# create a new variable that has the village name for all data (not just those with monthly follow-up)
village_all_data = sapply(strsplit(human_monthly_merged_data$HH_ID,""),head,1)
table(village_all_data, useNA = "always")
# add variable to data set
human_monthly_merged_data$village_all_data = village_all_data
# calculate average person-months per participant
participant_data = human_monthly_merged_data %>%
  group_by(village_all_data,unq_memID) %>%
  summarize(n_count=n())
length(which(participant_data$n_count < 2))
lessthan2 = participant_data$unq_memID[which(participant_data$n_count < 2)]
# 25 participants had < 2 months follow-up
lessthan2
undernames
intersect(lessthan2,undernames)
length(intersect(lessthan2,undernames))
# 5 participants <1 and less than 2 months follow-up
lessthan2
nofollowuptime
intersect(lessthan2,nofollowuptime)
length(intersect(lessthan2,nofollowuptime))
# 10 participants declined monthly follow-up and had less than 2 months follow-up
# 25-15 = 10 participants had < 2 months follow-up
# 268 - 10 -5 - 10 = 237 participants in final data set

# now exclude those participants that fit those exclusion criteria
# create a new variable of unq_memIDs of everyone that fits exclusion criteria
firstcomparison = intersect(lessthan2,undernames)
exclude_ids_1 = union(firstcomparison,lessthan2)
exclude_ids_2 = union(exclude_ids_1,nofollowuptime)
length(exclude_ids_2)
length(which(human_monthly_merged_data$unq_memID %in% exclude_ids_2)) # 25 times
# remove all occurrences of those 25 unq_memIDs
test_data = human_monthly_merged_data[-which(human_monthly_merged_data$unq_memID %in% exclude_ids_2),]
# check the results
nrow(test_data)
nrow(human_monthly_merged_data)
2646-2621 # = 25
# looks good
human_monthly_merged_data = test_data


#### --------- create table 1 based on the cohort data ----------------- ####

# create a new variable that has the village name for all data (not just those with monthly follow-up)
village_all_data = sapply(strsplit(human_monthly_merged_data$HH_ID,""),head,1)
table(village_all_data, useNA = "always")
# add variable to data set
human_monthly_merged_data$village_all_data = village_all_data

# summarize variables across villages
village_summary = human_monthly_merged_data %>%
  group_by(village_all_data) %>%
  summarize(n_person_months = n(), n_households = n_distinct(HH_ID),
            n_participants = n_distinct(unq_memID))

# subset the data set for each village
k_data = human_monthly_merged_data[which(human_monthly_merged_data$village_all_data == "K"),]
m_data = human_monthly_merged_data[which(human_monthly_merged_data$village_all_data == "M"),]
s_data = human_monthly_merged_data[which(human_monthly_merged_data$village_all_data == "S"),]

# calculate the number of households
length(unique(k_data$HH_ID)) # 12
length(unique(m_data$HH_ID)) # 13
length(unique(s_data$HH_ID)) # 13

# calculate the number of participants
length(unique(k_data$unq_memID)) # 80
length(unique(m_data$unq_memID)) # 75
length(unique(s_data$unq_memID)) # 88

# calculate the average household size
householdsize_data = human_monthly_merged_data %>%
  group_by(village_all_data,HH_ID) %>%
  summarize(n=n_distinct(unq_memID)) %>%
  group_by(village_all_data) %>%
  summarize(avg_size = mean(n), sd_size = sd(n))
all_households_householdsize_data = human_monthly_merged_data %>%
  group_by(village_all_data,HH_ID) %>%
  summarize(n=n_distinct(unq_memID))
mean(all_households_householdsize_data$n)
sd(all_households_householdsize_data$n)

# look at gender at the participant level
# females
participant_data = human_monthly_merged_data %>%
  filter(gender_hum_monthly_data == "female") %>%
  group_by(village_all_data,unq_memID) %>%
  summarize(n=n()) %>%
  group_by(village_all_data) %>%
  summarize(totaln = n(), pct=totaln/136)
# males
participant_data = human_monthly_merged_data %>%
  filter(gender_hum_monthly_data == "male") %>%
  group_by(village_all_data,unq_memID) %>%
  summarize(n=n()) %>%
  group_by(village_all_data) %>%
  summarize(totaln = n(), pct=totaln/115)

# look at the different age categories
participant_data = human_monthly_merged_data %>%
  group_by(village_all_data,unq_memID,age_cat) %>%
  summarize(n=n()) %>%
  group_by(village_all_data, age_cat) %>%
  summarize(totaln = n())

# create a new age categories variable
# 1-5 years is 1, 6-15 years is 2, 16+ years is 3
human_monthly_merged_data$age_cat_new = ifelse(human_monthly_merged_data$age_y >= 1 & human_monthly_merged_data$age_y <= 5,1,
                                          ifelse(human_monthly_merged_data$age_y >= 6 & human_monthly_merged_data$age_y <= 15,2,
                                                 ifelse(human_monthly_merged_data$age_y >= 16,3,NA)))
table(human_monthly_merged_data$age_cat_new,human_monthly_merged_data$age_y)
# tabulate how many participants in new categories
participant_data = human_monthly_merged_data %>%
  group_by(unq_memID,age_cat_new) %>%
  summarize(n=n()) %>%
  group_by(age_cat_new) %>%
  summarize(totaln = n())

# sleeping in a space with a net regularly
# calculate how many people sleep under net regularly
participant_data = human_monthly_merged_data %>%
  group_by(village_all_data,unq_memID) %>%
  summarize(slept_avg=mean(slept_times, na.rm =T))
# make a variable that indicates some slept under a net more than usual
slept_under_net_regularly = ifelse(is.na(participant_data$slept_avg),NA,ifelse(participant_data$slept_avg>5,1,0))
table(slept_under_net_regularly,participant_data$slept_avg, useNA = "always")
participant_data$slept_under_net_regularly = slept_under_net_regularly
participant_data_v2 = participant_data %>%
  group_by(village_all_data, slept_under_net_regularly) %>%
  summarize(totaln = n())

# calculate average person-months per participant
participant_data = human_monthly_merged_data %>%
  group_by(village_all_data,unq_memID) %>%
  summarize(n_count=n()) %>%
  group_by(village_all_data) %>%
  summarize(avg_followup = mean(n_count, na.rm=T), sd_followup = sd(n_count, na.rm=T))
participant_data = human_monthly_merged_data %>%
  group_by(village_all_data,unq_memID) %>%
  summarize(n_count=n())
mean(participant_data$n_count, na.rm=T)
sd(participant_data$n_count, na.rm=T)

# calculate the proportion of participants with >= 11 months of follow-up
# kinesamo
participant_data_k = participant_data[which(participant_data$village_all_data == "K"),]
length(which(participant_data_k$n_count>=11))
nrow(participant_data_k)
# maruti
participant_data_m = participant_data[which(participant_data$village_all_data == "M"),]
length(which(participant_data_m$n_count>=11))
nrow(participant_data_m)
# sitabicha
participant_data_s = participant_data[which(participant_data$village_all_data == "S"),]
length(which(participant_data_s$n_count>=11))
nrow(participant_data_s)
# all
length(which(participant_data$n_count>=11))
nrow(participant_data)

# calculate the proportion of participants with <2 months of follow-up
# kinesamo
participant_data_k = participant_data[which(participant_data$village_all_data == "K"),]
length(which(participant_data_k$n_count<2))
nrow(participant_data_k)
# maruti
participant_data_m = participant_data[which(participant_data$village_all_data == "M"),]
length(which(participant_data_m$n_count<2))
nrow(participant_data_m)
# sitabicha
participant_data_s = participant_data[which(participant_data$village_all_data == "S"),]
length(which(participant_data_s$n_count<2))
nrow(participant_data_s)
# all
length(which(participant_data$n_count<2))
nrow(participant_data)


#### --------- create table 2 based on the mosquito data ----------------- ####

# create tabulations of the mosquito data
# create table 2
table1=table(anoph_merged_data$abdominal_status, anoph_merged_data$village, useNA = "always")
prop.table(table1,1)
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
# create summaries across all villages
table(anoph_merged_data$pf_infection_status_mosquito_level, useNA = "always")
table(anoph_merged_data$pf_pcr_infection_status_sample_level_h, useNA = "always")
table(anoph_merged_data$pf_pcr_infection_status_sample_level_a, useNA = "always")


#### --------- create summaries of the sick data ---------- ####

# look at an overall summary of the sick data
summary(human_sick_data)



#### ------- create summaries of the qpcr data ---------- ####

# look at a summary of the qpcr data
summary(human_qpcr_data)


#### ------ create summaries of qpcr and rdt data stratified by sex -------- #####

# subset the data set into males and females
females = prelim_data[which(prelim_data$gender_hum_monthly_data == "female" | prelim_data$gender_hum_sick_data == "female"),]
males = prelim_data[which(prelim_data$gender_hum_monthly_data == "male" | prelim_data$gender_hum_sick_data == "male"),]

# summarize qpcr data
summary(females$pf_pcr_infection_status)
summary(males$pf_pcr_infection_status)

# summarize rdt data
summary(females$rdt_rst)
summary(males$rdt_rst)

# subset the data set into age categories
# 1-5 years is 1, 6-15 years is 2, 16+ years is 3
prelim_data$age_cat_new = ifelse(prelim_data$age_y >= 1 & prelim_data$age_y <= 5 | !(is.na(prelim_data$age_m)),1,
                                               ifelse(prelim_data$age_y >= 6 & prelim_data$age_y <= 15,2,
                                                      ifelse(prelim_data$age_y >= 16,3,NA)))
table(prelim_data$age_cat_new)

# reorder preliminary data
prelim_data_test = prelim_data[order(prelim_data$unq_memID),]
head(prelim_data_test$unq_memID)
prelim_data = prelim_data_test

# look at the observations in preliminary data
# if same memID then make sure age_cat new is same for the one where missing (because age not collected at symptomatic visits)
new_df = prelim_data %>%
  filter(visit_type == "monthly visit") %>%
  select(unq_memID,age_cat_new) %>%
  distinct
# join in the new age categories
prelim_data_test = left_join(prelim_data,new_df,by="unq_memID")
colnames(prelim_data_test)
prelim_data = prelim_data_test
# join the age cat new columns
table(prelim_data$age_cat_new.x, useNA = "always")
table(prelim_data$age_cat_new.y, useNA = "always")
prelim_data$age_cat_new.x[prelim_data$visit_type == "sick visit"] = prelim_data$age_cat_new.y[prelim_data$visit_type == "sick visit"]
table(prelim_data$age_cat_new.x, useNA = "always")
table(prelim_data$age_cat_new.y, useNA = "always")
length(unique(prelim_data$unq_memID))

# summarize qpcr data
qpcr_age_summary = prelim_data %>%
  group_by(age_cat_new.x,pf_pcr_infection_status) %>%
  summarize(n=n())

# summarize rdt data
rdt_age_summary = prelim_data %>%
  group_by(age_cat_new.x,rdt_rst) %>%
  summarize(n=n())

# look at distribution of main exposure across age categories
table(prelim_data$main_exposure,prelim_data$age_cat_new.x, useNA = "always")
length(which(is.na(prelim_data$main_exposure) & prelim_data$age_cat_new.x == 1 & prelim_data$visit_type == "monthly visit"))
length(which(is.na(prelim_data$main_exposure) & prelim_data$age_cat_new.x == 2 & prelim_data$visit_type == "monthly visit"))
length(which(is.na(prelim_data$main_exposure) & prelim_data$age_cat_new.x == 3 & prelim_data$visit_type == "monthly visit"))

# look at the distribution of outcomes across age categories
table(prelim_data$outcome_case_definition_1,prelim_data$age_cat_new.x, useNA = "always")
table(prelim_data$outcome_case_definition_2,prelim_data$age_cat_new.x, useNA = "always")
table(prelim_data$outcome_case_definition_3,prelim_data$age_cat_new.x, useNA = "always")

# create a month variable for sick visit
sick_month = month(prelim_data$today_hum_sick_data)
tail(sick_month)
tail(prelim_data$today_hum_sick_data)
sick_year = year(prelim_data$today_hum_sick_data)
tail(sick_year)
tail(prelim_data$today_hum_sick_data)
# month year combo
sick_month_year_combo = paste0(as.character(sick_month),"-",as.character(sick_year))
tail(sick_month_year_combo)
# add to data set
prelim_data$sick_month_year_combo = sick_month_year_combo

# combine monthly and sick follow-up visit month and year combo
prelim_data$all_visit_month_year_combo = ifelse(!(is.na(prelim_data$month_year_combo_monthly_data)),prelim_data$month_year_combo_monthly_data,prelim_data$sick_month_year_combo)
table(prelim_data$all_visit_month_year_combo,prelim_data$month_year_combo_monthly_data)
table(prelim_data$all_visit_month_year_combo,prelim_data$sick_month_year_combo)

# look up which particpants didn't report their malaria to us
# how many sick visits
month_data_sick = prelim_data %>%
  filter(outcome_case_definition_1 == "symptomatic infection") %>%
  group_by(unq_memID,all_visit_month_year_combo) %>%
  summarize(num_sick_visits = n())
# how many times had malaria-like illness
month_data_noreport = prelim_data %>%
  filter(mal_illness == "yes") %>%
  group_by(unq_memID,all_visit_month_year_combo) %>%
  summarize(num_sick_visits = n())
# how many times had positive test for malaria in past month
month_data_postest = prelim_data %>%
  filter(test_result == "positive") %>%
  group_by(unq_memID,all_visit_month_year_combo) %>%
  summarize(num_sick_visits = n())
table(prelim_data$mal_illness, useNA="always")







