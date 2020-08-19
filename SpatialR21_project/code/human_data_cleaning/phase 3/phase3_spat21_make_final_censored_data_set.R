# ------------------------------------- #
#           Spat21/Mozzie Study         #
#    Make final censored data set       #
#               Human Data              #
#             Mozzie Phase 3            #
#                K. Sumner              #
#             August 18, 2020           #
# ------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)



#### -------- read in the data sets --------- ####

# read in the spat21 human merged data set with dbs censoring
final_merged_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/De-identified Phase II_v13/final_merged_data/phase3_spat21_human_merged_data_with_dbs_censoring_18AUG2020.rds")
length(unique(final_merged_data$unq_memID))
# note: 261 participants

# read in the old spat21 data set before merging
old_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/De-identified Phase II_v13/merged_data/phase3_spat21_human_merged_all_data_18AUG2020.rds")
length(unique(old_data$unq_memID))
# note: 268 participants



#### ------- make a table of consecutive follow-up ---------- ####


# remove the 7 observations that had invalid pcr data but merged in
final_merged_data = final_merged_data[-which(is.na(final_merged_data$pf_pcr_infection_status)),]
table(final_merged_data$pf_pcr_infection_status, useNA = "always")

# check for duplicate sample ids in the social demographic data sample ids
length(unique(final_merged_data$sample_name_final)) # 6009 unique 
length(which(is.na(final_merged_data$sample_name_final) == T)) # 0 missing
count_table = table(final_merged_data$sample_name_final, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table

# check for duplicate sample ids in the dbs sample ids
length(unique(final_merged_data$sample_name_dbs)) # 6009 unique unique 
length(which(is.na(final_merged_data$sample_name_dbs) == T)) # 0 missing
count_table = table(final_merged_data$sample_name_dbs, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplciates
length(dups_table)
dups_table

# table total monthly follow-up per participant
total_follow_up_df = final_merged_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  group_by(unq_memID) %>%
  summarise(total_follow_up = n())
# 261 ppts, that's correct

# check for those that had a total monthly follow-up <1 month
length(which(total_follow_up_df$total_follow_up < 2))
# 4 participants had <1 month follow-up after dbs merging didn't pass
# remove these two participants from the data set
ppts_to_remove = total_follow_up_df$unq_memID[which(total_follow_up_df$total_follow_up < 2)]
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID %in% ppts_to_remove),]

# now look at new table of total monthly follow-up per participant
total_follow_up_df = final_merged_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  group_by(unq_memID) %>%
  summarise(total_follow_up = n())
# now 257 ppts, that's correct

# remove all observations where <1 year old participant was <1 year old
# look at participants <1 year old
table(final_merged_data$age_all_baseline, useNA = "always")
lessthan1 = final_merged_data[which(final_merged_data$age_all_baseline == "1mos" | final_merged_data$age_all_baseline == "2mos" | final_merged_data$age_all_baseline == "8mos" | final_merged_data$age_all_baseline == "9mos"),]
table(lessthan1$unq_memID)
# looked at consecutive follow-up and pull out observations where baby still <1 year
# K04_5 - was 1 month and entered in 2017 but only 2018 merged so keep 2018 
lessthan1 %>% filter(unq_memID == "K04_5") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data)
old_data %>% filter(unq_memID == "K04_5") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data)
# K11_7 - was 1 month and entered in 2017 but only 2018 merged so keep 2018
lessthan1 %>% filter(unq_memID == "K11_7") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data)
old_data %>% filter(unq_memID == "K11_7") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data)
# M01_8 - 9 months at baseline, started follow-up in June 2017, Sept 2017 enters study
lessthan1 %>% filter(unq_memID == "M01_8") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data) %>% View()
old_data %>% filter(unq_memID == "M01_8") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data) %>% View()
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "M01_8" & final_merged_data$today_hum_monthly_data == "2017-08-17"),]
# M13_3 - was 1 month and entered in 2017 but only 2018 merged so keep 2018
lessthan1 %>% filter(unq_memID == "M13_3") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data) %>% View()
old_data %>% filter(unq_memID == "M13_3") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data) %>% View()
# S03_5 - was 8 months at enrollment and aged in July 2018 so remove earlier dates
lessthan1 %>% filter(unq_memID == "S03_5") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data) %>% View()
old_data %>% filter(unq_memID == "S03_5") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data) %>% View()
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "S03_5" & final_merged_data$today_hum_monthly_data == "2018-03-22"),]
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "S03_5" & final_merged_data$today_hum_monthly_data == "2018-04-19"),]
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "S03_5" & final_merged_data$today_hum_monthly_data == "2018-05-17"),]
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "S03_5" & final_merged_data$today_hum_monthly_data == "2018-06-21"),]
# S09_7 - was 8 months at enrollment, enters study October 2017 but only had sick visit that month so really follow-up starts Nov 2017
lessthan1 %>% filter(unq_memID == "S09_7") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data) %>% View()
old_data %>% filter(unq_memID == "S09_7") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data) %>% View()
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "S09_7" & final_merged_data$today_hum_sick_data == "2017-10-11"),]
# do a final check to see if everyone who remains had proper follow-up remaining
lessthan1 = final_merged_data[which(final_merged_data$age_all_baseline == "1mos" | final_merged_data$age_all_baseline == "2mos" | final_merged_data$age_all_baseline == "8mos" | final_merged_data$age_all_baseline == "9mos"),]
table(lessthan1$unq_memID)
# looks good, export data set

# change these participants' age to 1 year now
table(final_merged_data$age_all_baseline, useNA = "always")
final_merged_data$age_all_baseline[which(final_merged_data$age_all_baseline == "1mos" | final_merged_data$age_all_baseline == "2mos" | final_merged_data$age_all_baseline == "8mos" | final_merged_data$age_all_baseline == "9mos")] = 1
table(final_merged_data$age_all_baseline, useNA = "always")

# now make age_cat_baseline variable
colnames(final_merged_data)
final_merged_data$age_cat <- NULL
final_merged_data$age_all_baseline = as.numeric(final_merged_data$age_all_baseline)
# <5 years is 1, 5-15 years is 2, >15 years is 3
final_merged_data$age_cat_baseline = ifelse(final_merged_data$age_all_baseline < 5,1,
                                   ifelse(final_merged_data$age_all_baseline >= 5 & final_merged_data$age_all_baseline <= 15,2,
                                          ifelse(final_merged_data$age_all_baseline > 15,3,NA)))
table(final_merged_data$age_cat_baseline,final_merged_data$age_all_baseline, useNA = "always")
# tabulate how many participants in new categories
participant_data = final_merged_data %>%
  group_by(unq_memID,age_cat_baseline) %>%
  summarize(n=n()) %>%
  group_by(age_cat_baseline) %>%
  summarize(totaln = n())
# if same memID then make sure age_cat new is same for the one where missing (because age not collected at symptomatic visits)
new_df = final_merged_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  select(unq_memID,age_cat_baseline) %>%
  distinct
# see where differences
setdiff(unique(final_merged_data$unq_memID),new_df$unq_memID) # no differences
# make a factor
# <5 years is 1, 5-15 years is 2, >15 years is 3
final_merged_data$age_cat_baseline = factor(final_merged_data$age_cat_baseline, levels=c(1,2,3), labels=c("<5 years","5-15 years",">15 years"))
table(final_merged_data$age_cat_baseline, useNA = "always")
str(final_merged_data$age_cat_baseline)

# look at how many participants remain
# table total monthly follow-up per participant
total_follow_up_df = final_merged_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  group_by(unq_memID) %>%
  summarise(total_follow_up = n())
# 257 ppts, that's correct


# now order and tabulate the consecutive follow-up
consecutive_follow_up_ordered_df = final_merged_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  select(unq_memID,sample_id_date) %>%
  group_by(unq_memID) %>%
  mutate(id = paste0(as.character(lubridate::month(sample_id_date)),"-",as.character(lubridate::year(sample_id_date)))) %>%
  spread(key=id,value=sample_id_date)


# reorder consecutive follow-up columns
consecutive_follow_up_ordered_df <- consecutive_follow_up_ordered_df[,c("unq_memID","6-2017", "7-2017", "8-2017","9-2017","10-2017","11-2017","12-2017","1-2018","2-2018","3-2018","4-2018","5-2018","6-2018","7-2018","8-2018","9-2018","10-2018","11-2018","12-2018","1-2019","2-2019","3-2019","4-2019","5-2019","6-2019","7-2019","8-2019","9-2019","10-2019","11-2019")]


# export correct consecutive follow-up measures
write_csv(consecutive_follow_up_ordered_df,"Desktop/phase3_aim1a_consecutive_follow_up_order_df_after_censoring_18AUG2020.csv")


# write out the data frame with the final censoring criteria applied
write_csv(final_merged_data,"Desktop/phase3_spat21_human_final_censored_data_for_dissertation_18AUG2020.csv")
write_rds(final_merged_data,"Desktop/phase3_spat21_human_final_censored_data_for_dissertation_18AUG2020.rds")

