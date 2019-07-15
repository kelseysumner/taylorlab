# ------------------------------------- #
#           Spat21/Mozzie Study         #
#    Make final censored data set       #
#               Human Data              #
#                K. Sumner              #
#             July 15, 2019             #
# ------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)



#### -------- read in the data sets --------- ####

# read in the spat21 human merged data set with dbs censoring
final_merged_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_merged_data_with_dbs_censoring_25JUN2019.rds")
length(unique(final_merged_data$unq_memID))
# note: 1 participant removed after dbs censoring



#### ------- make a table of consecutive follow-up ---------- ####

# table total monthly follow-up per participant
total_follow_up_df = final_merged_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  group_by(unq_memID) %>%
  summarise(total_follow_up = n())
# 242 ppts, that's correct

# check for those that had a total monthly follow-up <1 month
length(which(total_follow_up_df$total_follow_up < 2))
# 2 participants had <1 month follow-up after dbs merging didn't pass
# remove these two participants from the data set
ppts_to_remove = total_follow_up_df$unq_memID[which(total_follow_up_df$total_follow_up < 2)]
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID %in% ppts_to_remove),]

# now look at new table of total monthly follow-up per participant
total_follow_up_df = final_merged_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  group_by(unq_memID) %>%
  summarise(total_follow_up = n())
# now 240 ppts, that's correct

# remove all observations where <1 year old participant was <1 year old
# look at participants <1 year old
table(final_merged_data$age_all_baseline, useNA = "always")
lessthan1 = final_merged_data[which(final_merged_data$age_all_baseline == "1mos" | final_merged_data$age_all_baseline == "8mos" | final_merged_data$age_all_baseline == "9mos"),]
table(lessthan1$unq_memID)
# looked at consecutive follow-up and pull out observations where baby still <1 year
# K04_5 - was 1 month and only in for 2 months after merging so remove
lessthan1 %>% filter(unq_memID == "K04_5") %>% select(age_all_baseline,unq_memID)
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "K04_5"),]
# K11_7 - was 1 month and only in for 3 months after merging so remove
lessthan1 %>% filter(unq_memID == "K11_7") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data)
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "K11_7"),]
# M01_8 - 9 months at baseline, started follow-up in august 2017, nov 2017 enters study
lessthan1 %>% filter(unq_memID == "M01_8") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data)
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "M01_8" & final_merged_data$today_hum_monthly_data == "2017-10-12"),]
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "M01_8" & final_merged_data$today_hum_monthly_data == "2017-09-14"),]
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "M01_8" & final_merged_data$today_hum_monthly_data == "2017-08-17"),]
# M13_3 - was 1 month and only in for 2 months after merging so remove
lessthan1 %>% filter(unq_memID == "M13_3") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data)
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "M13_3"),]
# S03_5 - was 8 months at enrollment and had 5 months of follow-up so didn't age in until last month and then that's < 2 months follow-up so remove
lessthan1 %>% filter(unq_memID == "S03_5") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data)
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "S03_5"),]
# S09_7 - was 8 months at enrollment and had 9 months follow-up, enters study March 2018
lessthan1 %>% filter(unq_memID == "S09_7") %>% select(age_all_baseline,unq_memID,today_hum_monthly_data,today_hum_sick_data)
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "S09_7" & final_merged_data$today_hum_monthly_data == "2017-11-16"),]
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "S09_7" & final_merged_data$today_hum_monthly_data == "2017-12-21"),]
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "S09_7" & final_merged_data$today_hum_monthly_data == "2018-01-25"),]
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "S09_7" & final_merged_data$today_hum_monthly_data == "2018-02-15"),]
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID == "S09_7" & final_merged_data$today_hum_sick_data == "2017-10-11"),]
# do a final check to see if everyone who remains had proper follow-up remaining
lessthan1 = final_merged_data[which(final_merged_data$age_all_baseline == "1mos" | final_merged_data$age_all_baseline == "8mos" | final_merged_data$age_all_baseline == "9mos"),]
table(lessthan1$unq_memID)
# looks good, export data set


# look at how many participants remain
# table total monthly follow-up per participant
total_follow_up_df = final_merged_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  group_by(unq_memID) %>%
  summarise(total_follow_up = n())
# 236 ppts, that's correct



# write out the data frame with the final censoring criteria applied
write_csv(final_merged_data,"spat21_human_final_censored_data_for_dissertation_15JUL2019.csv")
write_rds(final_merged_data,"spat21_human_final_censored_data_for_dissertation_15JUL2019.rds")

