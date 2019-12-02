# ----------------------------------------- #
#  Clean Final Merged Data Set for Spat21   #
#                Human Data                 #
#              Mozzie Phase 2               #
#             December 2, 2019              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(tableone)
library(stringr)


#### -------- read in the final merged data set ------------- ####

human_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/merged_data/final_merged_data/phase2_spat21_human_merged_all_data_2DEC2019.rds")



#### -------- look at the remaining variables and clean ------- ####

# look at the column names
colnames(human_merged_data)
# 139 variables in final data set

# make a data frame of the colnames
colname_df = data.frame(colnames(human_merged_data))
colname_df
# write_csv(colname_df,"colname_df.csv")

# look at each variable and make sure it's clean

# today_hum_monthly_data
summary(human_merged_data$today_hum_monthly_data)
str(human_merged_data$today_hum_monthly_data)

# village_name_hum_monthly_data
table(human_merged_data$village_name_hum_monthly_data, useNA = "always")
str(human_merged_data$village_name_hum_monthly_data)

# village_name_hum_monthly_data
table(human_merged_data$village_name_hum_monthly_data, useNA = "always")
str(human_merged_data$village_name_hum_monthly_data)
# village_all_data
table(human_merged_data$village_all_data, useNA = "always")
str(human_merged_data$village_all_data)
# village_name_hum_sick_data
table(human_merged_data$village_name_hum_sick_data, useNA = "always")
str(human_merged_data$village_name_hum_sick_data)
# remove village_all_data
human_merged_data$village_all_data <- NULL
# create a new village variable based on sample name final
length(which(is.na(human_merged_data$sample_name_final))) # no missing
village_name = rep(NA,nrow(human_merged_data))
for (i in 1:nrow(human_merged_data)){
  first_name = str_split(human_merged_data$sample_name_final[i],"-")[[1]]
  first_letter = str_split(first_name,"")[[1]][1]
  if (first_letter == "K"){
    village_name[i] = "Kinesamo"
  }
  if (first_letter == "M"){
    village_name[i] = "Maruti"
  }
  if (first_letter == "S"){
    village_name[i] = "Sitabicha"
  }
}
table(village_name,useNA="always")
# add village name to data set
human_merged_data$village_name = village_name
# remove other village variables
human_merged_data$village_name_hum_monthly_data <- NULL
human_merged_data$village_name_hum_sick_data <- NULL
# check output for village_name one more time
sum(str_detect(human_merged_data$sample_name_final,"K")) # 995
sum(str_detect(human_merged_data$sample_name_final,"M")) # 894
sum(str_detect(human_merged_data$sample_name_final,"S")) # 1030
# all looks good

# gender
table(human_merged_data$gender_hum_monthly_data, useNA="always")
table(human_merged_data$gender_hum_sick_data, useNA = "always")
# check if there's any case wehre male and female didn't match
gender_df = data.frame(monthly_gender=human_merged_data$gender_hum_monthly_data,sick_gender=human_merged_data$gender_hum_sick_data)
gender_df = na.omit(gender_df)
identical(gender_df$monthly_gender,gender_df$sick_gender)
# if sick data gender is any different, default to monthly data gender
gender = ifelse(!(is.na(human_merged_data$gender_hum_monthly_data)),human_merged_data$gender_hum_monthly_data,human_merged_data$gender_hum_sick_data)
table(gender,useNA = "always")
table(human_merged_data$gender_hum_monthly_data)
table(human_merged_data$gender_hum_sick_data)
# make gender variable a factor
human_merged_data$gender = factor(gender,level=c(1,2),labels=c("male","female"))
table(human_merged_data$gender, useNA = "always")
# remove the old gender variables
human_merged_data$gender_hum_monthly_data <- NULL
human_merged_data$gender_hum_sick_data <- NULL

# clean up ages
table(human_merged_data$age_cat, useNA = "always")
table(human_merged_data$age_y, useNA = "always")
table(human_merged_data$age_m, useNA = "always")
table(human_merged_data$age_type, useNA = "always")
# look for how many people were <1
under1 = human_merged_data[which(!(is.na(human_merged_data$age_m))),]
length(unique(under1$unq_memID)) # 6, which makes sense because removed 5 that had <2 months follow up and <1 earlier
undernames = unique(under1$unq_memID)
# K04_5, K11_7, M01_8, M13_3, S03_5, S09_7
table(human_merged_data$unq_memID, useNA = "always")
# look at K04_5
k04_5 = human_merged_data[which(human_merged_data$unq_memID == "K04_5"),]
# 1 month at enrollment, but aged in based on follow-up
# look at K11_7
k11_7 = human_merged_data[which(human_merged_data$unq_memID == "K11_7"),]
# 1 month at enrollment, but also aged in based on follow-up
# look at M01_8
m01_8 = human_merged_data[which(human_merged_data$unq_memID == "M01_8"),]
# 9 months at enrollment, very long follow-up and aged in
# M13_3
m13_3 = human_merged_data[which(human_merged_data$unq_memID == "M13_3"),]
# 1 month at enrollment but aged in
# S03_5
s03_5 = human_merged_data[which(human_merged_data$unq_memID == "S03_5"),]
# 8 months at enrollment, aged in
# S09_7
s09_7 = human_merged_data[which(human_merged_data$unq_memID == "S09_7"),]
# 8 months at enrollment, aged in
# create one variable that is age_cat and corresponds to the age category for each participant
# create a new age categories variable
# <5 years is 1, 5-15 years is 2, >15 years is 3
human_merged_data$age_cat = ifelse(!(is.na(human_merged_data$age_m)) | human_merged_data$age_y < 5,1,
                                               ifelse(human_merged_data$age_y >= 5 & human_merged_data$age_y <= 15,2,
                                                      ifelse(human_merged_data$age_y > 15,3,NA)))
table(human_merged_data$age_cat,human_merged_data$age_y, useNA = "always")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,age_cat) %>%
  summarize(n=n()) %>%
  group_by(age_cat) %>%
  summarize(totaln = n())
# now move over the age data to the sick data set
# if same memID then make sure age_cat new is same for the one where missing (because age not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(visit_type == "monthly visit") %>%
  select(unq_memID,age_cat) %>%
  distinct
# see where differences
setdiff(unique(human_merged_data$unq_memID),new_df$unq_memID) # S08_10
# see where different, had monthly and sick visits on the same day which caused the difference
# do a different filter
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$age_type))) %>%
  select(unq_memID,age_cat) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 243, correct
# join in the new age categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the age cat new columns
table(human_merged_data$age_cat.x, useNA = "always")
table(human_merged_data$age_cat.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$age_cat.x <- NULL
human_merged_data = rename(human_merged_data,"age_cat"="age_cat.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,age_cat) %>%
  summarize(n=n()) %>%
  group_by(age_cat) %>%
  summarize(totaln = n())
# looks good, clean

# make a universal today variable
colnames(human_merged_data)
length(which(is.na(human_merged_data$today_hum_monthly_data)))
length(which(is.na(human_merged_data$today_hum_sick_data)))
# for the days that have monthly and sick visit on same day, make monthly data the date
human_merged_data$universal_today = ifelse(!(is.na(human_merged_data$today_hum_monthly_data)),human_merged_data$today_hum_monthly_data,human_merged_data$today_hum_sick_data)
length(which(is.na(human_merged_data$universal_today)))

# slept_home
table(human_merged_data$slept_home, useNA = "always")
str(human_merged_data$slept_home)

# slept_usual
table(human_merged_data$slept_usual, useNA = "always")
str(human_merged_data$slept_usual)

# slept_net
table(human_merged_data$slept_net, useNA = "always")
str(human_merged_data$slept_net)

# time_under_net
table(human_merged_data$time_under_net, useNA = "always")
str(human_merged_data$time_under_net)

# time_out_net
table(human_merged_data$time_out_net, useNA = "always")
str(human_merged_data$time_out_net)

# slept_times
table(human_merged_data$slept_times, useNA = "always")
str(human_merged_data$slept_times)

# time_to_bed
table(human_merged_data$time_to_bed, useNA = "always")
str(human_merged_data$time_to_bed)

# to_bed_sp
table(human_merged_data$to_bed_sp, useNA = "always")
str(human_merged_data$to_bed_sp)

# to_bed_oth
table(human_merged_data$to_bed_oth, useNA = "always")
str(human_merged_data$to_bed_oth)

# travelled
table(human_merged_data$travelled, useNA = "always")
str(human_merged_data$travelled)

# to_where
table(human_merged_data$to_where, useNA = "always")
str(human_merged_data$to_where)

# nights
table(human_merged_data$nights, useNA = "always")
str(human_merged_data$nights)

# slp_travelled
table(human_merged_data$slp_travelled, useNA = "always")
str(human_merged_data$slp_travelled)

# blood_spot
table(human_merged_data$blood_spot, useNA = "always")
str(human_merged_data$blood_spot)

# mal_illness
table(human_merged_data$mal_illness, useNA = "always")
str(human_merged_data$mal_illness)

# act_illness
table(human_merged_data$act_illness, useNA = "always")
str(human_merged_data$act_illness)

# act_ill_other
table(human_merged_data$act_ill_other, useNA = "always")
str(human_merged_data$act_ill_other)

# blood_test
table(human_merged_data$blood_test, useNA = "always")
str(human_merged_data$blood_test)

# test_type
table(human_merged_data$test_type, useNA = "always")
str(human_merged_data$test_type)

# test_result
table(human_merged_data$test_result, useNA = "always")
str(human_merged_data$test_result)

# test_obs
table(human_merged_data$test_obs, useNA = "always")
str(human_merged_data$test_obs)

# ill_med
table(human_merged_data$ill_med, useNA = "always")
str(human_merged_data$ill_med)

# medicine_hum_monthly_data
table(human_merged_data$medicine_hum_monthly_data, useNA = "always")
str(human_merged_data$medicine_hum_monthly_data)

# med_oth_hum_monthly_data
table(human_merged_data$med_oth_hum_monthly_data, useNA = "always")
str(human_merged_data$med_oth_hum_monthly_data)

# med_source
table(human_merged_data$med_source, useNA = "always")
str(human_merged_data$med_source)

# med_date
table(human_merged_data$med_date, useNA = "always")
str(human_merged_data$med_date)

# tablets
table(human_merged_data$tablets, useNA = "always")
str(human_merged_data$tablets)

# recover
table(human_merged_data$recover, useNA = "always")
str(human_merged_data$recover)

# mal_likely_hum_monthly_data
table(human_merged_data$mal_likely_hum_monthly_data, useNA = "always")
str(human_merged_data$mal_likely_hum_monthly_data)

# mal_rdt
table(human_merged_data$mal_rdt, useNA = "always")
str(human_merged_data$mal_rdt)

# mrdt
table(human_merged_data$mrdt, useNA = "always")
str(human_merged_data$mrdt)

# mrdt_n_hum_monthly_data
table(human_merged_data$mrdt_n_hum_monthly_data, useNA = "always")
str(human_merged_data$mrdt_n_hum_monthly_data)

# mrdt_p_hum_monthly_data
table(human_merged_data$mrdt_p_hum_monthly_data, useNA = "always")
str(human_merged_data$mrdt_p_hum_monthly_data)

# malaria_likely_hum_monthly_data
table(human_merged_data$malaria_likely_hum_monthly_data, useNA = "always")
str(human_merged_data$malaria_likely_hum_monthly_data)

# malaria_al_hum_monthly_data
table(human_merged_data$malaria_al_hum_monthly_data, useNA = "always")
str(human_merged_data$malaria_al_hum_monthly_data)

# key_hum_monthly_data
table(human_merged_data$key_hum_monthly_data, useNA = "always")
str(human_merged_data$key_hum_monthly_data)
length(which(is.na(human_merged_data$key_hum_monthly_data)))

# test_type_oth
table(human_merged_data$test_type_oth, useNA = "always")
str(human_merged_data$test_type_oth)

# medicine_ACT_hum_monthly_data
table(human_merged_data$medicine_ACT_hum_monthly_data, useNA = "always")
str(human_merged_data$medicine_ACT_hum_monthly_data)

# medicine_Qui_hum_monthly_data
table(human_merged_data$medicine_Qui_hum_monthly_data, useNA = "always")
str(human_merged_data$medicine_Qui_hum_monthly_data)

# medicine_SP_hum_monthly_data
table(human_merged_data$medicine_SP_hum_monthly_data, useNA = "always")
str(human_merged_data$medicine_SP_hum_monthly_data)

# medicine_OACT_hum_monthly_data
table(human_merged_data$medicine_OACT_hum_monthly_data, useNA = "always")
str(human_merged_data$medicine_OACT_hum_monthly_data)

# medicine_AMO_hum_monthly_data
table(human_merged_data$medicine_AMO_hum_monthly_data, useNA = "always")
str(human_merged_data$medicine_AMO_hum_monthly_data)

# medicine_SPT_hum_monthly_data
table(human_merged_data$medicine_SPT_hum_monthly_data, useNA = "always")
str(human_merged_data$medicine_SPT_hum_monthly_data)

# medicine_CIP_hum_monthly_data
table(human_merged_data$medicine_CIP_hum_monthly_data, useNA = "always")
str(human_merged_data$medicine_CIP_hum_monthly_data)

# medicine_PAN_hum_monthly_data
table(human_merged_data$medicine_PAN_hum_monthly_data, useNA = "always")
str(human_merged_data$medicine_PAN_hum_monthly_data)

# medicine_DNT_hum_monthly_data
table(human_merged_data$medicine_DNT_hum_monthly_data, useNA = "always")
str(human_merged_data$medicine_DNT_hum_monthly_data)

# medicine_OTH_hum_monthly_data
table(human_merged_data$medicine_OTH_hum_monthly_data, useNA = "always")
str(human_merged_data$medicine_OTH_hum_monthly_data)

# act_illness_a
table(human_merged_data$act_illness_a, useNA = "always")
str(human_merged_data$act_illness_a)

# act_illness_b
table(human_merged_data$act_illness_b, useNA = "always")
str(human_merged_data$act_illness_b)

# act_illness_c
table(human_merged_data$act_illness_c, useNA = "always")
str(human_merged_data$act_illness_c)

# act_illness_d
table(human_merged_data$act_illness_d, useNA = "always")
str(human_merged_data$act_illness_d)

# act_illness_e
table(human_merged_data$act_illness_e, useNA = "always")
str(human_merged_data$act_illness_e)

# act_illness_f
table(human_merged_data$act_illness_f, useNA = "always")
str(human_merged_data$act_illness_f)

# act_illness_g
table(human_merged_data$act_illness_g, useNA = "always")
str(human_merged_data$act_illness_g)

# act_illness_h
table(human_merged_data$act_illness_h, useNA = "always")
str(human_merged_data$act_illness_h)

# act_illness_i
table(human_merged_data$act_illness_i, useNA = "always")
str(human_merged_data$act_illness_i)

# act_illness_j
table(human_merged_data$act_illness_j, useNA = "always")
str(human_merged_data$act_illness_j)

# act_illness_k
table(human_merged_data$act_illness_k, useNA = "always")
str(human_merged_data$act_illness_k)

# unq_memID
table(human_merged_data$unq_memID, useNA = "always")
str(human_merged_data$unq_memID)

# monthly_unq_memID
table(human_merged_data$monthly_unq_memID, useNA = "always")
str(human_merged_data$monthly_unq_memID)
length(which(is.na(human_merged_data$monthly_unq_memID)))

# monthl_hum_monthly_data
table(human_merged_data$month_hum_monthly_data, useNA = "always")
str(human_merged_data$month_hum_monthly_data)
length(which(is.na(human_merged_data$month_hum_monthly_data)))

# year_hum_monthly_data
table(human_merged_data$year_hum_monthly_data, useNA = "always")
str(human_merged_data$year_hum_monthly_data)
length(which(is.na(human_merged_data$year_hum_monthly_data)))

# month_year_combo_monthly_data
table(human_merged_data$month_year_combo_monthly_data, useNA = "always")
str(human_merged_data$month_year_combo_monthly_data)
length(which(is.na(human_merged_data$month_year_combo_monthly_data)))

# today_hum_table_household_data
table(human_merged_data$today_hum_table_household_data, useNA = "always")
str(human_merged_data$today_hum_table_household_data)
length(which(is.na(human_merged_data$today_hum_table_household_data)))
# if same memID then make sure today_hum_table_household_data is same for the one where missing (because today_hum_table_household_data not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$today_hum_table_household_data))) %>%
  select(unq_memID,today_hum_table_household_data) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 243, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$today_hum_table_household_data.x, useNA = "always")
table(human_merged_data$today_hum_table_household_data.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$today_hum_table_household_data.x <- NULL
human_merged_data = rename(human_merged_data,"today_hum_table_household_data"="today_hum_table_household_data.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,today_hum_table_household_data) %>%
  summarize(n=n()) %>%
  group_by(today_hum_table_household_data) %>%
  summarize(totaln = n())
sum(participant_data$totaln)
# looks good, clean

# age_type
table(human_merged_data$age_type, useNA = "always")
str(human_merged_data$age_type)
length(which(is.na(human_merged_data$age_type)))

# age_y
table(human_merged_data$age_y, useNA = "always")
str(human_merged_data$age_y)
length(which(is.na(human_merged_data$age_y)))

# age_m
table(human_merged_data$age_m, useNA = "always")
str(human_merged_data$age_m)
length(which(is.na(human_merged_data$age_m)))

# educ_level
table(human_merged_data$educ_level, useNA = "always")
str(human_merged_data$educ_level)
length(which(is.na(human_merged_data$educ_level)))
# if same memID then make sure educ_level is same for the one where missing (because educ_level not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(visit_type == "monthly visit") %>%
  select(unq_memID,educ_level) %>%
  distinct
# see where differences
setdiff(unique(human_merged_data$unq_memID),new_df$unq_memID) # S08_10
# see where different, had monthly and sick visits on the same day which caused the difference
# do a different filter
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$educ_level))) %>%
  select(unq_memID,educ_level) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 243, correct
# join in the new age categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the age cat new columns
table(human_merged_data$educ_level.x, useNA = "always")
table(human_merged_data$educ_level.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$educ_level.x <- NULL
human_merged_data = rename(human_merged_data,"educ_level"="educ_level.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,educ_level) %>%
  summarize(n=n()) %>%
  group_by(educ_level) %>%
  summarize(totaln = n())
# looks good, clean

# oth_educ_level
table(human_merged_data$oth_educ_level, useNA = "always")
str(human_merged_data$oth_educ_level)
length(which(is.na(human_merged_data$oth_educ_level)))
# if same memID then make sure educ_level is same for the one where missing (because educ_level not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$oth_educ_level))) %>%
  select(unq_memID,oth_educ_level) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 1, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$oth_educ_level.x, useNA = "always")
table(human_merged_data$oth_educ_level.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$oth_educ_level.x <- NULL
human_merged_data = rename(human_merged_data,"oth_educ_level"="oth_educ_level.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,oth_educ_level) %>%
  summarize(n=n()) %>%
  group_by(oth_educ_level) %>%
  summarize(totaln = n())
# looks good, clean

# employment
table(human_merged_data$employment, useNA = "always")
str(human_merged_data$employment)
length(which(is.na(human_merged_data$employment)))
# if same memID then make sure employment is same for the one where missing (because employment not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$employment))) %>%
  select(unq_memID,employment) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 243, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$employment.x, useNA = "always")
table(human_merged_data$employment.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$employment.x <- NULL
human_merged_data = rename(human_merged_data,"employment"="employment.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,employment) %>%
  summarize(n=n()) %>%
  group_by(employment) %>%
  summarize(totaln = n())
sum(participant_data$totaln)
# looks good, clean

# oth_emp
table(human_merged_data$oth_emp, useNA = "always")
str(human_merged_data$oth_emp)
length(which(is.na(human_merged_data$oth_emp)))
# if same memID then make sure employment is same for the one where missing (because employment not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$oth_emp))) %>%
  select(unq_memID,oth_emp) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 4, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$oth_emp.x, useNA = "always")
table(human_merged_data$oth_emp.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$oth_emp.x <- NULL
human_merged_data = rename(human_merged_data,"oth_emp"="oth_emp.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,oth_emp) %>%
  summarize(n=n()) %>%
  group_by(oth_emp) %>%
  summarize(totaln = n())
sum(participant_data$totaln)
# looks good, clean

# sleep
table(human_merged_data$sleep, useNA = "always")
str(human_merged_data$sleep)
length(which(is.na(human_merged_data$sleep)))
# if same memID then make sure employment is same for the one where missing (because employment not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$sleep))) %>%
  select(unq_memID,sleep) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 243, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$sleep.x, useNA = "always")
table(human_merged_data$sleep.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$sleep.x <- NULL
human_merged_data = rename(human_merged_data,"sleep"="sleep.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,sleep) %>%
  summarize(n=n()) %>%
  group_by(sleep) %>%
  summarize(totaln = n())
sum(participant_data$totaln)
str(human_merged_data$sleep)
# looks good, clean

# know_rdt
table(human_merged_data$know_rdt, useNA = "always")
str(human_merged_data$know_rdt)
length(which(is.na(human_merged_data$know_rdt)))
# if same memID then make sure employment is same for the one where missing (because employment not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$know_rdt))) %>%
  select(unq_memID,know_rdt) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 95, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$know_rdt.x, useNA = "always")
table(human_merged_data$know_rdt.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$know_rdt.x <- NULL
human_merged_data = rename(human_merged_data,"know_rdt"="know_rdt.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,know_rdt) %>%
  summarize(n=n()) %>%
  group_by(know_rdt) %>%
  summarize(totaln = n())
sum(participant_data$totaln)
str(human_merged_data$know_rdt)
# looks good, clean

# mal_test
table(human_merged_data$mal_test, useNA = "always")
str(human_merged_data$mal_test)
length(which(is.na(human_merged_data$mal_test)))
# if same memID then make sure employment is same for the one where missing (because employment not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$mal_test))) %>%
  select(unq_memID,mal_test) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 82, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$mal_test.x, useNA = "always")
table(human_merged_data$mal_test.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$mal_test.x <- NULL
human_merged_data = rename(human_merged_data,"mal_test"="mal_test.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,mal_test) %>%
  summarize(n=n()) %>%
  group_by(mal_test) %>%
  summarize(totaln = n())
sum(participant_data$totaln)
str(human_merged_data$mal_test)
# looks good, clean

# rdt_result_n
table(human_merged_data$rdt_result_n, useNA = "always")
str(human_merged_data$rdt_result_n)
length(which(is.na(human_merged_data$rdt_result_n)))
# if same memID then make sure employment is same for the one where missing (because employment not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$rdt_result_n))) %>%
  select(unq_memID,rdt_result_n) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 82, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$rdt_result_n.x, useNA = "always")
table(human_merged_data$rdt_result_n.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$rdt_result_n.x <- NULL
human_merged_data = rename(human_merged_data,"rdt_result_n"="rdt_result_n.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,rdt_result_n) %>%
  summarize(n=n()) %>%
  group_by(rdt_result_n) %>%
  summarize(totaln = n())
sum(participant_data$totaln)
str(human_merged_data$rdt_result_n)
# looks good, clean

# rdt_result_p
table(human_merged_data$rdt_result_p, useNA = "always")
str(human_merged_data$rdt_result_p)
length(which(is.na(human_merged_data$rdt_result_p)))
# if same memID then make sure employment is same for the one where missing (because employment not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$rdt_result_p))) %>%
  select(unq_memID,rdt_result_p) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 82, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$rdt_result_p.x, useNA = "always")
table(human_merged_data$rdt_result_p.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$rdt_result_p.x <- NULL
human_merged_data = rename(human_merged_data,"rdt_result_p"="rdt_result_p.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,rdt_result_p) %>%
  summarize(n=n()) %>%
  group_by(rdt_result_p) %>%
  summarize(totaln = n())
sum(participant_data$totaln)
str(human_merged_data$rdt_result_p)
# looks good, clean

# malaria_likely_hum_table_household_data
table(human_merged_data$malaria_likely_hum_table_household_data, useNA = "always")
str(human_merged_data$malaria_likely_hum_table_household_data)
length(which(is.na(human_merged_data$malaria_likely_hum_table_household_data)))
# if same memID then make sure employment is same for the one where missing (because employment not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$malaria_likely_hum_table_household_data))) %>%
  select(unq_memID,malaria_likely_hum_table_household_data) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 94, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$malaria_likely_hum_table_household_data.x, useNA = "always")
table(human_merged_data$malaria_likely_hum_table_household_data.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$malaria_likely_hum_table_household_data.x <- NULL
human_merged_data = rename(human_merged_data,"malaria_likely_hum_table_household_data"="malaria_likely_hum_table_household_data.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,malaria_likely_hum_table_household_data) %>%
  summarize(n=n()) %>%
  group_by(malaria_likely_hum_table_household_data) %>%
  summarize(totaln = n())
sum(participant_data$totaln)
str(human_merged_data$malaria_likely_hum_table_household_data)
# looks good, clean

# malaria_al_hum_table_household_data
table(human_merged_data$malaria_al_hum_table_household_data, useNA = "always")
str(human_merged_data$malaria_al_hum_table_household_data)
length(which(is.na(human_merged_data$malaria_al_hum_table_household_data)))
# if same memID then make sure employment is same for the one where missing (because employment not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$malaria_al_hum_table_household_data))) %>%
  select(unq_memID,malaria_al_hum_table_household_data) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 94, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$malaria_al_hum_table_household_data.x, useNA = "always")
table(human_merged_data$malaria_al_hum_table_household_data.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$malaria_al_hum_table_household_data.x <- NULL
human_merged_data = rename(human_merged_data,"malaria_al_hum_table_household_data"="malaria_al_hum_table_household_data.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,malaria_al_hum_table_household_data) %>%
  summarize(n=n()) %>%
  group_by(malaria_al_hum_table_household_data) %>%
  summarize(totaln = n())
sum(participant_data$totaln)
str(human_merged_data$malaria_al_hum_table_household_data)
# looks good, clean

# parent_key_hum_table_household_data
table(human_merged_data$parent_key_hum_table_household_data, useNA = "always")
str(human_merged_data$parent_key_hum_table_household_data)
length(which(is.na(human_merged_data$parent_key_hum_table_household_data)))
# if same memID then make sure employment is same for the one where missing (because employment not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$parent_key_hum_table_household_data))) %>%
  select(unq_memID,parent_key_hum_table_household_data) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 243, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$parent_key_hum_table_household_data.x, useNA = "always")
table(human_merged_data$parent_key_hum_table_household_data.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$parent_key_hum_table_household_data.x <- NULL
human_merged_data = rename(human_merged_data,"parent_key_hum_table_household_data"="parent_key_hum_table_household_data.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,parent_key_hum_table_household_data) %>%
  summarize(n=n()) %>%
  group_by(parent_key_hum_table_household_data) %>%
  summarize(totaln = n())
sum(participant_data$totaln)
str(human_merged_data$parent_key_hum_table_household_data)
# looks good, clean

# key_hum_table_household_data
table(human_merged_data$key_hum_table_household_data, useNA = "always")
str(human_merged_data$key_hum_table_household_data)
length(which(is.na(human_merged_data$key_hum_table_household_data)))
# if same memID then make sure employment is same for the one where missing (because employment not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$key_hum_table_household_data))) %>%
  select(unq_memID,key_hum_table_household_data) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 243, correct
# join in the new categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the new columns
table(human_merged_data$key_hum_table_household_data.x, useNA = "always")
table(human_merged_data$key_hum_table_household_data.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$key_hum_table_household_data.x <- NULL
human_merged_data = rename(human_merged_data,"key_hum_table_household_data"="key_hum_table_household_data.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,key_hum_table_household_data) %>%
  summarize(n=n()) %>%
  group_by(key_hum_table_household_data) %>%
  summarize(totaln = n())
sum(participant_data$totaln)
str(human_merged_data$key_hum_table_household_data)
# looks good, clean

# HH_ID
table(human_merged_data$HH_ID, useNA = "always")
str(human_merged_data$HH_ID)

# memID
table(human_merged_data$memID, useNA = "always")
str(human_merged_data$memID)

# age_cat
table(human_merged_data$age_cat, useNA = "always")
str(human_merged_data$age_cat)
# make a factor
# <5 years is 1, 5-15 years is 2, >15 years is 3
human_merged_data$age_cat = factor(human_merged_data$age_cat, levels=c(1,2,3), labels=c("<5 years","5-15 years",">15 years"))
table(human_merged_data$age_cat, useNA = "always")
str(human_merged_data$age_cat)

# today_hum_sick_data
table(human_merged_data$today_hum_sick_data, useNA = "always")
str(human_merged_data$today_hum_sick_data)

# mrdt_n_hum_sick_data
table(human_merged_data$mrdt_n_hum_sick_data, useNA = "always")
str(human_merged_data$mrdt_n_hum_sick_data)

# mrdt_p_hum_sick_data
table(human_merged_data$mrdt_p_hum_sick_data, useNA = "always")
str(human_merged_data$mrdt_p_hum_sick_data)

# malaria_al_hum_sick_data
table(human_merged_data$malaria_al_hum_sick_data, useNA = "always")
str(human_merged_data$malaria_al_hum_sick_data)

# history
table(human_merged_data$history, useNA = "always")
str(human_merged_data$history)

# complaints
table(human_merged_data$complaints, useNA = "always")
str(human_merged_data$complaints)

# comp_oth
table(human_merged_data$comp_oth, useNA = "always")
str(human_merged_data$comp_oth)

# illness_d
table(human_merged_data$illness_d, useNA = "always")
str(human_merged_data$illness_d)

# severe
table(human_merged_data$severe, useNA = "always")
str(human_merged_data$severe)

# medic
table(human_merged_data$medic, useNA = "always")
str(human_merged_data$medic)

# medicine_hum_sick_data
table(human_merged_data$medicine_hum_sick_data, useNA = "always")
str(human_merged_data$medicine_hum_sick_data)

# med_oth_hum_sick_data
table(human_merged_data$med_oth_hum_sick_data, useNA = "always")
str(human_merged_data$med_oth_hum_sick_data)

# mal_likely_hum_sick_data
table(human_merged_data$mal_likely_hum_sick_data, useNA = "always")
str(human_merged_data$mal_likely_hum_sick_data)

# rdt_rst
table(human_merged_data$rdt_rst, useNA = "always")
str(human_merged_data$rdt_rst)

# ill_mal
table(human_merged_data$ill_mal, useNA = "always")
str(human_merged_data$ill_mal)

# taken_al
table(human_merged_data$taken_al, useNA = "always")
str(human_merged_data$taken_al)

# prescription
table(human_merged_data$prescription, useNA = "always")
str(human_merged_data$prescription)

# why
table(human_merged_data$why, useNA = "always")
str(human_merged_data$why)

# key_hum_sick_data
table(human_merged_data$key_hum_sick_data, useNA = "always")
str(human_merged_data$key_hum_sick_data)

# fever
table(human_merged_data$fever, useNA = "always")
str(human_merged_data$fever)

# Aches
table(human_merged_data$Aches, useNA = "always")
str(human_merged_data$Aches)

# Vomiting
table(human_merged_data$Vomiting, useNA = "always")
str(human_merged_data$Vomiting)

# Diarrhea
table(human_merged_data$Diarrhea, useNA = "always")
str(human_merged_data$Diarrhea)

# Chills
table(human_merged_data$Chills, useNA = "always")
str(human_merged_data$Chills)

# congestion
table(human_merged_data$congestion, useNA = "always")
str(human_merged_data$congestion)

# Cough
table(human_merged_data$Cough, useNA = "always")
str(human_merged_data$Cough)

# Other
table(human_merged_data$Other, useNA = "always")
str(human_merged_data$Other)

# medicine_ACT_hum_sick_data
table(human_merged_data$medicine_ACT_hum_sick_data, useNA = "always")
str(human_merged_data$medicine_ACT_hum_sick_data)

# medicine_Qui_hum_sick_data
table(human_merged_data$medicine_Qui_hum_sick_data, useNA = "always")
str(human_merged_data$medicine_Qui_hum_sick_data)

# medicine_SP_hum_sick_data
table(human_merged_data$medicine_SP_hum_sick_data, useNA = "always")
str(human_merged_data$medicine_SP_hum_sick_data)

# medicine_OACT_hum_sick_data
table(human_merged_data$medicine_OACT_hum_sick_data, useNA = "always")
str(human_merged_data$medicine_OACT_hum_sick_data)

# medicine_AMO_hum_sick_data
table(human_merged_data$medicine_AMO_hum_sick_data, useNA = "always")
str(human_merged_data$medicine_AMO_hum_sick_data)

# medicine_SPT_hum_sick_data
table(human_merged_data$medicine_SPT_hum_sick_data, useNA = "always")
str(human_merged_data$medicine_SPT_hum_sick_data)

# medicine_CIP_hum_sick_data
table(human_merged_data$medicine_CIP_hum_sick_data, useNA = "always")
str(human_merged_data$medicine_CIP_hum_sick_data)

# medicine_PAN_hum_sick_data
table(human_merged_data$medicine_PAN_hum_sick_data, useNA = "always")
str(human_merged_data$medicine_PAN_hum_sick_data)

# medicine_DNT_hum_sick_data
table(human_merged_data$medicine_DNT_hum_sick_data, useNA = "always")
str(human_merged_data$medicine_DNT_hum_sick_data)

# medicine_OTH_hum_sick_data
table(human_merged_data$medicine_OTH_hum_sick_data, useNA = "always")
str(human_merged_data$medicine_OTH_hum_sick_data)

# sick_unq_memID
table(human_merged_data$sick_unq_memID, useNA = "always")
str(human_merged_data$sick_unq_memID)

# Sample Name
table(human_merged_data$`Sample Name`, useNA = "always")
str(human_merged_data$`Sample Name`)
length(which(is.na(human_merged_data$`Sample Name`)))

# sample_id_date
table(human_merged_data$sample_id_date, useNA = "always")
str(human_merged_data$sample_id_date)
length(which(is.na(human_merged_data$sample_id_date)))
# check if same as universal today
identical(human_merged_data$universal_today,human_merged_data$sample_id_date)
not_identical_dates = human_merged_data[which(!(identical(human_merged_data$universal_today,human_merged_data$sample_id_date))),]
human_merged_data$today_hum_monthly_data[which(human_merged_data$monthly_unq_memID == "K01-010218-2")]
human_merged_data$today_hum_sick_data[which(human_merged_data$monthly_unq_memID == "K01-010218-2")]
human_merged_data$`Sample Name`[which(human_merged_data$monthly_unq_memID == "K01-010218-2")]
# remove universal today variable
human_merged_data$universal_today <- NULL

# sample_name_dbs
table(human_merged_data$sample_name_dbs, useNA = "always")
str(human_merged_data$sample_name_dbs)
length(which(is.na(human_merged_data$sample_name_dbs)))

# pf_pcr_infection_status
table(human_merged_data$pf_pcr_infection_status, useNA = "always")
str(human_merged_data$pf_pcr_infection_status)
length(which(is.na(human_merged_data$pf_pcr_infection_status)))

# pfr364Q_std_combined
table(human_merged_data$pfr364Q_std_combined, useNA = "always")
str(human_merged_data$pfr364Q_std_combined)
length(which(is.na(human_merged_data$pfr364Q_std_combined)))
summary(human_merged_data$pfr364Q_std_combined)

# sample_name_final
table(human_merged_data$sample_name_final, useNA = "always")
str(human_merged_data$sample_name_final)
length(which(is.na(human_merged_data$sample_name_final)))
# see if same as Sample Name
identical(human_merged_data$sample_name_final, human_merged_data$`Sample Name`)
# exactly the same, end up removing Sample Name
human_merged_data$`Sample Name` <- NULL

# double_visit_test
table(human_merged_data$double_visit_test, useNA = "always")
str(human_merged_data$double_visit_test)
length(which(is.na(human_merged_data$double_visit_test)))
# don't need anymore, removed
human_merged_data$double_visit_test <- NULL

# visit_type
table(human_merged_data$visit_type, useNA = "always")
str(human_merged_data$visit_type)
length(which(is.na(human_merged_data$visit_type)))
# make a factor
human_merged_data$visit_type = as.factor(human_merged_data$visit_type)
str(human_merged_data$visit_type)

# write out the cleaned data set
write_csv(human_merged_data,"spat21_human_merged_data_no_dbs_censoring_11JUN2019.csv")
write_rds(human_merged_data,"spat21_human_merged_data_no_dbs_censoring_11JUN2019.rds")

# now remove the rows where dbs didn't merge
human_merged_data_test = human_merged_data[-which(is.na(human_merged_data$sample_name_dbs)),]
table(human_merged_data$pf_pcr_infection_status, useNA = "always")
table(human_merged_data_test$pf_pcr_infection_status, useNA = "always")
# the 96 dbs that didn't merge in were successfully removed
# write out the cleaned data set with dbs that didn't merge in removed
write_csv(human_merged_data_test,"spat21_human_merged_data_with_dbs_censoring_11JUN2019.csv")
write_rds(human_merged_data_test,"spat21_human_merged_data_with_dbs_censoring_11JUN2019.rds")

# do one last check of colnames
colnames(human_merged_data)





