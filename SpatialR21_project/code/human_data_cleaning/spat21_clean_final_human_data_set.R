# ----------------------------------------- #
#  Clean Final Merged Data Set for Spat21   #
#                Human Data                 #
#               May 28, 2019                #
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

human_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/spat21_human_merged_all_data_21MAY2019.RDS")



#### -------- look at the remaining variables and clean ------- ####

# look at the column names
colnames(human_merged_data)
# 135 variables in final data set

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






