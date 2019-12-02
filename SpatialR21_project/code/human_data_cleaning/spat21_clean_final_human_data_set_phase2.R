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
table(human_merged_data$age_all_baseline, useNA = "always")
human_merged_data$age_m_hum_sick_data <- NULL
human_merged_data$age_type_hum_sick_data <- NULL
human_merged_data$age_y_hum_sick_data <- NULL
# look for how many people were <1
under1 = human_merged_data[which(human_merged_data$age_all_baseline == "1mos" | human_merged_data$age_all_baseline == "2mos" |
                                   human_merged_data$age_all_baseline == "8mos" | human_merged_data$age_all_baseline == "9mos"),]
length(unique(under1$unq_memID)) # 8
undernames = unique(under1$unq_memID)
# K04_5, K05_7, K11_7, M01_8, M06_6, M13_3, S03_5, S09_7
table(human_merged_data$unq_memID, useNA = "always")
# look at K04_5
k04_5 = human_merged_data[which(human_merged_data$unq_memID == "K04_5"),]
# 1 month at enrollment, but aged in based on follow-up
# look at K05_7
k05_7 = human_merged_data[which(human_merged_data$unq_memID == "K05_7"),]
human_merged_data = human_merged_data[-which(human_merged_data$unq_memID == "K05_7"),]
# had only 1 month of followup after merges so remove
# look at K11_7
k11_7 = human_merged_data[which(human_merged_data$unq_memID == "K11_7"),]
# 1 month at enrollment, but also aged in based on follow-up
# look at M01_8
m01_8 = human_merged_data[which(human_merged_data$unq_memID == "M01_8"),]
# 9 months at enrollment, very long follow-up and aged in
# look at M06_6
m06_6 = human_merged_data[which(human_merged_data$unq_memID == "M06_6"),]
human_merged_data = human_merged_data[-which(human_merged_data$unq_memID == "M06_6"),]
# only 1 month followup after merging to remove
# M13_3
m13_3 = human_merged_data[which(human_merged_data$unq_memID == "M13_3"),]
# 1 month at enrollment but aged in
# S03_5
s03_5 = human_merged_data[which(human_merged_data$unq_memID == "S03_5"),]
# 8 months at enrollment, aged in
# S09_7
s09_7 = human_merged_data[which(human_merged_data$unq_memID == "S09_7"),]
# 8 months at enrollment, aged in
# now move over the age data to the sick data set
# if same memID then make sure age_cat new is same for the one where missing (because age not collected at symptomatic visits)
new_df = human_merged_data %>%
  filter(visit_type == "monthly visit") %>%
  select(unq_memID,age_all_baseline) %>%
  distinct
# see where differences
setdiff(unique(human_merged_data$unq_memID),new_df$unq_memID) # no differences
# do a different filter
new_df = human_merged_data %>%
  filter(!(is.na(human_merged_data$age_all_baseline))) %>%
  select(unq_memID,age_all_baseline) %>%
  distinct
length(intersect(new_df$unq_memID,unique(human_merged_data$unq_memID))) # 269, correct
# join in the new age categories
human_merged_data_test = left_join(human_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
human_merged_data = human_merged_data_test
# join the age cat new columns
table(human_merged_data$age_all_baseline.x, useNA = "always")
table(human_merged_data$age_all_baseline.y, useNA = "always")
length(unique(human_merged_data$unq_memID))
human_merged_data$age_all_baseline.x <- NULL
human_merged_data = rename(human_merged_data,"age_all_baseline"="age_all_baseline.y")
# tabulate how many participants in new categories
participant_data = human_merged_data %>%
  group_by(unq_memID,age_all_baseline) %>%
  summarize(n=n()) %>%
  group_by(age_all_baseline) %>%
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

# malaria_likely
table(human_merged_data$malaria_likely, useNA = "always")
str(human_merged_data$malaria_likely)

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

# monthly_hum_monthly_data
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

# HH_ID
table(human_merged_data$HH_ID, useNA = "always")
str(human_merged_data$HH_ID)

# memID
table(human_merged_data$memID, useNA = "always")
str(human_merged_data$memID)

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
write_csv(human_merged_data,"Desktop/phase2_spat21_human_merged_data_no_dbs_censoring_2DEC2019.csv")
write_rds(human_merged_data,"Desktop/phase2_spat21_human_merged_data_no_dbs_censoring_2DEC2019.rds")

# now remove the rows where dbs didn't merge
human_merged_data_test = human_merged_data[-which(is.na(human_merged_data$sample_name_dbs)),]
table(human_merged_data$pf_pcr_infection_status, useNA = "always")
table(human_merged_data_test$pf_pcr_infection_status, useNA = "always")
# the 96 dbs that didn't merge in were successfully removed
# write out the cleaned data set with dbs that didn't merge in removed
write_csv(human_merged_data_test,"Desktop/phase2_spat21_human_merged_data_with_dbs_censoring_2DEC2019.csv")
write_rds(human_merged_data_test,"Desktop/phase2_spat21_human_merged_data_with_dbs_censoring_2DEC2019.rds")

# do one last check of colnames
colnames(human_merged_data)





