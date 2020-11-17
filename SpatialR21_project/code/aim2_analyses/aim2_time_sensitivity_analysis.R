# -------------------------------------- #
#           Spat21/Mozzie Study          #
#       Time Sensitivity Analysis        #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#           October 15, 2020             #
# -------------------------------------- #

# good resource for trouble shooting convergence problems
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html


#### --------- load packages ----------------- ####
library(tidyverse)
library(msm)
require(pracma)
library(lme4)


#### ------- load in the data sets ----------- ####

# read in the edgelist data set before p(TEt) and P(TEd) were added
model_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA_and_CSP/final/full data/spat21_aim2_merged_data_with_weights_full_data_5MAR2020.rds")

# read in the mosquito demographic data
mosquito_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")


#### --------- create a function to look at different time codings ----------- ####

# make new variable for coding time
# 15
p_te_t_15 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -15 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_15[i] = 1
  } else {
    p_te_t_15[i] = 0
  }
}
model_data$p_te_t_15 = p_te_t_15
# 16
p_te_t_16 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -16 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_16[i] = 1
  } else {
    p_te_t_16[i] = 0
  }
}
model_data$p_te_t_16 = p_te_t_16
# 17
p_te_t_17 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -17 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_17[i] = 1
  } else {
    p_te_t_17[i] = 0
  }
}
model_data$p_te_t_17 = p_te_t_17
# 18
p_te_t_18 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -18 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_18[i] = 1
  } else {
    p_te_t_18[i] = 0
  }
}
model_data$p_te_t_18 = p_te_t_18
# 19
p_te_t_19 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -19 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_19[i] = 1
  } else {
    p_te_t_19[i] = 0
  }
}
model_data$p_te_t_19 = p_te_t_19
# 20
p_te_t_20 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -20 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_20[i] = 1
  } else {
    p_te_t_20[i] = 0
  }
}
model_data$p_te_t_20 = p_te_t_20
# 21
p_te_t_21 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -21 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_21[i] = 1
  } else {
    p_te_t_21[i] = 0
  }
}
model_data$p_te_t_21 = p_te_t_21
# 22
p_te_t_22 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -22 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_22[i] = 1
  } else {
    p_te_t_22[i] = 0
  }
}
model_data$p_te_t_22 = p_te_t_22
# 23
p_te_t_23 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -23 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_23[i] = 1
  } else {
    p_te_t_23[i] = 0
  }
}
model_data$p_te_t_23 = p_te_t_23
# 24
p_te_t_24 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -24 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_24[i] = 1
  } else {
    p_te_t_24[i] = 0
  }
}
model_data$p_te_t_24 = p_te_t_24
# 25
p_te_t_25 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -25 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_25[i] = 1
  } else {
    p_te_t_25[i] = 0
  }
}
model_data$p_te_t_25 = p_te_t_25
# 26
p_te_t_26 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -26 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_26[i] = 1
  } else {
    p_te_t_26[i] = 0
  }
}
model_data$p_te_t_26 = p_te_t_26
# 27
p_te_t_27 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -27 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_27[i] = 1
  } else {
    p_te_t_27[i] = 0
  }
}
model_data$p_te_t_27 = p_te_t_27
# 28
p_te_t_28 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -28 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_28[i] = 1
  } else {
    p_te_t_28[i] = 0
  }
}
model_data$p_te_t_28 = p_te_t_28
# 29
p_te_t_29 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -29 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_29[i] = 1
  } else {
    p_te_t_29[i] = 0
  }
}
model_data$p_te_t_29 = p_te_t_29
# 30
p_te_t_30 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$date_difference_flipped[i] >= -30 & model_data$date_difference_flipped[i] <= 7){
    p_te_t_30[i] = 1
  } else {
    p_te_t_30[i] = 0
  }
}
model_data$p_te_t_30 = p_te_t_30


# now make a new p_te_all_csp variable
# 15
p_te_all_csp_15 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_15[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_15[i] = model_data$p_te_t_15[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_15[i] = 0
  }
}
model_data$p_te_all_csp_15 = p_te_all_csp_15
# 16
p_te_all_csp_16 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_16[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_16[i] = model_data$p_te_t_16[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_16[i] = 0
  }
}
model_data$p_te_all_csp_16 = p_te_all_csp_16
# 17
p_te_all_csp_17 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_17[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_17[i] = model_data$p_te_t_17[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_17[i] = 0
  }
}
model_data$p_te_all_csp_17 = p_te_all_csp_17
# 18
p_te_all_csp_18 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_18[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_18[i] = model_data$p_te_t_18[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_18[i] = 0
  }
}
model_data$p_te_all_csp_18 = p_te_all_csp_18
# 19
p_te_all_csp_19 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_19[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_19[i] = model_data$p_te_t_19[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_19[i] = 0
  }
}
model_data$p_te_all_csp_19 = p_te_all_csp_19
# 20
p_te_all_csp_20 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_20[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_20[i] = model_data$p_te_t_20[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_20[i] = 0
  }
}
model_data$p_te_all_csp_20 = p_te_all_csp_20
# 21
p_te_all_csp_21 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_21[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_21[i] = model_data$p_te_t_21[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_21[i] = 0
  }
}
model_data$p_te_all_csp_21 = p_te_all_csp_21
# 22
p_te_all_csp_22 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_22[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_22[i] = model_data$p_te_t_22[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_22[i] = 0
  }
}
model_data$p_te_all_csp_22 = p_te_all_csp_22
# 23
p_te_all_csp_23 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_23[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_23[i] = model_data$p_te_t_23[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_23[i] = 0
  }
}
model_data$p_te_all_csp_23 = p_te_all_csp_23
# 24
p_te_all_csp_24 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_24[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_24[i] = model_data$p_te_t_24[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_24[i] = 0
  }
}
model_data$p_te_all_csp_24 = p_te_all_csp_24
# 25
p_te_all_csp_25 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_25[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_25[i] = model_data$p_te_t_25[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_25[i] = 0
  }
}
model_data$p_te_all_csp_25 = p_te_all_csp_25
# 26
p_te_all_csp_26 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_26[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_26[i] = model_data$p_te_t_26[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_26[i] = 0
  }
}
model_data$p_te_all_csp_26 = p_te_all_csp_26
# 27
p_te_all_csp_27 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_27[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_27[i] = model_data$p_te_t_27[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_27[i] = 0
  }
}
model_data$p_te_all_csp_27 = p_te_all_csp_27
# 28
p_te_all_csp_28 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_28[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_28[i] = model_data$p_te_t_28[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_28[i] = 0
  }
}
model_data$p_te_all_csp_28 = p_te_all_csp_28
# 29
p_te_all_csp_29 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_29[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_29[i] = model_data$p_te_t_29[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_29[i] = 0
  }
}
model_data$p_te_all_csp_29 = p_te_all_csp_29
# 30
p_te_all_csp_30 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t_30[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_30[i] = model_data$p_te_t_30[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_30[i] = 0
  }
}
model_data$p_te_all_csp_30 = p_te_all_csp_30


#### ------- now create new data sets for each value -------- ####

# first: remove malaria infections that occurred within 14 days of symptomatic infection where received treatment in study

# tally up the people that had antimalarials at a monthly visit at least 14 days before their infection
had_antimalarials_recently = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" & mal_illness == "yes" & ill_med == "yes" &
           (medicine_ACT_hum_monthly_data == "yes" | medicine_Qui_hum_monthly_data == "yes" | medicine_SP_hum_monthly_data == "yes" | 
              medicine_OACT_hum_monthly_data == "yes" | medicine_AMO_hum_monthly_data == "yes" | medicine_SPT_hum_monthly_data == "yes" |
              medicine_CIP_hum_monthly_data == "yes" | medicine_PAN_hum_monthly_data == "yes" | medicine_DNT_hum_monthly_data == "yes" |
              medicine_OTH_hum_monthly_data == "yes")) %>%
  select(main_exposure_primary_case_def,med_date,recover,sample_id_date,ill_med,sample_name_dbs) %>%
  mutate(time_since_antimalarials_taken = sample_id_date - med_date) %>%
  filter((time_since_antimalarials_taken < 15 & time_since_antimalarials_taken >= 0) | is.na(time_since_antimalarials_taken)) %>%
  rename(sample_id_human = sample_name_dbs)

# see if any of these asymptomatic infections are in the paired data set
test = left_join(edgelist_data,had_antimalarials_recently,by="sample_id_human")
test = test %>%
  filter(ill_med=="yes")
# the self-reported use of antimalarials isn't that reputable

# let's see how many people had a positive RDT at a symptomatic visit and received an antimalarial within 14 days of the asymptomatic infection
# first reorder the data set
final_data = final_data[with(final_data, order(final_data$unq_memID, final_data$sample_id_date)),]
final_data %>%
  select(sample_name_final,sample_id_date,unq_memID) %>%
  View()


# check to see if there are any instances where you have two symptomatic infections within 14 days of each other
received_antimalarial = rep(NA,nrow(final_data))
date_diff = rep(NA,nrow(final_data))
for (i in 1:nrow(final_data)){
  if (final_data$unq_memID[i] == final_data$unq_memID[i+1] & final_data$prescription[i] == "prescribed" & !(is.na(final_data$prescription[i])) & i != nrow(final_data) &
      final_data$main_outcome_primary_case_def[i+1] == "symptomatic infection" & !(is.na(final_data$main_outcome_primary_case_def[i+1]))){
    date_diff[i] = final_data$sample_id_date[i+1] - final_data$sample_id_date[i]
    if (date_diff[i] < 15){
      received_antimalarial[i+1] = "yes"
    }
  } 
}
final_data$date_diff = date_diff
final_data$received_antimalarial = received_antimalarial
final_data %>%
  select(visit_type,sample_name_final,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,date_diff,received_antimalarial) %>%
  View()
final_data %>%
  select(visit_type,sample_name_final,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,date_diff,received_antimalarial) %>%
  filter(received_antimalarial == "yes") %>%
  View()
# found 1 sample that should be removed: K05-230518-4-R


# then do a for loop to see if any asymptomatic infections occurred within 14 days of a symptomatic infection where antimalarials prescribed
received_antimalarial = rep(NA,nrow(final_data))
date_diff = rep(NA,nrow(final_data))
for (i in 1:nrow(final_data)){
  if (final_data$unq_memID[i] == final_data$unq_memID[i+1] & final_data$prescription[i] == "prescribed" & !(is.na(final_data$prescription[i])) & i != nrow(final_data) &
      final_data$main_exposure_primary_case_def[i+1] == "asymptomatic infection" & !(is.na(final_data$main_exposure_primary_case_def[i+1]))){
    date_diff[i] = final_data$sample_id_date[i+1] - final_data$sample_id_date[i]
    if (date_diff[i] < 15){
      received_antimalarial[i+1] = "yes"
    }
  } 
}
final_data$date_diff = date_diff
final_data$received_antimalarial = received_antimalarial
final_data %>%
  select(visit_type,sample_name_final,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,date_diff,received_antimalarial) %>%
  View()
final_data %>%
  select(visit_type,sample_name_final,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,date_diff,received_antimalarial) %>%
  filter(received_antimalarial == "yes") %>%
  View()
# looks like there are 26 asymptomatic infections that should be censored
ppts_to_censor = final_data %>%
  select(visit_type,sample_name_final,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,date_diff,received_antimalarial) %>%
  filter(received_antimalarial == "yes")
test = final_data %>%
  select(age_all_baseline,unq_memID,sample_id_date,sample_name_final,pf_pcr_infection_status,rdt_rst,main_exposure_primary_case_def,main_outcome_primary_case_def,prescription,taken_al)

## final decision:
# Participants who had a symptomatic malaria infection and were prescribed antimalarials by the study team had asymptomatic
# infections that occurred within 14 days of the symptomatic infections removed from the data set. 

# censor these participants 
setdiff(ppts_to_censor$sample_name_final,model_data$sample_name_final)
length(unique(model_data$sample_name_final))
length(unique(model_data$sample_id_human))
length(which(model_data$sample_name_final %in% ppts_to_censor$sample_name_final))
model_data = model_data[-which(model_data$sample_name_final %in% ppts_to_censor$sample_name_final),]
model_data = model_data[-which(model_data$sample_name_final == "K05-230518-4-R"),]
length(unique(model_data$sample_name_final))
length(unique(model_data$sample_id_human))

# rescale parasite density
model_data$pfr364Q_std_combined_rescaled = scale(model_data$pfr364Q_std_combined)

# make mosquito week count cat
mosquito_week_count_df = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/time sensitivity analysis data sets/spat21_aim2_sensitivity_analysis_data_set_2FEB2020.rds")
colnames(mosquito_week_count_df)
mosquito_week_count_df = mosquito_week_count_df %>%
  select(sample_id_human,sample_id_abdomen,mosquito_week_count)
model_data = left_join(model_data,mosquito_week_count_df,by=c("sample_id_human","sample_id_abdomen"))
model_data$mosquito_week_count_cat = ifelse(model_data$mosquito_week_count < 75,"Low mosquito abundance","High mosquito abundance")

# make symptomatic infections the referent
model_data$aim2_exposure = as.factor(model_data$aim2_exposure)
model_data$aim2_exposure = relevel(model_data$aim2_exposure, ref="symptomatic infection")

# now make separate data sets for each sensitivity analysis value
original_data = model_data %>%
  filter(p_te_d > 0 & p_te_t > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_15 = model_data %>%
  filter(p_te_d > 0 & p_te_t_15 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_16 = model_data %>%
  filter(p_te_d > 0 & p_te_t_16 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_17 = model_data %>%
  filter(p_te_d > 0 & p_te_t_17 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_18 = model_data %>%
  filter(p_te_d > 0 & p_te_t_18 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_19 = model_data %>%
  filter(p_te_d > 0 & p_te_t_19 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_20 = model_data %>%
  filter(p_te_d > 0 & p_te_t_20 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_21 = model_data %>%
  filter(p_te_d > 0 & p_te_t_21 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_22 = model_data %>%
  filter(p_te_d > 0 & p_te_t_22 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_23 = model_data %>%
  filter(p_te_d > 0 & p_te_t_23 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_24 = model_data %>%
  filter(p_te_d > 0 & p_te_t_24 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_25 = model_data %>%
  filter(p_te_d > 0 & p_te_t_25 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_26 = model_data %>%
  filter(p_te_d > 0 & p_te_t_26 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_27 = model_data %>%
  filter(p_te_d > 0 & p_te_t_27 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_28 = model_data %>%
  filter(p_te_d > 0 & p_te_t_28 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_29 = model_data %>%
  filter(p_te_d > 0 & p_te_t_29 > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_30 = model_data %>%
  filter(p_te_d > 0 & p_te_t_30 > 0) %>%
  filter(!(is.na(csp_haps_shared)))



#### ------- now run new models ------- ####


# run the models for the new p_te_t estimates
# note: sensitivity analysis models have 5218 human-mosquito pair observations
model_original <- glmmTMB(p_te_all_csp~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = original_data)
model_15 <- glmmTMB(p_te_all_csp_15~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_15)
model_16 <- glmmTMB(p_te_all_csp_16~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_16)
model_17 <- glmmTMB(p_te_all_csp_17~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_17)
model_18 <- glmmTMB(p_te_all_csp_18~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_18)
model_19 <- glmmTMB(p_te_all_csp_19~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_19)
model_20 <- glmmTMB(p_te_all_csp_20~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_20)
model_21 <- glmmTMB(p_te_all_csp_21~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_21)
model_22 <- glmmTMB(p_te_all_csp_22~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_22)
model_23 <- glmmTMB(p_te_all_csp_23~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_23)
model_24 <- glmmTMB(p_te_all_csp_24~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_24)
model_25 <- glmmTMB(p_te_all_csp_25~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_25)
model_26 <- glmmTMB(p_te_all_csp_26~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_26)
model_27 <- glmmTMB(p_te_all_csp_27~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_27)
model_28 <- glmmTMB(p_te_all_csp_28~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_28)
model_29 <- glmmTMB(p_te_all_csp_29~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_29)
model_30 <- glmmTMB(p_te_all_csp_30~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_30)


# pull out the estimates
# model original
summary(model_original)
exp(confint(model_original, method="Wald"))
exp(confint(model_15, method="Wald"))
exp(confint(model_16, method="Wald"))
exp(confint(model_17, method="Wald"))
exp(confint(model_18, method="Wald"))
exp(confint(model_19, method="Wald"))
exp(confint(model_20, method="Wald"))
exp(confint(model_21, method="Wald"))
exp(confint(model_22, method="Wald"))
exp(confint(model_23, method="Wald"))
exp(confint(model_24, method="Wald"))
exp(confint(model_25, method="Wald"))
exp(confint(model_26, method="Wald"))
exp(confint(model_27, method="Wald"))
exp(confint(model_28, method="Wald"))
exp(confint(model_29, method="Wald"))
exp(confint(model_30, method="Wald"))



#### ------- now make two plots for the time sensitivity analysis ------- ####

# first plot the p_te_t values tested
p_te_t_sensitivity_plot = ggplot(data=data_30) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t),colour="black",lwd=2.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_15),colour="#67000d",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_16),colour="#3f007d",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_17),colour="#67000d",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_18),colour="#a50f15",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_19),colour="#9e0142",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_20),colour="#d53e4f",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_21),colour="#f46d43",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_22),colour="#fdae61",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_23),colour="#fee08b",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_24),colour="#ffffbf",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_25),colour="#e6f598",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_26),colour="#abdda4",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_27),colour="#66c2a5",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_28),colour="#3288bd",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_29),colour="#5e4fa2",lwd=1.5) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t_30),colour="#54278f",lwd=1.5) +
  xlab("Days between human infection and mosquito collection") +
  ylab("Probability of tranmission for time") +
  scale_x_continuous(breaks=c(7,6,5,4,3,2,1,0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,-30)) + 
  theme_bw()
p_te_t_sensitivity_plot
# export the plot
# ggsave(p_te_t_sensitivity_plot, filename="/Users/kelseysumner/Desktop/time_sensitivity_analysis_x_axis_plot.png", device="png",
       # height=7, width=8, units="in", dpi=500)

# then plot the new odds ratios across values of p_te_t
# read in the model results
model_results = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/computational_model_materials/time_sensitivity_analysis.csv")
model_results$day = c("-14 to +7","-15 to +7","-16 to +7","-17 to +7","-18 to +7","-19 to +7","-20 to +7","-21 to +7","-22 to +7","-23 to +7","-24 to +7","-25 to +7","-26 to +7","-27 to +7","-28 to +7","-29 to +7","-30 to +7")
# make a plot of the model results
model_plot_smooth = ggplot(data=model_results,aes(x=factor(day),y=estimate,group=1),cex=1.5,col="#762a83") +
  geom_smooth(data=model_results,aes(x=factor(day),y=estimate,group=1),cex=1.5,col="#762a83",fill="#762a83") +
  theme_bw() +
  xlab("Time window (day range)") +
  ylab("Odds ratio (95% CI)") + 
  scale_y_continuous(trans="log10") +
  geom_hline(yintercept=1,linetype="dashed") + 
  coord_flip() +
  theme(text = element_text(size=25)) 
model_plot_smooth
# another way to plot this
model_plot = ggplot(data=model_results,aes(x=factor(day),y=estimate,group=1),cex=1.5,col="#762a83") +
  geom_line(data=model_results,aes(x=factor(day),y=estimate,group=1),cex=1.5,col="#762a83") +
  geom_ribbon(data=model_results,aes(x=factor(day),ymin = lower_ci, ymax = upper_ci),alpha=0.2,fill="#762a83") +
  theme_bw() +
  xlab("Time window (day range)") + 
  ylab("Odds ratio (95% CI)") + 
  scale_y_continuous(trans="log10") +
  geom_hline(yintercept=1,linetype="dashed") + 
  coord_flip()
model_plot
# export the plot
ggsave(model_plot, filename="/Users/kelseysumner/Desktop/time_sensitivity_analysis_model_plot.png", device="png",
       height=7, width=8, units="in", dpi=500)




#### ------- now do a sensitivity analysis for distance allowing infections at any distance to be matched -------- ####

# make a new variable for distance
# 6 km
p_te_d_6 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$distance_km[i] >= 0 & model_data$distance_km[i] <= 6){
    p_te_d_6[i] = exp(-model_data$distance_km[i]*3)
  } else {
    p_te_d_6[i] = 0
  }
}
model_data$p_te_d_6 = p_te_d_6
# 9 km
p_te_d_9 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$distance_km[i] >= 0 & model_data$distance_km[i] <= 9){
    p_te_d_9[i] = exp(-model_data$distance_km[i]*3)
  } else {
    p_te_d_9[i] = 0
  }
}
model_data$p_te_d_9 = p_te_d_9
# 12 km
p_te_d_12 = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$distance_km[i] >= 0 & model_data$distance_km[i] <= 12){
    p_te_d_12[i] = exp(-model_data$distance_km[i]*3)
  } else {
    p_te_d_12[i] = 0
  }
}
model_data$p_te_d_12 = p_te_d_12

# now make a new p_te_all_csp variable
# 6km
p_te_all_csp_6km = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t[i] != 0 & model_data$p_te_d_6[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_6km[i] = model_data$p_te_t[i]*model_data$p_te_d_6[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_6km[i] = 0
  }
}
model_data$p_te_all_csp_6km = p_te_all_csp_6km
# 9km
p_te_all_csp_9km = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t[i] != 0 & model_data$p_te_d_9[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_9km[i] = model_data$p_te_t[i]*model_data$p_te_d_9[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_9km[i] = 0
  }
}
model_data$p_te_all_csp_9km = p_te_all_csp_9km
# 12 km
p_te_all_csp_12km = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t[i] != 0 & model_data$p_te_d_12[i] != 0 & model_data$rescaled_p_te_c[i] != 0){
    p_te_all_csp_12km[i] = model_data$p_te_t[i]*model_data$p_te_d_12[i]*model_data$rescaled_p_te_c[i]
  } else {
    p_te_all_csp_12km[i] = 0
  }
}
model_data$p_te_all_csp_12km = p_te_all_csp_12km

# now make separate data sets for each sensitivity analysis value
original_data = model_data %>%
  filter(p_te_d > 0 & p_te_t > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_6km = model_data %>%
  filter(p_te_d_6 > 0 & p_te_t > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_9km = model_data %>%
  filter(p_te_d_9 > 0 & p_te_t > 0) %>%
  filter(!(is.na(csp_haps_shared)))
data_12km = model_data %>%
  filter(p_te_d_12 > 0 & p_te_t > 0) %>%
  filter(!(is.na(csp_haps_shared)))


# run the models for the new p_te_t estimates
# note: sensitivity analysis models have 5218 human-mosquito pair observations
model_3km <- glmmTMB(p_te_all_csp~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = original_data)
model_6km <- glmmTMB(p_te_all_csp_6km~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_6km)
model_9km <- glmmTMB(p_te_all_csp_9km~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_9km)
model_12km <- glmmTMB(p_te_all_csp_12km~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_12km)


# pull out the estimates
# model original
summary(model_3km)
exp(confint(model_3km, method="Wald"))
exp(confint(model_6km, method="Wald"))
exp(confint(model_9km, method="Wald"))
exp(confint(model_12km, method="Wald"))


# then plot the new odds ratios across values of p_te_t
# read in the model results
model_results = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/computational_model_materials/distance_sensitivity_analysis.csv")
# make a plot of the model results
model_plot = ggplot(data=model_results,aes(x=factor(time_interval),y=estimate,group=1),cex=1.5,col="") +
  geom_line(data=model_results,aes(x=factor(time_interval),y=estimate,group=1),cex=1.5,col="#e34a33") +
  geom_ribbon(data=model_results,aes(x=factor(time_interval),ymin = lower_ci, ymax = upper_ci),alpha=0.2,fill="#e34a33") +
  theme_bw() +
  xlab("Maximum distance between specimens (Km)") + 
  ylab("Odds ratio (95% CI)") + 
  scale_y_continuous(trans="log10") +
  geom_hline(yintercept=1,linetype="dashed") + 
  coord_flip()
model_plot
# export the plot
ggsave(model_plot, filename="/Users/kelseysumner/Desktop/distance_sensitivity_analysis_model_plot.png", device="png",
       height=7, width=8, units="in", dpi=500)



# calculate amount of haplotype sharing at <= 3km and > 3 km
under3_data = model_data %>% filter(distance_km <= 3)
over3_data = model_data %>% filter(distance_km > 3)
summary(under3_data$distance_km)
summary(over3_data$distance_km)
summary(under3_data$csp_haps_shared)
summary(over3_data$csp_haps_shared)
model_data$under3 = ifelse(model_data$distance_km <= 3, "3 Km or less",ifelse(model_data$distance_km > 3,"Greater than 3 Km",NA))
box_plot = ggplot(data=model_data,aes(x=factor(under3),y=csp_haps_shared,fill=under3)) +
  geom_boxplot(alpha=0.7) +
  theme_bw() +
  ylab("Maximum distance between specimens (Km)") + 
  xlab("Number of pfcsp haplotypes shared") +
  scale_fill_manual(values = c("#fc8d59","#99d594")) +
  theme(legend.position = "")
box_plot
# export the plot
ggsave(box_plot, filename="/Users/kelseysumner/Desktop/distance_hap_sharing_box_plot.png", device="png",
       height=7, width=8, units="in", dpi=500)
