# -------------------------------------- #
#           Spat21/Mozzie Study          #
#       Time Sensitivity Analysis        #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#           January 31, 2020             #
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
merged_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/interim/spat21_merged_data_interim.rds")

# read in the mosquito demographic data
mosquito_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")


#### ------ look at the original variable codings and set up variables needed for model ------- ####

# create a formula for the P(TE) across time

# set up the variables
summary(merged_data$date_difference)
merged_data$date_difference = as.numeric(merged_data$date_difference)
merged_data$date_difference_flipped = merged_data$date_difference*-1
summary(merged_data$date_difference_flipped)


# first look at the distribution created by the original equation
# calculate p(TE) using the logistic function - original equation
# equation: Y = 1/(1+0.6e^(-x-16)))
p_te_t = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$date_difference_flipped[i] >= -18 & merged_data$date_difference_flipped[i] <= 0){
    p_te_t[i] = 1/(1+0.6*exp(-merged_data$date_difference_flipped[i]-16))
  } else {
    p_te_t[i] = 0
  }
}
p_te_t_no_zeroes = p_te_t[which(p_te_t != 0)]
summary(p_te_t_no_zeroes)
hist(p_te_t_no_zeroes)
merged_data$p_te_t = p_te_t
p_te_t_df = merged_data %>%
  filter(p_te_t != 0)
time_plot = ggplot(data=p_te_t_df) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t),linetype = "dashed") +
  xlab("Days between human infection and mosquito collection") +
  ylab("Probability of tranmission for time") +
  geom_vline(xintercept = -14,color="dark red") + 
  theme_bw() +
  theme(text = element_text(size=25)) +
  scale_x_continuous(limits=c(-18,0),breaks=c(-18,-15,-12,-9,-6,-3,0))
time_plot
ggsave(time_plot, filename="/Users/kelseysumner/Desktop/theoretical_time_distribution_plot.png", device="png",
       height=8, width=12, units="in", dpi=500)
# this distribution keeps the probability >0.9 until past day 14 but also allows the distribution to drop down below <0.2 by day 18


# now calculate p_te_d to use for sensitivity analysis
# make distance in km
merged_data$distance_km = merged_data$distance/1000
summary(merged_data$distance_km)
# calculate using the exponential decay formula but over distance (what we use) - this is testing out the equation
# our actual equation is y=e^(-3x)
p_te_d = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$distance_km[i] >= 0 & merged_data$distance_km[i] <= 3){
    p_te_d[i] = exp(-merged_data$distance_km[i]*3)
  } else {
    p_te_d[i] = 0
  }
}
summary(p_te_d)
hist(p_te_d)
p_te_d_no_zeroes = p_te_d[which(p_te_d != 0)]
summary(p_te_d_no_zeroes)
hist(p_te_d_no_zeroes)
merged_data$p_te_d = p_te_d
p_te_d_df = merged_data %>%
  filter(p_te_d != 0)
distance_plot = ggplot(data=p_te_d_df) +
  geom_line(aes(x=distance_km,y=p_te_d),linetype = "dashed") +
  xlab("Distance between human infection and mosquito collection (Km)") +
  ylab("Probability of tranmission for distance") +
  geom_vline(xintercept = 0.661,color="dark red") + 
  scale_x_continuous(limits = c(0,3)) +
  theme_bw() +
  theme(text = element_text(size=25))
distance_plot
ggsave(distance_plot, filename="/Users/kelseysumner/Desktop/theoretical_distance_distribution_plot.png", device="png",
       height=8, width=12, units="in", dpi=500)


# then combine p_te_a and p_te_c and rescale
p_te_a_c_combo = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$p_te_a[i] != 0 & merged_data$p_te_c[i] != 0){
    p_te_a_c_combo[i] = 1-(1-merged_data$p_te_a[i])*(1-merged_data$p_te_c[i])
  } else if (merged_data$p_te_a[i] != 0 & merged_data$p_te_c[i] == 0){
    p_te_a_c_combo[i] = 1-(1-merged_data$p_te_a[i])
  } else if (merged_data$p_te_a[i] == 0 & merged_data$p_te_c[i] != 0){
    p_te_a_c_combo[i] = 1-(1-merged_data$p_te_c[i])
  } else{
    p_te_a_c_combo[i] = 0
  }
}
summary(p_te_a_c_combo)
merged_data$p_te_a_c_combo = p_te_a_c_combo
length(which(p_te_a_c_combo == 0)) # 60797
length(which(merged_data$p_te_a == 0 & merged_data$p_te_c == 0)) # 60797
# rescale for p_te_a_c_combo to be between 0 and 1
merged_data$rescaled_p_te_a_c_combo = (merged_data$p_te_a_c_combo-min(merged_data$p_te_a_c_combo))/(max(merged_data$p_te_a_c_combo)-min(merged_data$p_te_a_c_combo))
hist(merged_data$p_te_a_c_combo)
hist(merged_data$rescaled_p_te_a_c_combo)
summary(merged_data$rescaled_p_te_a_c_combo)


# make the original final variable that is P(TEall)
# have that variable conditioned so you only calculate the probability of transmission if p_te is non-zero for all 4 variables
p_te_all = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$p_te_t[i] != 0 & merged_data$p_te_d[i] != 0 & merged_data$rescaled_p_te_a_c_combo[i] != 0){
    p_te_all[i] = merged_data$p_te_t[i]*merged_data$p_te_d[i]*merged_data$rescaled_p_te_a_c_combo[i]
  } else {
    p_te_all[i] = 0
  }
}
summary(p_te_all)
merged_data$p_te_all = p_te_all
length(which(p_te_all == 0)) # 168392
length(which(p_te_all > 0)) # 2262


# first create a count of whether or not mosquitoes collected within 7 days of the human sample
mosquito_week_count = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  count = 0
  for (j in 1:nrow(mosquito_data)){
    if ((mosquito_data$collection_date[j]-merged_data$human_date[i] <= 7) & (mosquito_data$collection_date[j]-merged_data$human_date[i] >= 0)){
      count = count + 1
    }
  }
  mosquito_week_count[i] = count
}
# add the new variable to the data set
merged_data$mosquito_week_count = mosquito_week_count
summary(merged_data$mosquito_week_count)
# remember: this variable looks at all mosquitoes collected across all three villages within that week


# check the covariates
str(merged_data$sample_id_human)
str(merged_data$HH_ID_human)
str(merged_data$unq_memID)
str(merged_data$p_te_all)
str(merged_data$age_cat_baseline)
merged_data$age_cat_baseline = as.factor(merged_data$age_cat_baseline)
str(merged_data$village_name)
merged_data$village_name = as.factor(merged_data$village_name)
merged_data$village_name = relevel(merged_data$village_name,ref = "Maruti")
str(merged_data$aim2_exposure)
merged_data$aim2_exposure = as.factor(merged_data$aim2_exposure)
merged_data$aim2_exposure = relevel(merged_data$aim2_exposure,ref = "symptomatic infection")

# create a variable for parasite density cubed
merged_data$pfr364Q_std_combined_rescaled = scale(merged_data$pfr364Q_std_combined)
summary(merged_data$pfr364Q_std_combined_rescaled)
merged_data$pfr364Q_std_combined_cubic = merged_data$pfr364Q_std_combined_rescaled*merged_data$pfr364Q_std_combined_rescaled*merged_data$pfr364Q_std_combined_rescaled
summary(merged_data$pfr364Q_std_combined_cubic)

# make a variable for the rescaled, centered cubic form of the mosquito week counts
merged_data$mosquito_week_count_rescaled = scale(merged_data$mosquito_week_count)
merged_data$mosquito_week_count_cubic_rescaled = merged_data$mosquito_week_count_rescaled*merged_data$mosquito_week_count_rescaled*merged_data$mosquito_week_count_rescaled
summary(merged_data$mosquito_week_count_cubic_rescaled)
hist(merged_data$mosquito_week_count_cubic_rescaled)
merged_data$mosquito_week_count_quad_rescaled = merged_data$mosquito_week_count_rescaled*merged_data$mosquito_week_count_rescaled
summary(merged_data$mosquito_week_count_quad_rescaled)
summary(merged_data$mosquito_week_count_rescaled)

# write out the data set 
# write_rds(merged_data,"spat21_aim2_sensitivity_analysis_data_set_2FEB2020.rds")
merged_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/time sensitivity analysis data sets/spat21_aim2_sensitivity_analysis_data_set_2FEB2020.rds")

#### --------- create a function to look at different time codings ----------- ####

# now write the function to calculate P_te_t between -30 and 0 days
p_te_t_sensivity_analysis = function(x) {
  
  # step 1: calculate p_te_t
  p_te_t = rep(NA,nrow(merged_data))
  for (i in 1:nrow(merged_data)){
    if (merged_data$date_difference_flipped[i] >= -30 & merged_data$date_difference_flipped[i] <= 0){
      p_te_t[i] = 1/(1+0.6*exp(-merged_data$date_difference_flipped[i]-x))
    } else {
      p_te_t[i] = 0
    }
  }
  
  # step 2: make a final variable that is P(TEall)
  # have that variable conditioned so you only calculate the probability of transmission if p_te is non-zero for all 4 variables
  p_te_all = rep(NA,nrow(merged_data))
  for (i in 1:nrow(merged_data)){
    if (p_te_t[i] != 0 & merged_data$p_te_d[i] != 0 & merged_data$rescaled_p_te_a_c_combo[i] != 0){
      p_te_all[i] = p_te_t[i]*merged_data$p_te_d[i]*merged_data$rescaled_p_te_a_c_combo[i]
    } else {
      p_te_all[i] = 0
    }
  }
  return(p_te_all)
}


# rewrite the function to have it export just the p_te_t values
p_te_t_sensivity_analysis_part2 = function(x) {
  
  # step 1: calculate p_te_t
  p_te_t = rep(NA,nrow(merged_data))
  for (i in 1:nrow(merged_data)){
    if (merged_data$date_difference_flipped[i] >= -30 & merged_data$date_difference_flipped[i] <= 0){
      p_te_t[i] = 1/(1+0.6*exp(-merged_data$date_difference_flipped[i]-x))
    } else {
      p_te_t[i] = 0
    }
  }
  return(p_te_t)
}


#### -------- now run the function to do a sensitivity analysis of time -------- ####

# slowly flatten the line going from -14 to -30 to return p_te_all
merged_data$p_te_all_17 = p_te_t_sensivity_analysis(17)
merged_data$p_te_all_18 = p_te_t_sensivity_analysis(18)
merged_data$p_te_all_19 = p_te_t_sensivity_analysis(19)
merged_data$p_te_all_20 = p_te_t_sensivity_analysis(20)
merged_data$p_te_all_21 = p_te_t_sensivity_analysis(21)
merged_data$p_te_all_22 = p_te_t_sensivity_analysis(22)
merged_data$p_te_all_23 = p_te_t_sensivity_analysis(23)
merged_data$p_te_all_24 = p_te_t_sensivity_analysis(24)
merged_data$p_te_all_25 = p_te_t_sensivity_analysis(25)
merged_data$p_te_all_26 = p_te_t_sensivity_analysis(26)
merged_data$p_te_all_27 = p_te_t_sensivity_analysis(27)
merged_data$p_te_all_28 = p_te_t_sensivity_analysis(28)
merged_data$p_te_all_29 = p_te_t_sensivity_analysis(29)
merged_data$p_te_all_30 = p_te_t_sensivity_analysis(30)
merged_data$p_te_all_31 = p_te_t_sensivity_analysis(31)
merged_data$p_te_all_32 = p_te_t_sensivity_analysis(32)


# slowly flatten the line going from -14 to -30 to return p_te_t
merged_data$p_te_t_17 = p_te_t_sensivity_analysis_part2(17)
merged_data$p_te_t_18 = p_te_t_sensivity_analysis_part2(18)
merged_data$p_te_t_19 = p_te_t_sensivity_analysis_part2(19)
merged_data$p_te_t_20 = p_te_t_sensivity_analysis_part2(20)
merged_data$p_te_t_21 = p_te_t_sensivity_analysis_part2(21)
merged_data$p_te_t_22 = p_te_t_sensivity_analysis_part2(22)
merged_data$p_te_t_23 = p_te_t_sensivity_analysis_part2(23)
merged_data$p_te_t_24 = p_te_t_sensivity_analysis_part2(24)
merged_data$p_te_t_25 = p_te_t_sensivity_analysis_part2(25)
merged_data$p_te_t_26 = p_te_t_sensivity_analysis_part2(26)
merged_data$p_te_t_27 = p_te_t_sensivity_analysis_part2(27)
merged_data$p_te_t_28 = p_te_t_sensivity_analysis_part2(28)
merged_data$p_te_t_29 = p_te_t_sensivity_analysis_part2(29)
merged_data$p_te_t_30 = p_te_t_sensivity_analysis_part2(30)
merged_data$p_te_t_31 = p_te_t_sensivity_analysis_part2(31)
merged_data$p_te_t_32 = p_te_t_sensivity_analysis_part2(32)


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
setdiff(ppts_to_censor$sample_name_final,merged_data$sample_name_final)
length(unique(merged_data$sample_name_final))
length(unique(merged_data$sample_id_human))
length(which(merged_data$sample_name_final %in% ppts_to_censor$sample_name_final))
merged_data = merged_data[-which(merged_data$sample_name_final %in% ppts_to_censor$sample_name_final),]
merged_data = merged_data[-which(merged_data$sample_name_final == "K05-230518-4-R"),]
length(unique(merged_data$sample_name_final))
length(unique(merged_data$sample_id_human))

# now make separate data sets for each sensitivity analysis value
original_data = merged_data %>%
  filter(p_te_d > 0 & p_te_t > 0)
data_17 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_17 > 0)
data_18 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_18 > 0)
data_19 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_19 > 0)
data_20 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_20 > 0)
data_21 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_21 > 0)
data_22 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_22 > 0)
data_23 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_23 > 0)
data_24 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_24 > 0)
data_25 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_25 > 0)
data_26 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_26 > 0)
data_27 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_27 > 0)
data_28 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_28 > 0)
data_29 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_29 > 0)
data_30 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_30 > 0)
data_31 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_31 > 0)
data_32 = merged_data %>%
  filter(p_te_d > 0 & p_te_t_32 > 0)


#### -------- now rerun the model to get new ORs for different time distributions ------- ####

# run the models for the new p_te_t estimates
# note: sensitivity analysis models have 5218 human-mosquito pair observations
model_original <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = original_data, control = glmerControl(optimizer="bobyqa"))
model_17 <- glmer(p_te_all_17~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_17, control = glmerControl(optimizer="bobyqa"))
model_18 <- glmer(p_te_all_18~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_18, control = glmerControl(optimizer="bobyqa"))
model_19 <- glmer(p_te_all_19~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_19, control = glmerControl(optimizer="bobyqa"))
model_20 <- glmer(p_te_all_20~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_20, control = glmerControl(optimizer="bobyqa"))
model_21 <- glmer(p_te_all_21~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_21, control = glmerControl(optimizer="bobyqa"))
model_22 <- glmer(p_te_all_22~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_22, control = glmerControl(optimizer="bobyqa"))
model_23 <- glmer(p_te_all_23~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_23, control = glmerControl(optimizer="bobyqa"))
model_24 <- glmer(p_te_all_24~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_24, control = glmerControl(optimizer="bobyqa"))
model_25 <- glmer(p_te_all_25~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_25, control = glmerControl(optimizer="bobyqa"))
model_26 <- glmer(p_te_all_26~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_26, control = glmerControl(optimizer="bobyqa"))
model_27 <- glmer(p_te_all_27~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_27, control = glmerControl(optimizer="bobyqa"))
model_28 <- glmer(p_te_all_28~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_28, control = glmerControl(optimizer="bobyqa"))
model_29 <- glmer(p_te_all_29~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_29, control = glmerControl(optimizer="bobyqa"))
model_30 <- glmer(p_te_all_30~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_30, control = glmerControl(optimizer="bobyqa"))
model_31 <- glmer(p_te_all_31~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_31, control = glmerControl(optimizer="bobyqa"))
model_32 <- glmer(p_te_all_32~aim2_exposure+age_cat_baseline+mosquito_week_count_rescaled+mosquito_week_count_quad_rescaled+mosquito_week_count_cubic_rescaled+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = data_32, control = glmerControl(optimizer="bobyqa"))

# pull out the estimates
# model original
summary(model_original)
exp(1.020800)
exp(confint(model_original, method="Wald"))
# day 17
summary(model_17)
exp(0.89263)
exp(confint(model_17, method="Wald"))
# day 18
summary(model_18)
exp(0.83631)
exp(confint(model_18, method="Wald"))
# day 19
summary(model_19)
exp(0.87228)
exp(confint(model_19, method="Wald"))
# day 20
summary(model_20)
exp(0.89639)
exp(confint(model_20, method="Wald"))
# day 21
summary(model_21)
exp(0.88070)
exp(confint(model_21, method="Wald"))
# day 22
summary(model_22)
exp(0.85781)
exp(confint(model_22, method="Wald"))
# day 23
summary(model_23)
exp(0.75121)
exp(confint(model_23, method="Wald"))
# day 24
summary(model_24)
exp(0.74038)
exp(confint(model_24, method="Wald"))
# day 25
summary(model_25)
exp(0.74195)
exp(confint(model_25, method="Wald"))
# day 26
summary(model_26)
exp(0.75502)
exp(confint(model_26, method="Wald"))
# day 27
summary(model_27)
exp(0.77803)
exp(confint(model_27, method="Wald"))
# day 28
summary(model_28)
exp(0.85387)
exp(confint(model_28, method="Wald"))
# day 29
summary(model_29)
exp(0.98737)
exp(confint(model_29, method="Wald"))
# day 30
summary(model_30)
exp(0.99146)
exp(confint(model_30, method="Wald"))
# day 31
summary(model_31)
exp(1.03872)
exp(confint(model_31, method="Wald"))



#### ------- now make two plots for the time sensitivity analysis ------- ####

# first plot the p_te_t values tested
p_te_t_sensitivity_plot = ggplot(data=data_17) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t),colour="black",lwd=2.5) +
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
  geom_line(aes(x=date_difference_flipped,y=p_te_t_31),colour="#3f007d",lwd=1.5) +
  xlab("Days between human infection and mosquito collection") +
  ylab("Probability of tranmission for time") +
  scale_x_continuous(breaks=c(0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,-30)) + 
  theme_bw()
p_te_t_sensitivity_plot
# export the plot
ggsave(p_te_t_sensitivity_plot, filename="/Users/kelseysumner/Desktop/time_sensitivity_analysis_x_axis_plot.png", device="png",
       height=7, width=8, units="in", dpi=500)

# then plot the new odds ratios across values of p_te_t
# read in the model results
model_results = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/computational_model_materials/time_sensitivity_analysis.csv")
model_results$day = c(-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29)
# make a plot of the model results
model_plot_smooth = ggplot(data=model_results,aes(x=factor(day),y=estimate,group=1),cex=1.5,col="#762a83") +
  geom_smooth(data=model_results,aes(x=factor(day),y=estimate,group=1),cex=1.5,col="#762a83",fill="#762a83") +
  theme_bw() +
  xlab("Probability decreases at this day") +
  ylab("Odds ratio (95% CI)") + 
  scale_y_continuous(breaks=c(0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0),trans="log10") +
  geom_hline(yintercept=1,linetype="dashed") + 
  coord_flip() +
  theme(text = element_text(size=25)) 
model_plot_smooth
# another way to plot this
model_plot = ggplot(data=model_results,aes(x=factor(day),y=estimate,group=1),cex=1.5,col="#762a83") +
  geom_line(data=model_results,aes(x=factor(day),y=estimate,group=1),cex=1.5,col="#762a83") +
  geom_ribbon(data=model_results,aes(x=factor(day),ymin = lower_ci, ymax = upper_ci),alpha=0.2,fill="#762a83") +
  theme_bw() +
  xlab("Probability decreases at this day") + 
  ylab("Odds ratio (95% CI)") + 
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12),trans="log10") +
  geom_hline(yintercept=1,linetype="dashed") + 
  coord_flip()
model_plot
# export the plot
ggsave(model_plot, filename="/Users/kelseysumner/Desktop/time_sensitivity_analysis_model_plot.png", device="png",
       height=7, width=8, units="in", dpi=500)
ggsave(model_plot_smooth, filename="/Users/kelseysumner/Desktop/time_sensitivity_analysis_model_plot_smooth.png", device="png",
       height=7, width=8, units="in", dpi=500)

