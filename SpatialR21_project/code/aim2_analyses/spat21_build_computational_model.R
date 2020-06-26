# -------------------------------------- #
#           Spat21/Mozzie Study          #
#       Build computational model        #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#          December 3, 2019              #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)
library(betareg)
library(lme4)
library(ggplot2)



#### ----- read in the data sets ----- ####

# read in the combined ama and csp data set for mosquito abdomens
edgelist_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA_and_CSP/final/full data/spat21_aim2_merged_data_with_weights_full_data_5MAR2020.rds")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")

# read in the mosquito demographic data
mosquito_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the clean ama haplotype data
ama_haplotypes <- read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")

# read in the clean csp haplotype data
csp_haplotypes <- read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")



### ------ check to see who had antimalarials recently ------- ####

# remove malaria infections that occurred within 14 days of symptomatic infection where received treatment in study

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
setdiff(ppts_to_censor$sample_name_final,edgelist_data$sample_name_final)
length(unique(edgelist_data$sample_name_final))
length(unique(edgelist_data$sample_id_human))
length(which(edgelist_data$sample_name_final %in% ppts_to_censor$sample_name_final))
edgelist_data = edgelist_data[-which(edgelist_data$sample_name_final %in% ppts_to_censor$sample_name_final),]
edgelist_data = edgelist_data[-which(edgelist_data$sample_name_final == "K05-230518-4-R"),]
length(unique(edgelist_data$sample_name_final))
length(unique(edgelist_data$sample_id_human))
# 6 weren't removed - 4 didn't pass genotyping and 2 were censored out in processing


# create a distance file
distance_data = edgelist_data %>% select(HH_ID_human,HH_ID_mosquito,distance_km)
distance_data = unique(distance_data)
write_csv(distance_data,"Desktop/distance_data.csv")


#### --------- set up the data ---------- ####

# look at the column names
colnames(edgelist_data)

# make sure the exposure and outcome are coded correctly
str(edgelist_data$aim2_exposure)
edgelist_data$aim2_exposure = as.factor(edgelist_data$aim2_exposure)
str(edgelist_data$csp_haps_shared)
str(edgelist_data$ama_haps_shared)
str(edgelist_data$village_name)
edgelist_data$village_name = as.factor(edgelist_data$village_name)
str(edgelist_data$total_num_mosq_in_hh)
str(edgelist_data$age_cat_baseline)
edgelist_data$age_cat_baseline = as.factor(edgelist_data$age_cat_baseline)
str(edgelist_data$pfr364Q_std_combined)
edgelist_data$aim2_exposure = relevel(edgelist_data$aim2_exposure,"symptomatic infection")
str(edgelist_data$p_te_all)

# create a categorical variable for parasite density
length(which(is.na(edgelist_data$pfr364Q_std_combined))) # 0 missing
edgelist_data$pfr364Q_std_combined_cat = ifelse(edgelist_data$pfr364Q_std_combined < 100,"less than 100 p/uL","greater than or equal to 100 p/uL")
table(edgelist_data$pfr364Q_std_combined_cat,edgelist_data$pfr364Q_std_combined, useNA = "always")
table(edgelist_data$pfr364Q_std_combined_cat, useNA = "always")
edgelist_data$pfr364Q_std_combined_cat = as.factor(edgelist_data$pfr364Q_std_combined_cat)
edgelist_data$pfr364Q_std_combined_cat = relevel(edgelist_data$pfr364Q_std_combined_cat,ref="less than 100 p/uL")

# add in mosquito week count here! - maybe merge in using data set you already created with full edgelist over the weekend
# read in the full edgelist of mosquito week count into
mosquito_week_count_df = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/time sensitivity analysis data sets/spat21_aim2_sensitivity_analysis_data_set_2FEB2020.rds")
colnames(mosquito_week_count_df)
mosquito_week_count_df = mosquito_week_count_df %>%
  select(sample_id_human,sample_id_abdomen,mosquito_week_count)
test_data = left_join(edgelist_data,mosquito_week_count_df,by=c("sample_id_human","sample_id_abdomen"))
colnames(test_data)
length(which(is.na(test_data$mosquito_week_count)))
summary(test_data$mosquito_week_count)
edgelist_data = test_data

# also add in moi for csp and ama
# subset the csp_haplotypes data set to sample id and moi
csp_haplotypes = csp_haplotypes %>% 
  select(sample_name_dbs,haplotype_number) %>%
  rename("sample_id_human" = "sample_name_dbs","csp_moi"="haplotype_number")
# subset the ama_haplotypes data set to sample id and moi
ama_haplotypes = ama_haplotypes %>% 
  select(sample_name_dbs,haplotype_number) %>%
  rename("sample_id_human" = "sample_name_dbs","ama_moi"="haplotype_number")
# merge the data sets to get moi
edgelist_data = left_join(edgelist_data,csp_haplotypes,by="sample_id_human")
length(which(is.na(edgelist_data$csp_moi)))
length(which(is.na(edgelist_data$csp_haps_shared)))
str(edgelist_data$csp_moi)
edgelist_data = left_join(edgelist_data,ama_haplotypes,by="sample_id_human")
length(which(is.na(edgelist_data$ama_moi)))
length(which(is.na(edgelist_data$ama_haps_shared)))
str(edgelist_data$ama_moi)
colnames(edgelist_data)

# create a variable for mean moi, averaging ama and csp participant moi
edgelist_data$mean_moi = rep(NA,nrow(edgelist_data))
for (i in 1:nrow(edgelist_data)){
  if (!(is.na(edgelist_data$ama_moi[i])) & !(is.na(edgelist_data$csp_moi[i]))){
    edgelist_data$mean_moi[i] = (edgelist_data$ama_moi[i] + edgelist_data$csp_moi[i])/2
  } else if (is.na(edgelist_data$ama_moi[i]) & !(is.na(edgelist_data$csp_moi[i]))){
    edgelist_data$mean_moi[i] = edgelist_data$csp_moi[i]
  } else {
    edgelist_data$mean_moi[i] = edgelist_data$ama_moi[i]
  }
}
summary(edgelist_data$mean_moi)
summary(edgelist_data$csp_moi)
summary(edgelist_data$ama_moi)
edgelist_data$ama_moi[1]
edgelist_data$csp_moi[1]
edgelist_data$mean_moi[1]
edgelist_data$ama_moi[2]
edgelist_data$csp_moi[2]
edgelist_data$mean_moi[2]
edgelist_data$ama_moi[100]
edgelist_data$csp_moi[100]
edgelist_data$mean_moi[100]
edgelist_data$ama_moi[1000]
edgelist_data$csp_moi[1000]
edgelist_data$mean_moi[1000]

# subset the data set to only look at observations where the probability of transmission > 0 based on distance and time
# (P(TEd) > 0 and P(TEt) > 0)
edgelist_data = edgelist_data %>%
  filter(p_te_d > 0 & p_te_t > 0)
summary(edgelist_data$p_te_all)
summary(edgelist_data$p_te_t)

# export the data set
write_csv(edgelist_data,"Desktop/spat21_aim2_computational_model_subset_data_6FEB2020.csv")
write_rds(edgelist_data,"Desktop/spat21_aim2_computational_model_subset_data_6FEB2020.rds")



#### ------ look at those that did and did not match prior to p_te_t and p_te_d subsetting ------- ####

# first make mosquito week count categorical
summary(edgelist_data$mosquito_week_count)
edgelist_data$mosquito_week_count_cat = ifelse(edgelist_data$mosquito_week_count < 50,"<50 mosquitoes ",
                                            ifelse(edgelist_data$mosquito_week_count >= 50 & edgelist_data$mosquito_week_count < 100,"50-99 mosquitoes","100-147 mosquitoes"))
table(edgelist_data$mosquito_week_count_cat, useNA = "always")

# create data sets of each category
edgelist_data$paired = ifelse(edgelist_data$p_te_d > 0 & edgelist_data$p_te_t > 0,"yes","no")
table(edgelist_data$paired,useNA = "always")
asymp_paired = edgelist_data %>%
  filter(aim2_exposure == "asymptomatic infection" & paired == "yes")
symp_paired = edgelist_data %>%
  filter(aim2_exposure == "symptomatic infection" & paired == "yes")
asymp_unpaired = edgelist_data %>%
  filter(aim2_exposure == "asymptomatic infection" & paired == "no")
symp_unpaired = edgelist_data %>%
  filter(aim2_exposure == "symptomatic infection" & paired == "no")

# look at the pairings across each variable
# p_te_all
mean(asymp_paired$p_te_all) ; sd(asymp_paired$p_te_all)
mean(symp_paired$p_te_all) ; sd(symp_paired$p_te_all)
mean(asymp_unpaired$p_te_all) ; sd(asymp_unpaired$p_te_all)
mean(symp_unpaired$p_te_all) ; sd(symp_unpaired$p_te_all)
# pfama1 moi
mean(asymp_paired$ama_moi, na.rm=T) ; sd(asymp_paired$ama_moi, na.rm=T)
mean(symp_paired$ama_moi, na.rm=T) ; sd(symp_paired$ama_moi, na.rm=T)
mean(asymp_unpaired$ama_moi, na.rm=T) ; sd(asymp_unpaired$ama_moi, na.rm=T)
mean(symp_unpaired$ama_moi, na.rm=T) ; sd(symp_unpaired$ama_moi, na.rm=T)
# pfcsp moi
mean(asymp_paired$csp_moi, na.rm=T) ; sd(asymp_paired$csp_moi, na.rm=T)
mean(symp_paired$csp_moi, na.rm=T) ; sd(symp_paired$csp_moi, na.rm=T)
mean(asymp_unpaired$csp_moi, na.rm=T) ; sd(asymp_unpaired$csp_moi, na.rm=T)
mean(symp_unpaired$csp_moi, na.rm=T) ; sd(symp_unpaired$csp_moi, na.rm=T)
# parasite density
mean(asymp_paired$pfr364Q_std_combined, na.rm=T) ; sd(asymp_paired$pfr364Q_std_combined, na.rm=T)
mean(symp_paired$pfr364Q_std_combined, na.rm=T) ; sd(symp_paired$pfr364Q_std_combined, na.rm=T)
mean(asymp_unpaired$pfr364Q_std_combined, na.rm=T) ; sd(asymp_unpaired$pfr364Q_std_combined, na.rm=T)
mean(symp_unpaired$pfr364Q_std_combined, na.rm=T) ; sd(symp_unpaired$pfr364Q_std_combined, na.rm=T)
# pfama1 haplotypes shared
mean(asymp_paired$ama_haps_shared, na.rm=T) ; sd(asymp_paired$ama_haps_shared, na.rm=T)
mean(symp_paired$ama_haps_shared, na.rm=T) ; sd(symp_paired$ama_haps_shared, na.rm=T)
mean(asymp_unpaired$ama_haps_shared, na.rm=T) ; sd(asymp_unpaired$ama_haps_shared, na.rm=T)
mean(symp_unpaired$ama_haps_shared, na.rm=T) ; sd(symp_unpaired$ama_haps_shared, na.rm=T)
# pfcsp haplotypes shared
mean(asymp_paired$csp_haps_shared, na.rm=T) ; sd(asymp_paired$csp_haps_shared, na.rm=T)
mean(symp_paired$csp_haps_shared, na.rm=T) ; sd(symp_paired$csp_haps_shared, na.rm=T)
mean(asymp_unpaired$csp_haps_shared, na.rm=T) ; sd(asymp_unpaired$csp_haps_shared, na.rm=T)
mean(symp_unpaired$csp_haps_shared, na.rm=T) ; sd(symp_unpaired$csp_haps_shared, na.rm=T)
# age 
table(asymp_paired$age_cat_baseline, useNA = "always")
table(symp_paired$age_cat_baseline, useNA = "always")
table(asymp_unpaired$age_cat_baseline, useNA = "always")
table(symp_unpaired$age_cat_baseline, useNA = "always")
# mosquito week count
table(asymp_paired$mosquito_week_count_cat, useNA = "always")
table(symp_paired$mosquito_week_count_cat, useNA = "always")
table(asymp_unpaired$mosquito_week_count_cat, useNA = "always")
table(symp_unpaired$mosquito_week_count_cat, useNA = "always")
# village
table(asymp_paired$village_name, useNA = "always")
table(symp_paired$village_name, useNA = "always")
table(asymp_unpaired$village_name, useNA = "always")
table(symp_unpaired$village_name, useNA = "always")
# mean moi
mean(asymp_paired$mean_moi, na.rm=T) ; sd(asymp_paired$mean_moi, na.rm=T)
mean(symp_paired$mean_moi, na.rm=T) ; sd(symp_paired$mean_moi, na.rm=T)
mean(asymp_unpaired$mean_moi, na.rm=T) ; sd(asymp_unpaired$mean_moi, na.rm=T)
mean(symp_unpaired$mean_moi, na.rm=T) ; sd(symp_unpaired$mean_moi, na.rm=T)
# p_te_t
mean(asymp_paired$p_te_t, na.rm=T) ; sd(asymp_paired$p_te_t, na.rm=T)
mean(symp_paired$p_te_t, na.rm=T) ; sd(symp_paired$p_te_t, na.rm=T)
mean(asymp_unpaired$p_te_t, na.rm=T) ; sd(asymp_unpaired$p_te_t, na.rm=T)
mean(symp_unpaired$p_te_t, na.rm=T) ; sd(symp_unpaired$p_te_t, na.rm=T)
# p_te_d
mean(asymp_paired$p_te_d, na.rm=T) ; sd(asymp_paired$p_te_d, na.rm=T)
mean(symp_paired$p_te_d, na.rm=T) ; sd(symp_paired$p_te_d, na.rm=T)
mean(asymp_unpaired$p_te_d, na.rm=T) ; sd(asymp_unpaired$p_te_d, na.rm=T)
mean(symp_unpaired$p_te_d, na.rm=T) ; sd(symp_unpaired$p_te_d, na.rm=T)
# p_te_a
mean(asymp_paired$p_te_a, na.rm=T) ; sd(asymp_paired$p_te_a, na.rm=T)
mean(symp_paired$p_te_a, na.rm=T) ; sd(symp_paired$p_te_a, na.rm=T)
mean(asymp_unpaired$p_te_a, na.rm=T) ; sd(asymp_unpaired$p_te_a, na.rm=T)
mean(symp_unpaired$p_te_a, na.rm=T) ; sd(symp_unpaired$p_te_a, na.rm=T)
# p_te_c
mean(asymp_paired$p_te_c, na.rm=T) ; sd(asymp_paired$p_te_c, na.rm=T)
mean(symp_paired$p_te_c, na.rm=T) ; sd(symp_paired$p_te_c, na.rm=T)
mean(asymp_unpaired$p_te_c, na.rm=T) ; sd(asymp_unpaired$p_te_c, na.rm=T)
mean(symp_unpaired$p_te_c, na.rm=T) ; sd(symp_unpaired$p_te_c, na.rm=T)




# calculate porportion of mosquitoes that had an asymptomatic infection with pairings
nrow(asymp_paired)/(nrow(asymp_paired) + nrow(asymp_unpaired))
nrow(asymp_paired)
(nrow(asymp_paired) + nrow(asymp_unpaired))

# calculate porportion of mosquitoes that had an symptomatic infection with pairings
nrow(symp_paired)/(nrow(symp_paired) + nrow(symp_unpaired))
nrow(symp_paired)
(nrow(symp_paired) + nrow(symp_unpaired))


#### ----- work with the regular models ------ ####

# look at distribution of p_te_all
hist(edgelist_data$p_te_all)
p_te_all_plot = ggplot(data=edgelist_data,aes(x=p_te_all)) +
  geom_density(alpha=0.6,fill="#e31a1c") +
  theme_bw() + 
  xlab("P(TE,all)")
p_te_all_plot
ggsave(p_te_all_plot, filename="/Users/kelseysumner/Desktop/p_te_all_plot_unstratified.png", device="png",
       height=4, width=7, units="in", dpi=500)
# looks very right-skewed
# could be a beta distribution
# options for models: 1. log binomial model or 2. beta model

# try a log-binomial model
regular_log_binomial = glm(p_te_all ~ aim2_exposure + age_cat_baseline + pfr364Q_std_combined_cat + village_name + aim2_exposure*age_cat_baseline, family=binomial(link="log"), data=edgelist_data)
summary(regular_log_binomial)

# try a logistic regression model
regular_logistic = glm(p_te_all ~ aim2_exposure + age_cat_baseline + pfr364Q_std_combined_cat + village_name + aim2_exposure*age_cat_baseline, family=binomial(link="logit"), data=edgelist_data)
summary(regular_logistic)

# try a poisson model
regular_poisson = glm(p_te_all ~ aim2_exposure + age_cat_baseline + pfr364Q_std_combined_cat + village_name + aim2_exposure*age_cat_baseline, family=c("poisson"), data=edgelist_data)
summary(regular_poisson)

# try a beta model
regular_beta = betareg(p_te_all ~ aim2_exposure + age_cat_baseline + pfr364Q_std_combined_cat + village_name + aim2_exposure*age_cat_baseline, data=edgelist_data)
summary(regular_beta)
# note: not an easy way to do multi-level beta models



#### ------- measure the outcome variable in different ways -------- ####

# look at a summary of the outcome variable
summary(edgelist_data$p_te_all)
length(which(is.na(edgelist_data$p_te_all)))
hist(edgelist_data$p_te_all)


# make a binary variable for <0.1 or >= 0.1
edgelist_data$outcome_binary_lessthan0.1 = ifelse(edgelist_data$p_te_all < 0.1,"less than 0.1","greater than 0.1")
table(edgelist_data$outcome_binary_lessthan0.1,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.1, useNA = "always")
edgelist_data$outcome_binary_lessthan0.1 = factor(edgelist_data$outcome_binary_lessthan0.1)
levels(edgelist_data$outcome_binary_lessthan0.1)
edgelist_data$outcome_binary_lessthan0.1 = relevel(edgelist_data$outcome_binary_lessthan0.1,ref = "less than 0.1")

# make a binary variable for <0.2 or >= 0.2
edgelist_data$outcome_binary_lessthan0.2 = ifelse(edgelist_data$p_te_all < 0.2,"less than 0.2","greater than 0.2")
table(edgelist_data$outcome_binary_lessthan0.2,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.2, useNA = "always")
edgelist_data$outcome_binary_lessthan0.2 = factor(edgelist_data$outcome_binary_lessthan0.2)
levels(edgelist_data$outcome_binary_lessthan0.2)
edgelist_data$outcome_binary_lessthan0.2 = relevel(edgelist_data$outcome_binary_lessthan0.2,ref = "less than 0.2")

# make a binary variable for <0.3 or >= 0.3
edgelist_data$outcome_binary_lessthan0.3 = ifelse(edgelist_data$p_te_all < 0.3,"less than 0.3","greater than 0.3")
table(edgelist_data$outcome_binary_lessthan0.3,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.3, useNA = "always")
edgelist_data$outcome_binary_lessthan0.3 = factor(edgelist_data$outcome_binary_lessthan0.3)
levels(edgelist_data$outcome_binary_lessthan0.3)
edgelist_data$outcome_binary_lessthan0.3 = relevel(edgelist_data$outcome_binary_lessthan0.3,ref = "less than 0.3")

# make a binary variable for <0.4 or >= 0.4
edgelist_data$outcome_binary_lessthan0.4 = ifelse(edgelist_data$p_te_all < 0.4,"less than 0.4","greater than 0.4")
table(edgelist_data$outcome_binary_lessthan0.4,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.4, useNA = "always")
edgelist_data$outcome_binary_lessthan0.4 = factor(edgelist_data$outcome_binary_lessthan0.4)
levels(edgelist_data$outcome_binary_lessthan0.4)
edgelist_data$outcome_binary_lessthan0.4 = relevel(edgelist_data$outcome_binary_lessthan0.4,ref = "less than 0.4")

# make a binary variable for <0.5 or >= 0.5
edgelist_data$outcome_binary_lessthan0.5 = ifelse(edgelist_data$p_te_all < 0.5,"less than 0.5","greater than 0.5")
table(edgelist_data$outcome_binary_lessthan0.5,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.5, useNA = "always")
edgelist_data$outcome_binary_lessthan0.5 = factor(edgelist_data$outcome_binary_lessthan0.5)
levels(edgelist_data$outcome_binary_lessthan0.5)
edgelist_data$outcome_binary_lessthan0.5 = relevel(edgelist_data$outcome_binary_lessthan0.5,ref = "less than 0.5")

# make a binary variable for <0.6 or >= 0.6
edgelist_data$outcome_binary_lessthan0.6 = ifelse(edgelist_data$p_te_all < 0.6,"less than 0.6","greater than 0.6")
table(edgelist_data$outcome_binary_lessthan0.6,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.6, useNA = "always")
edgelist_data$outcome_binary_lessthan0.6 = factor(edgelist_data$outcome_binary_lessthan0.6)
levels(edgelist_data$outcome_binary_lessthan0.6)
edgelist_data$outcome_binary_lessthan0.6 = relevel(edgelist_data$outcome_binary_lessthan0.6,ref = "less than 0.6")

# make a binary variable for <0.7 or >= 0.7
edgelist_data$outcome_binary_lessthan0.7 = ifelse(edgelist_data$p_te_all < 0.7,"less than 0.7","greater than 0.7")
table(edgelist_data$outcome_binary_lessthan0.7,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.7, useNA = "always")
edgelist_data$outcome_binary_lessthan0.7 = factor(edgelist_data$outcome_binary_lessthan0.7)
levels(edgelist_data$outcome_binary_lessthan0.7)
edgelist_data$outcome_binary_lessthan0.7 = relevel(edgelist_data$outcome_binary_lessthan0.7,ref = "less than 0.7")

# make a binary variable for <0.8 or >= 0.8
edgelist_data$outcome_binary_lessthan0.8 = ifelse(edgelist_data$p_te_all < 0.8,"less than 0.8","greater than 0.8")
table(edgelist_data$outcome_binary_lessthan0.8,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.8, useNA = "always")
edgelist_data$outcome_binary_lessthan0.8 = factor(edgelist_data$outcome_binary_lessthan0.8)
levels(edgelist_data$outcome_binary_lessthan0.8)
edgelist_data$outcome_binary_lessthan0.8 = relevel(edgelist_data$outcome_binary_lessthan0.8,ref = "less than 0.8")

# make a binary variable for <0.9 or >= 0.9
edgelist_data$outcome_binary_lessthan0.9 = ifelse(edgelist_data$p_te_all < 0.9,"less than 0.9","greater than 0.9")
table(edgelist_data$outcome_binary_lessthan0.9,edgelist_data$p_te_all,useNA = "always")
table(edgelist_data$outcome_binary_lessthan0.9, useNA = "always")
edgelist_data$outcome_binary_lessthan0.9 = factor(edgelist_data$outcome_binary_lessthan0.9)
levels(edgelist_data$outcome_binary_lessthan0.9)
edgelist_data$outcome_binary_lessthan0.9 = relevel(edgelist_data$outcome_binary_lessthan0.9,ref = "less than 0.9")



# make a categorical variable for the outcome with 5 categories
edgelist_data$outcome_categorical_5 = ifelse(edgelist_data$p_te_all < 0.2,"less than 0.2",
                                           ifelse(edgelist_data$p_te_all >= 0.2 & edgelist_data$p_te_all < 0.4,"0.2 to <0.4",
                                                  ifelse(edgelist_data$p_te_all >= 0.4 & edgelist_data$p_te_all < 0.6, "0.4 to <0.6",
                                                         ifelse(edgelist_data$p_te_all >= 0.6 & edgelist_data$p_te_all < 0.8, "0.6 to <0.8",
                                                                "0.8 to 1.00"))))
table(edgelist_data$outcome_categorical_5, useNA = "always")
edgelist_data$outcome_categorical_5 = factor(edgelist_data$outcome_categorical_5)
levels(edgelist_data$outcome_categorical_5)
edgelist_data$outcome_categorical_5 = relevel(edgelist_data$outcome_categorical_5,"less than 0.2")


# make a categorical variable for the outcome with 4 categories
edgelist_data$outcome_categorical_4 = ifelse(edgelist_data$p_te_all < 0.25,"less than 0.25",
                                           ifelse(edgelist_data$p_te_all >= 0.25 & edgelist_data$p_te_all < 0.5,"0.25 to <0.5",
                                                  ifelse(edgelist_data$p_te_all >= 0.5 & edgelist_data$p_te_all < 0.75, "0.5 to <0.75",
                                                                "0.75 to 1.00")))
table(edgelist_data$outcome_categorical_4, useNA = "always")
edgelist_data$outcome_categorical_4 = factor(edgelist_data$outcome_categorical_4)
levels(edgelist_data$outcome_categorical_4)
edgelist_data$outcome_categorical_4 = relevel(edgelist_data$outcome_categorical_4,"less than 0.25")







#### -------- run some multi-level models that these different outcomes ------ ####

## first run all the models with the full set of covariates

# continuous proportion outcome with a logistic model
model1 <- glmer(p_te_all~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model1)
# did not work - singular fit

# binary outcome <0.1 with a logistic model
model.1 <- glmer(outcome_binary_lessthan0.1~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model.1)
exp(0.26213)
exp(confint(model.1, devtol=c(1e-7)))
# converged

# binary outcome <0.2 with a logistic model
model.2 <- glmer(outcome_binary_lessthan0.2~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model.2)
exp(0.19470)
exp(confint(model.2, devtol=c(1e-7)))
# converged

# binary outcome <0.3 with a logistic model
model.3 <- glmer(outcome_binary_lessthan0.3~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model.3)
exp(0.12142)
exp(confint(model.3, devtol=c(1e-7)))
# converged

# binary outcome <0.4 with a logistic model
model.4 <- glmer(outcome_binary_lessthan0.4~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model.4)
exp(0.3378)
exp(confint(model.4, devtol=c(1e-7)))
# converged

# binary outcome <0.5 with a logistic model
model.5 <- glmer(outcome_binary_lessthan0.5~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model.5)
exp(0.9646)
exp(confint(model.5, devtol=c(1e-7)))
# converged

# binary outcome <0.6 with a logistic model
model.6 <- glmer(outcome_binary_lessthan0.6~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model.6)
exp(0.9859)
exp(confint(model.6, devtol=c(1e-7)))
# converged

# binary outcome <0.7 with a logistic model
model.7 <- glmer(outcome_binary_lessthan0.7~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model.7)
exp(1.0666)
exp(confint(model.7, devtol=c(1e-7)))
# converged

# binary outcome <0.8 with a logistic model
model.8 <- glmer(outcome_binary_lessthan0.8~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model.8)
exp(0.9314)
exp(confint(model.8, devtol=c(1e-6)))
# converged

# binary outcome <0.9 with a logistic model
model.9 <- glmer(outcome_binary_lessthan0.9~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(model.9)
# model did not converge

# categorical outcome with a logistic model - 4 categories
modelcat_4 <- glmer(outcome_categorical_4~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(modelcat_4)
exp(0.09023)
exp(confint(modelcat_4, devtol=c(1e-7)))
# converged

# categorical outcome with a logistic model - 5 categories
modelcat_5 <- glmer(outcome_categorical_5~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = edgelist_data)
summary(modelcat_5)
exp(0.19470)
exp(confint(modelcat_5, devtol=c(1e-7)))
# converged

# read in the model results
model_results = read_csv("Desktop/computational_model_materials/aim2_binary_outcome_sensitivity_analysis.csv")

# subset to just binary results for now
model_results_binary = model_results[-which(model_results$binary_outcome == "4 categories" | model_results$binary_outcome == "5 categories"),]

# make a plot of the results
model_results$binary_outcome = as.factor(model_results$binary_outcome)
model_plot = ggplot(data=model_results_binary, aes(x=binary_outcome, y=estimate)) + 
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.1) +
  geom_line() +
  geom_point() + 
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  theme_bw() +
  ylab("Point estimate - Odds ratio") +
  xlab("Binary outcome coding") + 
  coord_flip() +
  geom_hline(yintercept=1,linetype="dashed")
model_plot
ggsave(model_plot, filename="/Users/kelseysumner/Desktop/model_plot.png", device="png",
       height=7, width=8, units="in", dpi=500)







