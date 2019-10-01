# -------------------------------------- #
#           Spat21/Mozzie Study          #
# Make plot of survival for participants #
#                 Aim 1A                 #
#               Human Data               #
#                K. Sumner               #
#             July 16, 2019              #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)


#### ------- read in the data sets -------- ####

# read in the full data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_1OCT2019.rds")

# read in the consecutive monthly follow-up data set
followup_data = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/Consecutive Follow-up Tables/aim1a_consecutive_follow_up_order_df_after_censoring_1OCT2019.csv")


#### -------- code the three main exposures ------ ####

# add a variable for if had a least 1 symptom to the data set
had_at_least_1_symptom = rep(NA,nrow(final_data))
for (i in 1:nrow(final_data)){
  if ((final_data$visit_type[i] == "monthly and sick visit" | final_data$visit_type[i]=="sick visit") &
      (final_data$fever[i] == "yes" | final_data$Aches[i] == "yes" | final_data$Vomiting[i] == "yes" | final_data$Diarrhea[i] == "yes" | final_data$Chills[i] == "yes" | final_data$congestion[i] =="yes" |
       final_data$Cough[i] =="yes" | final_data$Other[i] == "yes")){
    had_at_least_1_symptom[i] = "yes"
  } else {
    had_at_least_1_symptom[i] = "no"
  }
}
table(had_at_least_1_symptom, useNA="always")
length(which((final_data$visit_type == "monthly and sick visit" | final_data$visit_type=="sick visit") &
               (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                  final_data$Cough =="yes" | final_data$Other == "yes"))) # 414 that meet this
final_data$had_at_least_1_symptom = had_at_least_1_symptom




# code the main exposure for the primary case definition
main_exposure_primary_case_def = rep(NA,nrow(final_data))
for (i in 1:nrow(final_data)){
  if (final_data$visit_type[i] == "monthly visit" & final_data$pf_pcr_infection_status[i] == "positive"){
    main_exposure_primary_case_def[i] = "asymptomatic infection"
  } else if (final_data$visit_type[i] == "monthly visit" & final_data$pf_pcr_infection_status[i] == "negative"){
    main_exposure_primary_case_def[i] = "no infection"
  } else if (final_data$visit_type[i] == "monthly and sick visit" & !(final_data$pf_pcr_infection_status[i] == "positive" & final_data$rdt_rst[i] == "positive" &
                                                                      (final_data$fever[i] == "yes" | final_data$Aches[i] == "yes" | final_data$Vomiting[i] == "yes" | final_data$Diarrhea[i] == "yes" | final_data$Chills[i] == "yes" | final_data$congestion[i] =="yes" |
                                                                       final_data$Cough[i] =="yes" | final_data$Other[i] == "yes"))) {
    main_exposure_primary_case_def[i] = "no infection"
  } 
}
# check the output
table(main_exposure_primary_case_def, useNA="always")
table(main_exposure_primary_case_def,final_data$pf_pcr_infection_status,useNA="always")
table(main_exposure_primary_case_def,final_data$rdt_rst,useNA="always")
length(which(final_data$visit_type == "monthly and sick visit" & final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
  !(final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
                (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                   final_data$Cough =="yes" | final_data$Other == "yes")))) # 6 that meet this
table(main_exposure_primary_case_def,final_data$visit_type, useNA="always")
length(which(final_data$visit_type == "monthly and sick visit" & final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
                   (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                      final_data$Cough =="yes" | final_data$Other == "yes"))) # 52 meet this
table(main_exposure_primary_case_def,final_data$had_at_least_1_symptom, useNA="always")
# looks good, add to the data set
final_data$main_exposure_primary_case_def = main_exposure_primary_case_def


# code the main exposure for secondary stringent case definition
main_exposure_secondary_stringent_case_def = rep(NA,nrow(final_data))
for (i in 1:nrow(final_data)){
  if (final_data$visit_type[i] == "monthly visit" & final_data$pf_pcr_infection_status[i] == "positive"){
    main_exposure_secondary_stringent_case_def[i] = "asymptomatic infection"
  } else if (final_data$visit_type[i] == "monthly visit" & final_data$pf_pcr_infection_status[i] == "negative"){
    main_exposure_secondary_stringent_case_def[i] = "no infection"
  } else if (final_data$visit_type[i] == "monthly and sick visit" & !(final_data$pf_pcr_infection_status[i] == "positive" & final_data$rdt_rst[i] == "positive" &
                                                                      final_data$fever[i] == "yes")) {
    main_exposure_secondary_stringent_case_def[i] = "no infection"
  } 
}
# check the output
table(main_exposure_secondary_stringent_case_def, useNA="always")
table(main_exposure_secondary_stringent_case_def,final_data$pf_pcr_infection_status,useNA="always")
table(main_exposure_secondary_stringent_case_def,final_data$rdt_rst,useNA="always")
table(main_exposure_secondary_stringent_case_def,final_data$fever,useNA="always")
length(which(final_data$visit_type == "monthly and sick visit" & final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
               !(final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
                   final_data$fever == "yes" ))) #  17 that meet this
table(main_exposure_secondary_stringent_case_def,final_data$visit_type, useNA="always")
length(which(final_data$visit_type == "monthly and sick visit" & (final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
                  final_data$fever == "yes"))) # 41 meet this
table(main_exposure_secondary_stringent_case_def,final_data$had_at_least_1_symptom, useNA="always")
# looks good, add to the data set
final_data$main_exposure_secondary_stringent_case_def = main_exposure_secondary_stringent_case_def


# code the main exposure for secondary permissive case definition
main_exposure_secondary_permissive_case_def = rep(NA,nrow(final_data))
for (i in 1:nrow(final_data)){
  if (final_data$visit_type[i] == "monthly visit" & final_data$pf_pcr_infection_status[i] == "positive"){
    main_exposure_secondary_permissive_case_def[i] = "asymptomatic infection"
  } else if (final_data$visit_type[i] == "monthly visit" & final_data$pf_pcr_infection_status[i] == "negative"){
    main_exposure_secondary_permissive_case_def[i] = "no infection"
  } else if (final_data$visit_type[i] == "monthly and sick visit" & !(final_data$pf_pcr_infection_status[i] == "positive" & (final_data$fever[i] == "yes" | final_data$Aches[i] == "yes" | final_data$Vomiting[i] == "yes" | final_data$Diarrhea[i] == "yes" | final_data$Chills[i] == "yes" | final_data$congestion[i] =="yes" |
                                                                       final_data$Cough[i] =="yes" | final_data$Other[i] == "yes"))) {
    main_exposure_secondary_permissive_case_def[i] = "no infection"
  } 
}
# check the output
table(main_exposure_secondary_permissive_case_def, useNA="always")
table(main_exposure_secondary_permissive_case_def,final_data$pf_pcr_infection_status,useNA="always")
table(main_exposure_secondary_permissive_case_def,final_data$rdt_rst,useNA="always")
table(main_exposure_secondary_permissive_case_def,final_data$fever,useNA="always")
length(which(final_data$visit_type == "monthly and sick visit" & final_data$pf_pcr_infection_status == "positive"  &
               !(final_data$pf_pcr_infection_status == "positive" &
                   (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                      final_data$Cough =="yes" | final_data$Other == "yes")))) # 19 that meet this
table(main_exposure_secondary_permissive_case_def,final_data$visit_type, useNA="always")
length(which(final_data$visit_type == "monthly and sick visit" & final_data$pf_pcr_infection_status == "positive"  &
                  (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                     final_data$Cough =="yes" | final_data$Other == "yes"))) # 89 meet this
table(main_exposure_secondary_permissive_case_def,final_data$had_at_least_1_symptom, useNA="always")
# looks good, add to the data set
final_data$main_exposure_secondary_permissive_case_def = main_exposure_secondary_permissive_case_def


#### --------- code the three main outcomes -------- ####

# code the main outcome for the primary case definition
main_outcome_primary_case_def = rep(NA,nrow(final_data))
for (i in 1:nrow(final_data)){
  if (final_data$visit_type[i] == "sick visit" & final_data$pf_pcr_infection_status[i] == "positive" & final_data$rdt_rst[i] == "positive" &
      (final_data$fever[i] == "yes" | final_data$Aches[i] == "yes" | final_data$Vomiting[i] == "yes" | final_data$Diarrhea[i] == "yes" | final_data$Chills[i] == "yes" | final_data$congestion[i] =="yes" |
       final_data$Cough[i] =="yes" | final_data$Other[i] == "yes")){
    main_outcome_primary_case_def[i] = "symptomatic infection"
  } else if (final_data$visit_type[i] == "monthly and sick visit" & final_data$pf_pcr_infection_status[i] == "positive" & final_data$rdt_rst[i] == "positive" &
                                                                      (final_data$fever[i] == "yes" | final_data$Aches[i] == "yes" | final_data$Vomiting[i] == "yes" | final_data$Diarrhea[i] == "yes" | final_data$Chills[i] == "yes" | final_data$congestion[i] =="yes" |
                                                                       final_data$Cough[i] =="yes" | final_data$Other[i] == "yes")) {
    main_outcome_primary_case_def[i] = "symptomatic infection"
  } 
}
# check the output
table(main_outcome_primary_case_def, useNA="always")
table(main_outcome_primary_case_def,final_data$pf_pcr_infection_status,useNA="always")
table(main_outcome_primary_case_def,final_data$rdt_rst,useNA="always")
length(which((final_data$visit_type == "monthly and sick visit" | final_data$visit_type=="sick visit") & final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
                   (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                      final_data$Cough =="yes" | final_data$Other == "yes"))) # 137 that meet this
length(which((final_data$visit_type == "monthly and sick visit" | final_data$visit_type=="sick visit") & final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive"))
length(which((final_data$visit_type == "monthly and sick visit" | final_data$visit_type=="sick visit") &
               (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                  final_data$Cough =="yes" | final_data$Other == "yes"))) # 414 that meet this
table(final_data$visit_type, useNA="always")
table(main_outcome_primary_case_def,final_data$visit_type, useNA="always")
table(main_outcome_primary_case_def,final_data$had_at_least_1_symptom, useNA="always")
length(which(final_data$had_at_least_1_symptom=="yes" & final_data$pf_pcr_infection_status=="positive" & final_data$rdt_rst=="positive"))
length(which(final_data$had_at_least_1_symptom=="yes" & !(final_data$pf_pcr_infection_status=="positive" & final_data$rdt_rst=="positive")))
# looks good, add to the data set
final_data$main_outcome_primary_case_def = main_outcome_primary_case_def


# code the main outcome for the secondary stringent
main_outcome_secondary_stringent_case_def = rep(NA,nrow(final_data))
for (i in 1:nrow(final_data)){
  if (final_data$visit_type[i] == "sick visit" & final_data$pf_pcr_infection_status[i] == "positive" & final_data$rdt_rst[i] == "positive" &
      (final_data$fever[i] == "yes")){
    main_outcome_secondary_stringent_case_def[i] = "symptomatic infection"
  } else if (final_data$visit_type[i] == "monthly and sick visit" & final_data$pf_pcr_infection_status[i] == "positive" & final_data$rdt_rst[i] == "positive" &
             (final_data$fever[i] == "yes")) {
    main_outcome_secondary_stringent_case_def[i] = "symptomatic infection"
  } 
}
# check the output
table(main_outcome_secondary_stringent_case_def, useNA="always")
table(main_outcome_secondary_stringent_case_def,final_data$pf_pcr_infection_status,useNA="always")
table(main_outcome_secondary_stringent_case_def,final_data$rdt_rst,useNA="always")
table(main_outcome_secondary_stringent_case_def,final_data$fever,useNA="always")
table(main_outcome_secondary_stringent_case_def,final_data$visit_type, useNA="always")
length(which(final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
                                                                    final_data$fever == "yes")) # 110 meet this
table(main_outcome_secondary_stringent_case_def,final_data$had_at_least_1_symptom, useNA="always")
# looks good, add to the data set
final_data$main_outcome_secondary_stringent_case_def = main_outcome_secondary_stringent_case_def

# code the main outcome for the secondary permissive case definition
main_outcome_secondary_permissive_case_def = rep(NA,nrow(final_data))
for (i in 1:nrow(final_data)){
  if (final_data$visit_type[i] == "sick visit" & final_data$pf_pcr_infection_status[i] == "positive" & 
      (final_data$fever[i] == "yes" | final_data$Aches[i] == "yes" | final_data$Vomiting[i] == "yes" | final_data$Diarrhea[i] == "yes" | final_data$Chills[i] == "yes" | final_data$congestion[i] =="yes" |
       final_data$Cough[i] =="yes" | final_data$Other[i] == "yes")){
    main_outcome_secondary_permissive_case_def[i] = "symptomatic infection"
  } else if (final_data$visit_type[i] == "monthly and sick visit" & final_data$pf_pcr_infection_status[i] == "positive" &
             (final_data$fever[i] == "yes" | final_data$Aches[i] == "yes" | final_data$Vomiting[i] == "yes" | final_data$Diarrhea[i] == "yes" | final_data$Chills[i] == "yes" | final_data$congestion[i] =="yes" |
              final_data$Cough[i] =="yes" | final_data$Other[i] == "yes")) {
    main_outcome_secondary_permissive_case_def[i] = "symptomatic infection"
  } 
}
# check the output
table(main_outcome_secondary_permissive_case_def, useNA="always")
table(main_outcome_secondary_permissive_case_def,final_data$pf_pcr_infection_status,useNA="always")
table(main_outcome_secondary_permissive_case_def,final_data$rdt_rst,useNA="always")
length(which((final_data$visit_type == "monthly and sick visit" | final_data$visit_type=="sick visit") & final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
               (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                  final_data$Cough =="yes" | final_data$Other == "yes"))) # 137 that meet this
length(which((final_data$visit_type == "monthly and sick visit" | final_data$visit_type=="sick visit") & final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive"))
length(which((final_data$visit_type == "monthly and sick visit" | final_data$visit_type=="sick visit") &
               (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                  final_data$Cough =="yes" | final_data$Other == "yes"))) # 414 that meet this
table(final_data$visit_type, useNA="always")
table(main_outcome_secondary_permissive_case_def,final_data$visit_type, useNA="always")
table(main_outcome_secondary_permissive_case_def,final_data$had_at_least_1_symptom, useNA="always")
length(which(final_data$had_at_least_1_symptom=="yes" & final_data$pf_pcr_infection_status=="positive"))
length(which(final_data$had_at_least_1_symptom=="yes" & !(final_data$pf_pcr_infection_status=="positive")))
table(main_outcome_secondary_permissive_case_def,final_data$pf_pcr_infection_status, useNA="always")
# looks good, add to the data set
final_data$main_outcome_secondary_permissive_case_def = main_outcome_secondary_permissive_case_def

# check the exposure and outcomes coding
# for the primary coding
table(final_data$main_exposure_primary_case_def,final_data$main_outcome_primary_case_def,useNA="always")
# for the secondary stringent coding
table(final_data$main_exposure_secondary_stringent_case_def,final_data$main_outcome_secondary_stringent_case_def,useNA="always")
# for the secondary permissive coding
table(final_data$main_exposure_secondary_permissive_case_def,final_data$main_outcome_secondary_permissive_case_def,useNA="always")
# all looks good

# export data sets
write_csv(final_data,"Desktop/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.csv")
write_rds(final_data,"Desktop/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")


#### ------- set up the data sets to be in survival format -------- ####

# sleeping in a space with a net regularly
# calculate how many people sleep under net regularly
participant_data = final_data %>%
  group_by(village_name,unq_memID) %>%
  summarize(slept_avg=mean(slept_times, na.rm =T))
# make a variable that indicates some slept under a net more than usual
slept_under_net_regularly = ifelse(is.na(participant_data$slept_avg),NA,ifelse(participant_data$slept_avg>5,"yes","no"))
table(slept_under_net_regularly,participant_data$slept_avg, useNA = "always")
participant_data$slept_under_net_regularly = as.factor(slept_under_net_regularly)
participant_data_v2 = participant_data %>%
  group_by(village_name, slept_under_net_regularly) %>%
  summarize(totaln = n())
participant_data_v2$slept_under_net_regularly=as.factor(participant_data_v2$slept_under_net_regularly)
# add the variables to the final data set with all observations
participant_data$village_name <- NULL
new_data = left_join(final_data,participant_data,by="unq_memID")
length(intersect(final_data$unq_memID,participant_data$unq_memID))
setdiff(participant_data$unq_memID,final_data$unq_memID)
final_data = new_data

# subset the data set to just the variables of interest for aims 1A
# when you set up the survival analysis data sets, will have to have separate data sets for different follow-up time 
# for the three outcome definitions
colnames(final_data)
# for the primary case definition
survival_data_primary = final_data %>%
  select(sample_name_final,age_all_baseline,gender,village_name,unq_memID,visit_type,slept_under_net_regularly,sample_id_date,main_exposure_primary_case_def,main_outcome_primary_case_def)
# for the secondary stringent case definition
survival_data_secondary_stringent = final_data %>%
  select(sample_name_final,age_all_baseline,gender,village_name,unq_memID,visit_type,slept_under_net_regularly,sample_id_date,main_exposure_secondary_stringent_case_def,main_outcome_secondary_stringent_case_def)
# for the secondary permissive case definition
survival_data_secondary_permissive = final_data %>%
  select(sample_name_final,age_all_baseline,gender,village_name,unq_memID,visit_type,slept_under_net_regularly,sample_id_date,main_exposure_secondary_permissive_case_def,main_outcome_secondary_permissive_case_def)

# look at the number of participants with more than one symptomatic infection
participant_data = final_data %>%
  filter(main_outcome_primary_case_def == "symptomatic infection") %>%
  group_by(unq_memID) %>%
  summarize(n=n()) %>%
  filter(n>1)

# for each case definition data set, decide on coding scheme for follow-up







