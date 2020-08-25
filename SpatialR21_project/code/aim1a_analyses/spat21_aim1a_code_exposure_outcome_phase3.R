# -------------------------------------- #
#           Spat21/Mozzie Study          #
#       Code exposure and outcome        #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#            August 18, 2020             #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)


#### ------- read in the data sets -------- ####

# read in the full data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/De-identified Phase II_v13/final_merged_data/phase3_spat21_human_final_censored_data_for_dissertation_18AUG2020.rds")

# read in the consecutive monthly follow-up data set
followup_data = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/Consecutive Follow-up Tables/phase3_aim1a_consecutive_follow_up_order_df_after_censoring_18AUG2020.csv")


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
  } 
}
# check the output
table(main_exposure_primary_case_def, useNA="always")
table(main_exposure_primary_case_def,final_data$pf_pcr_infection_status,useNA="always")
table(main_exposure_primary_case_def,final_data$rdt_rst,useNA="always")
length(which(final_data$visit_type == "monthly and sick visit" & final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
  !(final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
                (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                   final_data$Cough =="yes" | final_data$Other == "yes")))) # 13 that meet this
table(main_exposure_primary_case_def,final_data$visit_type, useNA="always")
length(which(final_data$visit_type == "monthly and sick visit" & final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
                   (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                      final_data$Cough =="yes" | final_data$Other == "yes"))) # 89 meet this
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
  }  
}
# check the output
table(main_exposure_secondary_stringent_case_def, useNA="always")
table(main_exposure_secondary_stringent_case_def,final_data$pf_pcr_infection_status,useNA="always")
table(main_exposure_secondary_stringent_case_def,final_data$rdt_rst,useNA="always")
table(main_exposure_secondary_stringent_case_def,final_data$fever,useNA="always")
length(which(final_data$visit_type == "monthly and sick visit" & final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
               !(final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
                   final_data$fever == "yes" ))) #  34 that meet this
table(main_exposure_secondary_stringent_case_def,final_data$visit_type, useNA="always")
length(which(final_data$visit_type == "monthly and sick visit" & (final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive" &
                  final_data$fever == "yes"))) # 68 meet this
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
                      final_data$Cough =="yes" | final_data$Other == "yes")))) # 32 that meet this
table(main_exposure_secondary_permissive_case_def,final_data$visit_type, useNA="always")
length(which(final_data$visit_type == "monthly and sick visit" & final_data$pf_pcr_infection_status == "positive"  &
                  (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                     final_data$Cough =="yes" | final_data$Other == "yes"))) # 146 meet this
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
                      final_data$Cough =="yes" | final_data$Other == "yes"))) # 310 that meet this
length(which((final_data$visit_type == "monthly and sick visit" | final_data$visit_type=="sick visit") & final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive"))
length(which((final_data$visit_type == "monthly and sick visit" | final_data$visit_type=="sick visit") &
               (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                  final_data$Cough =="yes" | final_data$Other == "yes"))) # 814 that meet this
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
                                                                    final_data$fever == "yes")) # 247 meet this
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
                  final_data$Cough =="yes" | final_data$Other == "yes"))) # 310 that meet this
length(which((final_data$visit_type == "monthly and sick visit" | final_data$visit_type=="sick visit") & final_data$pf_pcr_infection_status == "positive" & final_data$rdt_rst == "positive"))
length(which((final_data$visit_type == "monthly and sick visit" | final_data$visit_type=="sick visit") &
               (final_data$fever == "yes" | final_data$Aches == "yes" | final_data$Vomiting == "yes" | final_data$Diarrhea == "yes" | final_data$Chills == "yes" | final_data$congestion =="yes" |
                  final_data$Cough =="yes" | final_data$Other == "yes"))) # 814 that meet this
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
write_csv(final_data,"Desktop/phase3_spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_18AUG2020.csv")
write_rds(final_data,"Desktop/phase3_spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_18AUG2020.rds")









