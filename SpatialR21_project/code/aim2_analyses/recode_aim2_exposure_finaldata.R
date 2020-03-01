# --------------------------------------------- #
#       Recode main exposure in final data      #
#             Mozzie Phase 1                    #
#              March 1, 2020                    #
#                K. Sumner                      #
# --------------------------------------------- #



#### --------- load packages ----------------- ####
library(tidyverse)



#### -------- read in the final_data human data set ------- ####

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")



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
               final_data$fever == "yes")) # 211 meet this
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
write_csv(final_data,"Desktop/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.csv")
write_rds(final_data,"Desktop/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")





