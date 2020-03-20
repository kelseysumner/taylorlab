# -------------------------------- #
# Look at within-person infections #
#         Mozzie phase 1           #
#             Aim 1B               #
#        March 19, 2020            #
#           K. Sumner              #
# -------------------------------- #

# will call repeated infections: "reinfection"

#### ------- load libraries -------- ####
library(tidyverse)



#### ------- read in the data set -------- ####

# load in the haplotype data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
csp_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")
ama_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")

# read in the data set of csp haplotypes within each sample
csp_haplotype_summary = read_rds("Desktop/clean_ids_haplotype_results/CSP/haplotype_summary/spat21_csp_summarized_haplotype_list_31DEC2019.rds")
ama_haplotype_summary = read_rds("Desktop/clean_ids_haplotype_results/AMA/haplotype_summary/spat21_ama_summarized_haplotype_list_31DEC2019.rds")

# read in the human demographic data
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")


#### -------- set up the data set -------- ####

# first cut down the human demographic data set to the variables of interest
colnames(csp_haplotype_summary)
colnames(final_data)
human_data = final_data %>%
  select(sample_name_dbs,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,age_all_baseline,age_cat_baseline,village_name)

# remove the mosquito samples from the data set
csp_haplotype_summary = csp_haplotype_summary %>%
  filter(!(str_detect(sample_name_dbs,"H")) & !(str_detect(sample_name_dbs,"A")) | sample_name_dbs == "K14-170717-1A-R")

# merge symptomatic status and other variables of interest into the data set (age, village, household)
csp_data = left_join(csp_haplotype_summary,human_data,by="sample_name_dbs")

# check the merge
length(which(is.na(csp_data$age_all_baseline)))
# looks good

# now create the outcome variable: symptomatic status
csp_data = csp_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  mutate(symptomatic_status = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def))) %>%
  select(-c(main_exposure_primary_case_def,main_outcome_primary_case_def))
table(csp_data$symptomatic_status, useNA="always")


#### -------- remove participants who had infections within 14 days taking antimalarials -------- ####

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
setdiff(ppts_to_censor$sample_name_final,csp_data$sample_name_dbs)
length(unique(csp_data$sample_name_dbs))
length(which(csp_data$sample_name_dbs %in% ppts_to_censor$sample_name_final))
csp_data = csp_data[-which(csp_data$sample_name_dbs %in% ppts_to_censor$sample_name_final),]
csp_data = csp_data[-which(csp_data$sample_name_dbs == "K05-230518-4-R"),]
length(unique(csp_data$sample_name_dbs))
# removed 20 participants in the csp merged data set out of the 26 that were in the full demographic data set


#### -------- categorize participants' infections over time based on haplotypes ------- ####

# first order the data set by date
csp_data = dplyr::arrange(csp_data, sample_id_date)

# write some code that looks at each participants' infections over time
haplotype_category = rep(NA,nrow(csp_data))
unique_participants = unique(csp_data$unq_memID)
for (i in 1:length(unique_participants)){
  person_list = c()
  for (j in 1:nrow(csp_data)){
    
    # look at just that person's infections
    if (unique_participants[i] == csp_data$unq_memID[j]){
      haps = str_split(csp_data$haplotype_list[j],",")[[1]]
      interim_category = ""

      # now look at specific haplotypes in that infection
      for (k in 1:length(haps)){
        if (haps[k] %in% person_list){
          one_piece = "o"
        } else {
          one_piece = "n"
        }
        interim_category = paste0(interim_category,one_piece)
        
      }
     
      # now check for old and new haplotypes
      if (str_detect(interim_category,"n") & str_detect(interim_category,"o")){
        haplotype_category[j] = "old and new"
      } else if (str_detect(interim_category,"n") & !(str_detect(interim_category,"o"))){
        haplotype_category[j] = "all new"
      } else if (str_detect(interim_category,"o") & !(str_detect(interim_category,"n"))) {
        haplotype_category[j] = "all old"
      } else {
        haplotype_category[j] = NA
      }

      # add to the person list
      person_list = c(person_list,haps)
    }
  }
}
# check the output
table(haplotype_category,useNA="always")
# add the new haplotype category to the data set
csp_data$haplotype_category = haplotype_category


