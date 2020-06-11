# -------------------------------- #
# Look at within-person infections #
#        coding peristent          #
#         Mozzie phase 1           #
#             Aim 1B               #
#          Jun 11, 2020            #
#           K. Sumner              #
# -------------------------------- #

# will call repeated infections: "reinfection"

#### ------- load libraries -------- ####
library(tidyverse)



#### ------- read in the data set -------- ####

# load in the haplotype data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
csp_haplotypes <- read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")
ama_haplotypes <- read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")

# read in the data set of csp haplotypes within each sample
csp_haplotype_summary = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/CSP/haplotype_summary/spat21_csp_summarized_haplotype_list_31DEC2019.rds")
ama_haplotype_summary = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA/haplotype_summary/spat21_ama_summarized_haplotype_list_31DEC2019.rds")

# read in the human demographic data
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")




####
## CREATE THE DATA SET FOR PFCSP
####


#### -------- set up the data set -------- ####

# remove the mosquito samples from the data set
csp_haplotype_summary = csp_haplotype_summary %>%
  filter(!(str_detect(sample_name_dbs,"H")) & !(str_detect(sample_name_dbs,"A")) | sample_name_dbs == "K14-170717-1A-R")

# make csp_data
csp_data = csp_haplotype_summary


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


#### ----- finish merging in the data sets --------- ####

# first cut down the human demographic data set to the variables of interest
colnames(csp_haplotype_summary)
colnames(final_data)
human_data = final_data %>%
  select(sample_name_dbs,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,age_all_baseline,age_cat_baseline,village_name,prescription)

# merge symptomatic status and other variables of interest into the data set (age, village, household)
csp_data = left_join(csp_data,human_data,by="sample_name_dbs")

# check the merge
length(which(is.na(csp_data$age_all_baseline)))
# looks good

# now create the outcome variable: symptomatic status
csp_data = csp_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  mutate(symptomatic_status = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def))) %>%
  select(-c(main_exposure_primary_case_def,main_outcome_primary_case_def))
table(csp_data$symptomatic_status, useNA="always")



#### ----- make a full data set of all visits for a person ordered by date ------ ####

# set up the data set for tabulating by visit
final_data$count = rep(1,nrow(final_data))
cum_subset = final_data %>% 
  group_by(unq_memID) %>%
  mutate(num_infections = cumsum(count)) %>%
  select(c(unq_memID,sample_id_date,num_infections))
cum_subset = data.frame(cum_subset)

# order and tabulate each person's visit
follow_up_order = cum_subset %>%
  spread(key=unq_memID,value=sample_id_date)

# get rid of the num infections column
follow_up_order$num_infections <- NULL



#### -------- code infections as new, recurrent or persistent ----- ####

# make a data set of the haplotypes and final data
merge_data = final_data %>% 
  select(sample_name_dbs,unq_memID,sample_id_date)
csp_data_m = csp_data %>%
  select(-c(unq_memID,sample_id_date))
hap_coding_data = left_join(merge_data,csp_data_m,by="sample_name_dbs")

# first order the data set by date
hap_coding_data = dplyr::arrange(hap_coding_data,unq_memID,sample_id_date)

# create a small test data set
# hap_coding_data = hap_coding_data[1:50,]

# write some code that looks at each participants' infections over time
# set up the input vectors
haplotype_category = rep(NA,nrow(hap_coding_data))
count_new = rep(NA,nrow(hap_coding_data))
count_recurrent = rep(NA,nrow(hap_coding_data))
count_persistent = rep(NA,nrow(hap_coding_data))
unique_participants = unique(hap_coding_data$unq_memID)
# start the for loop
for (i in 1:length(unique_participants)){
  person_list = c()
  for (j in 1:nrow(hap_coding_data)){
    
    # look at just that person's infections
    if (unique_participants[i] == hap_coding_data$unq_memID[j] & !(is.na(hap_coding_data$haplotype_number[j]))){
      haps = str_split(hap_coding_data$haplotype_list[j],",")[[1]]
      interim_category = ""
      
      # now look at specific haplotypes in that infection
      for (k in 1:length(haps)){
        one_piece = ""
        
        # if first infection
        if (unique_participants[i] != hap_coding_data$unq_memID[j-1]){
          if (haps[k] %in% person_list){
            one_piece = "o"
          } else {
            one_piece = "n"
          }
          interim_category = paste0(interim_category,one_piece)
          # if previous visit had no haplotypes
        } else if (unique_participants[i] == hap_coding_data$unq_memID[j-1] & is.na(hap_coding_data$haplotype_number[j-1])){
          if (haps[k] %in% person_list){
            one_piece = "o"
          } else {
            one_piece = "n"
          }
          interim_category = paste0(interim_category,one_piece)
          # if previous infection had haplotypes
        } else if (unique_participants[i] == hap_coding_data$unq_memID[j-1] & !(is.na(hap_coding_data$haplotype_number[j-1]))){
          # create list of most recent previous infection haplotypes
          previous_haps = str_split(hap_coding_data$haplotype_list[j-1],",")[[1]]
          if (haps[k] %in% person_list & haps[k] %in% previous_haps){
            one_piece = "p"
          } else if (haps[k] %in% person_list & !(haps[k] %in% previous_haps)){
            one_piece = "o"
          } else {
            one_piece = "n"
          }
          interim_category = paste0(interim_category,one_piece)
        }
      }
      
      # now check for old and new haplotypes
      if (str_detect(interim_category,"n") & str_detect(interim_category,"o") & str_detect(interim_category,"p")){
        haplotype_category[j] = "new, recurrent, and persistent"
        count_new[j] = str_count(interim_category,"n")
        count_recurrent[j] = str_count(interim_category,"o")
        count_persistent[j] = str_count(interim_category,"p")
      } else if (str_detect(interim_category,"n") & !(str_detect(interim_category,"o")) & !(str_detect(interim_category,"p"))){
        haplotype_category[j] = "all new"
        count_new[j] = str_count(interim_category,"n")
        count_recurrent[j] = 0
        count_persistent[j] = 0
      } else if (str_detect(interim_category,"o") & !(str_detect(interim_category,"n")) & !(str_detect(interim_category,"p"))) {
        haplotype_category[j] = "all recurrent"
        count_new[j] = 0
        count_recurrent[j] = str_count(interim_category,"o")
        count_persistent[j] = 0
      } else if (str_detect(interim_category,"p") & !(str_detect(interim_category,"n")) & !(str_detect(interim_category,"o"))) {
        haplotype_category[j] = "all persistent"
        count_new[j] = 0
        count_recurrent[j] = 0
        count_persistent[j] = str_count(interim_category,"p")
      } else if (str_detect(interim_category,"p") & str_detect(interim_category,"n") & !(str_detect(interim_category,"o"))) {
        haplotype_category[j] = "new and persistent"
        count_new[j] = str_count(interim_category,"n")
        count_recurrent[j] = 0
        count_persistent[j] = str_count(interim_category,"p")
      } else if (!(str_detect(interim_category,"p")) & str_detect(interim_category,"n") & str_detect(interim_category,"o")) {
        haplotype_category[j] = "new and recurrent"
        count_new[j] = str_count(interim_category,"n")
        count_recurrent[j] = str_count(interim_category,"o")
        count_persistent[j] = 0
      } else if (str_detect(interim_category,"p") & !(str_detect(interim_category,"n")) & str_detect(interim_category,"o")) {
        haplotype_category[j] = "recurrent and persistent"
        count_new[j] = 0
        count_recurrent[j] = str_count(interim_category,"o")
        count_persistent[j] = str_count(interim_category,"p")
      } else {
        haplotype_category[j] = NA
        count_new[j] = NA
        count_recurrent[j] = NA
      }
      
      # add to the person list
      person_list = c(person_list,haps)
    }
  }
}

# check the output
table(haplotype_category,useNA="always")
table(count_new,useNA = "always")
table(count_recurrent,useNA = "always")
table(count_persistent, useNA = "always")
table(haplotype_category,count_new,useNA = "always")
table(haplotype_category,count_recurrent,useNA = "always")
table(haplotype_category,count_persistent,useNA = "always")

# add the new haplotype category to the data set
hap_coding_data$haplotype_category = haplotype_category
hap_coding_data$count_new_haplotypes = count_new
hap_coding_data$count_persistent_haplotypes = count_persistent
hap_coding_data$count_recurrent_haplotypes = count_recurrent

# cut down the data set to just the csp data
data_to_export = hap_coding_data %>% filter(hap_coding_data$sample_name_dbs %in% csp_data$sample_name_dbs)

# create a variable that is the proportion of haplotypes in the infection that were new
data_to_export$proportion_new_haplotypes = data_to_export$count_new_haplotypes/data_to_export$haplotype_number

# create a variable that is the proportion of haplotypes in the infection that were recurrent
data_to_export$proportion_recurrent_haplotypes = data_to_export$count_recurrent_haplotypes/data_to_export$haplotype_number

# create a variable that is the proportion of haplotypes in the infection that were persistent
data_to_export$proportion_persistent_haplotypes = data_to_export$count_persistent_haplotypes/data_to_export$haplotype_number

# export the data set
write_csv(data_to_export,"Desktop/csp_data_aim1b_11JUN2020.csv")
write_rds(data_to_export,"Desktop/csp_data_aim1b_11JUN2020.rds")



####
## CREATE THE DATA SET FOR PFAMA1
####


#### -------- set up the data set -------- ####

# remove the mosquito samples from the data set
ama_haplotype_summary = ama_haplotype_summary %>%
  filter(!(str_detect(sample_name_dbs,"H")) & !(str_detect(sample_name_dbs,"A")) | sample_name_dbs == "K14-170717-1A-R")

# make ama_data
ama_data = ama_haplotype_summary


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
setdiff(ppts_to_censor$sample_name_final,ama_data$sample_name_dbs)
length(unique(ama_data$sample_name_dbs))
length(which(ama_data$sample_name_dbs %in% ppts_to_censor$sample_name_final))
ama_data = ama_data[-which(ama_data$sample_name_dbs %in% ppts_to_censor$sample_name_final),]
length(unique(ama_data$sample_name_dbs))
# removed 18 participants in the ama merged data set out of the 26 that were in the full demographic data set


#### ----- finish merging in the data sets --------- ####

# first cut down the human demographic data set to the variables of interest
colnames(ama_haplotype_summary)
colnames(final_data)
human_data = final_data %>%
  select(sample_name_dbs,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,age_all_baseline,age_cat_baseline,village_name,prescription)

# merge symptomatic status and other variables of interest into the data set (age, village, household)
ama_data = left_join(ama_data,human_data,by="sample_name_dbs")

# check the merge
length(which(is.na(ama_data$age_all_baseline)))
# looks good

# now create the outcome variable: symptomatic status
ama_data = ama_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  mutate(symptomatic_status = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def))) %>%
  select(-c(main_exposure_primary_case_def,main_outcome_primary_case_def))
table(ama_data$symptomatic_status, useNA="always")



#### -------- code infections as new, recurrent or persistent ----- ####

# make a data set of the haplotypes and final data
merge_data = final_data %>% 
  select(sample_name_dbs,unq_memID,sample_id_date)
ama_data_m = ama_data %>%
  select(-c(unq_memID,sample_id_date))
hap_coding_data = left_join(merge_data,ama_data_m,by="sample_name_dbs")

# first order the data set by date
hap_coding_data = dplyr::arrange(hap_coding_data,unq_memID,sample_id_date)

# create a small test data set
# hap_coding_data = hap_coding_data[1:50,]

# write some code that looks at each participants' infections over time
# set up the input vectors
haplotype_category = rep(NA,nrow(hap_coding_data))
count_new = rep(NA,nrow(hap_coding_data))
count_recurrent = rep(NA,nrow(hap_coding_data))
count_persistent = rep(NA,nrow(hap_coding_data))
unique_participants = unique(hap_coding_data$unq_memID)
# start the for loop
for (i in 1:length(unique_participants)){
  person_list = c()
  for (j in 1:nrow(hap_coding_data)){
    
    # look at just that person's infections
    if (unique_participants[i] == hap_coding_data$unq_memID[j] & !(is.na(hap_coding_data$haplotype_number[j]))){
      haps = str_split(hap_coding_data$haplotype_list[j],",")[[1]]
      interim_category = ""
      
      # now look at specific haplotypes in that infection
      for (k in 1:length(haps)){
        one_piece = ""
        
        # if first infection
        if (unique_participants[i] != hap_coding_data$unq_memID[j-1]){
          if (haps[k] %in% person_list){
            one_piece = "o"
          } else {
            one_piece = "n"
          }
          interim_category = paste0(interim_category,one_piece)
          # if previous visit had no haplotypes
        } else if (unique_participants[i] == hap_coding_data$unq_memID[j-1] & is.na(hap_coding_data$haplotype_number[j-1])){
          if (haps[k] %in% person_list){
            one_piece = "o"
          } else {
            one_piece = "n"
          }
          interim_category = paste0(interim_category,one_piece)
          # if previous infection had haplotypes
        } else if (unique_participants[i] == hap_coding_data$unq_memID[j-1] & !(is.na(hap_coding_data$haplotype_number[j-1]))){
          # create list of most recent previous infection haplotypes
          previous_haps = str_split(hap_coding_data$haplotype_list[j-1],",")[[1]]
          if (haps[k] %in% person_list & haps[k] %in% previous_haps){
            one_piece = "p"
          } else if (haps[k] %in% person_list & !(haps[k] %in% previous_haps)){
            one_piece = "o"
          } else {
            one_piece = "n"
          }
          interim_category = paste0(interim_category,one_piece)
        }
      }
      
      # now check for old and new haplotypes
      if (str_detect(interim_category,"n") & str_detect(interim_category,"o") & str_detect(interim_category,"p")){
        haplotype_category[j] = "new, recurrent, and persistent"
        count_new[j] = str_count(interim_category,"n")
        count_recurrent[j] = str_count(interim_category,"o")
        count_persistent[j] = str_count(interim_category,"p")
      } else if (str_detect(interim_category,"n") & !(str_detect(interim_category,"o")) & !(str_detect(interim_category,"p"))){
        haplotype_category[j] = "all new"
        count_new[j] = str_count(interim_category,"n")
        count_recurrent[j] = 0
        count_persistent[j] = 0
      } else if (str_detect(interim_category,"o") & !(str_detect(interim_category,"n")) & !(str_detect(interim_category,"p"))) {
        haplotype_category[j] = "all recurrent"
        count_new[j] = 0
        count_recurrent[j] = str_count(interim_category,"o")
        count_persistent[j] = 0
      } else if (str_detect(interim_category,"p") & !(str_detect(interim_category,"n")) & !(str_detect(interim_category,"o"))) {
        haplotype_category[j] = "all persistent"
        count_new[j] = 0
        count_recurrent[j] = 0
        count_persistent[j] = str_count(interim_category,"p")
      } else if (str_detect(interim_category,"p") & str_detect(interim_category,"n") & !(str_detect(interim_category,"o"))) {
        haplotype_category[j] = "new and persistent"
        count_new[j] = str_count(interim_category,"n")
        count_recurrent[j] = 0
        count_persistent[j] = str_count(interim_category,"p")
      } else if (!(str_detect(interim_category,"p")) & str_detect(interim_category,"n") & str_detect(interim_category,"o")) {
        haplotype_category[j] = "new and recurrent"
        count_new[j] = str_count(interim_category,"n")
        count_recurrent[j] = str_count(interim_category,"o")
        count_persistent[j] = 0
      } else if (str_detect(interim_category,"p") & !(str_detect(interim_category,"n")) & str_detect(interim_category,"o")) {
        haplotype_category[j] = "recurrent and persistent"
        count_new[j] = 0
        count_recurrent[j] = str_count(interim_category,"o")
        count_persistent[j] = str_count(interim_category,"p")
      } else {
        haplotype_category[j] = NA
        count_new[j] = NA
        count_recurrent[j] = NA
      }
      
      # add to the person list
      person_list = c(person_list,haps)
    }
  }
}

# check the output
table(haplotype_category,useNA="always")
table(count_new,useNA = "always")
table(count_recurrent,useNA = "always")
table(count_persistent, useNA = "always")
table(haplotype_category,count_new,useNA = "always")
table(haplotype_category,count_recurrent,useNA = "always")
table(haplotype_category,count_persistent,useNA = "always")

# add the new haplotype category to the data set
hap_coding_data$haplotype_category = haplotype_category
hap_coding_data$count_new_haplotypes = count_new
hap_coding_data$count_persistent_haplotypes = count_persistent
hap_coding_data$count_recurrent_haplotypes = count_recurrent

# cut down the data set to just the ama data
data_to_export = hap_coding_data %>% filter(hap_coding_data$sample_name_dbs %in% ama_data$sample_name_dbs)

# create a variable that is the proportion of haplotypes in the infection that were new
data_to_export$proportion_new_haplotypes = data_to_export$count_new_haplotypes/data_to_export$haplotype_number

# create a variable that is the proportion of haplotypes in the infection that were recurrent
data_to_export$proportion_recurrent_haplotypes = data_to_export$count_recurrent_haplotypes/data_to_export$haplotype_number

# create a variable that is the proportion of haplotypes in the infection that were persistent
data_to_export$proportion_persistent_haplotypes = data_to_export$count_persistent_haplotypes/data_to_export$haplotype_number

# export the data set
write_csv(data_to_export,"Desktop/ama_data_aim1b_11JUN2020.csv")
write_rds(data_to_export,"Desktop/ama_data_aim1b_11JUN2020.rds")

