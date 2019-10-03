# ------------------------------------- #
#         Mozzie Phase 1 Data           #
#   Pull sporozoite positive samples    #
#      for mosquitoes for Wendy         #
#          October 1, 2019              #
#              K. Sumner                #
# ------------------------------------- #

#### -------- load libraries -------- ####
library(tidyverse)
library(haven)
library(lubridate)


#### ---------- load in the data sets ---------- ####

# read in the merged anpopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")

# read in the pfcsp1 haplotype data set
ama_merge_data = read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# read in the pfcsp haplotype data set
csp_merge_data = read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")



#### ------- pull out the sporozoite positive samples -------- ####

# calculate how many mosquitoes have a Pf positive head
met_criteria_mosq_data = anoph_merged_data %>%
  filter(anoph_merged_data$pf_pcr_infection_status_sample_level_h == "positive" |
           anoph_merged_data$pf_pcr_infection_status_sample_level_a == "positive")

# look at what households these mosquitoes were found within
table(met_criteria_mosq_data$HH_ID, useNA="always")
length(unique(met_criteria_mosq_data$HH_ID, useNA="always"))
# 28/38 households

# look at the month and year these mosquites were collected
table(met_criteria_mosq_data$collection_month_year_combo, useNA="always")

# look at how many were pf positive in the head only
met_criteria_head = met_criteria_mosq_data %>%
  filter(pf_pcr_infection_status_sample_level_h=="positive")
met_criteria_abdomen = met_criteria_mosq_data %>%
  filter(pf_pcr_infection_status_sample_level_a=="positive")

# subset to just the variables of interest for Wendy
met_criteria_head = met_criteria_head %>%
  select(HH_ID,collection_date,collection_month_year_combo,village,sample_id_head) %>%
  rename("sample_id"="sample_id_head")
met_criteria_abdomen = met_criteria_abdomen %>%
  select(HH_ID,collection_date,collection_month_year_combo,village,sample_id_abdomen) %>%
  rename("sample_id"="sample_id_abdomen")
# and bind together
met_criteria_all = rbind(met_criteria_head,met_criteria_abdomen)


#### ------ merge in the human and mosquito demographic data for matching ------ ####

## ----- do this for ama

# check if duplicates in the ama_haplotypes data set
length(unique(ama_merge_data$sample_name_dbs)) # 1116 unique 
length(which(is.na(ama_merge_data$sample_name_dbs) == T)) # 0 missing
count_table = table(ama_merge_data$sample_name_dbs, useNA = "always")
dups_table_inventory = count_table[which(count_table > 1)] # 0 duplicates
# check if duplicates in human_data

# now merge ids for the mosquitoes
# check the mosquito merging
# first for mosquito heads
ama_merge_data = ama_merge_data %>%
  rename("sample_id"="sample_name_dbs")
ama_merge_test = right_join(ama_merge_data,met_criteria_all,by="sample_id")
# check which ones didn't merge
length(which(is.na(ama_merge_test$HH_ID)))
length(which(is.na(ama_merge_test$haplotype_number)))
length(setdiff(ama_merge_test$sample_id,met_criteria_all$sample_id))
ama_unmerged_samples = data.frame(unmerged_samples = setdiff(ama_merge_test$sample_id,met_criteria_all$sample_id))
ama_unmerged_samples %>% filter((str_detect(unmerged_samples,"H")))
ama_merge_test = ama_merge_test[which(!(is.na(ama_merge_test$haplotype_number))),]

# filter just to the variables of interest
full_ama_mosquito_samples = ama_merge_test%>%
  select(HH_ID,collection_date,collection_month_year_combo,village,sample_id,haplotype_number,haplotype_reads,Run)


## ----- do this for csp

# check if duplicates in the ama_haplotypes data set
length(unique(csp_merge_data$sample_name_dbs)) # 1281 unique 
length(which(is.na(csp_merge_data$sample_name_dbs) == T)) # 0 missing
count_table = table(csp_merge_data$sample_name_dbs, useNA = "always")
dups_table_inventory = count_table[which(count_table > 1)] # 0 duplicates
# check if duplicates in human_data

# now merge ids for the mosquitoes
# check the mosquito merging
# first for mosquito heads
csp_merge_data = csp_merge_data %>%
  rename("sample_id"="sample_name_dbs")
csp_merge_test = right_join(csp_merge_data,met_criteria_all,by="sample_id")
# check which ones didn't merge
length(which(is.na(csp_merge_test$HH_ID)))
length(which(is.na(csp_merge_test$haplotype_number)))
length(setdiff(csp_merge_test$sample_id,met_criteria_all$sample_id))
csp_unmerged_samples = data.frame(unmerged_samples = setdiff(csp_merge_test$sample_id,met_criteria_all$sample_id))
csp_unmerged_samples %>% filter((str_detect(unmerged_samples,"H")))
csp_merge_test = csp_merge_test[which(!(is.na(csp_merge_test$haplotype_number))),]

# filter just to the variables of interest
full_csp_mosquito_samples = csp_merge_test%>%
  select(HH_ID,collection_date,collection_month_year_combo,village,sample_id,haplotype_number,haplotype_reads,Run)


#### ------ save the mosquito results -------- ####

write_csv(full_ama_mosquito_samples,"Desktop/spat21_ama_mosquito_samples_passed_sequencing_spor_positive_1OCT2019.csv")
write_csv(full_csp_mosquito_samples,"Desktop/spat21_csp_mosquito_samples_passed_sequencing_spor_positive_1OCT2019.csv")



#### ---- pull out human samples that passed sequencing ----- ####

# read in the pfcsp1 haplotype data set
ama_merge_data = read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# read in the pfcsp haplotype data set
csp_merge_data = read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")


## --- first do ama

# pull out human samples that passed sequencing for ama
ama_human_passed = ama_merge_data %>%
  filter(sample_type=="Human")%>%
  select(sample_name_dbs,haplotype_number,haplotype_reads,Run)

# merge the human samples with the full data set
ama_merge_test = left_join(ama_human_passed,final_data,by="sample_name_dbs")
# check which ones didn't merge
length(which(is.na(ama_merge_test$sample_id_date)))
length(setdiff(ama_merge_test$sample_name_dbs,final_data$sample_name_dbs))
ama_unmerged_samples = data.frame(unmerged_samples = setdiff(ama_merge_test$sample_name_dbs,final_data$sample_name_dbs))
ama_unmerged_samples %>% filter(!(str_detect(unmerged_samples,"A") | str_detect(unmerged_samples,"H")))
length(which(is.na(ama_merge_test$memID)))
# looks good

# subset to just the variables of interest
ama_human_passed = ama_merge_test %>%
  select(sample_name_final,sample_name_dbs,sample_id_date,HH_ID,village_name,haplotype_number,haplotype_reads,Run)


## --- then do csp

# pull out human samples that passed sequencing for ama
csp_human_passed = csp_merge_data %>%
  filter(sample_type=="Human")%>%
  select(sample_name_dbs,haplotype_number,haplotype_reads,Run)

# merge the human samples with the full data set
csp_merge_test = left_join(csp_human_passed,final_data,by="sample_name_dbs")
# check which ones didn't merge
length(which(is.na(csp_merge_test$sample_id_date)))
length(setdiff(csp_merge_test$sample_name_dbs,final_data$sample_name_dbs))
csp_unmerged_samples = data.frame(unmerged_samples = setdiff(csp_merge_test$sample_name_dbs,final_data$sample_name_dbs))
csp_unmerged_samples %>% filter(!(str_detect(unmerged_samples,"A") | str_detect(unmerged_samples,"H")))
length(which(is.na(csp_merge_test$memID)))
# looks good

# subset to just the variables of interest
csp_human_passed = csp_merge_test %>%
  select(sample_name_final,sample_name_dbs,sample_id_date,HH_ID,village_name,haplotype_number,haplotype_reads,Run)



#### ------ save the human results -------- ####

write_csv(ama_human_passed,"Desktop/spat21_ama_human_samples_passed_sequencing_1OCT2019.csv")
write_csv(csp_human_passed,"Desktop/spat21_csp_human_samples_passed_sequencing_1OCT2019.csv")



#### ---- read back in the data sets you made above and calculate how many infected mosquitoes are in infected households at same time ------- ####

# read in the ama data
human_ama = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Samples that failed sequencing info/human samples that passed/spat21_ama_human_samples_passed_sequencing_1OCT2019.csv")
mosquito_ama = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Samples that failed sequencing info/mosquito samples that passed/spat21_ama_mosquito_samples_passed_sequencing_spor_positive_1OCT2019.csv")


# now read in the csp data
human_csp = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Samples that failed sequencing info/human samples that passed/spat21_csp_human_samples_passed_sequencing_1OCT2019.csv")
mosquito_csp = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Samples that failed sequencing info/mosquito samples that passed/spat21_csp_mosquito_samples_passed_sequencing_spor_positive_1OCT2019.csv")


# calculate the number of mosquito samples within those that passed in the same household for ama
count = 0
for (i in 1:nrow(human_ama)){
  for (j in 1:nrow(mosquito_ama)){
    if (human_ama$HH_ID[i] == mosquito_ama$HH_ID[j] & (mosquito_ama$collection_date[j]-human_ama$sample_id_date[i] < 19) & (mosquito_ama$collection_date[j]-human_ama$sample_id_date[i] >= 0)){
      count = count + 1
    }
  }
}
count # 625


# calculate the number of mosquito samples within those that passed in the same household for csp
count = 0
for (i in 1:nrow(human_csp)){
  for (j in 1:nrow(mosquito_csp)){
    if (human_csp$HH_ID[i] == mosquito_csp$HH_ID[j] & (mosquito_csp$collection_date[j]-human_csp$sample_id_date[i] < 19) & (mosquito_csp$collection_date[j]-human_csp$sample_id_date[i] >= 0)){
      count = count + 1
    }
  }
}
count # 687




