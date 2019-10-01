# ----------------------------------------- #
# Create aim 2 data set prior to haplotypes #
#             Mozzie Phase 1                #
#           September 19, 2019              #
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


#### ---------- load in the data sets ---------- ####

# read in the merged anpopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")


#### --------- subset the human and mosquito data sets to just the variables of interest ---------- ####

# make sure main exposure and main outcome for primary case definition are factors
final_data$main_exposure_primary_case_def = as.factor(final_data$main_exposure_primary_case_def)
final_data$main_outcome_primary_case_def = as.factor(final_data$main_outcome_primary_case_def)

# select variables you need for human data
colnames(final_data)
human_data = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  select(visit_type,sample_id_date,sample_name_final,age_cat_baseline,unq_memID,village_name,HH_ID,main_exposure_primary_case_def,main_outcome_primary_case_def) %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def))) %>%
  select(-main_exposure_primary_case_def,-main_outcome_primary_case_def,-visit_type)

# select variables you need for mosquito data
colnames(anoph_merged_data)
mosquito_data = anoph_merged_data %>%
  filter(!(is.na(sample_id_head) & is.na(sample_id_abdomen))) %>%
  select(HH_ID,collection_date,total_num_mosq_in_hh,sample_id_abdomen,sample_id_head,sample_id_mosquito)
# note: there are 17 entries where the lab didn't have mosquitoes so didn't have separate head and abdomen ids, removed these entries

# create a dummy edgelist to test the code with
sample_name_final = as.character(c("K01-030218-8","K01-030817-2-R","K01-030817-3","K01-030817-4","M05-060418-2-R","K01-060717-2"))
sample_id_head = as.character(c("K01 H00001","K01 H00004","K01 H00003","M14 H00007","M03 H00005","K01 H00019"))
sample_id_abdomen = as.character(c("K01 A00010","K01 A00014","M14 A00007","M09 A00012","M07 A00018","K01 A00019"))
haps_shared = as.numeric(c(1,3,2,3,5,2))
dummy_edgelist_heads = data.frame(sample_name_final,sample_id_head,haps_shared)
dummy_edgelist_abdomens = data.frame(sample_name_final,sample_id_abdomen,haps_shared)

# rename dummy edgelists to what will call original edgelists
edgelist_head = dummy_edgelist_heads
edgelist_abdomen = dummy_edgelist_abdomens


## --- write code to work with the mosquito heads outcome

# add the human and mosquito data sets' variables to the edgelist
# the edgelist will be the level of analysis
edgelist_head = left_join(edgelist_head,human_data,by="sample_name_final")
edgelist_head = left_join(edgelist_head,mosquito_data,by="sample_id_head")

# rename some of the variables in the data set for clarity
colnames(edgelist_head)
edgelist_head = edgelist_head %>%
  rename("sample_id_human" = "sample_name_final","HH_ID_human"="HH_ID.x","HH_ID_mosquito"="HH_ID.y","human_date"="sample_id_date","mosquito_date"="collection_date") %>%
  select(-sample_id_abdomen,-sample_id_mosquito) 
colnames(edgelist_head)

# first create a variable that is the time diff between human and mosquito samples subtract human time from mosquito time
# if time date difference is positive than the mosquito was collected after the human sample
edgelist_head = edgelist_head %>%
  mutate(date_difference = mosquito_date - human_date)

# now restrict the merged data set to only shared haplotypes with the same HH_ID
length(which(edgelist_head$HH_ID_human==edgelist_head$HH_ID_mosquito)) # 3 obs are in same HH
edgelist_head = edgelist_head %>%
  filter(HH_ID_human==HH_ID_mosquito)

# now restrict the merged data set to only shared haplotypes in the correct time frame
edgelist_head = edgelist_head %>%
  filter(date_difference > 7 & date_difference < 19)

# clean up the final merged data set for the mosquito heads
colnames(edgelist_head)
edgelist_head = edgelist_head %>%
  rename(HH_ID = HH_ID_human) %>%
  select(-HH_ID_mosquito,-date_difference)


## --- write code to work with the mosquito abdomens outcome

# add the human and mosquito data sets' variables to the edgelist
# the edgelist will be the level of analysis
edgelist_abdomen = left_join(edgelist_abdomen,human_data,by="sample_name_final")
edgelist_abdomen = left_join(edgelist_abdomen,mosquito_data,by="sample_id_abdomen")

# rename some of the variables in the data set for clarity
colnames(edgelist_abdomen)
edgelist_abdomen = edgelist_abdomen %>%
  rename("sample_id_human" = "sample_name_final","HH_ID_human"="HH_ID.x","HH_ID_mosquito"="HH_ID.y","human_date"="sample_id_date","mosquito_date"="collection_date") %>%
  select(-sample_id_head,-sample_id_mosquito) 
colnames(edgelist_abdomen)

# first create a variable that is the time diff between human and mosquito samples subtract human time from mosquito time
# if time date difference is positive than the mosquito was collected after the human sample
edgelist_abdomen = edgelist_abdomen %>%
  mutate(date_difference = mosquito_date - human_date)

# now restrict the merged data set to only shared haplotypes with the same HH_ID
length(which(edgelist_abdomen$HH_ID_human==edgelist_abdomen$HH_ID_mosquito)) # 3 obs are in same HH
edgelist_abdomen = edgelist_abdomen %>%
  filter(HH_ID_human==HH_ID_mosquito)

# now restrict the merged data set to only shared haplotypes in the correct time frame
edgelist_abdomen = edgelist_abdomen %>%
  filter(date_difference > 0 & date_difference < 8)

# clean up the final merged data set for the mosquito abdomens
colnames(edgelist_abdomen)
edgelist_abdomen = edgelist_abdomen %>%
  rename(HH_ID = HH_ID_human) %>%
  select(-HH_ID_mosquito,-date_difference)




