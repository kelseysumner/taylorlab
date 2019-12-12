# ----------------------------------------- #
# Create aim 2 data set prior to haplotypes #
#        with no HH or time restrictions    #
#             Mozzie Phase 1                #
#            December 3, 2019               #
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



#### ----- AMA ------- ####


#### ---------- load in the data sets ---------- ####

# read in the merged anpopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")

# read in the ama simplified edgelist
ama_edgelist = read_csv("Desktop/clean_ids_haplotype_results/AMA/AMA_haplotypes_edgelist_simplified_number_haps_shared.csv")


#### ------ clean up the edgelists to be in the proper format --------- ####

# first look at the columns
colnames(ama_edgelist)

# remove the X1 column
ama_edgelist = ama_edgelist %>%
  dplyr::select(-"X1")

# look at how many unique observations
length(unique(ama_edgelist$from)) # 1115
length(unique(ama_edgelist$to)) # 1115
# looks correct

# subset the data set to just have heads in left column and mosquitoes in right column
table(nchar(ama_edgelist$from))
table(nchar(ama_edgelist$to))

# remove the rows where both the to and from columns are human or mosquito samples
ama_edgelist = ama_edgelist %>%
  filter(!(str_detect(from,"-") & str_detect(to,"-"))) %>%
  filter(!(str_detect(from," ") & str_detect(to," "))) 

# make the first column human samples and second column mosquito samples
# create a for loop that checks to see if each sample is sharing with a mosquito or not
# switch the from column first
new_from = rep(NA,nrow(ama_edgelist))
new_to = rep(NA,nrow(ama_edgelist))
for (i in 1:nrow(ama_edgelist)){
  if (str_detect(ama_edgelist$from[i]," ")){
    new_from[i] = ama_edgelist$to[i]
    new_to[i] = ama_edgelist$from[i]
  } else {
    new_from[i] = ama_edgelist$from[i]
    new_to[i] = ama_edgelist$to[i]
  }
}
ama_edgelist$from = new_from
ama_edgelist$to = new_to

# rename the column headers in the ama edgelist
ama_edgelist = ama_edgelist %>%
  rename("sample_name_dbs"="from","sample_id_mosquito"="to","haps_shared"="weight")

# split up the edgelist into the shared mosquito heads and abdomens
ama_edgelist_head = ama_edgelist %>%
  filter(str_detect(sample_id_mosquito,"H")) %>%
  rename("sample_id_head"="sample_id_mosquito")
ama_edgelist_abdomen = ama_edgelist %>%
  filter(str_detect(sample_id_mosquito,"A")) %>%
  rename("sample_id_abdomen"="sample_id_mosquito")



#### --------- subset the human and mosquito data sets to just the variables of interest ---------- ####

# make sure main exposure and main outcome for primary case definition are factors
final_data$main_exposure_primary_case_def = as.factor(final_data$main_exposure_primary_case_def)
final_data$main_outcome_primary_case_def = as.factor(final_data$main_outcome_primary_case_def)

# select variables you need for human data
colnames(final_data)
human_data = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  dplyr::select(visit_type,sample_id_date,sample_name_final,sample_name_dbs,age_cat_baseline,unq_memID,village_name,HH_ID,main_exposure_primary_case_def,main_outcome_primary_case_def,pfr364Q_std_combined,age_all_baseline) %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def))) %>%
  dplyr::select(-main_exposure_primary_case_def,-main_outcome_primary_case_def,-visit_type)

# select variables you need for mosquito data
colnames(anoph_merged_data)
mosquito_data = anoph_merged_data %>%
  filter(!(is.na(sample_id_head) & is.na(sample_id_abdomen)) | sample_id_mosquito == "K01 00030" | sample_id_mosquito == "K01 00047") %>%
  dplyr::select(HH_ID,collection_date,total_num_mosq_in_hh,sample_id_abdomen,sample_id_head,sample_id_mosquito)
# note: there are 15 entries where the lab didn't have mosquitoes so didn't have separate head and abdomen ids, removed these entries
# K01 00030 and K01 00047 were sequenced and pf positive but were original test samples so weren't in the normal qpcr data set
# add their information here for the data set ids
mosquito_data$sample_id_abdomen[which(mosquito_data$sample_id_mosquito=="K01 00030")] = "K01 A00030"
mosquito_data$sample_id_head[which(mosquito_data$sample_id_mosquito=="K01 00030")] = "K01 H00030"
mosquito_data$sample_id_abdomen[which(mosquito_data$sample_id_mosquito=="K01 00047")] = "K01 A00047"
mosquito_data$sample_id_head[which(mosquito_data$sample_id_mosquito=="K01 00047")] = "K01 H00047"


## --- write code to work with the mosquito heads outcome

# check how the samples would merge with the full data set before asymptomatic/symptomatic criteria is enforced
# check this observation
final_data = final_data %>%
  dplyr::select(sample_name_dbs,sample_name_final)
merge_check = left_join(ama_edgelist_head,final_data,by="sample_name_dbs")
merge_check %>%
  filter(is.na(sample_name_final)) %>%
  View()

# add the human and mosquito data sets' variables to the edgelist
# the edgelist will be the level of analysis
# merge the human info first
ama_edgelist_head = left_join(ama_edgelist_head,human_data,by="sample_name_dbs")
# then merge the mosquito info
ama_edgelist_head = left_join(ama_edgelist_head,mosquito_data,by="sample_id_head")
# check the merge
ama_edgelist_head %>%
  filter(is.na(sample_name_final)) %>%
  View()
ama_edgelist_head %>%
  filter(is.na(collection_date)) %>%
  View()
# the samples that didn't merge did not meet the case definition for an asymptomatic or symptomatic infection
# this observation has been shown in the code chunk above around line 110

# rename some of the variables in the data set for clarity
colnames(ama_edgelist_head)
ama_edgelist_head = ama_edgelist_head %>%
  rename("sample_id_human" = "sample_name_dbs","HH_ID_human"="HH_ID.x","HH_ID_mosquito"="HH_ID.y","human_date"="sample_id_date","mosquito_date"="collection_date") %>%
  dplyr::select(-sample_id_abdomen,-sample_id_mosquito) 
colnames(ama_edgelist_head)

# first create a variable that is the time diff between human and mosquito samples subtract human time from mosquito time
# if time date difference is positive than the mosquito was collected after the human sample
ama_edgelist_head = ama_edgelist_head %>%
  mutate(date_difference = mosquito_date - human_date)

# note there are some sequenced results that didn't match the symptomatic and asymptomatic case definitions, remove these
ama_edgelist_head = ama_edgelist_head[-which(is.na(ama_edgelist_head$human_date) | is.na(ama_edgelist_head$mosquito_date)),]
length(which(is.na(ama_edgelist_head$date_difference))) # 0, correct

# count how many haplotypes were shared between mosquito heads and humans
length(which(ama_edgelist_head$haps_shared >0)) # 28544 heads

# write out the edgelist
write_rds(ama_edgelist_head,"Desktop/spat21_ama_edgelist_head_no_restrictions_3DEC2019.rds")
write_csv(ama_edgelist_head,"Desktop/spat21_ama_edgelist_head_no_restrictions_3DEC2019.csv")



## --- write code to work with the mosquito abdomens outcome

# add the human and mosquito data sets' variables to the edgelist
# the edgelist will be the level of analysis
ama_edgelist_abdomen = left_join(ama_edgelist_abdomen,human_data,by="sample_name_dbs")
ama_edgelist_abdomen = left_join(ama_edgelist_abdomen,mosquito_data,by="sample_id_abdomen")
# check the merge
ama_edgelist_abdomen %>%
  filter(is.na(sample_name_final)) %>%
  View()
ama_edgelist_abdomen %>%
  filter(is.na(collection_date)) %>%
  View()
# the samples that didn't merge did not meet the case definition for an asymptomatic or symptomatic infection
# this observation has been shown in the code chunk above around line 110

# rename some of the variables in the data set for clarity
colnames(ama_edgelist_abdomen)
ama_edgelist_abdomen = ama_edgelist_abdomen %>%
  rename("sample_id_human" = "sample_name_dbs","HH_ID_human"="HH_ID.x","HH_ID_mosquito"="HH_ID.y","human_date"="sample_id_date","mosquito_date"="collection_date") %>%
  dplyr::select(-sample_id_head,-sample_id_mosquito) 
colnames(ama_edgelist_abdomen)

# first create a variable that is the time diff between human and mosquito samples subtract human time from mosquito time
# if time date difference is positive than the mosquito was collected after the human sample
ama_edgelist_abdomen = ama_edgelist_abdomen %>%
  mutate(date_difference = mosquito_date - human_date)

# note there are some sequenced results that didn't match the symptomatic and asymptomatic case definitions, remove these
ama_edgelist_abdomen = ama_edgelist_abdomen[-which(is.na(ama_edgelist_abdomen$human_date) | is.na(ama_edgelist_abdomen$mosquito_date)),]
length(which(is.na(ama_edgelist_abdomen$date_difference))) # 0, correct

# count how many haplotypes were shared between mosquito abdomens and humans
length(which(ama_edgelist_abdomen$haps_shared >0)) # 46639 abdomens

# write out the edgelist
write_rds(ama_edgelist_abdomen,"Desktop/spat21_ama_edgelist_abdomen_no_restrictions_3DEC2019.rds")
write_csv(ama_edgelist_abdomen,"Desktop/spat21_ama_edgelist_abdomen_no_restrictions_3DEC2019.csv")




#### ----- CSP ------- ####


#### ---------- load in the data sets ---------- ####

# read in the merged anpopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")

# read in the ama simplified edgelist
csp_edgelist = read_csv("Desktop/clean_ids_haplotype_results/CSP/CSP_haplotypes_edgelist_simplified_number_haps_shared.csv")


#### ------ clean up the edgelists to be in the proper format --------- ####

# first look at the columns
colnames(csp_edgelist)

# remove the X1 column
csp_edgelist = csp_edgelist %>%
  dplyr::select(-"X1")

# look at how many unique observations
length(unique(csp_edgelist$from)) # 1280
length(unique(csp_edgelist$to)) # 1280
# looks correct

# subset the data set to just have heads in left column and mosquitoes in right column
table(nchar(csp_edgelist$from))
table(nchar(csp_edgelist$to))

# remove the rows where both the to and from columns are human samples
csp_edgelist = csp_edgelist %>%
  filter(!(str_detect(from,"-") & str_detect(to,"-"))) %>%
  filter(!(str_detect(from," ") & str_detect(to," "))) 

# make the first column human samples and second column mosquito samples
# create a for loop that checks to see if each sample is sharing with a mosquito or not
# switch the from column first
new_from = rep(NA,nrow(csp_edgelist))
new_to = rep(NA,nrow(csp_edgelist))
for (i in 1:nrow(csp_edgelist)){
  if (str_detect(csp_edgelist$from[i]," ")){
    new_from[i] = csp_edgelist$to[i]
    new_to[i] = csp_edgelist$from[i]
  } else {
    new_from[i] = csp_edgelist$from[i]
    new_to[i] = csp_edgelist$to[i]
  }
}
csp_edgelist$from = new_from
csp_edgelist$to = new_to

# rename the column headers in the csp edgelist
csp_edgelist = csp_edgelist %>%
  rename("sample_name_dbs"="from","sample_id_mosquito"="to","haps_shared"="weight")

# split up the edgelist into the shared mosquito heads and abdomens
csp_edgelist_head = csp_edgelist %>%
  filter(str_detect(sample_id_mosquito,"H")) %>%
  rename("sample_id_head"="sample_id_mosquito")
csp_edgelist_abdomen = csp_edgelist %>%
  filter(str_detect(sample_id_mosquito,"A")) %>%
  rename("sample_id_abdomen"="sample_id_mosquito")



#### --------- subset the human and mosquito data sets to just the variables of interest ---------- ####

# make sure main exposure and main outcome for primary case definition are factors
final_data$main_exposure_primary_case_def = as.factor(final_data$main_exposure_primary_case_def)
final_data$main_outcome_primary_case_def = as.factor(final_data$main_outcome_primary_case_def)

# select variables you need for human data
colnames(final_data)
human_data = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  dplyr::select(visit_type,sample_id_date,sample_name_final,sample_name_dbs,age_cat_baseline,unq_memID,village_name,HH_ID,main_exposure_primary_case_def,main_outcome_primary_case_def,pfr364Q_std_combined,age_all_baseline) %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def))) %>%
  dplyr::select(-main_exposure_primary_case_def,-main_outcome_primary_case_def,-visit_type)

# select variables you need for mosquito data
colnames(anoph_merged_data)
mosquito_data = anoph_merged_data %>%
  filter(!(is.na(sample_id_head) & is.na(sample_id_abdomen)) | sample_id_mosquito == "K01 00030" | sample_id_mosquito == "K01 00047") %>%
  dplyr::select(HH_ID,collection_date,total_num_mosq_in_hh,sample_id_abdomen,sample_id_head,sample_id_mosquito)
# note: there are 15 entries where the lab didn't have mosquitoes so didn't have separate head and abdomen ids, removed these entries
# K01 00030 and K01 00047 were sequenced and pf positive but were original test samples so weren't in the normal qpcr data set
# add their information here for the data set ids
mosquito_data$sample_id_abdomen[which(mosquito_data$sample_id_mosquito=="K01 00030")] = "K01 A00030"
mosquito_data$sample_id_head[which(mosquito_data$sample_id_mosquito=="K01 00030")] = "K01 H00030"
mosquito_data$sample_id_abdomen[which(mosquito_data$sample_id_mosquito=="K01 00047")] = "K01 A00047"
mosquito_data$sample_id_head[which(mosquito_data$sample_id_mosquito=="K01 00047")] = "K01 H00047"


## --- write code to work with the mosquito heads outcome

# check how the samples would merge with the full data set before asymptomatic/symptomatic criteria is enforced
# check this observation
final_data = final_data %>%
  dplyr::select(sample_name_dbs,sample_name_final)
merge_check = left_join(csp_edgelist_head,final_data,by="sample_name_dbs")
merge_check %>%
  filter(is.na(sample_name_final)) %>%
  View()

# add the human and mosquito data sets' variables to the edgelist
# the edgelist will be the level of analysis
# merge the human info first
csp_edgelist_head = left_join(csp_edgelist_head,human_data,by="sample_name_dbs")
# then merge the mosquito info
csp_edgelist_head = left_join(csp_edgelist_head,mosquito_data,by="sample_id_head")
# check the merge
csp_edgelist_head %>%
  filter(is.na(sample_name_final)) %>%
  View()
csp_edgelist_head %>%
  filter(is.na(collection_date)) %>%
  View()
# the samples that didn't merge did not meet the case definition for an asymptomatic or symptomatic infection
# this observation has been shown in the code chunk above around line 110

# rename some of the variables in the data set for clarity
colnames(csp_edgelist_head)
csp_edgelist_head = csp_edgelist_head %>%
  rename("sample_id_human" = "sample_name_dbs","HH_ID_human"="HH_ID.x","HH_ID_mosquito"="HH_ID.y","human_date"="sample_id_date","mosquito_date"="collection_date") %>%
  dplyr::select(-sample_id_abdomen,-sample_id_mosquito) 
colnames(csp_edgelist_head)

# first create a variable that is the time diff between human and mosquito samples subtract human time from mosquito time
# if time date difference is positive than the mosquito was collected after the human sample
csp_edgelist_head = csp_edgelist_head %>%
  mutate(date_difference = mosquito_date - human_date)

# note there are some sequenced results that didn't match the symptomatic and asymptomatic case definitions, remove these
csp_edgelist_head = csp_edgelist_head[-which(is.na(csp_edgelist_head$human_date) | is.na(csp_edgelist_head$mosquito_date)),]
length(which(is.na(csp_edgelist_head$date_difference))) # 0, correct

# count how many haplotypes were shared between mosquito heads and humans
length(which(csp_edgelist_head$haps_shared >0)) # 60968 heads

# write out the edgelist
write_rds(csp_edgelist_head,"Desktop/spat21_csp_edgelist_head_no_restrictions_3DEC2019.rds")
write_csv(csp_edgelist_head,"Desktop/spat21_csp_edgelist_head_no_restrictions_3DEC2019.csv")



## --- write code to work with the mosquito abdomens outcome

# add the human and mosquito data sets' variables to the edgelist
# the edgelist will be the level of analysis
csp_edgelist_abdomen = left_join(csp_edgelist_abdomen,human_data,by="sample_name_dbs")
csp_edgelist_abdomen = left_join(csp_edgelist_abdomen,mosquito_data,by="sample_id_abdomen")
# check the merge
csp_edgelist_abdomen %>%
  filter(is.na(sample_name_final)) %>%
  View()
csp_edgelist_abdomen %>%
  filter(is.na(collection_date)) %>%
  View()
# the samples that didn't merge did not meet the case definition for an asymptomatic or symptomatic infection
# this observation has been shown in the code chunk above around line 110

# rename some of the variables in the data set for clarity
colnames(csp_edgelist_abdomen)
csp_edgelist_abdomen = csp_edgelist_abdomen %>%
  rename("sample_id_human" = "sample_name_dbs","HH_ID_human"="HH_ID.x","HH_ID_mosquito"="HH_ID.y","human_date"="sample_id_date","mosquito_date"="collection_date") %>%
  dplyr::select(-sample_id_head,-sample_id_mosquito) 
colnames(csp_edgelist_abdomen)

# first create a variable that is the time diff between human and mosquito samples subtract human time from mosquito time
# if time date difference is positive than the mosquito was collected after the human sample
csp_edgelist_abdomen = csp_edgelist_abdomen %>%
  mutate(date_difference = mosquito_date - human_date)

# note there are some sequenced results that didn't match the symptomatic and asymptomatic case definitions, remove these
csp_edgelist_abdomen = csp_edgelist_abdomen[-which(is.na(csp_edgelist_abdomen$human_date) | is.na(csp_edgelist_abdomen$mosquito_date)),]
length(which(is.na(csp_edgelist_abdomen$date_difference))) # 0, correct

# count how many haplotypes were shared between mosquito abdomens and humans
length(which(csp_edgelist_abdomen$haps_shared >0)) # 96544 abdomens

# write out the edgelist
write_rds(csp_edgelist_abdomen,"Desktop/spat21_csp_edgelist_abdomens_no_restrictions_3DEC2019.rds")
write_csv(csp_edgelist_abdomen,"Desktop/spat21_csp_edgelist_abdomen__no_restrictions_3DEC2019.csv")




