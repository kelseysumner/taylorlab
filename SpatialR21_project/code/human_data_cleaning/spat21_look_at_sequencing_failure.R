# -------------------------------------- #
#           Spat21/Mozzie Study          #
#        Look at sequencing failure      #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#            January 7, 2019             #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)



#### ----- read in the data sets ----- ####

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")

# read in the full mosquito data set
mosquito_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the full inventory for sequencing
full_inventory = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Sequencing Inventories/All Samples/spat21_sequencing_all_samples_inventory_24SEP2019.csv")

# read in the list of samples for ama that passed sequencing and filtering
ama_passed = read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")

# read in the list of samples for csp that passed sequencing and filtering
csp_passed = read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")


#### ------- clean up the ama and csp passed data sets to just the info you need -------- ####

# clean up the sample IDs in the full inventory
# now clean up the lab id for Sample Name
table(full_inventory$`Sample ID`, useNA = "always")
table(nchar(full_inventory$`Sample ID`), useNA = "always")
length(which(is.na(full_inventory$`Sample ID`))) # 0 missing IDs
# most sample names are 12 characters or 14 long but range from 10 to 15
# clean up the Sample Names
clean_sample_id = rep(NA,nrow(full_inventory))
for (i in 1:nrow(full_inventory)){
  if (full_inventory$`Sample ID`[i] == "K01- A00043"){
    clean_sample_id[i] = "K01 A00043"
  }
  if (nchar(full_inventory$`Sample ID`[i]) == 10 & !(is.na(full_inventory$`Sample ID`[i])) & !(str_detect(full_inventory$`Sample ID`[i],"A") | str_detect(full_inventory$`Sample ID`[i],"H"))){
    clean_sample_id[i] = full_inventory$`Sample ID`[i]
  }
  if (nchar(full_inventory$`Sample ID`[i]) == 10 & !(is.na(full_inventory$`Sample ID`[i])) & (str_detect(full_inventory$`Sample ID`[i],"A") | str_detect(full_inventory$`Sample ID`[i],"H")) & str_detect(full_inventory$`Sample ID`[i],"-")){
    new_name = strsplit(full_inventory$`Sample ID`[i],"-")[[1]]
    clean_sample_id[i] = paste0(new_name[1]," ",new_name[2])
  }
  if (nchar(full_inventory$`Sample ID`[i]) == 10 & !(is.na(full_inventory$`Sample ID`[i])) & (str_detect(full_inventory$`Sample ID`[i],"A") | str_detect(full_inventory$`Sample ID`[i],"H")) & !(str_detect(full_inventory$`Sample ID`[i],"-"))){
    new_name = strsplit(full_inventory$`Sample ID`[i]," ")[[1]]
    clean_sample_id[i] = paste0(new_name[1]," ",new_name[2])
  }
  if (nchar(full_inventory$`Sample ID`[i]) == 11 & !(is.na(full_inventory$`Sample ID`[i])) & full_inventory$`Sample ID`[i] != "K01- A00043"){
    parts = strsplit(full_inventory$`Sample ID`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[1],"-","0",parts[2],"-",parts[3]))
  }
  if (nchar(full_inventory$`Sample ID`[i]) == 12 & !(is.na(full_inventory$`Sample ID`[i]))){
    clean_sample_id[i] = toupper(full_inventory$`Sample ID`[i])
  }
  if (nchar(full_inventory$`Sample ID`[i]) == 13 & str_count(full_inventory$`Sample ID`[i], "-") == 2 & !(is.na(full_inventory$`Sample ID`[i]))){
    clean_sample_id[i] = toupper(full_inventory$`Sample ID`[i])
  }
  if (nchar(full_inventory$`Sample ID`[i]) == 14 & !(is.na(full_inventory$`Sample ID`[i]))){
    parts = strsplit(full_inventory$`Sample ID`[i],"-")[[1]]
    if (parts[4] != "R" & !(is.na(parts[4]))){
      clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
    } else {
      clean_sample_id[i] = full_inventory$`Sample ID`[i]
    }
  }
  if (nchar(full_inventory$`Sample ID`[i]) == 15 & !(is.na(full_inventory$`Sample ID`[i]))){
    parts = strsplit(full_inventory$`Sample ID`[i],"-")[[1]]
    if (parts[4] != "R" & !(is.na(parts[4]))){
      clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
    } else {
      clean_sample_id[i] = full_inventory$`Sample ID`[i]
    }
  }
}
# check the recode
table(clean_sample_id, useNA = "always")
length(which(is.na(clean_sample_id))) # 12 missing
table(nchar(clean_sample_id))
# check the recoding
recode_check = data.frame(full_inventory$`Sample ID`,clean_sample_id)
# add the recode to the data set
full_inventory$`Sample ID` = clean_sample_id

# check for duplicates in full inventory
length(unique(full_inventory$`Sample ID`)) # 1523 unique 
length(which(is.na(full_inventory$`Sample ID`) == T)) # 12 missing
count_table = table(full_inventory$`Sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # blanks are duplicates but also some sample ids are duplicates
dups_table
# duplicates because sequencing pilot study twice

# set up the mosquito_data data set
mosquito_data = mosquito_data %>%
  select(sample_id_abdomen,pfr364Q_combined_a,sample_id_head,pfr364Q_combined_h)
abdomen_data = mosquito_data %>%
  filter(str_detect(sample_id_abdomen,"A")) %>%
  select(sample_id_abdomen,pfr364Q_combined_a) %>%
  rename(sample_name_dbs = sample_id_abdomen)
head_data = mosquito_data %>%
  filter(str_detect(sample_id_head,"H")) %>%
  select(sample_id_head,pfr364Q_combined_h) %>%
  rename(sample_name_dbs = sample_id_head)

# set up the final_data data set
final_data = final_data %>%
  select(sample_name_dbs, pfr364Q_std_combined)

# merge in human ama values
ama_passed = ama_passed %>%
  select(MiSeq.ID,sample_name_dbs,Run) %>%
  left_join(final_data,by="sample_name_dbs") %>%
  left_join(abdomen_data,by="sample_name_dbs") %>%
  left_join(head_data,by="sample_name_dbs") %>%
  mutate(pfr364_all = ifelse(!(is.na(pfr364Q_std_combined)),pfr364Q_std_combined,ifelse(
    !(is.na(pfr364Q_combined_a)),pfr364Q_combined_a,pfr364Q_combined_h
  )))

# now merge in human csp values
csp_passed = csp_passed %>%
  select(MiSeq.ID,sample_name_dbs,Run) %>%
  left_join(final_data,by="sample_name_dbs") %>%
  left_join(abdomen_data,by="sample_name_dbs") %>%
  left_join(head_data,by="sample_name_dbs") %>%
  mutate(pfr364_all = ifelse(!(is.na(pfr364Q_std_combined)),pfr364Q_std_combined,ifelse(
    !(is.na(pfr364Q_combined_a)),pfr364Q_combined_a,pfr364Q_combined_h
  )))


# check the output
summary(ama_passed$pfr364_all)
summary(csp_passed$pfr364_all)
ama_passed %>%
  filter(is.na(pfr364_all)) %>%
  View()
csp_passed %>%
  filter(is.na(pfr364_all)) %>%
  View()
# some pcr values not available



#### ------- create a data set that is the full inventory and says whether passed sequencing or not -------- ####

# rename the inventory variable
full_inventory = rename(full_inventory, "MiSeq.ID"="New MiSeq ID")

# join inventory with samples that passed sequencing and filtering for ama
ama_sequencing_data = left_join(full_inventory,ama_passed,by="MiSeq.ID") %>%
  select(MiSeq.ID,"Sample ID",sample_name_dbs,pfr364_all,Run.x) %>%
  rename(sequencing_sample_id = "Sample ID","run" = "Run.x") %>%
  mutate(passed_sequencing = ifelse(is.na(sample_name_dbs),"failed","passed"))

# join inventory with samples that passed sequencing and filtering for csp
csp_sequencing_data = left_join(full_inventory,csp_passed,by="MiSeq.ID") %>%
  select(MiSeq.ID,"Sample ID",sample_name_dbs,pfr364_all,Run.x) %>%
  rename(sequencing_sample_id = "Sample ID","run" = "Run.x") %>%
  mutate(passed_sequencing = ifelse(is.na(sample_name_dbs),"failed","passed"))

# need to add in parasite density for those that failed sequencing for ama
final_data = rename(final_data,"sequencing_sample_id" = "sample_name_dbs")
abdomen_data = rename(abdomen_data,"sequencing_sample_id"="sample_name_dbs")
head_data = rename(head_data,"sequencing_sample_id"="sample_name_dbs")
ama_sequencing_data = ama_sequencing_data %>%
  left_join(final_data,by="sequencing_sample_id") %>%
  left_join(abdomen_data,by="sequencing_sample_id") %>%
  left_join(head_data,by="sequencing_sample_id") %>%
  mutate(pfr364_all_new = ifelse(!(is.na(pfr364Q_std_combined)),pfr364Q_std_combined,ifelse(
    !(is.na(pfr364Q_combined_a)),pfr364Q_combined_a,pfr364Q_combined_h
  ))) %>%
  select(-c(pfr364Q_combined_a,pfr364Q_combined_h,pfr364Q_std_combined))
length(which(is.na(ama_sequencing_data$pfr364_all_new))) # 48 missing because some miscoded
ama_sequencing_data = ama_sequencing_data %>%
  mutate(pfr364_all_final = ifelse(!(is.na(pfr364_all)),pfr364_all,ifelse(
    !(is.na(pfr364_all_new)),pfr364_all_new,NA
  )))
length(which(is.na(ama_sequencing_data$pfr364_all_final))) # 30 missing now 
ama_sequencing_data$pfr364_all <- NULL
ama_sequencing_data$pfr364_all_new <- NULL

# need to add in parasite density for those that failed sequencing for csp
csp_sequencing_data = csp_sequencing_data %>%
  left_join(final_data,by="sequencing_sample_id") %>%
  left_join(abdomen_data,by="sequencing_sample_id") %>%
  left_join(head_data,by="sequencing_sample_id") %>%
  mutate(pfr364_all_new = ifelse(!(is.na(pfr364Q_std_combined)),pfr364Q_std_combined,ifelse(
    !(is.na(pfr364Q_combined_a)),pfr364Q_combined_a,pfr364Q_combined_h
  ))) %>%
  select(-c(pfr364Q_combined_a,pfr364Q_combined_h,pfr364Q_std_combined))
length(which(is.na(csp_sequencing_data$pfr364_all_new))) # 48 missing because some miscoded
csp_sequencing_data = csp_sequencing_data %>%
  mutate(pfr364_all_final = ifelse(!(is.na(pfr364_all)),pfr364_all,ifelse(
    !(is.na(pfr364_all_new)),pfr364_all_new,NA
  )))
length(which(is.na(csp_sequencing_data$pfr364_all_final))) # 26 missing now 
csp_sequencing_data$pfr364_all <- NULL
csp_sequencing_data$pfr364_all_new <- NULL

# make a variable to categorize parasite density for ama
ama_sequencing_data = ama_sequencing_data %>%
  mutate(pfr364_cat = ifelse(is.na(pfr364_all_final),NA,ifelse(pfr364_all_final < 100 & pfr364_all_final > 0,"<100",">= 100")))
table(ama_sequencing_data$pfr364_cat,ama_sequencing_data$pfr364_all_final)

# make a variable to categorize parasite density for csp
csp_sequencing_data = csp_sequencing_data %>%
  mutate(pfr364_cat = ifelse(is.na(pfr364_all_final),NA,ifelse(pfr364_all_final < 100 & pfr364_all_final > 0,"<100",">= 100")))
table(csp_sequencing_data$pfr364_cat,csp_sequencing_data$pfr364_all_final)




#### -------- look at the tabulations and correlation of passing/failing sequencing with parasite density --------- ####

# table those that passed vs. failed sequencing with run
# for ama
table(ama_sequencing_data$run,ama_sequencing_data$passed_sequencing, useNA = "always")
# for csp
table(csp_sequencing_data$run,csp_sequencing_data$passed_sequencing, useNA = "always")


# table those that passed vs. failed sequencing with parasite density
# for ama
table(ama_sequencing_data$pfr364_cat,ama_sequencing_data$passed_sequencing, useNA = "always")
# for csp
table(csp_sequencing_data$pfr364_cat,csp_sequencing_data$passed_sequencing, useNA = "always")
# note: made notes in teal blue notebook for these results (January 7, 2020)


# look at the average difference between those who failed and those who passed sequencing
# for failing
ama_failed_data = ama_sequencing_data %>% filter(passed_sequencing == "failed")
csp_failed_data = csp_sequencing_data %>% filter(passed_sequencing == "failed")
mean(ama_failed_data$pfr364_all_final,na.rm=T)
mean(csp_failed_data$pfr364_all_final,na.rm=T)
summary(ama_failed_data)
summary(csp_failed_data)
# for passing
ama_passed_data = ama_sequencing_data %>% filter(passed_sequencing == "passed")
csp_passed_data = csp_sequencing_data %>% filter(passed_sequencing == "passed")
mean(ama_passed_data$pfr364_all_final,na.rm=T)
mean(csp_passed_data$pfr364_all_final,na.rm=T)
summary(ama_passed_data)
summary(csp_passed_data)
# make passed sequencing a factor
ama_sequencing_data$passed_sequencing = as.factor(ama_sequencing_data$passed_sequencing)
csp_sequencing_data$passed_sequencing = as.factor(csp_sequencing_data$passed_sequencing)
# test the average difference in parasite density between passed and failed for ama
wilcox.test(pfr364_all_final ~ passed_sequencing,ama_sequencing_data)
# test the average difference in parasite density between passed and failed for csp
wilcox.test(pfr364_all_final ~ passed_sequencing,csp_sequencing_data)

# look at box plot of those that failed vs. passed for ama
boxplot(ama_failed_data$pfr364_all_final,ama_passed_data$pfr364_all_final)
median(ama_failed_data$pfr364_all_final,na.rm = T)
median(ama_passed_data$pfr364_all_final,na.rm = T)
mean(ama_failed_data$pfr364_all_final,na.rm = T)
mean(ama_passed_data$pfr364_all_final,na.rm = T)

# look at box plot of those that failed vs. passed for csp
boxplot(csp_failed_data$pfr364_all_final,csp_passed_data$pfr364_all_final)
median(csp_failed_data$pfr364_all_final,na.rm = T)
median(csp_passed_data$pfr364_all_final,na.rm = T)
mean(csp_failed_data$pfr364_all_final,na.rm = T)
mean(csp_passed_data$pfr364_all_final,na.rm = T)
summary(csp_failed_data$pfr364_all_final)
summary(csp_passed_data$pfr364_all_final)

# double check that no duplicates
length(unique(ama_failed_data$MiSeq.ID))
length(unique(ama_passed_data$MiSeq.ID))
length(unique(csp_passed_data$MiSeq.ID))
length(unique(csp_failed_data$MiSeq.ID))
intersect(ama_failed_data$MiSeq.ID,ama_passed$MiSeq.ID)
intersect(csp_failed_data$MiSeq.ID,csp_passed$MiSeq.ID)
