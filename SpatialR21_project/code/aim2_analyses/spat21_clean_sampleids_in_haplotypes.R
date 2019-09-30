# ----------------------------------------- #
#       Spat21 Haplotype Output Cleaning    #
#       All Samples (Human and Mosquito)    #
# Clean up sample ids in haplotype output   #
#           September 30, 2020              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(tidyverse)



#### ---------- read in the data sets ---------- ####

# read in the ama files
ama_merge_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Haplotype Results/AMA/final censored haplotype output/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_24SEPT2019.rds")

# read in the csp files
csp_merge_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Haplotype Results/CSP/final censored haplotype output/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_26SEPT2019.rds")

# read in the merged anpopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_13AUG2019.rds")




#### ------- read back in the files and fix the type of sample code  ------ ####

# now fix the coding for ama sample type
colnames(ama_merge_data)
table(ama_merge_data$sample_type, useNA="always")
test = ama_merge_data[which(ama_merge_data$sample_type=="Abdomen"),]
test$sample_id
# rename K14-170717-1A-R to human
ama_merge_data$sample_type[which(ama_merge_data$sample_id == "K14-170717-1A-R")] = "Human"
# check the renaming
test = ama_merge_data[which(ama_merge_data$sample_type=="Abdomen"),]
test$sample_id
# check head renaming
test = ama_merge_data[which(ama_merge_data$sample_type=="Head"),]
test$sample_id
# check the human renaming
test = ama_merge_data[which(ama_merge_data$sample_type=="Human"),]
test$sample_id

# now fix the coding for csp sample type
colnames(csp_merge_data)
table(csp_merge_data$sample_type, useNA="always")
test = csp_merge_data[which(csp_merge_data$sample_type=="Abdomen"),]
test$sample_id
# rename K14-170717-1A-R to human
csp_merge_data$sample_type[which(csp_merge_data$sample_id == "K14-170717-1A-R")] = "Human"
# check the renaming
test = csp_merge_data[which(csp_merge_data$sample_type=="Abdomen"),]
test$sample_id
# check head renaming
test = csp_merge_data[which(csp_merge_data$sample_type=="Head"),]
test$sample_id
# check the human renaming
test = csp_merge_data[which(csp_merge_data$sample_type=="Human"),]
test$sample_id

# read in the mosquito miseq inventory again (with all samples sequenced)
miseq_inventory = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Sequencing Inventories/All Samples/spat21_sequencing_all_samples_inventory_24SEP2019.csv")

# pull out the sample ids that didn't end up with reads in the end
# for ama
no_reads_ama = setdiff(miseq_inventory$`New MiSeq ID`,ama_merge_data$MiSeq.ID)
length(no_reads_ama)
ama_no_reads_miseq_inventory = miseq_inventory %>%
  filter(`New MiSeq ID` %in% no_reads_ama)
# write_csv(ama_no_reads_miseq_inventory,"Desktop/ama_no_reads_miseq_inventory.csv")
# for csp
no_reads_csp = setdiff(miseq_inventory$`New MiSeq ID`,csp_merge_data$MiSeq.ID)
length(no_reads_csp)
csp_no_reads_miseq_inventory = miseq_inventory %>%
  filter(`New MiSeq ID` %in% no_reads_csp)
# write_csv(csp_no_reads_miseq_inventory,"Desktop/csp_no_reads_miseq_inventory.csv")

# make a little table of the reads that passed sequencing
run = c("Human 1","Human 2","Human 3","Human Pilot","Mosquito")
ama_failed = c(43,54,278,20,68)
csp_failed = c(65,18,165,4,32)
total_sequenced = c(446,384,397,20,338)
passed_sequencing_table = data.frame(run,ama_failed,csp_failed,total_sequenced)
passed_sequencing_table$ama_passed = passed_sequencing_table$total_sequenced - passed_sequencing_table$ama_failed
passed_sequencing_table$csp_passed = passed_sequencing_table$total_sequenced - passed_sequencing_table$csp_failed
passed_sequencing_table$ama_percent_passed = (passed_sequencing_table$ama_passed/passed_sequencing_table$total_sequenced)*100
passed_sequencing_table$csp_percent_passed = (passed_sequencing_table$csp_passed/passed_sequencing_table$total_sequenced)*100
# write_csv(passed_sequencing_table,"Desktop/spat21_passed_sequencing_table.csv")

# fix the way that some of the samples are named for ama
# now clean up the lab id for Sample Name
table(ama_merge_data$sample_id, useNA = "always")
table(nchar(ama_merge_data$sample_id), useNA = "always")
length(which(is.na(ama_merge_data$sample_id))) # 0 missing IDs
# most sample names are 12 characters or 14 long but range from 10 to 15
# clean up the Sample Names
clean_sample_id = rep(NA,nrow(ama_merge_data))
for (i in 1:nrow(ama_merge_data)){
  if (ama_merge_data$sample_id[i] == "K01- A00043"){
    clean_sample_id[i] = "K01 A00043"
  }
  if (nchar(ama_merge_data$sample_id[i]) == 10 & !(is.na(ama_merge_data$sample_id[i])) & !(str_detect(ama_merge_data$sample_id[i],"A") | str_detect(ama_merge_data$sample_id[i],"H"))){
    clean_sample_id[i] = ama_merge_data$sample_id[i]
  }
  if (nchar(ama_merge_data$sample_id[i]) == 10 & !(is.na(ama_merge_data$sample_id[i])) & (str_detect(ama_merge_data$sample_id[i],"A") | str_detect(ama_merge_data$sample_id[i],"H")) & str_detect(ama_merge_data$sample_id[i],"-")){
    new_name = strsplit(ama_merge_data$sample_id[i],"-")[[1]]
    clean_sample_id[i] = paste0(new_name[1]," ",new_name[2])
  }
  if (nchar(ama_merge_data$sample_id[i]) == 10 & !(is.na(ama_merge_data$sample_id[i])) & (str_detect(ama_merge_data$sample_id[i],"A") | str_detect(ama_merge_data$sample_id[i],"H")) & !(str_detect(ama_merge_data$sample_id[i],"-"))){
    new_name = strsplit(ama_merge_data$sample_id[i]," ")[[1]]
    clean_sample_id[i] = paste0(new_name[1]," ",new_name[2])
  }
  if (nchar(ama_merge_data$sample_id[i]) == 11 & !(is.na(ama_merge_data$sample_id[i])) & ama_merge_data$sample_id[i] != "K01- A00043"){
    parts = strsplit(ama_merge_data$sample_id[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[1],"-","0",parts[2],"-",parts[3]))
  }
  if (nchar(ama_merge_data$sample_id[i]) == 12 & !(is.na(ama_merge_data$sample_id[i]))){
    clean_sample_id[i] = toupper(ama_merge_data$sample_id[i])
  }
  if (nchar(ama_merge_data$sample_id[i]) == 13 & str_count(ama_merge_data$sample_id[i], "-") == 2 & !(is.na(ama_merge_data$sample_id[i]))){
    clean_sample_id[i] = toupper(ama_merge_data$sample_id[i])
  }
  if (nchar(ama_merge_data$sample_id[i]) == 14 & !(is.na(ama_merge_data$sample_id[i]))){
    parts = strsplit(ama_merge_data$sample_id[i],"-")[[1]]
    if (parts[4] != "R"){
      clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
    } else {
      clean_sample_id[i] = ama_merge_data$sample_id[i]
    }
  }
  if (nchar(ama_merge_data$sample_id[i]) == 15 & !(is.na(ama_merge_data$sample_id[i]))){
    parts = strsplit(ama_merge_data$sample_id[i],"-")[[1]]
    if (parts[4] != "R"){
      clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
    } else {
      clean_sample_id[i] = ama_merge_data$sample_id[i]
    }
  }
}
# check the recode
table(clean_sample_id, useNA = "always")
length(which(is.na(clean_sample_id))) # 0 missing
table(nchar(clean_sample_id))
# check the recoding
recode_check = data.frame(ama_merge_data$sample_id,clean_sample_id)
# add the recode to the data set
ama_merge_data$sample_id = clean_sample_id

# fix the way that some of the samples are named for csp
# now clean up the lab id for Sample Name
table(csp_merge_data$sample_id, useNA = "always")
table(nchar(csp_merge_data$sample_id), useNA = "always")
length(which(is.na(csp_merge_data$sample_id))) # 0 missing IDs
# most sample names are 12 characters or 14 long but range from 10 to 15
# clean up the Sample Names
clean_sample_id = rep(NA,nrow(csp_merge_data))
for (i in 1:nrow(csp_merge_data)){
  if (csp_merge_data$sample_id[i] == "K01- A00043"){
    clean_sample_id[i] = "K01 A00043"
  }
  if (csp_merge_data$sample_id[i] == "R-21087-K01-5"){
    clean_sample_id[i] = "K01-210817-5-R"
  }
  if (nchar(csp_merge_data$sample_id[i]) == 10 & !(is.na(csp_merge_data$sample_id[i])) & !(str_detect(csp_merge_data$sample_id[i],"A") | str_detect(csp_merge_data$sample_id[i],"H"))){
    clean_sample_id[i] = csp_merge_data$sample_id[i]
  }
  if (nchar(csp_merge_data$sample_id[i]) == 10 & !(is.na(csp_merge_data$sample_id[i])) & (str_detect(csp_merge_data$sample_id[i],"A") | str_detect(csp_merge_data$sample_id[i],"H")) & str_detect(csp_merge_data$sample_id[i],"-")){
    new_name = strsplit(csp_merge_data$sample_id[i],"-")[[1]]
    clean_sample_id[i] = paste0(new_name[1]," ",new_name[2])
  }
  if (nchar(csp_merge_data$sample_id[i]) == 10 & !(is.na(csp_merge_data$sample_id[i])) & (str_detect(csp_merge_data$sample_id[i],"A") | str_detect(csp_merge_data$sample_id[i],"H")) & !(str_detect(csp_merge_data$sample_id[i],"-"))){
    new_name = strsplit(csp_merge_data$sample_id[i]," ")[[1]]
    clean_sample_id[i] = paste0(new_name[1]," ",new_name[2])
  }
  if (nchar(csp_merge_data$sample_id[i]) == 11 & !(is.na(csp_merge_data$sample_id[i])) & csp_merge_data$sample_id[i] != "K01- A00043"){
    parts = strsplit(csp_merge_data$sample_id[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[1],"-","0",parts[2],"-",parts[3]))
  }
  if (nchar(csp_merge_data$sample_id[i]) == 12 & !(is.na(csp_merge_data$sample_id[i]))){
    clean_sample_id[i] = toupper(csp_merge_data$sample_id[i])
  }
  if (nchar(csp_merge_data$sample_id[i]) == 13 & str_count(csp_merge_data$sample_id[i], "-") == 2 & !(is.na(csp_merge_data$sample_id[i])) & csp_merge_data$sample_id[i] != "R-21087-K01-5"){
    clean_sample_id[i] = toupper(csp_merge_data$sample_id[i])
  }
  if (nchar(csp_merge_data$sample_id[i]) == 14 & !(is.na(csp_merge_data$sample_id[i]))){
    parts = strsplit(csp_merge_data$sample_id[i],"-")[[1]]
    if (parts[4] != "R"){
      clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
    } else {
      clean_sample_id[i] = csp_merge_data$sample_id[i]
    }
  }
  if (nchar(csp_merge_data$sample_id[i]) == 15 & !(is.na(csp_merge_data$sample_id[i]))){
    parts = strsplit(csp_merge_data$sample_id[i],"-")[[1]]
    if (parts[4] != "R"){
      clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
    } else {
      clean_sample_id[i] = csp_merge_data$sample_id[i]
    }
  }
}
# check the recode
table(clean_sample_id, useNA = "always")
length(which(is.na(clean_sample_id))) # 0 missing
table(nchar(clean_sample_id))
# check the recoding
recode_check = data.frame(csp_merge_data$sample_id,clean_sample_id)
# add the recode to the data set
csp_merge_data$sample_id = clean_sample_id

# figure out why some sample ids are not unique for ama
# check if duplicates in the miseq inventory (there are some)
length(unique(miseq_inventory$`Sample ID`)) # 1566 unique 
length(which(is.na(miseq_inventory$`Sample ID`) == T)) # 0 missing
count_table = table(miseq_inventory$`Sample ID`, useNA = "always")
dups_table_inventory = count_table[which(count_table > 1)] # 18 duplicates (one a control)
# look at the ama merge data duplicates 
length(unique(ama_merge_data$sample_id)) # 1120 unique 
length(which(is.na(ama_merge_data$sample_id) == T)) # 0 missing
count_table = table(ama_merge_data$sample_id, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 2 samples ended up as duplicates
# looks like K02-050718-5 was sequenced on human runs 1 and 3
test = ama_merge_data %>%
  filter(sample_id == "K02-050718-5") %>%
  print(haplotype_number)
test$haplotype_number
test$haplotype_reads
# go with the human first sequencing run results and delete the human third run results
ama_merge_data = ama_merge_data[-which(ama_merge_data$sample_id=="K02-050718-5" & ama_merge_data$Run=="Human 3"),]
# looks like S10-050717-7-R was sequenced twice on human run 1
test = ama_merge_data %>%
  filter(sample_id == "S10-050717-7-R") %>%
  print(haplotype_number)
test$haplotype_number
test$haplotype_reads
# id was miscoded
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="S10-050717-7-R" & ama_merge_data$MiSeq.ID=="USID552")] == "S10-050717-6-R"
# check duplicates one more time
length(unique(ama_merge_data$sample_id)) # 1120 unique 
length(which(is.na(ama_merge_data$sample_id) == T)) # 0 missing
count_table = table(ama_merge_data$sample_id, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no duplicates left

# figure out why some sample ids are not unique for csp
# look at the ama merge data duplicates 
length(unique(csp_merge_data$sample_id)) # 1286 unique 
length(which(is.na(csp_merge_data$sample_id) == T)) # 0 missing
count_table = table(csp_merge_data$sample_id, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 15 samples were duplicates
# K01-030817-2-R
# looks like this sample was in the pilot study and then reran in human run 2 for csp, remove pilot study for now
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-030817-2-R" & csp_merge_data$Run=="Human Pilot"),]
# K01-030817-4
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-030817-4" & csp_merge_data$Run=="Human Pilot"),]
# K01-030817-7
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-030817-7" & csp_merge_data$Run=="Human Pilot"),]
# K01-060717-2
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-060717-2" & csp_merge_data$Run=="Human Pilot"),]
# K01-060717-3
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-060717-3" & csp_merge_data$Run=="Human Pilot"),]
# K01-060717-4
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-060717-4" & csp_merge_data$Run=="Human Pilot"),]
# K01-060717-7
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-060717-7" & csp_merge_data$Run=="Human Pilot"),]
# K01-060717-8
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-060717-8" & csp_merge_data$Run=="Human Pilot"),]
# K01-080717-3-R
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-080717-3-R" & csp_merge_data$Run=="Human Pilot"),]
# K01-080717-5-R
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-080717-5-R" & csp_merge_data$Run=="Human Pilot"),]
# K01-090817-1-R
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-090817-1-R" & csp_merge_data$Run=="Human Pilot"),]
# K01-110717-9-R 
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-110717-9-R" & csp_merge_data$Run=="Human Pilot"),]
# K01-210817-5-R
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K01-210817-5-R" & csp_merge_data$Run=="Human Pilot"),]
# K02-050718-5
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id=="K02-050718-5" & csp_merge_data$Run=="Human 3"),]
# S10-050717-7-R
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="S10-050717-7-R" & csp_merge_data$MiSeq.ID=="USID552")] == "S10-050717-6-R"
# check again for duplicates
length(unique(csp_merge_data$sample_id)) # 1286 unique 
length(which(is.na(csp_merge_data$sample_id) == T)) # 0 missing
count_table = table(csp_merge_data$sample_id, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates now

#### ------ now that the haplotype data sets are clean, see which will merge with the full data set ------- ####

## try this first for ama

# first rename the sample id variable for the haplotype data set
ama_merge_data = ama_merge_data %>%
  rename("sample_name_dbs"="sample_id")

# now merge the data sets together
ama_merge_test = left_join(ama_merge_data,final_data,by="sample_name_dbs")
# check which ones didn't merge
length(which(is.na(ama_merge_test$sample_id_date)))
length(setdiff(ama_merge_test$sample_name_dbs,final_data$sample_name_dbs))
ama_unmerged_samples = data.frame(unmerged_samples = setdiff(ama_merge_test$sample_name_dbs,final_data$sample_name_dbs))
# looks like most of these are mosquitoes so filter those out
ama_unmerged_samples %>% filter(!(str_detect(unmerged_samples,"A") | str_detect(unmerged_samples,"H")))
# 23 samples didn't merge that were human samples, some look miscoded
# K01-120617-4 - this was a dbs that had two collected on same day, kept one with R in the data set so delete this one
ama_merge_data = ama_merge_data[-which(ama_merge_data$sample_name_dbs == "K01-120617-4"),]
# K140051017-4 - this is miscoded so recode
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="K140051017-4")]="K14-051017-4"
# M05-170817-5 - this is miscoded so recode
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="M05- 170817-5")]="M05-170817-5"
# M15-311017-6 - this is miscoded so recode to have an R
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="M15-311017-6")]="M15-311017-6-R"
# M06-130617-4 - this was a dbs that had two collected on same day, kept one with R in the data set so delete this one
ama_merge_data = ama_merge_data[-which(ama_merge_data$sample_name_dbs == "M06-130617-4"),]
# M16-2011117-2-R - this is miscoded so recode
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="M16-2011117-2-R")]="M16-201117-2-R"
# M03-120717-3 - this is miscoded so recode M03-130717-3
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="M03-120717-3")]="M03-130717-3"
# S10-170717-5 - this is miscoded so recode to have an R
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="S10-170717-5")]="S10-170717-5-R"
# M04-012017-1 - this is miscoded so recode
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="M04-012017-1")]="M04-121017-1"
# M12-130617-3 - not in final data set so delete
ama_merge_data = ama_merge_data[-which(ama_merge_data$sample_name_dbs == "M12-130617-3"),]
# M05-260618-14-R - this is miscoded so recode
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="M05-260618-14-R")]="M05-260618-4-R"
# S13-090418-1 - this is miscoded so recode to have an R
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="S13-090418-1")]="S13-090418-1-R"
# S09-110418-6 - this is miscoded so recode to have an R
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="S09-110418-6")]="S09-110418-6-R"
# S10-110418-4 - this is miscoded so recode to have an R
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="S10-110418-4")]="S10-110418-4-R"
# S05-270618-3 - this is miscoded so recode to have an R
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="S05-270618-3")]="S05-270618-3-R"
# K10-100418-4 - this is miscoded so recode to have an R
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="K10-100418-4")]="K10-100418-4-R"
# K12-230518-3 - this is miscoded so recode to have an R
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="K12-230518-3")]="K12-230518-3-R"
# K01-250718-3 - this is miscoded so recode to have an R
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="K01-250718-3")]="K01-250718-3-R"
# K02-060727-5 - this is miscoded so recode
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="K02-060727-5")]="K02-060717-5"
# K14-071217-04 - this is miscoded so recode
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="K14-071217-04")]="K14-071217-4"
# K02-002117-5 - this is miscoded so recode
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="K02-002117-5")]="K02-021117-5"
# M06-130517-1 - this is miscoded so recode
ama_merge_data$sample_name_dbs[which(ama_merge_data$sample_name_dbs=="M06-130517-1")]="M06-130717-1"
# S03-210618-5 - did not merge into final data set so removed
ama_merge_data = ama_merge_data[-which(ama_merge_data$sample_name_dbs == "S03-210618-5"),]
# now do another test to merge the data sets together
ama_merge_test = left_join(ama_merge_data,final_data,by="sample_name_dbs")
# check which ones didn't merge
length(which(is.na(ama_merge_test$sample_id_date)))
length(setdiff(ama_merge_test$sample_name_dbs,final_data$sample_name_dbs))
ama_unmerged_samples = data.frame(unmerged_samples = setdiff(ama_merge_test$sample_name_dbs,final_data$sample_name_dbs))
ama_unmerged_samples %>% filter(!(str_detect(unmerged_samples,"A") | str_detect(unmerged_samples,"H")))
# looks good

# check the mosquito merging
# first for mosquito heads
ama_merge_data = ama_merge_data %>%
  rename("sample_id_head"="sample_name_dbs")
ama_merge_test = left_join(ama_merge_data,anoph_merged_data,by="sample_id_head")
# check which ones didn't merge
length(which(is.na(ama_merge_test$HH_ID)))
length(setdiff(ama_merge_test$sample_id_head,anoph_merged_data$sample_id_head))
ama_unmerged_samples = data.frame(unmerged_samples = setdiff(ama_merge_test$sample_id_head,anoph_merged_data$sample_id_head))
ama_unmerged_samples %>% filter((str_detect(unmerged_samples,"H")))
table(ama_merge_data$sample_type,useNA="always")
length(which(!(is.na(ama_merge_test$HH_ID)))) # looks good
# then for mosquito abdomens
ama_merge_data = ama_merge_data %>%
  rename("sample_id_abdomen"="sample_id_head")
ama_merge_test = left_join(ama_merge_data,anoph_merged_data,by="sample_id_abdomen")
# check which ones didn't merge
length(which(is.na(ama_merge_test$HH_ID)))
length(setdiff(ama_merge_test$sample_id_abdomen,anoph_merged_data$sample_id_abdomen))
ama_unmerged_samples = data.frame(unmerged_samples = setdiff(ama_merge_test$sample_id_abdomen,anoph_merged_data$sample_id_abdomen))
ama_unmerged_samples %>% filter((str_detect(unmerged_samples,"A")))
table(ama_merge_data$sample_type,useNA="always")
length(which(!(is.na(ama_merge_test$HH_ID)))) # still have two abdomen IDs to merge in
# these two ids are: K01 A00030 and K01 A00047
# we will be able to merge these into the data set later on

# first rename the sample id variable for the haplotype data set
ama_merge_data = ama_merge_data %>%
  rename("sample_name_dbs"="sample_id_abdomen")

# write out as an RDS and CSV files for ama
# write_rds(ama_merge_data,"Desktop/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")
# write_csv(ama_merge_data,"Desktop/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.csv")



## try this first for csp

# first rename the sample id variable for the haplotype data set
csp_merge_data = csp_merge_data %>%
  rename("sample_name_dbs"="sample_id")

# now merge the data sets together
csp_merge_test = left_join(csp_merge_data,final_data,by="sample_name_dbs")
# check which ones didn't merge
length(which(is.na(csp_merge_test$sample_id_date)))
length(setdiff(csp_merge_test$sample_name_dbs,final_data$sample_name_dbs))
csp_unmerged_samples = data.frame(unmerged_samples = setdiff(csp_merge_test$sample_name_dbs,final_data$sample_name_dbs))
# looks like most of these are mosquitoes so filter those out
csp_unmerged_samples %>% filter(!(str_detect(unmerged_samples,"A") | str_detect(unmerged_samples,"H")))
# 28 samples didn't merge that were human samples, some look miscoded
# K01-120617-4 - this was a dbs that had two collected on same day, kept one with R in the data set so delete this one
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_name_dbs == "K01-120617-4"),]
# K140051017-4 - this is miscoded so recode
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="K140051017-4")]="K14-051017-4"
# M13-1700817-6 - this is miscoded so recode
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="M13-1700817-6")]="M13-170817-6"
# M05-170817-5 - this is miscoded so recode
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="M05- 170817-5")]="M05-170817-5"
# M15-311017-6 - this is miscoded so recode to have an R
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="M15-311017-6")]="M15-311017-6-R"
# M06-130617-4 - this was a dbs that had two collected on same day, kept one with R in the data set so delete this one
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_name_dbs == "M06-130617-4"),]
# M16-2011117-2-R - this is miscoded so recode
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="M16-2011117-2-R")]="M16-201117-2-R"
# M03-120717-3 - this is miscoded so recode M03-130717-3
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="M03-120717-3")]="M03-130717-3"
# S10-170717-5 - this is miscoded so recode to have an R
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="S10-170717-5")]="S10-170717-5-R"
# M04-012017-1 - this is miscoded so recode
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="M04-012017-1")]="M04-121017-1"
# M12-130617-3 - not in final data set so delete
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_name_dbs == "M12-130617-3"),]
# M05-260618-14-R - this is miscoded so recode
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="M05-260618-14-R")]="M05-260618-4-R"
# S13-090418-1 - this is miscoded so recode to have an R
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="S13-090418-1")]="S13-090418-1-R"
# S09-110418-6 - this is miscoded so recode to have an R
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="S09-110418-6")]="S09-110418-6-R"
# S10-110418-4 - this is miscoded so recode to have an R
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="S10-110418-4")]="S10-110418-4-R"
# S05-270618-3 - this is miscoded so recode to have an R
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="S05-270618-3")]="S05-270618-3-R"
# K10-100418-4 - this is miscoded so recode to have an R
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="K10-100418-4")]="K10-100418-4-R"
# K12-230518-3 - this is miscoded so recode to have an R
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="K12-230518-3")]="K12-230518-3-R"
# K01-250718-3 - this is miscoded so recode to have an R
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="K01-250718-3")]="K01-250718-3-R"
# K02-060727-5 - this is miscoded so recode
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="K02-060727-5")]="K02-060717-5"
# K14-071217-04 - this is miscoded so recode
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="K14-071217-04")]="K14-071217-4"
# K02-002117-5 - this is miscoded so recode
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="K02-002117-5")]="K02-021117-5"
# M06-130517-1 - this is miscoded so recode
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="M06-130517-1")]="M06-130717-1"
# S08-1910176-6 - this is miscoded so recoded
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="S08-1910176-6")]="S08-191017-6"
# M11-120617-3 - this is miscoded so recode to M11-130617-3
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="M11-120617-3")]="M11-130617-3"
# S11-240817-9 - this is miscoded so recode to S11-240617-9
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="S11-240817-9")]="S11-240617-9"
# K01-130717-9 - this is miscoded so recode to K01-110717-9-R
csp_merge_data$sample_name_dbs[which(csp_merge_data$sample_name_dbs=="K01-130717-9")]="K01-110717-9-R"
# now do another test to merge the data sets together
csp_merge_test = left_join(csp_merge_data,final_data,by="sample_name_dbs")
# check which ones didn't merge
length(which(is.na(csp_merge_test$sample_id_date)))
length(setdiff(csp_merge_test$sample_name_dbs,final_data$sample_name_dbs))
csp_unmerged_samples = data.frame(unmerged_samples = setdiff(csp_merge_test$sample_name_dbs,final_data$sample_name_dbs))
csp_unmerged_samples %>% filter(!(str_detect(unmerged_samples,"A") | str_detect(unmerged_samples,"H")))
# there's one more sample left to recode
# S03-220318-5 - not in final data set so delete
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_name_dbs == "S03-220318-5"),]
# now do another test to merge the data sets together
csp_merge_test = left_join(csp_merge_data,final_data,by="sample_name_dbs")
# check which ones didn't merge
length(which(is.na(csp_merge_test$sample_id_date)))
length(setdiff(csp_merge_test$sample_name_dbs,final_data$sample_name_dbs))
csp_unmerged_samples = data.frame(unmerged_samples = setdiff(csp_merge_test$sample_name_dbs,final_data$sample_name_dbs))
csp_unmerged_samples %>% filter(!(str_detect(unmerged_samples,"A") | str_detect(unmerged_samples,"H")))
# looks good

# check the mosquito merging
# first for mosquito heads
csp_merge_data = csp_merge_data %>%
  rename("sample_id_head"="sample_name_dbs")
csp_merge_test = left_join(csp_merge_data,anoph_merged_data,by="sample_id_head")
# check which ones didn't merge
length(which(is.na(csp_merge_test$HH_ID)))
length(setdiff(csp_merge_test$sample_id_head,anoph_merged_data$sample_id_head))
csp_unmerged_samples = data.frame(unmerged_samples = setdiff(csp_merge_test$sample_id_head,anoph_merged_data$sample_id_head))
csp_unmerged_samples %>% filter((str_detect(unmerged_samples,"H")))
table(csp_merge_data$sample_type,useNA="always")
length(which(!(is.na(csp_merge_test$HH_ID)))) # 2 heads didn't merge
# these were: K01 H00030 and K01 H00047, were sequenced and we will be able to merge later
# then for mosquito abdomens
csp_merge_data = csp_merge_data %>%
  rename("sample_id_abdomen"="sample_id_head")
csp_merge_test = left_join(csp_merge_data,anoph_merged_data,by="sample_id_abdomen")
# check which ones didn't merge
length(which(is.na(csp_merge_test$HH_ID)))
length(setdiff(csp_merge_test$sample_id_abdomen,anoph_merged_data$sample_id_abdomen))
csp_unmerged_samples = data.frame(unmerged_samples = setdiff(csp_merge_test$sample_id_abdomen,anoph_merged_data$sample_id_abdomen))
csp_unmerged_samples %>% filter((str_detect(unmerged_samples,"A")))
table(csp_merge_data$sample_type,useNA="always")
length(which(!(is.na(csp_merge_test$HH_ID)))) # still have two abdomen IDs to merge in
# these two ids are: K01 A00030 and K01 A00047
# we will be able to merge these into the data set later on


# first rename the sample id variable for the haplotype data set
csp_merge_data = csp_merge_data %>%
  rename("sample_name_dbs"="sample_id_abdomen")


# write out as an RDS and CSV files for csp
write_rds(csp_merge_data,"Desktop/spat21_csp_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")
write_csv(csp_merge_data,"Desktop/spat21_csp_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.csv")




