# ----------------------------------------- #
#     Figure out Sequencing Flow Chart      #
#           October 15, 2019                #
#                K. Sumner                  #
# ----------------------------------------- #

#### ------- load libraries ---------- ####

# load in libraries
library(tidyverse)


#### ------- read in the data sets --------- #####

# read in the miseq inventory
miseq_inventory = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Sequencing Inventories/All Samples/spat21_sequencing_all_samples_inventory_24SEP2019.csv")

# read in the merged anpopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")

# load in the ama data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
ama_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# load in the csp data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
csp_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")


##### -------- look at the samples collected ---------- ####

# look at the human samples collected
length(which(is.na(final_data$sample_name_dbs)))

# look at the mosquito samples collected
length(which(!(is.na(anoph_merged_data$sample_id_abdomen)))) # 1450 abdomens
length(which(!(is.na(anoph_merged_data$sample_id_head)))) # 1465 heads
# note: have 2 more heads and abdomens to add: K01 00047 and K01 00030




#### ------- look at the samples sequenced --------- ####

# change some of the column names
miseq_inventory = miseq_inventory %>%
  rename("sample_id"="Sample ID")

# remove the controls
miseq_inventory= miseq_inventory[-which(miseq_inventory$`New MiSeq ID` == "USID289" | miseq_inventory$`New MiSeq ID` == "USID294" | miseq_inventory$`New MiSeq ID` == "USID303" | miseq_inventory$`New MiSeq ID` == "USID304" | miseq_inventory$`New MiSeq ID` == "USID305" | miseq_inventory$`New MiSeq ID` == "USID779" | miseq_inventory$`New MiSeq ID` == "USID780" | miseq_inventory$`New MiSeq ID` == "USID781" | miseq_inventory$`New MiSeq ID` == "USID782" | miseq_inventory$`New MiSeq ID` == "USID783" | miseq_inventory$`New MiSeq ID` == "USID784" | miseq_inventory$`New MiSeq ID` == "USID350" | miseq_inventory$`New MiSeq ID` == "USID353" | miseq_inventory$`New MiSeq ID` == "USID936"),]

# clean one of the ids
miseq_inventory$sample_id[which(miseq_inventory$sample_id=="R-21087-K01-5")] = "R-210817-K01-5"

# first clean the sample ids
# fix the way that some of the samples are named for ama
# now clean up the lab id for Sample Name
table(miseq_inventory$sample_id, useNA = "always")
table(nchar(miseq_inventory$sample_id), useNA = "always")
length(which(is.na(miseq_inventory$sample_id))) # 0 missing IDs
# most sample names are 12 characters or 14 long but range from 10 to 15
# clean up the Sample Names
clean_sample_id = rep(NA,nrow(miseq_inventory))
for (i in 1:nrow(miseq_inventory)){
  if (miseq_inventory$sample_id[i] == "K01- A00043"){
    clean_sample_id[i] = "K01 A00043"
  }
  if (nchar(miseq_inventory$sample_id[i]) == 10 & !(is.na(miseq_inventory$sample_id[i])) & !(str_detect(miseq_inventory$sample_id[i],"A") | str_detect(miseq_inventory$sample_id[i],"H"))){
    clean_sample_id[i] = miseq_inventory$sample_id[i]
  }
  if (nchar(miseq_inventory$sample_id[i]) == 10 & !(is.na(miseq_inventory$sample_id[i])) & (str_detect(miseq_inventory$sample_id[i],"A") | str_detect(miseq_inventory$sample_id[i],"H")) & str_detect(miseq_inventory$sample_id[i],"-")){
    new_name = strsplit(miseq_inventory$sample_id[i],"-")[[1]]
    clean_sample_id[i] = paste0(new_name[1]," ",new_name[2])
  }
  if (nchar(miseq_inventory$sample_id[i]) == 10 & !(is.na(miseq_inventory$sample_id[i])) & (str_detect(miseq_inventory$sample_id[i],"A") | str_detect(miseq_inventory$sample_id[i],"H")) & !(str_detect(miseq_inventory$sample_id[i],"-"))){
    new_name = strsplit(miseq_inventory$sample_id[i]," ")[[1]]
    clean_sample_id[i] = paste0(new_name[1]," ",new_name[2])
  }
  if (nchar(miseq_inventory$sample_id[i]) == 11 & !(is.na(miseq_inventory$sample_id[i])) & miseq_inventory$sample_id[i] != "K01- A00043"){
    parts = strsplit(miseq_inventory$sample_id[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[1],"-","0",parts[2],"-",parts[3]))
  }
  if (nchar(miseq_inventory$sample_id[i]) == 12 & !(is.na(miseq_inventory$sample_id[i]))){
    clean_sample_id[i] = toupper(miseq_inventory$sample_id[i])
  }
  if (nchar(miseq_inventory$sample_id[i]) == 13 & str_count(miseq_inventory$sample_id[i], "-") == 2 & !(is.na(miseq_inventory$sample_id[i]))){
    clean_sample_id[i] = toupper(miseq_inventory$sample_id[i])
  }
  if (nchar(miseq_inventory$sample_id[i]) == 14 & !(is.na(miseq_inventory$sample_id[i]))){
    parts = strsplit(miseq_inventory$sample_id[i],"-")[[1]]
    if (parts[4] != "R"){
      clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
    } else {
      clean_sample_id[i] = miseq_inventory$sample_id[i]
    }
  }
  if (nchar(miseq_inventory$sample_id[i]) == 15 & !(is.na(miseq_inventory$sample_id[i]))){
    parts = strsplit(miseq_inventory$sample_id[i],"-")[[1]]
    if (parts[4] != "R"){
      clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
    } else {
      clean_sample_id[i] = miseq_inventory$sample_id[i]
    }
  }
}
# check the recode
table(clean_sample_id, useNA = "always")
length(which(is.na(clean_sample_id))) # 0 missing
table(nchar(clean_sample_id))
# check the recoding
recode_check = data.frame(miseq_inventory$sample_id,clean_sample_id)
# add the recode to the data set
miseq_inventory$sample_id = clean_sample_id

# look up each type of sample
test1 = miseq_inventory %>%
  filter(str_detect(sample_id,"A") & !(str_detect(sample_id,"1A")) & !(str_detect(sample_id,"NA")))
length(unique(test1$sample_id))
test2 = miseq_inventory %>%
  filter(str_detect(sample_id,"H"))
length(unique(test2$sample_id))
test3 = miseq_inventory %>%
  filter(nchar(sample_id)>10)
length(unique(test3$sample_id))


#### ------- figure out how many samples successfully genotyped -------- ####

# figure out how many ama samples successfully genotyped and merged
table(ama_haplotypes$sample_type, useNA = "always")

# figure out how many ama samples successfully genotyped and merged
table(csp_haplotypes$sample_type, useNA = "always")








