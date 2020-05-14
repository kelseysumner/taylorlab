# ----------------------------------------- #
#    Create figure of pipeline for aim 2    #
#                 manuscript                #
#       Spat21 Haplotype Output Cleaning    #
#       All Samples (Human and Mosquito)    #
#               May 1, 2020                 #
#                K. Sumner                  #
# ----------------------------------------- #


#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(tableone)
library(MHCtools)
library(dada2)
library(ggplot2)
library(stringr)
library(Biostrings)
library(schoolmath)

#### ------- read in the CSP haplotype output -------------- ####

# read in the haplotype data set
foo = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Haplotype Results/CSP/raw haplotype output/CSP_spat21_allsamples_haplotypes.rds")

# figure out how many rows and columns
nrow(foo)
ncol(foo)
table(nchar(getSequences(foo)))

# read in the mosquito miseq inventory
miseq_inventory = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Sequencing Inventories/All Samples/spat21_sequencing_all_samples_inventory_24SEP2019.csv")

# cut down the miseq_inventory to just the columns of interest
miseq_inventory = miseq_inventory[,c(2,3,4)]
miseq_inventory = dplyr::rename(miseq_inventory, "MiSeq.ID" = "New MiSeq ID","sample_id"="Sample ID")

# make the matrix a dataframe
foo = as.data.frame(foo)

# create a new column of foo that is the sample names (rownames)
foo$`MiSeq.ID` = rownames(foo)
colnames(foo)

# merge in the mosquito miseq inventory with the ama haplotype data
csp_merge_data = dplyr::left_join(miseq_inventory,foo,by="MiSeq.ID")

# read in the final data set of human demographic data
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")
final_data = final_data %>% select(c(sample_name_dbs,main_exposure_primary_case_def,main_outcome_primary_case_def))
final_data = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def)))
table(final_data$aim2_exposure)

# fix the way that some of the samples are named for csp
# now clean up the lab id for Sample Name
table(csp_merge_data$sample_id, useNA = "always")
table(nchar(csp_merge_data$sample_id), useNA = "always")
length(which(is.na(csp_merge_data$sample_id))) # 0 missing IDs
csp_merge_data$sample_id[which(nchar(csp_merge_data$sample_id)==3)]
csp_merge_data$sample_id[which(nchar(csp_merge_data$sample_id)==5)]
csp_merge_data$sample_id[which(nchar(csp_merge_data$sample_id)==7)]
csp_merge_data$sample_id[which(nchar(csp_merge_data$sample_id)==9)]
csp_merge_data$sample_id[which(nchar(csp_merge_data$sample_id)==10)]
csp_merge_data$sample_id[which(nchar(csp_merge_data$sample_id)==11)]
csp_merge_data = csp_merge_data[-which(csp_merge_data$sample_id == "3D7/7g8/Dd2" | csp_merge_data$sample_id == "3D7 gDNA stock" | nchar(csp_merge_data$sample_id) < 10 | nchar(csp_merge_data$sample_id) > 15),]
csp_merge_data$sample_id[which(nchar(csp_merge_data$sample_id)==12)]
csp_merge_data$sample_id[which(nchar(csp_merge_data$sample_id)==13)]
csp_merge_data$sample_id[which(nchar(csp_merge_data$sample_id)==14)]
csp_merge_data$sample_id[which(nchar(csp_merge_data$sample_id)==15)]
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

# cut down csp_merge_data
length(intersect(final_data$sample_name_dbs,csp_merge_data$sample_id))
setdiff(final_data$sample_name_dbs,csp_merge_data$sample_id)
csp_merge_data = csp_merge_data %>%
  filter(csp_merge_data$sample_id %in% final_data$sample_name_dbs)

