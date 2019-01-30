# ----------------------------------------- #
# Pull out MESA data that failed sequencing #
#            January 29, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(tableone)


#### -------- load data sets --------- ####

# load in the MESA social demographic data set
social_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/FINAL_DATA/final_results_20DEC2018.csv")

# read in one of the haplotype data sets
csp_haplotypes = read_rds("Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/CSP/CSP_haplotype_output_with_sample_names.rds")

# read in the sample id inventory
sample_inventory = read_csv("Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/inventory/MESA_sequencing_ids_ALLSAMPLES.csv")


#### ---- figure out the samples that didn't create haplotypes ----- ####

# rename MESA ID
csp_haplotypes = rename(csp_haplotypes, lab_mesa_id = `MESA ID`)
colnames(csp_haplotypes)

# create a function for something not being in the list
"%ni%" <- Negate("%in%")

## clean the csp_haplotypes data set
# look at the data set
table(csp_haplotypes$lab_mesa_id, useNA = "always")
# recode the "none1" and "none2" labids to NA
csp_haplotypes$lab_mesa_id[csp_haplotypes$lab_mesa_id == "none1"] = NA
csp_haplotypes$lab_mesa_id[csp_haplotypes$lab_mesa_id == "none2"] = NA
length(which(is.na(csp_haplotypes$lab_mesa_id)))
# check for "-" in labids
length(which(is.na(csp_haplotypes$lab_mesa_id) == T))
mesaid = rep(NA,nrow(csp_haplotypes))
for (m in 1:nrow(csp_haplotypes)){
  if ("-" %in% strsplit(csp_haplotypes$lab_mesa_id[m],"")[[1]]){
    mesaid[m] = m
  } else {
    mesaid[m] = NA
  }
}
length(na.omit(mesaid))
# now clean the labid variable and rename it to labid_old_labinventory
cleanlab_id = rep(NA,nrow(csp_haplotypes))
for (k in 1:nrow(csp_haplotypes)){
  if (is.na(csp_haplotypes$lab_mesa_id[k]) == T){
    cleanlab_id[k] = NA
  } else if ("_" %ni% strsplit(csp_haplotypes$lab_mesa_id[k],"")[[1]]) {
    if (nchar(csp_haplotypes$lab_mesa_id[k]) == 4){
      cleanlab_id[k] = paste0(csp_haplotypes$lab_mesa_id[k])
    }
    if (nchar(csp_haplotypes$lab_mesa_id[k]) == 3){
      cleanlab_id[k] = paste0("0",csp_haplotypes$lab_mesa_id[k])
    }
    if (nchar(csp_haplotypes$lab_mesa_id[k]) == 2){
      cleanlab_id[k] = paste0("00",csp_haplotypes$lab_mesa_id[k])
    }
    if (nchar(csp_haplotypes$lab_mesa_id[k]) == 1){
      cleanlab_id[k] = paste0("000",csp_haplotypes$lab_mesa_id[k])
    }
  } else {
    part_mesa_id = strsplit(csp_haplotypes$lab_mesa_id[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      cleanlab_id[k] = paste0(csp_haplotypes$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 3){
      cleanlab_id[k] = paste0("0",csp_haplotypes$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 2){
      cleanlab_id[k] = paste0("00",csp_haplotypes$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 1){
      cleanlab_id[k] = paste0("000",csp_haplotypes$lab_mesa_id[k])
    }
  }
}
csp_haplotypes$labid_old_labinventory = toupper(cleanlab_id)
# change controls to NA
csp_haplotypes$labid_old_labinventory[csp_haplotypes$labid_old_labinventory == "03D7"] = NA
# should now have 5 NAs
length(which(is.na(csp_haplotypes$labid_old_labinventory) == T)) # 5 NAs so looks good
# remove lab_mesa_id
csp_haplotypes$lab_mesa_id <- NULL

# rename MESA ID
sample_inventory = rename(sample_inventory, lab_mesa_id  = `MESA ID`)
colnames(sample_inventory)

## clean the csp_haplotypes data set
# look at the data set
table(sample_inventory$lab_mesa_id, useNA = "always")
# recode the "none1" and "none2" labids to NA
sample_inventory$lab_mesa_id[sample_inventory$lab_mesa_id == "none1"] = NA
sample_inventory$lab_mesa_id[sample_inventory$lab_mesa_id == "none2"] = NA
length(which(is.na(sample_inventory$lab_mesa_id)))
# check for "-" in labids
length(which(is.na(sample_inventory$lab_mesa_id) == T))
mesaid = rep(NA,nrow(sample_inventory))
for (m in 1:nrow(sample_inventory)){
  if ("-" %in% strsplit(sample_inventory$lab_mesa_id[m],"")[[1]]){
    mesaid[m] = m
  } else {
    mesaid[m] = NA
  }
}
length(na.omit(mesaid))
# now clean the labid variable and rename it to labid_old_labinventory
cleanlab_id = rep(NA,nrow(sample_inventory))
for (k in 1:nrow(sample_inventory)){
  if (is.na(sample_inventory$lab_mesa_id[k]) == T){
    cleanlab_id[k] = NA
  } else if ("_" %ni% strsplit(sample_inventory$lab_mesa_id[k],"")[[1]]) {
    if (nchar(sample_inventory$lab_mesa_id[k]) == 4){
      cleanlab_id[k] = paste0(sample_inventory$lab_mesa_id[k])
    }
    if (nchar(sample_inventory$lab_mesa_id[k]) == 3){
      cleanlab_id[k] = paste0("0",sample_inventory$lab_mesa_id[k])
    }
    if (nchar(sample_inventory$lab_mesa_id[k]) == 2){
      cleanlab_id[k] = paste0("00",sample_inventory$lab_mesa_id[k])
    }
    if (nchar(sample_inventory$lab_mesa_id[k]) == 1){
      cleanlab_id[k] = paste0("000",sample_inventory$lab_mesa_id[k])
    }
  } else {
    part_mesa_id = strsplit(sample_inventory$lab_mesa_id[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      cleanlab_id[k] = paste0(sample_inventory$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 3){
      cleanlab_id[k] = paste0("0",sample_inventory$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 2){
      cleanlab_id[k] = paste0("00",sample_inventory$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 1){
      cleanlab_id[k] = paste0("000",sample_inventory$lab_mesa_id[k])
    }
  }
}
sample_inventory$labid_old_labinventory = toupper(cleanlab_id)
# change controls to NA
sample_inventory$labid_old_labinventory[sample_inventory$labid_old_labinventory == "03D7"] = NA
# should now have 5 NAs
length(which(is.na(sample_inventory$labid_old_labinventory) == T)) # 5 NAs so looks good
# remove lab_mesa_id
sample_inventory$lab_mesa_id <- NULL
# remove MiSeqID
sample_inventory$`MiSeq ID` <- NULL

# check for duplicates in sample_inventory
count_table = table(sample_inventory$labid_old_labinventory, useNA = "always")
dups_table = count_table[which(count_table > 1)]
# looks like there are two sample duplicates that we ended up recoding later on

# check for duplicates in csp_haplotypes
count_table = table(csp_haplotypes$labid_old_labinventory, useNA = "always")
dups_table = count_table[which(count_table > 1)]
# looks like there are two sample duplicates that we ended up recoding later on

# merge the sample inventory and csp haplotypes data sets together
merged_inventory = left_join(sample_inventory, csp_haplotypes, by = "labid_old_labinventory")
colnames(merged_inventory)

# keep only rows that are all missing 
length(which(is.na(merged_inventory$`MiSeq ID`))) # 273
all_missing_merged_inventory = merged_inventory[which(is.na(merged_inventory$`MiSeq ID`)),]

# merge this with the social demographic data set
csp_merge = left_join(all_missing_merged_inventory, social_data, by = "labid_old_labinventory")

# now subset the columns to only the columns of interest
colnames(csp_merge)
cols_to_keep = c("MiSeq ID","labid_old_labinventory","gdnaplate","gdnacolumn","gdnarow","Experiment Name","Well Position","pfr364CT1","pfr364CT2","pfr364Q_std_combined","")
csp_merge = csp_merge[,colnames(csp_merge) %in% cols_to_keep]

# pull out rows 233:242
only_ten = csp_merge[which(csp_merge$`Experiment Name` == "MESA DBS plate5-6 Taqman duplex 07-23-18"),]
# remove one row
only_ten = only_ten[-1,]

# export the data set
write_csv(only_ten,"MESA_10_samples_failed_sequencing_29JAN2019.csv")


