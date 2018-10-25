# ----------------------------------------- #
#       Make Haplotype Data for Cody        #
#            October 25, 2018               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(igraph)


#### --------- for the csp data set ----------------- ####

# read in the merged data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")

## CSP
# create a summary data frame of each of the households and the samples within them for the 514 1/1/18 sequenced samples
csp_df <- merged_data %>% 
  filter(CSP_number_of_haplotypes != "Removed" & !(is.na(CSP_number_of_haplotypes)) & !(is.na(studyid_case_control_new))) %>%
  group_by(studyid_case_control_new) %>%
  summarise(n=n())

# looks like there are too many people at the HH level so are changing to the look at the number of haplotypes
# that are shared at the "location" level
## CSP
# create a summary data frame of each of the village locations and the samples within them for the 514 1/1/18 sequenced samples
csp_df <- merged_data %>% 
  filter(CSP_number_of_haplotypes != "Removed" & !(is.na(CSP_number_of_haplotypes)) & !(is.na(labid_new)) & !(is.na(location)) & !(is.na(studyid_case_control_new))) %>%
  group_by(location, labid_new, studyid_case_control_new) %>%
  summarise(n=n())
# rename the lab id column
names(csp_df)[names(csp_df) == "labid_new"] <- "labid_old_labinventory"

# read in the CSP haplotype sample summary data set
csp_haplotypes = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/CSP_sample_summary.csv")

# subset the csp_haplotypes data set to just the variables of interest
csp_haplotypes = csp_haplotypes[,c("lab_miseq_sample","lab_mesa_id","number_of_haplotypes","total_reads_all_noncontrol_haplotypes")]

# create a function for something not being in the list
"%ni%" <- Negate("%in%")

## clean the csp_haplotypes data set
# look at the data set
table(csp_haplotypes$lab_mesa_id, useNA = "always")
# recode the "none1" and "none2" labids to NA
csp_haplotypes$lab_mesa_id[csp_haplotypes$lab_mesa_id == "none1"] = NA
csp_haplotypes$lab_mesa_id[csp_haplotypes$lab_mesa_id == "none2"] = NA
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

# merge these two data sets
csp_merge = left_join(csp_haplotypes, csp_df, by = "labid_old_labinventory")

# remove the missing values
csp_merge = csp_merge[which(is.na(csp_merge$location) == F),]

# created a new variable in merged IDs that is just the sample IDs Betsy gave the sequenced samples (S1, S2, etc.)
sids = rep(NA,nrow(csp_merge))
for (i in 1:nrow(csp_merge)){
  sids[i] = strsplit(csp_merge$lab_miseq_sample,"_")[[i]][1]
}
table(sids, useNA = "always")
csp_merge$sids = sids

# read in the CSP haplotype sequence table
csp_data = readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/MESA_CSP_haplotypes_final.rds")
# look at the data set
head(csp_data)
# make it a data frame
csp_data = data.frame(csp_data)
# note: this is the final cleaned CSP haplotype sequence table with the H67 column removed (that haplotype only found in controls)

# create a new column in the CSP haplotype sequence table that is a list of all the sample names
csp_data$sids = rownames(csp_data)

# merge the SIDs with the rownames of the CSP haplotype sequence table
haplotype_merge = left_join(csp_data, csp_merge, by = "sids")

# write out as a csv
write_csv(haplotype_merge,"haplotype_merge_CSP.csv")


#### --------- for the ama data set ----------------- ####

# read in the merged data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")

## ama
# create a summary data frame of each of the households and the samples within them for the 514 1/1/18 sequenced samples
ama_df <- merged_data %>% 
  filter(AMA_number_of_haplotypes != "Removed" & !(is.na(AMA_number_of_haplotypes)) & !(is.na(studyid_case_control_new))) %>%
  group_by(studyid_case_control_new) %>%
  summarise(n=n())

# looks like there are too many people at the HH level so are changing to the look at the number of haplotypes
# that are shared at the "location" level
# create a summary data frame of each of the village locations and the samples within them for the 514 1/1/18 sequenced samples
ama_df <- merged_data %>% 
  filter(AMA_number_of_haplotypes != "Removed" & !(is.na(AMA_number_of_haplotypes)) & !(is.na(labid_new)) & !(is.na(location)) & !(is.na(studyid_case_control_new))) %>%
  group_by(location, labid_new, studyid_case_control_new) %>%
  summarise(n=n())
# rename the lab id column
names(ama_df)[names(ama_df) == "labid_new"] <- "labid_old_labinventory"

# read in the CSP haplotype sample summary data set
ama_haplotypes = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_sample_summary.csv")

# subset the csp_haplotypes data set to just the variables of interest
ama_haplotypes = ama_haplotypes[,c("lab_miseq_sample","lab_mesa_id","number_of_haplotypes","total_reads_all_noncontrol_haplotypes")]

# create a function for something not being in the list
"%ni%" <- Negate("%in%")

## clean the csp_haplotypes data set
# look at the data set
table(ama_haplotypes$lab_mesa_id, useNA = "always")
# recode the "none1" and "none2" labids to NA
ama_haplotypes$lab_mesa_id[ama_haplotypes$lab_mesa_id == "none1"] = NA
ama_haplotypes$lab_mesa_id[ama_haplotypes$lab_mesa_id == "none2"] = NA
# check for "-" in labids
length(which(is.na(ama_haplotypes$lab_mesa_id) == T))
mesaid = rep(NA,nrow(ama_haplotypes))
for (m in 1:nrow(ama_haplotypes)){
  if ("-" %in% strsplit(ama_haplotypes$lab_mesa_id[m],"")[[1]]){
    mesaid[m] = m
  } else {
    mesaid[m] = NA
  }
}
length(na.omit(mesaid))
# now clean the labid variable and rename it to labid_old_labinventory
cleanlab_id = rep(NA,nrow(ama_haplotypes))
for (k in 1:nrow(ama_haplotypes)){
  if (is.na(ama_haplotypes$lab_mesa_id[k]) == T){
    cleanlab_id[k] = NA
  } else if ("_" %ni% strsplit(ama_haplotypes$lab_mesa_id[k],"")[[1]]) {
    if (nchar(ama_haplotypes$lab_mesa_id[k]) == 4){
      cleanlab_id[k] = paste0(ama_haplotypes$lab_mesa_id[k])
    }
    if (nchar(ama_haplotypes$lab_mesa_id[k]) == 3){
      cleanlab_id[k] = paste0("0",ama_haplotypes$lab_mesa_id[k])
    }
    if (nchar(ama_haplotypes$lab_mesa_id[k]) == 2){
      cleanlab_id[k] = paste0("00",ama_haplotypes$lab_mesa_id[k])
    }
    if (nchar(ama_haplotypes$lab_mesa_id[k]) == 1){
      cleanlab_id[k] = paste0("000",ama_haplotypes$lab_mesa_id[k])
    }
  } else {
    part_mesa_id = strsplit(ama_haplotypes$lab_mesa_id[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      cleanlab_id[k] = paste0(ama_haplotypes$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 3){
      cleanlab_id[k] = paste0("0",ama_haplotypes$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 2){
      cleanlab_id[k] = paste0("00",ama_haplotypes$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 1){
      cleanlab_id[k] = paste0("000",ama_haplotypes$lab_mesa_id[k])
    }
  }
}
ama_haplotypes$labid_old_labinventory = toupper(cleanlab_id)
# change controls to NA
ama_haplotypes$labid_old_labinventory[ama_haplotypes$labid_old_labinventory == "03D7"] = NA
# should now have 5 NAs
length(which(is.na(ama_haplotypes$labid_old_labinventory) == T)) # 5 NAs so looks good
# remove lab_mesa_id
ama_haplotypes$lab_mesa_id <- NULL

# merge these two data sets
ama_merge = left_join(ama_haplotypes, ama_df, by = "labid_old_labinventory")

# remove the missing values
ama_merge = ama_merge[which(is.na(ama_merge$location) == F),]

# created a new variable in merged IDs that is just the sample IDs Betsy gave the sequenced samples (S1, S2, etc.)
sids = rep(NA,nrow(ama_merge))
for (i in 1:nrow(ama_merge)){
  sids[i] = strsplit(ama_merge$lab_miseq_sample,"_")[[i]][1]
}
table(sids, useNA = "always")
ama_merge$sids = sids

# read in the ama haplotype sequence table
ama_data = readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/MESA_AMA_haplotypes_final.rds")
# look at the data set
head(ama_data)
# make it a data frame
ama_data = data.frame(ama_data)
# note: this is the final cleaned CSP haplotype sequence table with the H67 column removed (that haplotype only found in controls)

# create a new column in the CSP haplotype sequence table that is a list of all the sample names
ama_data$sids = rownames(ama_data)

# merge the SIDs with the rownames of the CSP haplotype sequence table
haplotype_merge = left_join(ama_data, ama_merge, by = "sids")

# write out as a csv
write_csv(haplotype_merge,"haplotype_merge_AMA.csv")


#### --------- for the histb data set ----------------- ####

# read in the merged data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")

## histb
# create a summary data frame of each of the households and the samples within them for the 514 1/1/18 sequenced samples
histb_df <- merged_data %>% 
  filter(HistB_number_of_haplotypes != "Removed" & !(is.na(HistB_number_of_haplotypes)) & !(is.na(studyid_case_control_new))) %>%
  group_by(studyid_case_control_new) %>%
  summarise(n=n())

# looks like there are too many people at the HH level so are changing to the look at the number of haplotypes
# that are shared at the "location" level
## CSP
# create a summary data frame of each of the village locations and the samples within them for the 514 1/1/18 sequenced samples
histb_df <- merged_data %>% 
  filter(HistB_number_of_haplotypes != "Removed" & !(is.na(HistB_number_of_haplotypes)) & !(is.na(labid_new)) & !(is.na(location)), !(is.na(studyid_case_control_new))) %>%
  group_by(location, labid_new, studyid_case_control_new) %>%
  summarise(n=n())
# rename the lab id column
names(histb_df)[names(histb_df) == "labid_new"] <- "labid_old_labinventory"

# read in the CSP haplotype sample summary data set
histb_haplotypes = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/HistB_sample_summary.csv")

# subset the csp_haplotypes data set to just the variables of interest
histb_haplotypes = histb_haplotypes[,c("lab_miseq_sample","lab_mesa_id","number_of_haplotypes","total_reads_all_noncontrol_haplotypes")]

# create a function for something not being in the list
"%ni%" <- Negate("%in%")

## clean the csp_haplotypes data set
# look at the data set
table(histb_haplotypes$lab_mesa_id, useNA = "always")
# recode the "none1" and "none2" labids to NA
histb_haplotypes$lab_mesa_id[histb_haplotypes$lab_mesa_id == "none1"] = NA
histb_haplotypes$lab_mesa_id[histb_haplotypes$lab_mesa_id == "none2"] = NA
# check for "-" in labids
length(which(is.na(histb_haplotypes$lab_mesa_id) == T))
mesaid = rep(NA,nrow(histb_haplotypes))
for (m in 1:nrow(histb_haplotypes)){
  if ("-" %in% strsplit(histb_haplotypes$lab_mesa_id[m],"")[[1]]){
    mesaid[m] = m
  } else {
    mesaid[m] = NA
  }
}
length(na.omit(mesaid))
# now clean the labid variable and rename it to labid_old_labinventory
cleanlab_id = rep(NA,nrow(histb_haplotypes))
for (k in 1:nrow(histb_haplotypes)){
  if (is.na(histb_haplotypes$lab_mesa_id[k]) == T){
    cleanlab_id[k] = NA
  } else if ("_" %ni% strsplit(histb_haplotypes$lab_mesa_id[k],"")[[1]]) {
    if (nchar(histb_haplotypes$lab_mesa_id[k]) == 4){
      cleanlab_id[k] = paste0(histb_haplotypes$lab_mesa_id[k])
    }
    if (nchar(histb_haplotypes$lab_mesa_id[k]) == 3){
      cleanlab_id[k] = paste0("0",histb_haplotypes$lab_mesa_id[k])
    }
    if (nchar(histb_haplotypes$lab_mesa_id[k]) == 2){
      cleanlab_id[k] = paste0("00",histb_haplotypes$lab_mesa_id[k])
    }
    if (nchar(histb_haplotypes$lab_mesa_id[k]) == 1){
      cleanlab_id[k] = paste0("000",histb_haplotypes$lab_mesa_id[k])
    }
  } else {
    part_mesa_id = strsplit(histb_haplotypes$lab_mesa_id[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      cleanlab_id[k] = paste0(histb_haplotypes$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 3){
      cleanlab_id[k] = paste0("0",histb_haplotypes$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 2){
      cleanlab_id[k] = paste0("00",histb_haplotypes$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 1){
      cleanlab_id[k] = paste0("000",histb_haplotypes$lab_mesa_id[k])
    }
  }
}
histb_haplotypes$labid_old_labinventory = toupper(cleanlab_id)
# change controls to NA
histb_haplotypes$labid_old_labinventory[histb_haplotypes$labid_old_labinventory == "03D7"] = NA
# should now have 5 NAs
length(which(is.na(histb_haplotypes$labid_old_labinventory) == T)) # 5 NAs so looks good
# remove lab_mesa_id
histb_haplotypes$lab_mesa_id <- NULL

# merge these two data sets
histb_merge = left_join(histb_haplotypes, histb_df, by = "labid_old_labinventory")

# remove the missing values
histb_merge = histb_merge[which(is.na(histb_merge$location) == F),]

# created a new variable in merged IDs that is just the sample IDs Betsy gave the sequenced samples (S1, S2, etc.)
sids = rep(NA,nrow(histb_merge))
for (i in 1:nrow(histb_merge)){
  sids[i] = strsplit(histb_merge$lab_miseq_sample,"_")[[i]][1]
}
table(sids, useNA = "always")
histb_merge$sids = sids

# read in the histb haplotype sequence table
histb_data = readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/MESA_HistB_haplotypes_final.rds")
# look at the data set
head(histb_data)
# make it a data frame
histb_data = data.frame(histb_data)
# note: this is the final cleaned histb haplotype sequence table with the H67 column removed (that haplotype only found in controls)

# create a new column in the histb haplotype sequence table that is a list of all the sample names
histb_data$sids = rownames(histb_data)

# merge the SIDs with the rownames of the histb haplotype sequence table
haplotype_merge = left_join(histb_data, histb_merge, by = "sids")

# write out as a csv
write_csv(haplotype_merge,"haplotype_merge_HistB.csv")
