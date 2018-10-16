# ------------------------------------ #
#       MESA Haplotype Analysis        #
#  Merge All 3 Data Set (Social, lab,  #
#            and haplotype)            #
#           October 14, 2018           #
#             K. Sumner                #
# ------------------------------------ #


#### ----- LOAD PACKAGES & DATA SETS ----- ####
# load packages
library(readr) # for reading in csvs using read_csv (more efficient and better at keeping data format)
library(dplyr) # for left_join function
# read in the data sets
qpcr_alldups = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/Compiled MESA DBS detection results 9Oct2018_DUPLICATESONLY.csv")
qpcr_nodups = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/Compiled MESA DBS detection results 9Oct2018 working_NODUPLICATES.csv")
meta_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_data_clean.csv")
lab_key = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/clean_lab_key.csv")
ama_haplotypes = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_sample_summary.csv")
csp_haplotypes = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/CSP_sample_summary.csv")
histb_haplotypes = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/HistB_sample_summary.csv")


#### ------ ClEAN QPCR DATA ------ ####

# create a function for something not being in the list
"%ni%" <- Negate("%in%")

# create new MESA ID column
## for qpcr_nodups
# look for miscoded "-" in the data
table(qpcr_nodups$LabID,useNA = "always")
length(which(is.na(qpcr_nodups$LabID) == T))
nodups_labid = rep(NA,nrow(qpcr_nodups))
for (m in 1:nrow(qpcr_nodups)){
  if ("-" %in% strsplit(qpcr_nodups$LabID[m],"")[[1]]){
    nodups_labid[m] = m
  } else {
    nodups_labid[m] = NA
  }
}
length(na.omit(nodups_labid))
# first find all "-"
changed_labid = rep(NA,nrow(qpcr_nodups))
for (i in 1:nrow(qpcr_nodups)){
  if("-" %in% strsplit(qpcr_nodups$LabID[i],"")[[1]]){
    splitup = strsplit(qpcr_nodups$LabID[i],"-")
    changed_labid[i] = paste0(splitup[[1]][1],"_",splitup[[1]][2])
  } else {
    changed_labid[i] = qpcr_nodups$LabID[i]
  }
}
qpcr_nodups$LabID = changed_labid
# check missingness
length(which(is.na(qpcr_nodups$LabID) == T))
# then add leading 0s and make all uppercase
cleanlab_id = rep(NA,nrow(qpcr_nodups))
for (k in 1:nrow(qpcr_nodups)){
  if (is.na(qpcr_nodups$LabID[k]) == T){
    cleanlab_id[k] = NA
  } else if ("_" %ni% strsplit(qpcr_nodups$LabID[k],"")[[1]]) {
    if (nchar(qpcr_nodups$LabID[k]) == 4){
      cleanlab_id[k] = paste0(qpcr_nodups$LabID[k])
    }
    if (nchar(qpcr_nodups$LabID[k]) == 3){
      cleanlab_id[k] = paste0("0",qpcr_nodups$LabID[k])
    }
    if (nchar(qpcr_nodups$LabID[k]) == 2){
      cleanlab_id[k] = paste0("00",qpcr_nodups$LabID[k])
    }
    if (nchar(qpcr_nodups$LabID[k]) == 1){
      cleanlab_id[k] = paste0("000",qpcr_nodups$LabID[k])
    }
  } else {
    part_mesa_id = strsplit(qpcr_nodups$LabID[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      cleanlab_id[k] = paste0(qpcr_nodups$LabID[k])
    }
    if (nchar(part_mesa_id) == 3){
      cleanlab_id[k] = paste0("0",qpcr_nodups$LabID[k])
    }
    if (nchar(part_mesa_id) == 2){
      cleanlab_id[k] = paste0("00",qpcr_nodups$LabID[k])
    }
    if (nchar(part_mesa_id) == 1){
      cleanlab_id[k] = paste0("000",qpcr_nodups$LabID[k])
    }
  }
}
# check the output
table(cleanlab_id, useNA = "always")
length(which(is.na(cleanlab_id) == T)) # created 7 missing values and should have 0 so something is going on
check_df = data.frame(qpcr_nodups$LabID,cleanlab_id)
check_df = check_df[-which(is.na(check_df$cleanlab_id) == F),]
# add the cleaned variable to the data set
qpcr_nodups$labid_old_labinventory= toupper(cleanlab_id)
# some of these variables were coded differently than usual so will keep their coding in the new variable
qpcr_nodups$labid_old_labinventory[1459] = "0386A"
qpcr_nodups$labid_old_labinventory[1460] = "0386B"
qpcr_nodups$labid_old_labinventory[1461] = "0386C"
qpcr_nodups$labid_old_labinventory[1549] = "0410A"
qpcr_nodups$labid_old_labinventory[1550] = "0410B"
qpcr_nodups$labid_old_labinventory[1551] = "0410C"
qpcr_nodups$labid_old_labinventory[1791] = "04884_B"
# check the output
length(which(is.na(qpcr_nodups$labid_old_labinventory) == T)) # 0 missing so correct now

## for qpcr_alldups
# remove the rows with no labid
qpcr_alldups = qpcr_alldups[-(which(is.na(qpcr_alldups$LabID)==T)),]
# look for miscoded "-" in the data
table(qpcr_alldups$LabID,useNA = "always")
length(which(is.na(qpcr_alldups$LabID) == T))
alldups_labid = rep(NA,nrow(qpcr_alldups))
for (m in 1:nrow(qpcr_alldups)){
  if ("-" %in% strsplit(qpcr_alldups$LabID[m],"")[[1]]){
    alldups_labid[m] = m
  } else {
    alldups_labid[m] = NA
  }
}
length(na.omit(alldups_labid))
# now clean the labid
cleanlab_id = rep(NA,nrow(qpcr_alldups))
for (k in 1:nrow(qpcr_alldups)){
  if (is.na(qpcr_alldups$LabID[k]) == T){
    cleanlab_id[k] = NA
  } else if ("_" %ni% strsplit(qpcr_alldups$LabID[k],"")[[1]]) {
    if (nchar(qpcr_alldups$LabID[k]) == 4){
      cleanlab_id[k] = paste0(qpcr_alldups$LabID[k])
    }
    if (nchar(qpcr_alldups$LabID[k]) == 3){
      cleanlab_id[k] = paste0("0",qpcr_alldups$LabID[k])
    }
    if (nchar(qpcr_alldups$LabID[k]) == 2){
      cleanlab_id[k] = paste0("00",qpcr_alldups$LabID[k])
    }
    if (nchar(qpcr_alldups$LabID[k]) == 1){
      cleanlab_id[k] = paste0("000",qpcr_alldups$LabID[k])
    }
  } else {
    part_mesa_id = strsplit(qpcr_alldups$LabID[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      cleanlab_id[k] = paste0(qpcr_alldups$LabID[k])
    }
    if (nchar(part_mesa_id) == 3){
      cleanlab_id[k] = paste0("0",qpcr_alldups$LabID[k])
    }
    if (nchar(part_mesa_id) == 2){
      cleanlab_id[k] = paste0("00",qpcr_alldups$LabID[k])
    }
    if (nchar(part_mesa_id) == 1){
      cleanlab_id[k] = paste0("000",qpcr_alldups$LabID[k])
    }
  }
}
# check the output
table(cleanlab_id, useNA = "always")
length(which(is.na(cleanlab_id) == T)) # still 0 missing so all is good
# add the new variable to the data set
qpcr_alldups$labid_old_labinventory = toupper(cleanlab_id)

# remove the original labid column
qpcr_alldups$LabID <- NULL
qpcr_nodups$LabID <- NULL

# look at summaries of the data sets
summary(qpcr_nodups)
summary(qpcr_alldups)


#### ------ ClEAN THE AMA, CSP, AND HistB HAPLOTYPE DATA SETS ------ ####

## for AMA
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

## for CSP
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

## for HistB
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

# create subset data sets of the haplotype data sets with just labid_old_labinventory, total_reads_all_noncontrol_haplotypes, and number_of_haplotypes columns
ama_haplotypes = ama_haplotypes[,c("labid_old_labinventory", "total_reads_all_noncontrol_haplotypes","number_of_haplotypes")]
csp_haplotypes = csp_haplotypes[,c("labid_old_labinventory", "total_reads_all_noncontrol_haplotypes","number_of_haplotypes")]
histb_haplotypes = histb_haplotypes[,c("labid_old_labinventory", "total_reads_all_noncontrol_haplotypes","number_of_haplotypes")]

# look at new data sets
length(which(is.na(ama_haplotypes$labid_old_labinventory)))
length(which(is.na(csp_haplotypes$labid_old_labinventory)))
length(which(is.na(histb_haplotypes$labid_old_labinventory)))

# remove the rows where the labid_old_inventory is missing from the data sets
ama_haplotypes = ama_haplotypes[-which(is.na(ama_haplotypes$labid_old_labinventory)),]
csp_haplotypes = csp_haplotypes[-which(is.na(csp_haplotypes$labid_old_labinventory)),]
histb_haplotypes = histb_haplotypes[-which(is.na(histb_haplotypes$labid_old_labinventory)),]


#### ------ CLEAN THE META DATA LABID ------ ####

# create new MESA ID column
## for meta_data
# look for miscoded "-" in the data
table(meta_data$labid,useNA = "always")
length(which(is.na(meta_data$labid) == T))
meta_labid = rep(NA,nrow(meta_data))
for (m in 1:nrow(meta_data)){
  if ("-" %in% strsplit(meta_data$labid[m],"")[[1]]){
    meta_labid[m] = m
  } else {
    meta_labid[m] = NA
  }
}
length(na.omit(meta_labid))
# first find all "-"
changed_labid = rep(NA,nrow(meta_data))
for (i in 1:nrow(meta_data)){
  if("-" %in% strsplit(meta_data$labid[i],"")[[1]]){
    splitup = strsplit(meta_data$labid[i],"-")
    changed_labid[i] = paste0(splitup[[1]][1],"_",splitup[[1]][2])
  } else {
    changed_labid[i] = meta_data$labid[i]
  }
}
meta_data$labid = changed_labid
# check missingness
length(which(is.na(meta_data$labid) == T))
# then add leading 0s and make all uppercase
cleanlab_id = rep(NA,nrow(meta_data))
for (k in 1:nrow(meta_data)){
  if (is.na(meta_data$labid[k]) == T){
    cleanlab_id[k] = NA
  } else if ("_" %ni% strsplit(meta_data$labid[k],"")[[1]]) {
    if (nchar(meta_data$labid[k]) == 4){
      cleanlab_id[k] = paste0(meta_data$labid[k])
    }
    if (nchar(meta_data$labid[k]) == 3){
      cleanlab_id[k] = paste0("0",meta_data$labid[k])
    }
    if (nchar(meta_data$labid[k]) == 2){
      cleanlab_id[k] = paste0("00",meta_data$labid[k])
    }
    if (nchar(meta_data$labid[k]) == 1){
      cleanlab_id[k] = paste0("000",meta_data$labid[k])
    }
  } else {
    part_mesa_id = strsplit(meta_data$labid[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      cleanlab_id[k] = paste0(meta_data$labid[k])
    }
    if (nchar(part_mesa_id) == 3){
      cleanlab_id[k] = paste0("0",meta_data$labid[k])
    }
    if (nchar(part_mesa_id) == 2){
      cleanlab_id[k] = paste0("00",meta_data$labid[k])
    }
    if (nchar(part_mesa_id) == 1){
      cleanlab_id[k] = paste0("000",meta_data$labid[k])
    }
  }
}
# check the output
table(cleanlab_id, useNA = "always")
length(which(is.na(cleanlab_id) == T)) # created 570 missing values and should have 569 so something is going on
check_df = data.frame(meta_data$labid,cleanlab_id)
check_df = check_df[-which(is.na(check_df$cleanlab_id) == F),]
# add the cleaned variable to the data set
meta_data$labid_new= toupper(cleanlab_id)
# some of these variables were coded differently than usual so will keep their coding in the new variable
meta_data$labid_new[2783] = "04884_4"
# check the output
length(which(is.na(meta_data$labid_new) == T)) # 569 missing so correct now

# remove the original labid column
meta_data$labid <- NULL

# remove the rows where the labid_new is missing from the data sets
meta_data = meta_data[-which(is.na(meta_data$labid_new)),]

# look at the new data set
summary(meta_data)


#### ------ CLEAN LAB KEY DATA ------ ####

## clean labid_new variable
# look for miscoded "-" in the data
table(lab_key$labid_new,useNA = "always")
length(which(is.na(lab_key$labid_new) == T))
labidnew = rep(NA,nrow(lab_key))
for (m in 1:nrow(lab_key)){
  if ("-" %in% strsplit(lab_key$labid_new[m],"")[[1]]){
    labidnew[m] = m
  } else {
    labidnew[m] = NA
  }
}
length(na.omit(labidnew))
# then add leading 0s and make all uppercase
cleanlab_id = rep(NA,nrow(lab_key))
for (k in 1:nrow(lab_key)){
  if (is.na(lab_key$labid_new[k]) == T){
    cleanlab_id[k] = NA
  } else if ("_" %ni% strsplit(lab_key$labid_new[k],"")[[1]]) {
    if (nchar(lab_key$labid_new[k]) == 4){
      cleanlab_id[k] = paste0(lab_key$labid_new[k])
    }
    if (nchar(lab_key$labid_new[k]) == 3){
      cleanlab_id[k] = paste0("0",lab_key$labid_new[k])
    }
    if (nchar(lab_key$labid_new[k]) == 2){
      cleanlab_id[k] = paste0("00",lab_key$labid_new[k])
    }
    if (nchar(lab_key$labid_new[k]) == 1){
      cleanlab_id[k] = paste0("000",lab_key$labid_new[k])
    }
  } else {
    part_mesa_id = strsplit(lab_key$labid_new[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      cleanlab_id[k] = paste0(lab_key$labid_new[k])
    }
    if (nchar(part_mesa_id) == 3){
      cleanlab_id[k] = paste0("0",lab_key$labid_new[k])
    }
    if (nchar(part_mesa_id) == 2){
      cleanlab_id[k] = paste0("00",lab_key$labid_new[k])
    }
    if (nchar(part_mesa_id) == 1){
      cleanlab_id[k] = paste0("000",lab_key$labid_new[k])
    }
  }
}
# check the output
length(which(is.na(cleanlab_id) == T)) # added 9 missing so something going on
check_df = data.frame(lab_key$labid_new,cleanlab_id)
check_df = check_df[-which(is.na(check_df$cleanlab_id) == F),]
# add the cleaned variable to the data set
lab_key$labid_new = toupper(cleanlab_id)
# some of these variables were coded differently than usual so will keep their coding in the new variable
lab_key$labid_new[282] = "04884_4"
lab_key$labid_new[5191] = "0381A"
lab_key$labid_new[5192] = "0381B"
lab_key$labid_new[5193] = "0381C"
lab_key$labid_new[5194] = "0381D"
lab_key$labid_new[5200] = "0386A"
lab_key$labid_new[5201] = "0386B"
lab_key$labid_new[5202] = "0386C"
lab_key$labid_new[5239] = "04884_B"
# check the output
length(which(is.na(lab_key$labid_new) == T)) # 0 missing so correct now

## clean labid_old_socialinventory variable
# look for miscoded "-" in the data
table(lab_key$labid_old_socialinventory,useNA = "always")
length(which(is.na(lab_key$labid_old_socialinventory) == T))
labidoldsoc = rep(NA,nrow(lab_key))
for (m in 1:nrow(lab_key)){
  if ("-" %in% strsplit(lab_key$labid_old_socialinventory[m],"")[[1]]){
    labidoldsoc[m] = m
  } else {
    labidoldsoc[m] = NA
  }
}
length(na.omit(labidoldsoc))
# so just add leading 0s and make all uppercase
cleanlab_id = rep(NA,nrow(lab_key))
for (k in 1:nrow(lab_key)){
  if (is.na(lab_key$labid_old_socialinventory[k]) == T){
    cleanlab_id[k] = NA
  } else if ("_" %ni% strsplit(lab_key$labid_old_socialinventory[k],"")[[1]]) {
    if (nchar(lab_key$labid_old_socialinventory[k]) == 4){
      cleanlab_id[k] = paste0(lab_key$labid_old_socialinventory[k])
    }
    if (nchar(lab_key$labid_old_socialinventory[k]) == 3){
      cleanlab_id[k] = paste0("0",lab_key$labid_old_socialinventory[k])
    }
    if (nchar(lab_key$labid_old_socialinventory[k]) == 2){
      cleanlab_id[k] = paste0("00",lab_key$labid_old_socialinventory[k])
    }
    if (nchar(lab_key$labid_old_socialinventory[k]) == 1){
      cleanlab_id[k] = paste0("000",lab_key$labid_old_socialinventory[k])
    }
  } else {
    part_mesa_id = strsplit(lab_key$labid_old_socialinventory[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      cleanlab_id[k] = paste0(lab_key$labid_old_socialinventory[k])
    }
    if (nchar(part_mesa_id) == 3){
      cleanlab_id[k] = paste0("0",lab_key$labid_old_socialinventory[k])
    }
    if (nchar(part_mesa_id) == 2){
      cleanlab_id[k] = paste0("00",lab_key$labid_old_socialinventory[k])
    }
    if (nchar(part_mesa_id) == 1){
      cleanlab_id[k] = paste0("000",lab_key$labid_old_socialinventory[k])
    }
  }
}
# check the output
length(which(is.na(cleanlab_id) == T)) # 274 missing like there was originally
check_df = data.frame(lab_key$labid_old_socialinventory,cleanlab_id)
check_df = check_df[-which(is.na(check_df$cleanlab_id) == F),]
# add the cleaned variable to the data set
lab_key$labid_old_socialinventory = toupper(cleanlab_id)

## clean labid_old_labinventory variable
# look for miscoded "-" in the data
table(lab_key$labid_old_labinventory,useNA = "always")
length(which(is.na(lab_key$labid_old_labinventory) == T))
labidoldlab = rep(NA,nrow(lab_key))
for (m in 1:nrow(lab_key)){
  if ("-" %in% strsplit(lab_key$labid_old_labinventory[m],"")[[1]]){
    labidoldlab[m] = m
  } else {
    labidoldlab[m] = NA
  }
}
length(na.omit(labidoldlab))
# then add leading 0s and make all uppercase
cleanlab_id = rep(NA,nrow(lab_key))
for (k in 1:nrow(lab_key)){
  if (is.na(lab_key$labid_old_labinventory[k]) == T){
    cleanlab_id[k] = NA
  } else if ("_" %ni% strsplit(lab_key$labid_old_labinventory[k],"")[[1]]) {
    if (nchar(lab_key$labid_old_labinventory[k]) == 4){
      cleanlab_id[k] = paste0(lab_key$labid_old_labinventory[k])
    }
    if (nchar(lab_key$labid_old_labinventory[k]) == 3){
      cleanlab_id[k] = paste0("0",lab_key$labid_old_labinventory[k])
    }
    if (nchar(lab_key$labid_old_labinventory[k]) == 2){
      cleanlab_id[k] = paste0("00",lab_key$labid_old_labinventory[k])
    }
    if (nchar(lab_key$labid_old_labinventory[k]) == 1){
      cleanlab_id[k] = paste0("000",lab_key$labid_old_labinventory[k])
    }
  } else {
    part_mesa_id = strsplit(lab_key$labid_old_labinventory[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      cleanlab_id[k] = paste0(lab_key$labid_old_labinventory[k])
    }
    if (nchar(part_mesa_id) == 3){
      cleanlab_id[k] = paste0("0",lab_key$labid_old_labinventory[k])
    }
    if (nchar(part_mesa_id) == 2){
      cleanlab_id[k] = paste0("00",lab_key$labid_old_labinventory[k])
    }
    if (nchar(part_mesa_id) == 1){
      cleanlab_id[k] = paste0("000",lab_key$labid_old_labinventory[k])
    }
  }
}
# check the output
length(which(is.na(cleanlab_id) == T)) # 9 missing so something is going on
check_df = data.frame(lab_key$labid_old_labinventory,cleanlab_id)
check_df = check_df[-which(is.na(check_df$cleanlab_id) == F),]
# add the cleaned variable to the data set
lab_key$labid_old_labinventory = toupper(cleanlab_id)
# some of these variables were coded differently than usual so will keep their coding in the new variable
lab_key$labid_old_labinventory[282] = "04884_4"
lab_key$labid_old_labinventory[5191] = "0381A"
lab_key$labid_old_labinventory[5192] = "0381B"
lab_key$labid_old_labinventory[5193] = "0381C"
lab_key$labid_old_labinventory[5194] = "0381D"
lab_key$labid_old_labinventory[5200] = "0386A"
lab_key$labid_old_labinventory[5201] = "0386B"
lab_key$labid_old_labinventory[5202] = "0386C"
lab_key$labid_old_labinventory[5239] = "04884_B"
# check the output
length(which(is.na(lab_key$labid_old_labinventory) == T)) # 0 missing so correct now


#### ------ REMOVE DUPLICATES ------ ####

# remove duplicates from each of the data sets
## qpcr_nodups
# check for duplicates and missingness
length(unique(qpcr_nodups$labid_old_labinventory)) # 4336/4336 unique 
length(which(is.na(qpcr_nodups$labid_old_labinventory) == T)) # 0 missing
count_table = table(qpcr_nodups$labid_old_labinventory, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no duplicates
## qpcr_alldups
length(unique(qpcr_alldups$labid_old_labinventory)) # 43/74 -> looks like could be some duplicates
length(which(is.na(qpcr_alldups$labid_old_labinventory) == T)) # 0 missing 
count_table = table(qpcr_alldups$labid_old_labinventory, useNA = "always")
dups_table_labid_old_labinventory_alldups = data.frame(count_table[which(count_table > 1)]) # 29 duplicates
# look for duplicates 
duplicates_qpcr = qpcr_alldups[which(qpcr_alldups$labid_old_labinventory %in% dups_table_labid_old_labinventory_alldups$Var1),]
# remove the duplicates
qpcr_alldups = qpcr_alldups[-which(qpcr_alldups$labid_old_labinventory %in% duplicates_qpcr$labid_old_labinventory),]
# now rbind to attach qpcr_alldups to the bottom (add rows) of qpcr_nodups and keep the qpcr_nodups name
qpcr_nodups = rbind(qpcr_nodups, qpcr_alldups)
## ama_haplotypes
# check for duplicates and missingness
length(unique(ama_haplotypes$labid_old_labinventory)) # 509/509 unique 
length(which(is.na(ama_haplotypes$labid_old_labinventory) == T)) # 0 missing
count_table = table(ama_haplotypes$labid_old_labinventory, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no duplicates
## csp_haplotypes
# check for duplicates and missingness
length(unique(csp_haplotypes$labid_old_labinventory)) # 509/509 unique 
length(which(is.na(csp_haplotypes$labid_old_labinventory) == T)) # 0 missing
count_table = table(csp_haplotypes$labid_old_labinventory, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no duplicates
## histb_haplotypes
# check for duplicates and missingness
length(unique(histb_haplotypes$labid_old_labinventory)) # 509/509 unique 
length(which(is.na(histb_haplotypes$labid_old_labinventory) == T)) # 0 missing
count_table = table(histb_haplotypes$labid_old_labinventory, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no duplicates
## meta_data
# check for duplicates and missingness
length(unique(meta_data$labid_new)) # 5104/5104 unique 
length(which(is.na(meta_data$labid_new) == T)) # 0 missing
count_table = table(meta_data$labid_new, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no duplicates
## lab_key
# check for duplicates and missingness
# in labid_new
length(unique(lab_key$labid_new)) # 5366/5379 unique -> looks like some duplicates
length(which(is.na(lab_key$labid_new) == T)) # 0 missing
count_table = table(lab_key$labid_new, useNA = "always")
dups_table_labid_new = data.frame(count_table[which(count_table > 1)]) # 13 duplicates
# in labid_old_socialinventory
length(unique(lab_key$labid_old_socialinventory)) # 5089/5379 -> looks like could be some duplicates
length(which(is.na(lab_key$labid_old_socialinventory) == T)) # 274 missing 
count_table = table(lab_key$labid_old_socialinventory, useNA = "always")
dups_table_labid_old_socialinventory = data.frame(count_table[which(count_table > 1)]) # 16 duplicates + NA
dups_table_labid_old_socialinventory = dups_table_labid_old_socialinventory[-which(is.na(dups_table_labid_old_socialinventory$Var1) == T),]
# in labid_old_labinventory
length(unique(lab_key$labid_old_labinventory)) # 5366/5379 -> looks like could be some duplicates
length(which(is.na(lab_key$labid_old_labinventory) == T)) # 0 missing 
count_table = table(lab_key$labid_old_labinventory, useNA = "always")
dups_table_labid_old_labinventory = data.frame(count_table[which(count_table > 1)]) # 13 duplicates
# lot of duplicates in lab_key 
# look for duplicates on the same plate by sorting the data frame of duplicates for labid_new and labid_old_labinventory
# left out labid_socialinventory right now because might not need it for merging and has a lot of duplicates correct in labid_new and labid_old_labinventory
duplicates_df = lab_key[which((lab_key$labid_old_labinventory %in% dups_table_labid_old_labinventory$Var1) |
                                (lab_key$labid_new %in% dups_table_labid_new$Var1)),]
# remove the duplicates in the same plate for the labid_new and labid_old_labinventory columns
duplicates_to_remove = c("0046_C","0047_C","0174_4","0175_2","0225","0248_1","0273_2","9075_4","9239_1","9259_3","9426_1","9479_B","9493_D")
lab_key = lab_key[-which(lab_key$labid_new %in% duplicates_to_remove),]
# now repull out duplicates remaining for labid_old_labinventory
length(unique(lab_key$labid_old_labinventory)) # 5353/5353 -> looks like could be some duplicates
length(which(is.na(lab_key$labid_old_labinventory) == T)) # 0 missing 
count_table = table(lab_key$labid_old_labinventory, useNA = "always")
dups_table_labid_old_labinventory = data.frame(count_table[which(count_table > 1)]) # 0 duplicates
# now repull out duplicates remaining for labid_new
length(unique(lab_key$labid_new)) # 5353/5353 -> looks like could be some duplicates
length(which(is.na(lab_key$labid_new) == T)) # 0 missing 
count_table = table(lab_key$labid_new, useNA = "always")
dups_table_labid_new = data.frame(count_table[which(count_table > 1)]) # 0 duplicates
## old code to find the duplicates (have now recoded those so they aren't duplicates - see 10/16/18 notebook for recoding and plate/row/col)
# # take out the remaining duplicates that can be matched (because have different plates) to match by hand later
# remaining_duplicates = c("0032","0337_2","0337_3","0375_3","0382_C","0482_A","0482_B")
# lab_key = lab_key[-which(lab_key$labid_new %in% remaining_duplicates),]
# # one last check for duplicates in the lab key
# # repull out duplicates remaining for labid_old_labinventory
# length(unique(lab_key$labid_old_labinventory)) # 5339/5339 no duplicates
# length(which(is.na(lab_key$labid_old_labinventory) == T)) # 0 missing 
# count_table = table(lab_key$labid_old_labinventory, useNA = "always")
# dups_table_labid_old_labinventory = data.frame(count_table[which(count_table > 1)]) # 0 duplicates
# # repull out duplicates remaining for labid_new
# length(unique(lab_key$labid_new)) # 5339/5339 no duplicates
# length(which(is.na(lab_key$labid_new) == T)) # 0 missing 
# count_table = table(lab_key$labid_new, useNA = "always")
# dups_table_labid_new = data.frame(count_table[which(count_table > 1)]) # 0 duplicates


#### ------ MERGE DATA SETS ------ ####

## first merge
# rename column headers to indiciate if AMA, CSP or HistB
names(ama_haplotypes)[names(ama_haplotypes) == "total_reads_all_noncontrol_haplotypes"] <- 'AMA_total_reads_all_noncontrol_haplotypes'
names(ama_haplotypes)[names(ama_haplotypes) == "number_of_haplotypes"] <- 'AMA_number_of_haplotypes'
names(csp_haplotypes)[names(csp_haplotypes) == "total_reads_all_noncontrol_haplotypes"] <- 'CSP_total_reads_all_noncontrol_haplotypes'
names(csp_haplotypes)[names(csp_haplotypes) == "number_of_haplotypes"] <- 'CSP_number_of_haplotypes'
names(histb_haplotypes)[names(histb_haplotypes) == "total_reads_all_noncontrol_haplotypes"] <- 'HistB_total_reads_all_noncontrol_haplotypes'
names(histb_haplotypes)[names(histb_haplotypes) == "number_of_haplotypes"] <- 'HistB_number_of_haplotypes'
# merge all three haplotype data sets by labid_old_inventory
merge1a = left_join(ama_haplotypes,csp_haplotypes,by = "labid_old_labinventory")
merge1 = left_join(merge1a,histb_haplotypes, by = "labid_old_labinventory")
# check the merge
summary(merge1)
summary(ama_haplotypes)
summary(csp_haplotypes)
summary(histb_haplotypes)

## second merge
# merged the merged haplotype dataset (with all 3 targets'info) with lab_key by labid_old_labinventory
merge2 = left_join(lab_key, merge1, by = "labid_old_labinventory")
# check the merge
summary(merge2)
summary(lab_key)
summary(merge1)
length(which(is.na(merge2$labid_old_labinventory)==T))
length(which(is.na(merge2$AMA_total_reads_all_noncontrol_haplotypes)==F))
length(which(is.na(merge2$AMA_number_of_haplotypes)==F)) 
length(which(is.na(merge2$CSP_total_reads_all_noncontrol_haplotypes)==F))
length(which(is.na(merge2$CSP_number_of_haplotypes)==F))
length(which(is.na(merge2$HistB_total_reads_all_noncontrol_haplotypes)==F))
length(which(is.na(merge2$HistB_number_of_haplotypes)==F))
# looks like only 509/509 observations from merge1 merged in merge2

## third merge
# merged the merged haplotype/lab_key data set with qpcr_nodups by labid_old_labinventory
merge3 = left_join(merge2, qpcr_nodups, by = "labid_old_labinventory")
# check the merge
summary(merge3)
summary(qpcr_nodups)
summary(merge2)
length(which(is.na(merge3$labid_old_labinventory)==T))
length(which(is.na(merge3$AMA_number_of_haplotypes)==F)) 
length(which(is.na(merge3$AMA_total_reads_all_noncontrol_haplotypes)==F))
length(which(is.na(merge3$CSP_total_reads_all_noncontrol_haplotypes)==F))
length(which(is.na(merge3$CSP_number_of_haplotypes)==F))
length(which(is.na(merge3$HistB_total_reads_all_noncontrol_haplotypes)==F))
length(which(is.na(merge3$HistB_number_of_haplotypes)==F))
# looks like 509/509 observations from merge2 for haplotype data remained for merge 3
length(which(is.na(merge3$pfr364Std10b)==F)) # 4338
length(which(is.na(qpcr_nodups$pfr364Std10b)==F)) # 4350
length(which(is.na(merge3$pfr364Std10a)==F)) # 4338
length(which(is.na(qpcr_nodups$pfr364Std10a)==F)) # 4350
# looks like 4338/4350 observations from qpcr_nodups made it into the data set 
# pull out labid_old_inventory that didn't merge from qpcr_nodups
matched_qpcrnodups_list = merge3$labid_old_labinventory[which(is.na(merge3$pfr364Std10a) == F)]
unmatched_qpcrnodups = qpcr_nodups[!(qpcr_nodups$labid_old_labinventory %in% matched_qpcrnodups_list),]
# pull out the indices of the unmatched IDs
unmatched_qpcrnodups_indices = which(!(qpcr_nodups$labid_old_labinventory %in% matched_qpcrnodups_list))
# recode or remove the indices based on checking the original data sets (10/16/18 notebook entry has description of choices)
qpcr_nodups$labid_old_labinventory[104] = NA
qpcr_nodups$labid_old_labinventory[1549] = "0410_A"
qpcr_nodups$labid_old_labinventory[1550] = "0410_B"
qpcr_nodups$labid_old_labinventory[1551] = "0410_C"
qpcr_nodups$labid_old_labinventory[1899] = "9136_6"
qpcr_nodups$labid_old_labinventory[1900] = "9174_5"
qpcr_nodups$labid_old_labinventory[1996] = "0381A"
qpcr_nodups$labid_old_labinventory[1997] = "0381B"
qpcr_nodups$labid_old_labinventory[1998] = "0381C"
qpcr_nodups$labid_old_labinventory[1999] = "0381D"
qpcr_nodups$labid_old_labinventory[2394] = NA
qpcr_nodups$labid_old_labinventory[2395] = NA
# remove the rows where labid_old_labinventory is NA in qpcr_nodups
qpcr_nodups = qpcr_nodups[-which(is.na(qpcr_nodups$labid_old_labinventory) == T),]
# now retry merge 3
merge3 = left_join(merge2, qpcr_nodups, by = "labid_old_labinventory")
# check the merge
summary(merge3)
summary(qpcr_nodups)
summary(merge2)
length(which(is.na(merge3$labid_old_labinventory)==T))
length(which(is.na(merge3$AMA_number_of_haplotypes)==F)) 
length(which(is.na(merge3$AMA_total_reads_all_noncontrol_haplotypes)==F))
length(which(is.na(merge3$CSP_total_reads_all_noncontrol_haplotypes)==F))
length(which(is.na(merge3$CSP_number_of_haplotypes)==F))
length(which(is.na(merge3$HistB_total_reads_all_noncontrol_haplotypes)==F))
length(which(is.na(merge3$HistB_number_of_haplotypes)==F))
# looks like 506/506 observations from merge2 for haplotype data remained for merge 3
length(which(is.na(merge3$pfr364Std10b)==F)) # 4347
length(which(is.na(qpcr_nodups$pfr364Std10b)==F)) # 4347
length(which(is.na(merge3$pfr364Std10a)==F)) # 4347
length(which(is.na(qpcr_nodups$pfr364Std10a)==F)) # 4347
# looks like 4347/4347 observations from qpcr_nodups made it into the data set so it's now corrected
# double check that you don't have any duplicates in the labid_old_labinventory column
length(unique(merge3$labid_old_labinventory)) # 5353/5353 unique so still no duplicates (good!)

## fourth merge
# merge the merged haplotype/lab_key/qpcr data set with meta_data by labid_new
merge4 = left_join(merge3, meta_data, by = "labid_new")
# check the merge
summary(merge4)
summary(meta_data)
summary(merge3)
length(which(is.na(merge4$labid_old_labinventory)==T))
length(which(is.na(merge4$AMA_number_of_haplotypes)==F)) 
length(which(is.na(merge4$AMA_total_reads_all_noncontrol_haplotypes)==F))
length(which(is.na(merge4$CSP_total_reads_all_noncontrol_haplotypes)==F))
length(which(is.na(merge4$CSP_number_of_haplotypes)==F))
length(which(is.na(merge4$HistB_total_reads_all_noncontrol_haplotypes)==F))
length(which(is.na(merge4$HistB_number_of_haplotypes)==F))
# looks like 509/509 observations from merge3 for haplotype data remained for merge 4
length(which(is.na(merge4$pfr364Std10b)==F)) # 4347
length(which(is.na(merge3$pfr364Std10a)==F)) # 4347
# looks like 4347/4347 observations from merge 3 into merge 4 so looks good there
table(merge4$case_control_child,useNA = "always")
table(meta_data$case_control_child,useNA = "always")
length(which(is.na(merge4$case_control_child)==F)) # 5069
length(which(is.na(meta_data$case_control_child)==F)) # 5079
# looks like we only had 5069/5079 observations transferring
# pull out labid_new that didn't merge from meta_data
matched_meta_list = merge4$labid_new[which(is.na(merge4$case_control_child) == F)]
unmatched_meta = meta_data[!(meta_data$labid_new %in% matched_meta_list),]
# pull out the indices of the unmatched IDs
unmatched_qpcrnodups_indices = which(!(qpcr_nodups$labid_old_labinventory %in% matched_qpcrnodups_list))
# these are due to duplicates that were removed already
# double check that you don't have any duplicates in the labid_old_labinventory column
length(unique(merge4$labid_new)) # 5353/5353 unique so still no duplicates (good!)


#### ------ CHECK MERGED DATA SETS & RENAME COLUMNS/FINAL CLEAN ------ ####

# check the columns that show up twice in the last merge
colnames(merge4)
## interview_date
length(which(is.na(merge4$interview_date.x))) # 470
length(which(is.na(merge4$interview_date.y))) # 473
str(merge4$interview_date.x)
str(merge4$interview_date.y)
# slightly different = go with lab_key because less missing data and used for future lab decisions
merge4$interview_date.y <- NULL
names(merge4)[names(merge4) == 'interview_date.x'] <- 'interview_date'
## mem_rdt_results
table(merge4$mem_rdt_results.x, useNA = "always") 
table(merge4$mem_rdt_results.y, useNA = "always") 
# slightly different = go with lab_key because less missing data and used for future lab decisions
merge4$mem_rdt_results.y <- NULL
names(merge4)[names(merge4) == 'mem_rdt_results.x'] <- 'mem_rdt_results'
## gdnaplate
table(merge4$gdnaplate.x, useNA = "always") 
table(merge4$gdnaplate.y, useNA = "always") 
# slightly different = go with lab_key because less missing data and used for future lab decisions
merge4$gdnaplate.y <- NULL
names(merge4)[names(merge4) == 'gdnaplate.x'] <- 'gdnaplate'
## gdnacolumn
table(merge4$gdnacolumn.x, useNA = "always") 
table(merge4$gdnacolumn.y, useNA = "always") 
# slightly different = go with lab_key because less missing data and used for future lab decisions
merge4$gdnacolumn.y <- NULL
names(merge4)[names(merge4) == 'gdnacolumn.x'] <- 'gdnacolumn'
## gdnarow
table(merge4$gdnarow.x, useNA = "always") 
table(merge4$gdnarow.y, useNA = "always") 
# slightly different = go with lab_key because less missing data and used for future lab decisions
merge4$gdnarow.y <- NULL
names(merge4)[names(merge4) == 'gdnarow.x'] <- 'gdnarow'
## dbsbox
table(merge4$dbsbox.x, useNA = "always") 
table(merge4$dbsbox.y, useNA = "always") 
# slightly different = go with lab_key because less missing data and used for future lab decisions
merge4$dbsbox.y <- NULL
names(merge4)[names(merge4) == 'dbsbox.x'] <- 'dbsbox'
## dbsbag
table(merge4$dbsbag.x, useNA = "always") 
table(merge4$dbsbag.y, useNA = "always") 
# slightly different = go with lab_key because less missing data and used for future lab decisions
merge4$dbsbag.y <- NULL
names(merge4)[names(merge4) == 'dbsbag.x'] <- 'dbsbag'


# export the merged data
write_csv(merge4, "MESA_merged_final.csv")


















