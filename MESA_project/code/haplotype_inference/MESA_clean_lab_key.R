# ------------------------------------ #
#         MESA Clean Lab Key           #
#          October 10, 2018            #
#             K. Sumner                #
# ------------------------------------ #

#### ----- load the necessary libraries ----- ####
library(tidyverse)


#### ----- read in all data sets ----- ####
# read in the data set of the lab key Wendy created 10/6/2018
lab_key = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/MESA inventory matched to labid Oct 6 minimum.csv")


#### --------------- CLEAN LAB KEY FOR MESA ------------------- ####

# remove variables that are not needed
lab_key$sid <- NULL
lab_key$studyid_case_controldata <- NULL
lab_key$case_control_childdata <- NULL
lab_key$mem_rdt_idindata <- NULL
lab_key$`_merge` <- NULL

# check the levels of the RDT results and recode the variable
table(lab_key$mem_rdt_results, useNA = "always")
lab_key$mem_rdt_results[lab_key$mem_rdt_results == "POSITIVE"] = "positive"
lab_key$mem_rdt_results = as.factor(lab_key$mem_rdt_results)

# rename labid variable names
names(lab_key)[names(lab_key) == "labid edited for matching"] <- 'labid_new'
names(lab_key)[names(lab_key) == "labid_from_inventory"] <- 'labid_old_socialinventory'
names(lab_key)[names(lab_key) == "labid_original"] <- 'labid_old_labinventory'

# check remaining variables
table(lab_key$interview_date,useNA = "always")
table(lab_key$studyid_case_control_new,useNA = "always")
table(lab_key$mem_rdt_results,useNA = "always")
# labid_old_socialinventory
table(lab_key$labid_old_socialinventory,useNA = "always")
lab_key$labid_old_socialinventory[lab_key$labid_old_socialinventory == "_"] = NA
length(which(is.na(lab_key$labid_old_socialinventory) == T))
socialinventory = rep(NA,nrow(lab_key))
for (m in 1:nrow(lab_key)){
  if ("-" %in% strsplit(lab_key$labid_old_socialinventory[m],"")[[1]]){
    socialinventory[m] = m
  } else {
    socialinventory[m] = NA
  }
}
length(na.omit(socialinventory))
# labid_old_labinventory
table(lab_key$labid_old_labinventory,useNA = "always")
length(which(is.na(lab_key$labid_old_labinventory) == T))
labinventory = rep(NA,nrow(lab_key))
for (m in 1:nrow(lab_key)){
  if ("-" %in% strsplit(lab_key$labid_old_labinventory[m],"")[[1]]){
    labinventory[m] = m
  } else {
    labinventory[m] = NA
  }
}
length(na.omit(labinventory))
# labid_new
table(lab_key$labid_new,useNA = "always")
length(which(is.na(lab_key$labid_new) == T))
new = rep(NA,nrow(lab_key))
for (m in 1:nrow(lab_key)){
  if ("-" %in% strsplit(lab_key$labid_new[m],"")[[1]]){
    new[m] = m
  } else {
    new[m] = NA
  }
}
length(na.omit(new))
table(lab_key$gdnaplate,useNA = "always")
table(lab_key$gdnacolumn,useNA = "always")
table(lab_key$gdnarow,useNA = "always")
table(lab_key$dbsbox,useNA = "always")
table(lab_key$dbsbag,useNA = "always")

# create a function for something not being in the list
"%ni%" <- Negate("%in%")

# clean labid_new variable
# first find all "-"
changed_labid = rep(NA,nrow(lab_key))
for (i in 1:nrow(lab_key)){
  if("-" %in% strsplit(lab_key$labid_new[i],"")[[1]]){
    splitup = strsplit(lab_key$labid_new[i],"-")
    changed_labid[i] = paste0(splitup[[1]][1],"_",splitup[[1]][2])
  } else {
    changed_labid[i] = lab_key$labid_new[i]
  }
}
lab_key$labid_new = changed_labid
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

# clean labid_old_socialinventory variable
# doesn't have any "-"
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

# clean labid_old_labinventory variable
# first find all "-"
changed_labid = rep(NA,nrow(lab_key))
for (i in 1:nrow(lab_key)){
  if("-" %in% strsplit(lab_key$labid_old_labinventory[i],"")[[1]]){
    splitup = strsplit(lab_key$labid_old_labinventory[i],"-")
    changed_labid[i] = paste0(splitup[[1]][1],"_",splitup[[1]][2])
  } else {
    changed_labid[i] = lab_key$labid_new[i]
  }
}
lab_key$labid_old_labinventory = changed_labid
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

# double check remaining variables
table(lab_key$interview_date,useNA = "always")
table(lab_key$studyid_case_control_new,useNA = "always")
table(lab_key$mem_rdt_results,useNA = "always")
table(lab_key$labid_old_socialinventory,useNA = "always")
table(lab_key$labid_old_labinventory,useNA = "always")
table(lab_key$labid_new,useNA = "always")
table(lab_key$gdnaplate,useNA = "always")
table(lab_key$gdnacolumn,useNA = "always")
table(lab_key$gdnarow,useNA = "always")
table(lab_key$dbsbox,useNA = "always")
table(lab_key$dbsbag,useNA = "always")

# output the new file
write_csv(lab_key,"clean_lab_key.csv")


#### --------------- EXPORT RDT+ SAMPLES THAT STILL NEED TO BE SEQUENCED ------------------- ####

# read in the inventory of the 514 RDT+ samples originally sent out for sequencing (in spring 2018)
# will just pull these numbers from the database already matched to AMA samples
original_rdt_pos = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_sample_summary.csv")

# create a function for something not being in the list
"%ni%" <- Negate("%in%")

# for AMA
# look at the data set
table(original_rdt_pos$lab_mesa_id, useNA = "always")
# recode the "none1" and "none2" labids to NA
original_rdt_pos$lab_mesa_id[original_rdt_pos$lab_mesa_id == "none1"] = NA
original_rdt_pos$lab_mesa_id[original_rdt_pos$lab_mesa_id == "none2"] = NA
# check for "-" in labids
length(which(is.na(original_rdt_pos$lab_mesa_id) == T))
mesaid = rep(NA,nrow(original_rdt_pos))
for (m in 1:nrow(original_rdt_pos)){
  if ("-" %in% strsplit(original_rdt_pos$lab_mesa_id[m],"")[[1]]){
    mesaid[m] = m
  } else {
    mesaid[m] = NA
  }
}
length(na.omit(mesaid))
# now clean the labid variable anda rename it to "labid_old
cleanlab_id = rep(NA,nrow(original_rdt_pos))
for (k in 1:nrow(original_rdt_pos)){
  if (is.na(original_rdt_pos$lab_mesa_id[k]) == T){
    cleanlab_id[k] = NA
  } else if ("_" %ni% strsplit(original_rdt_pos$lab_mesa_id[k],"")[[1]]) {
    if (nchar(original_rdt_pos$lab_mesa_id[k]) == 4){
      cleanlab_id[k] = paste0(original_rdt_pos$lab_mesa_id[k])
    }
    if (nchar(original_rdt_pos$lab_mesa_id[k]) == 3){
      cleanlab_id[k] = paste0("0",original_rdt_pos$lab_mesa_id[k])
    }
    if (nchar(original_rdt_pos$lab_mesa_id[k]) == 2){
      cleanlab_id[k] = paste0("00",original_rdt_pos$lab_mesa_id[k])
    }
    if (nchar(original_rdt_pos$lab_mesa_id[k]) == 1){
      cleanlab_id[k] = paste0("000",original_rdt_pos$lab_mesa_id[k])
    }
  } else {
    part_mesa_id = strsplit(original_rdt_pos$lab_mesa_id[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      cleanlab_id[k] = paste0(original_rdt_pos$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 3){
      cleanlab_id[k] = paste0("0",original_rdt_pos$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 2){
      cleanlab_id[k] = paste0("00",original_rdt_pos$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 1){
      cleanlab_id[k] = paste0("000",original_rdt_pos$lab_mesa_id[k])
    }
  }
}
original_rdt_pos$labid_old_labinventory = toupper(cleanlab_id)
# change controls to NA
original_rdt_pos$labid_old_labinventory[original_rdt_pos$labid_old_labinventory == "03D7"] = NA
# should now have 5 NAs
length(which(is.na(original_rdt_pos$labid_old_labinventory) == T)) # 5 NAs so looks good

# now look at how many of the labid_old_labinventory values in the lab_key match the labid_old_labinventory values in the original_rdt_pos
length(which(original_rdt_pos$labid_old_labinventory %in% lab_key$labid_old_labinventory)) # 509 match
# check with the lab_key new_labid coding
length(which(original_rdt_pos$labid_old_labinventory %in% lab_key$labid_new)) # 509 match
# check with the lab_key labid_old_socialinventory
length(which(original_rdt_pos$labid_old_labinventory %in% lab_key$labid_old_socialinventory)) # 514 match

# look at differences in those that are unique and those that are duplicated  in the original_rdt_pos dataset
length(unique(original_rdt_pos$labid_old_labinventory)) # 510 unique -> probably saying NA is unique so is the 509 samples present plus 5 NA
length(which(is.na(original_rdt_pos$labid_old_labinventory) == T)) # 5 missing (controls + 2 coded "none1" & "none2")
count_table = table(original_rdt_pos$labid_old_labinventory, useNA = "always")
dups_table = count_table[which(count_table > 1)] # only NAs are duplicates
# no duplicates in the original_rdt_pos data set - yay!

# look at differences in those that are unique and those that are duplicated in lab_key$labid_old_labinventory
length(unique(lab_key$labid_old_labinventory)) # 5359/5379 -> looks like good be some duplicates
length(which(is.na(lab_key$labid_old_labinventory) == T)) # 0 missing 
count_table = table(lab_key$labid_old_labinventory, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 20 duplicates

# look at differences in those that are unique and those that are duplicated in lab_key$labid_old_socialinventory
length(unique(lab_key$labid_old_socialinventory)) # 5089/5379 -> looks like good be some duplicates
length(which(is.na(lab_key$labid_old_socialinventory) == T)) # 274 missing 
count_table = table(lab_key$labid_old_socialinventory, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 16 duplicates

# look at differences in those that are unique and those that are duplicated in lab_key$labid_new
length(unique(lab_key$labid_new)) # 5359/5379 -> looks like good be some duplicates
length(which(is.na(lab_key$labid_new) == T)) # 0 missing 
count_table = table(lab_key$labid_new, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 20 duplicates

# pull out the indices that match and don't match
matched_indices = original_rdt_pos[which(original_rdt_pos$labid_old_labinventory %in% lab_key$labid_old_labinventory),] # 509/514 matched
unmatched_indices = original_rdt_pos[which(!(original_rdt_pos$labid_old_labinventory %in% lab_key$labid_old_labinventory)),] # 5/514 unmatched - 3 were controls, 2 didn't have an associated mesaID 

# now pull out the old_labid values from the lab_key to make sure those are reprocessed
unsequenced_lab_key = lab_key[which(!(lab_key$labid_old_labinventory %in% original_rdt_pos$labid_old_labinventory)),] 
# subset this lab key to those that are rdt_positive
rdtpos_unsequenced_lab_key = unsequenced_lab_key[which(unsequenced_lab_key$mem_rdt_results == "positive"),]

# note 337_2, 337_3, 375_3 are in the sequenced data but are also duplicates in the labid_old_labinventory in the lab_key

# write out this data set
write_csv(rdtpos_unsequenced_lab_key, "rdtpos_unsequenced_lab_key.csv")




