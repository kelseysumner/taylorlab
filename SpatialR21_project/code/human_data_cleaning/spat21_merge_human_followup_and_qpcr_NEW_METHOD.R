# ----------------------------------------- #
#  Spat21 Merging Follow-up and qPCR Data   #
#                Human Data                 #
#    New Method based on Sam Kim's Method   #
#             April 12, 2019                #
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
library(magrittr)
library(reshape2)


#### ------- read in the data sets -------- ####

# read in the merged human monthly and table data set
human_merged_all_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/hum_monthly_merged_with_table_and_sick_with_exclusion_data_12APR2019.RDS")

# read in the preliminary qpcr data
human_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/spat21_qpcr_data_clean_human_dbs_16JAN2019.RDS")


#### ------- merge in the qpcr data with the human monthly, table, and sick data ------ ####


# fix typos
# add an "R" to the end of M03-0618-2-R for the one with M03-260618-2 because was actually a sick visit for the qpcr data
human_qpcr_data$`Sample Name`[human_qpcr_data$`Sample Name` == "M03-260618-2"] = "M03-260618-2-R" 
# change M16-270618-P-R to M16-270618-4-R in the qpcr data
human_qpcr_data$`Sample Name`[human_qpcr_data$`Sample Name` == "M16-270618-P-R"] = "M16-270618-4-R" 
# change M15-311017-P-R to M15-311017-6-R in the qpcr data
human_qpcr_data$`Sample Name`[human_qpcr_data$`Sample Name` == "M15-311017-P-R"] = "M15-311017-6-R" 
# change K07-030817-08 to K07-030817-8 in the qpcr data
human_qpcr_data$`Sample Name`[human_qpcr_data$`Sample Name` == "K07-030817-08"] = "K07-030817-8" 
# change K07-030817-09 to K07-030817-9 in the qpcr data
human_qpcr_data$`Sample Name`[human_qpcr_data$`Sample Name` == "K07-030817-09"] = "K07-030817-9" 


# first check for duplicates in the sample name column for the human_merged_all_data data set
length(unique(human_merged_all_data$`Sample Name`)) # 3140 unique 
length(which(is.na(human_merged_all_data$`Sample Name`) == T)) # 0 missing
count_table = table(human_merged_all_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates

# first remove the columns that are not needed (standards)
cols_to_remove = c("HbtubStd1a","HbtubStd1b","HbtubStd2a","HbtubStd2b","HbtubStd3a","HbtubStd3b",
                   "HbtubStd4a","HbtubStd4b","HbtubStd5a","HbtubStd5b","HbtubStd6a","HbtubStd6b",
                   "HbtubStd7a","HbtubStd7b","HbtubStd8a","HbtubStd8b","HbtubStd9a","HbtubStd9b",
                   "HbtubStd10a","HbtubStd10b","pfr364Std1a","pfr364Std1b","pfr364Std2a","pfr364Std2b",
                   "pfr364Std3a","pfr364Std3b","pfr364Std4a","pfr364Std4b","pfr364Std5a","pfr364Std5b",
                   "pfr364Std6a","pfr364Std6b","pfr364Std7a","pfr364Std7b","pfr364Std8a","pfr364Std8b",
                   "pfr364Std9a","pfr364Std9b","pfr364Std10a","pfr364Std10b","r_value_std","intercept_std",
                   "slope_std", "Well Position","HbtubCT1","HbtubCT2","pfr364CT1","pfr364CT2",
                   "pfr364Q1","pfr364Q2","pfr364Q1_std","pfr364Q2_std","Experiment Name")
human_qpcr_data = human_qpcr_data[,!(colnames(human_qpcr_data) %in% cols_to_remove)]

# add an empty column for the date associated with the sample id for the social demographic data
str(human_merged_all_data$today_hum_monthly_data)
str(human_merged_all_data$today_hum_sick_data)
human_merged_all_data$sample_id_date = if_else(is.na(human_merged_all_data$today_hum_sick_data),human_merged_all_data$today_hum_monthly_data,human_merged_all_data$today_hum_sick_data)
table(human_merged_all_data$sample_id_date, useNA = "always")
str(human_merged_all_data$sample_id_date)

# add an empty column for the date associated with the sample id for the qpcr data
qpcr_new_date = rep(NA,nrow(human_qpcr_data))
for (i in 1:nrow(human_qpcr_data)){
  split_id=strsplit(human_qpcr_data$`Sample Name`[i],"-")[[1]]
  qpcr_new_date[i] = split_id[2]
}
human_qpcr_data$qpcr_new_date = dmy(qpcr_new_date)
table(human_qpcr_data$qpcr_new_date, useNA="always")
head(human_qpcr_data$qpcr_new_date)
head(human_qpcr_data$`Sample Name`)

# create a household id variable for qpcr data
mem_id = rep(NA,nrow(human_qpcr_data))
for (i in 1:nrow(human_qpcr_data)){
  split_id=strsplit(human_qpcr_data$`Sample Name`[i],"-")[[1]]
  mem_id[i] = paste0(split_id[1],"_",split_id[3])
}
human_qpcr_data$mem_id = mem_id
table(human_qpcr_data$mem_id, useNA="always")
head(human_qpcr_data$mem_id)
head(human_qpcr_data$`Sample Name`)

# create a large for loop that will merge together the qpcr results if they match the social demographic data by within 6 years
# add three empty columns to the human social demographic data set
human_merged_all_data$sample_name_from_merge = rep(NA,nrow(human_merged_all_data))
human_merged_all_data$pf_pcr_infection_status = rep(NA,nrow(human_merged_all_data))
human_merged_all_data$pfr364Q_std_combined = rep(NA,nrow(human_merged_all_data))
# human_merged_all_data = human_merged_all_data[c(1:10,3100:3110),]
# then do the for loop
for (i in 1:nrow(human_merged_all_data)){
  for (j in 1:nrow(human_qpcr_data)){
    if (human_merged_all_data$`Sample Name`[i]==human_qpcr_data$`Sample Name`[j]){
      human_merged_all_data[i,130:132] = human_qpcr_data[j,1:3]
    } else if (str_detect(human_qpcr_data$`Sample Name`[j],"R") & 
               str_detect(human_merged_all_data$`Sample Name`[i],"R") & 
               (human_qpcr_data$mem_id[j]==human_merged_all_data$unq_memID[i]) & 
               (as.numeric(human_merged_all_data$sample_id_date[i]-human_qpcr_data$qpcr_new_date[j])<=6) &
               (as.numeric(human_merged_all_data$sample_id_date[i]-human_qpcr_data$qpcr_new_date[j])>0)) {
      human_merged_all_data[i,130:132] = human_qpcr_data[j,1:3]
    } else if (!(str_detect(human_qpcr_data$`Sample Name`[j],"R")) & 
               !(str_detect(human_merged_all_data$`Sample Name`[i],"R")) & 
               (human_qpcr_data$mem_id[j]==human_merged_all_data$unq_memID[i]) & 
               (as.numeric(human_merged_all_data$sample_id_date[i]-human_qpcr_data$qpcr_new_date[j])<=6) &
               (as.numeric(human_merged_all_data$sample_id_date[i]-human_qpcr_data$qpcr_new_date[j])>0)) {
      human_merged_all_data[i,130:132] = human_qpcr_data[j,1:3]
    }
  }
}
small_data = human_merged_all_data[,125:132]

# export the data set
# write_csv(human_merged_all_data,"spat21_human_merged_all_data.csv")
# write_rds(human_merged_all_data,"spat21_human_merged_all_data.rds")

# check the data merge
# see how many ids didn't merge
length(which(is.na(human_merged_all_data$sample_name_from_merge))) # 381 missing
length(which(!(is.na(human_merged_all_data$sample_name_from_merge)))) # 2759
# 3140-2890= 250 should not have merged
# had 381 not merge so: 381-250=131 that didn't merge
# check if all merged ids unique
length(unique(human_merged_all_data$sample_name_from_merge)) # 2759 unique
# 2890-131 = 2759 so all that merged in were unique and no duplicate merges

# figure out what qpcr results didn't merge in
length(intersect(human_merged_all_data$sample_name_from_merge,human_qpcr_data$`Sample Name`)) # 2758 samples merged (but 2759 unique?)
length(setdiff(human_merged_all_data$sample_name_from_merge,human_qpcr_data$`Sample Name`)) # 1: missing value NA
length(setdiff(human_qpcr_data$`Sample Name`,human_merged_all_data$sample_name_from_merge)) # 132
nomerge_list = setdiff(human_qpcr_data$`Sample Name`,human_merged_all_data$sample_name_from_merge)
# look at those that didn't merge and try to figure out why they didn't merge
nomerge = human_qpcr_data[which(human_qpcr_data$`Sample Name` %in% nomerge_list),]
# export to decide how to change each sample
# write_csv(nomerge,"human_qpcr_no_merge.csv")
# look at those that didn't merge in the large human social demograhpic data set
nomerge_social = human_merged_all_data[which(is.na(human_merged_all_data$sample_name_from_merge)),]
# export to decide how to change each sample
# write_csv(nomerge_social,"human_social_no_merge.csv")

# add an empty column for the date associated with the sample id for the qpcr data merged into the social data
merge_date = rep(NA,nrow(human_merged_all_data))
for (i in 1:nrow(human_merged_all_data)){
  split_id=strsplit(human_merged_all_data$sample_name_from_merge[i],"-")[[1]]
  merge_date[i] = split_id[2]
}
human_merged_all_data$merge_date = dmy(merge_date)
table(human_merged_all_data$merge_date, useNA="always")
head(human_merged_all_data$merge_date)
head(human_merged_all_data$sample_name_from_merge)

# check that all the samples that merged had dates within 6 days
count = 0
for (k in 1:nrow(human_merged_all_data)){
  if ((as.numeric(human_merged_all_data$sample_id_date[k]-human_merged_all_data$merge_date[k])<=6) &
      (as.numeric(human_merged_all_data$sample_id_date[k]-human_merged_all_data$merge_date[k])>=0) &
      !(is.na(human_merged_all_data$sample_id_date[k])) &
      !(is.na(human_merged_all_data$merge_date[k]))){
    count = count + 1
  }
}
count
# count = 2759 so the merge worked correctly!

# for the samples that didn't merge and had typos, merge those in
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="K01-110717-9")] = "K01-130717-9"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="K01-071017-9")] = "K01-070917-9"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="K05-080717-6")] = "K05-130717-6"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="K07-060717-8")] = "K07-130717-8"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="K07-060717-9")] = "K07-130717-9"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="K09-220218-7")] = "K09-010218-7"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="K13-161017-4")] = "K13-051017-4"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="M03-130718-1")] = "M03-130717-1"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="M07-290817-3")] = "M07-170817-3"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="K14-170717-1-R")] = "K14-170717-1A-R"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S02-130617-1")] = "S02-140617-1"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S02-130617-2")] = "S02-140617-2"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S02-130617-3")] = "S02-140617-3"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S02-130617-4")] = "S02-140617-4"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S02-130617-5")] = "S02-140617-5"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S03-130617-1")] = "S03-140617-1"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S03-130617-2")] = "S03-140617-2"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S03-130617-3")] = "S03-140617-3"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S03-130617-4")] = "S03-140617-4"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S04-130617-1")] = "S04-140617-1"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S04-130617-2")] = "S04-140617-2"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S04-130617-3")] = "S04-140617-3"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S04-130617-4")] = "S04-140617-4"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S04-130617-5")] = "S04-140617-5"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S04-130617-6")] = "S04-140617-6"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S04-130617-7")] = "S04-140617-7"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S04-130617-8")] = "S04-140617-8"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S05-130617-3")] = "S05-140617-3"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S05-130617-5")] = "S05-140617-5"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S05-130617-7")] = "S05-140617-7"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S05-130617-8")] = "S05-140617-8"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S07-130617-1")] = "S07-140617-1"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S07-130617-2")] = "S07-140617-2"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S07-130617-3")] = "S07-140617-3"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S07-130617-4")] = "S07-140617-4"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S07-130617-5")] = "S07-140617-5"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S07-200617-6")] = "S07-200717-6"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S08-130617-2")] = "S08-140617-2"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S08-130617-3")] = "S08-140617-3"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S08-130617-5")] = "S08-140617-5"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S08-130617-7")] = "S08-140617-7"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S09-130617-1")] = "S09-140617-1"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S09-130617-3")] = "S09-140617-3"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S09-130617-5")] = "S09-140617-5"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S09-130617-6")] = "S09-140617-6"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S09-130617-8")] = "S09-140617-8"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S09-211217-7")] = "S09-211218-7"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S10-130617-1")] = "S10-140617-1"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S10-130617-2")] = "S10-140617-2"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S10-130617-3")] = "S10-140617-3"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S10-130617-4")] = "S10-140617-4"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S10-130617-5")] = "S10-140617-5"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S10-130617-6")] = "S10-140617-6"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S10-130617-7")] = "S10-140617-7"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S11-210917-2")] = "S11-210817-2"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S11-210917-7")] = "S11-210817-7"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S11-240817-1")] = "S11-240617-1"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S11-240817-5")] = "S11-240617-5"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S11-240817-9")] = "S11-240617-9"

# check the data merge
# see how many ids didn't merge
length(which(is.na(human_merged_all_data$sample_name_from_merge))) # 322 missing, looks like worked correctly
length(which(!(is.na(human_merged_all_data$sample_name_from_merge)))) # 2818
# 3140-2890= 250 should not have merged
# had 381 not merge so: 322-250=72 that didn't merge
# check if all merged ids unique
length(unique(human_merged_all_data$sample_name_from_merge)) # 2818 unique
# 2890-72 = 2818 so all that merged in were unique and no duplicate merges

# now merge in the rest of the qpcr result information for the 58 new samples
qpcr_small_data = human_qpcr_data[,1:3]
qpcr_small_data = rename(qpcr_small_data,"sample_name_from_merge"="Sample Name")
qpcr_small_data = qpcr_small_data[which(qpcr_small_data$sample_name_from_merge %in% nomerge$`Sample Name`),]
human_merged_all_data = left_join(human_merged_all_data,qpcr_small_data,by = "sample_name_from_merge")
# check the merge
length(which(!(is.na(human_merged_all_data$pf_pcr_infection_status.y)))) # 59 
# look correct

# remove merge_date column
human_merged_all_data$merge_date <- NULL

# make a separate data set in case messes up in for loop
orig_data = human_merged_all_data

# if messes up do this
human_merged_all_data = orig_data

# now loop through each row and move over those two columns
# check colnames
colnames(human_merged_all_data)
# start for loop to combine qpcr results
for (i in 1:nrow(human_merged_all_data)){
  if (is.na(human_merged_all_data[i,131])){
    for (k in 1:2){   # this is for all data that is present in .y files but not in .x
      startpoint = 130 + k
      human_merged_all_data[i,startpoint] = human_merged_all_data[i,startpoint+2]
      human_merged_all_data[i,startpoint+2] <- NA
    }
  } else if (is.na(human_merged_all_data[i,133])){
    for (k in 1:2){ # this is for all data that is present in .x files but not in .y -> just make it NULL
      startpoint = 130 + k
      human_merged_all_data[i,startpoint+2] <- NA
    }
  } else {
    for (k in 1:2){ # both data sets are missing -> just make it NULL at .y location
      startpoint = 130 + k
      human_merged_all_data[i,startpoint+2] <- NA
    }
  } 
}
# check the output
length(which(is.na(human_merged_all_data$pf_pcr_infection_status.x))) # 322 missing
length(which(is.na(human_merged_all_data$pf_pcr_infection_status.y))) # 3140 missing
# looks like it worked correctly
small_check = human_merged_all_data[,130:134]
# remove the .y columns
human_merged_all_data_final = human_merged_all_data[,-c(133:134)]
human_merged_all_data_final = rename(human_merged_all_data_final,"pf_pcr_infection_status"="pf_pcr_infection_status.x","pfr364Q_std_combined"="pfr364Q_std_combined.x")
colnames(human_merged_all_data_final)

# now move up the columns for those ids that had the monthly follow-up and sick visit on the same day
# re-add the double visit test names to the data set to point out people who had the sick and monthly visit on the same day
# create a new sample name variable
# merge together the monthly_unq_memID and sick_unq_memID columns
length(which(is.na(human_merged_all_data_final$monthly_unq_memID))) # 519 - all the sick IDs
length(which(is.na(human_merged_all_data_final$sick_unq_memID))) # 2621 - all the monthly IDs
monthly_or_sick_unq_memID = ifelse(is.na(human_merged_all_data_final$sick_unq_memID),human_merged_all_data_final$monthly_unq_memID,human_merged_all_data_final$sick_unq_memID)
length(which(is.na(monthly_or_sick_unq_memID))) # no missing, which is good
# add to the data set and compare with the monthly and sick ID columns
human_merged_all_data_final$sample_name_final = monthly_or_sick_unq_memID
comparison_df = data.frame(human_merged_all_data_final$sample_name_final,human_merged_all_data_final$monthly_unq_memID, human_merged_all_data_final$sick_unq_memID)

# take off R for sick data and test to see if any IDs match monthly data
double_visit_test = rep(NA,nrow(human_merged_all_data_final))
for (i in 1:nrow(human_merged_all_data_final)){
  if (str_detect(human_merged_all_data_final$sample_name_final[i],"R")){
    firstsplit = strsplit(human_merged_all_data_final$sample_name_final[i],"-")[[1]]
    double_visit_test[i] = paste0(firstsplit[1],"-",firstsplit[2],"-",firstsplit[3],collapse="")
  } else {
    double_visit_test[i] = human_merged_all_data_final$sample_name_final[i]
  }
}

# look for duplicates in double visit_test
length(unique(double_visit_test)) # 2919 unique/3140 obs
length(which(is.na(double_visit_test) == T)) # 0 missing
count_table = table(double_visit_test, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 221 duplicates
length(dups_table)
dups_table_df = data.frame(dups_table)

# for all these IDs, remove the asymptomatic visit information 
human_merged_all_data_final$double_visit_test = double_visit_test
head(human_merged_all_data_final$double_visit_test,20)
small_check = human_merged_all_data_final[,120:134]

# sort the double_visit_test variable
human_merged_all_data_final = human_merged_all_data_final[order(human_merged_all_data_final$double_visit_test),]
small_check = human_merged_all_data_final[,130:134]

# look at the colnames
colnames(human_merged_all_data_final)
length(which(is.na(double_visit_test))) # no missing for double_visit_test

# for each duplicate visit, write some code that pulls the duplicate sick visit information into the corresponding sick columns for the monthly visit
# want to keep all the sick visit and monthly visit entries and the qpcr results (which could be associated with either entry)
# write a for loop that will check for if the participant has a sick visit on the same day and moves information all on one line
# start for loop
for (i in 1:nrow(human_merged_all_data_final)){
  if ((human_merged_all_data_final$double_visit_test[i] == human_merged_all_data_final$double_visit_test[i+1]) & !(is.na(human_merged_all_data_final$double_visit_test[i])) & !(is.na(human_merged_all_data_final$double_visit_test[i+1])) & i != nrow(human_merged_all_data_final)){
    for (k in 1:ncol(human_merged_all_data_final)){
      if (is.na(human_merged_all_data_final[i,k]) & !(is.na(human_merged_all_data_final[i+1,k]))){
        human_merged_all_data_final[i,k] = human_merged_all_data_final[i+1,k]
      }
      if (is.na(human_merged_all_data_final[i+1,k]) & !(is.na(human_merged_all_data_final[i,k]))){
        human_merged_all_data_final[i+1,k] = human_merged_all_data_final[i,k]
      }
    }
  } else {
    human_merged_all_data_final[i,] = human_merged_all_data_final[i,]
  }
}
# check the output
colnames(human_merged_all_data_final)
length(which(is.na(human_merged_all_data_final$monthly_unq_memID))) # 298 - (519-221 = 298)
length(which(is.na(human_merged_all_data_final$sick_unq_memID))) # 2400 - (2621-221 = 2400)
# looks like it is working correctly
# delete the rows that are now duplicates
for (i in 1:nrow(human_merged_all_data_final)){
  if ((human_merged_all_data_final$double_visit_test[i] == human_merged_all_data_final$double_visit_test[i+1]) & !(is.na(human_merged_all_data_final$double_visit_test[i])) & !(is.na(human_merged_all_data_final$double_visit_test[i+1])) & i != nrow(human_merged_all_data_final)){
    human_merged_all_data_final = human_merged_all_data_final[-i,]
  }
}
# check the output
length(which(is.na(human_merged_all_data_final$monthly_unq_memID))) # 298 - (519-221 = 298)
length(which(is.na(human_merged_all_data_final$sick_unq_memID))) # 2400 - (2621-221 = 2400)
# also tested a few ids in dups_table_df to see if occurred in duplicate still
# all looks good

# check colnames
colnames(human_merged_all_data_final)

# rename some column names
human_merged_all_data_final = rename(human_merged_all_data_final, sample_name_dbs = sample_name_from_merge)
colnames(human_merged_all_data_final)

# create a new column that differentiates whether it was a monthly or sick visit
visit_type = rep("monthly visit",nrow(human_merged_all_data_final))
human_merged_all_data_final$visit_type = visit_type
human_merged_all_data_final$visit_type[which(str_detect(human_merged_all_data_final$sample_name_final,"R"))] = "sick visit"
# check the output
table(human_merged_all_data_final$visit_type, useNA = "always")
# looks good

# export as a CSV and RDS file
write_csv(human_merged_all_data_final,"spat21_human_merged_all_data_29APR2019.csv")
write_rds(human_merged_all_data_final,"spat21_human_merged_all_data_29APR2019.rds")



















