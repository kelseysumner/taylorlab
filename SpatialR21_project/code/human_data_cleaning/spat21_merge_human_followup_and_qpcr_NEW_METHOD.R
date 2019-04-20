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
human_merged_all_data = human_merged_all_data[c(1:10,3100:3110),]
# then do the for loop
for (i in 1:nrow(human_merged_all_data)){
  for (j in 1:nrow(human_qpcr_data)){
    if (human_merged_all_data$`Sample Name`[i]==human_qpcr_data$`Sample Name`[j]){
      human_merged_all_data[i,130:132] = human_qpcr_data[j,1:3]
    } else if (str_detect(human_qpcr_data$`Sample Name`[j],"R") & 
               str_detect(human_merged_all_data$`Sample Name`[i],"R") & 
               (human_qpcr_data$mem_id[j]==human_merged_all_data$unq_memID[i]) & 
               (as.numeric(human_qpcr_data$qpcr_new_date[j] - human_merged_all_data$sample_id_date[i])<=6) &
               (as.numeric(human_qpcr_data$qpcr_new_date[j] - human_merged_all_data$sample_id_date[i])>0)) {
      human_merged_all_data[i,130:132] = human_qpcr_data[j,1:3]
    } else if (!(str_detect(human_qpcr_data$`Sample Name`[j],"R")) & 
               !(str_detect(human_merged_all_data$`Sample Name`[i],"R")) & 
               (human_qpcr_data$mem_id[j]==human_merged_all_data$unq_memID[i]) & 
               (as.numeric(human_qpcr_data$qpcr_new_date[j] - human_merged_all_data$sample_id_date[i])<=6) &
               (as.numeric(human_qpcr_data$qpcr_new_date[j] - human_merged_all_data$sample_id_date[i])>0)){
      human_merged_all_data[i,130:132] = human_qpcr_data[j,1:3]
    }
  }
}
small_data = human_merged_all_data[,125:132]


# extra code in case need to add in
!(human_qpcr_data$`Sample Name`[j] %in% human_merged_all_data$sample_name_from_merge[i]) & 
  !(human_merged_all_data$`Sample Name`[i] %in% human_merged_all_data$sample_name_from_merge[i]) & 




