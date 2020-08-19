# ----------------------------------------- #
#   Spat21 Merging Repeated qPCR Samples    #
#                Human Data                 #
#              Mozzie Phase 3               #
#            Adding in redo qpcr            #
#             August 18, 2020               #
#                K. Sumner                  #
# ----------------------------------------- #


#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(stringr)
library(broom)
library(ggplot2)


#### ----- read in the data sets ------------ ####

# read in the merged human monthly and table data set
human_merged_all_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/De-identified Phase II_v13/merged_data/phase3_hum_monthly_merged_with_table_and_sick_18AUG2020.RDS")

# read in the preliminary qpcr data (mozzie phase 1)
human_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/spat21_qpcr_data_clean_human_dbs_16JAN2019.RDS")

# read in the redo qpcr data (mozzie phase 1)
redo_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/spat21_qpcr_data_B8-10-11redo.rds")

# read in the last qpcr data run (mozzie phase 2)
human_pcr_data_p2 = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/clean_data/phase2_spat21_qpcr_data_clean_human_dbs_19NOV2019.RDS")

# read in the fourth qpcr data fun (mozzie phase 3)
human_pcr_data_p3 = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/De-identified Phase II_v13/clean_data/phase3_spat21_qpcr_data_clean_human_dbs_18AUG2020.RDS")



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
length(unique(human_merged_all_data$`Sample Name`)) # 6685 unique 
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
redo_qpcr_data = redo_qpcr_data[,!(colnames(redo_qpcr_data) %in% cols_to_remove)]
human_pcr_data_p2 = human_pcr_data_p2[,!(colnames(human_pcr_data_p2) %in% cols_to_remove)]
human_pcr_data_p2$`pfr364R²` <- NULL
human_pcr_data_p3 = human_pcr_data_p3[,!(colnames(human_pcr_data_p3) %in% cols_to_remove)]


# check for duplicates in the qpcr data
intersect(human_qpcr_data$`Sample Name`,redo_qpcr_data$`Sample Name`)
length(intersect(human_qpcr_data$`Sample Name`,redo_qpcr_data$`Sample Name`))
# 16 samples intersect

# compare qpcr results for each of these duplicate ids in pcr data, remove one row from original
# M09-130617-2
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="M09-130617-2")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="M09-130617-2")]
# same result
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="M09-130617-2"),]

# M09-130617-3
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="M09-130617-3")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="M09-130617-3")]
# same result
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="M09-130617-3"),]

# M09-130617-4
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="M09-130617-4")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="M09-130617-4")]
# same result
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="M09-130617-4"),]

# M09-130617-5
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="M09-130617-5")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="M09-130617-5")]
# negative in the redo, positive in original, default to original
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="M09-130617-5"),]

# S02-211217-1
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="S02-211217-1")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="S02-211217-1")]
# same result
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="S02-211217-1"),]

# S02-211217-2
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="S02-211217-2")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="S02-211217-2")]
# same result
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="S02-211217-2"),]

# S02-211217-5
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="S02-211217-5")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="S02-211217-5")]
# same result
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="S02-211217-5"),]

# S09-161117-2
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="S09-161117-2")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="S09-161117-2")]
# positive in the redo, negative in original, default to original
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="S09-161117-2"),]

# S09-161117-3
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="S09-161117-3")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="S09-161117-3")]
# positive in the redo, negative in original, default to original
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="S09-161117-3"),]

# S09-161117-4
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="S09-161117-4")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="S09-161117-4")]
# positive in the redo, negative in original, default to original
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="S09-161117-4"),]

# S09-161117-5
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="S09-161117-5")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="S09-161117-5")]
# positive in the redo, negative in original, default to original
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="S09-161117-5"),]

# S09-161117-7
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="S09-161117-7")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="S09-161117-7")]
# same result
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="S09-161117-7"),]

# S09-161117-8
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="S09-161117-8")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="S09-161117-8")]
# same result
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="S09-161117-8"),]

# S09-210917-1
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="S09-210917-1")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="S09-210917-1")]
# positive in original, negative in redo
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="S09-210917-1"),]

# S12-240817-2
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="S12-240817-2")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="S12-240817-2")]
# positive in original, negative in redo
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="S12-240817-2"),]

# S12-240817-3
human_qpcr_data$pf_pcr_infection_status[which(human_qpcr_data$`Sample Name`=="S12-240817-3")]
redo_qpcr_data$pf_pcr_infection_status[which(redo_qpcr_data$`Sample Name`=="S12-240817-3")]
# same result
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name`=="S12-240817-3"),]

# check if any duplicates left
intersect(human_qpcr_data$`Sample Name`,redo_qpcr_data$`Sample Name`)
# none left

# now rbind the two qpcr data sets together
human_qpcr_data = rbind(human_qpcr_data,redo_qpcr_data)

# now check if any duplicates between that human_qpcr_data and the human_pcr_p2_data
intersect(human_qpcr_data$`Sample Name`,human_pcr_data_p2$`Sample Name`)
# looks like no duplicates 

# now rbind the two data sets together
human_qpcr_data = rbind(human_qpcr_data,human_pcr_data_p2)

# now check if any duplicates between that human_qpcr_data and the human_pcr_p3_data
intersect(human_qpcr_data$`Sample Name`,human_pcr_data_p3$`Sample Name`)
# looks like no duplicates 

# now rbind the two data sets together
human_qpcr_data = rbind(human_qpcr_data,human_pcr_data_p3)

# add an empty column for the date associated with the sample id for the social demographic data
str(human_merged_all_data$today_hum_monthly_data)
str(human_merged_all_data$today_hum_sick_data)
human_merged_all_data$sample_id_date = if_else(is.na(human_merged_all_data$today_hum_sick_data),human_merged_all_data$today_hum_monthly_data,human_merged_all_data$today_hum_sick_data)
table(human_merged_all_data$sample_id_date, useNA = "always")
str(human_merged_all_data$sample_id_date)

# 21 sample ids need to be recoded in the qpcr data
# K05-2502019-5-R
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K05-2502019-5-R")] = "K05-250219-5-R"
# S11-201319-7
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "S11-201319-7")] = "S11-200319-7"
# S09-115118-7
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "S09-115118-7")] = "S09-151118-7"
# K10-061918-1
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K10-061918-1")] = "K10-190618-1"
# K02-60619-10
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K02-60619-10")] = "K02-060619-10"
# K07-60619-14
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K07-60619-14")] = "K07-060619-14"
# K07-60619-13
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K07-60619-13")] = "K07-060619-13"
# K09-60619-12
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K09-60619-12")] = "K09-060619-12"
# K09-50919-14
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K09-50919-14")] = "K09-050919-14"
# K09-50919-12
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K09-50919-12")] = "K09-050919-12"
# K07-50919-12
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K07-50919-12")] = "K07-050919-12"
# K07-40719-13
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K07-40719-13")] = "K07-040719-13"
# K02-40719-10
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K02-40719-10")] = "K02-040719-10"
# K07-40719-14
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K07-40719-14")] = "K07-040719-14"
# K09-60619-14
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K09-60619-14")] = "K09-060619-14"
# K09-40719-14
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K09-40719-14")] = "K09-040719-14"
# K07-50719-14-R
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K07-50719-14-R")] = "K07-050719-14-R"
# M13-40619-12-R
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "M13-40619-12-R")] = "M13-040619-12-R"
# K07-50919-13
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K07-50919-13")] = "K07-050919-13"
# K09-80819-15
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K09-80819-15")] = "K09-080819-15"
# K07-50919-14
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "K07-50919-14")] = "K07-050919-14"
# M13-91019-11
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "M13-91019-11")] = "M13-091019-11"
# M13-91019-12
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "M13-91019-12")] = "M13-091019-12"
# M13-91019-13
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "M13-91019-13")] = "M13-091019-13"
# M13-91019-15
human_qpcr_data$`Sample Name`[which(human_qpcr_data$`Sample Name` == "M13-91019-15")] = "M13-091019-15"

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
human_qpcr_data %>%
  select(`Sample Name`,qpcr_new_date) %>%
  filter(is.na(qpcr_new_date)) %>%
  View()
# one sample iD is miscoded and will not be able to be correct so remove
human_qpcr_data = human_qpcr_data[-which(human_qpcr_data$`Sample Name` == "R-2019-S03-3"),]



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

# check if any missing sample ids
length(which(is.na(human_merged_all_data$`Sample Name`)))
length(which(is.na(human_qpcr_data$`Sample Name`)))
human_qpcr_data$`Sample Name` = as.character(human_qpcr_data$`Sample Name`)
human_qpcr_data$pf_pcr_infection_status = as.character(human_qpcr_data$pf_pcr_infection_status)
human_qpcr_data$pfr364Q_std_combined = as.numeric(human_qpcr_data$pfr364Q_std_combined)

# create a large for loop that will merge together the qpcr results if they match the social demographic data by within 6 days
# add three empty columns to the human social demographic data set
human_merged_all_data$sample_name_from_merge = as.character(rep(NA,nrow(human_merged_all_data)))
human_merged_all_data$pf_pcr_infection_status = as.character(rep(NA,nrow(human_merged_all_data)))
human_merged_all_data$pfr364Q_std_combined = as.numeric(rep(NA,nrow(human_merged_all_data)))
# human_merged_all_data = human_merged_all_data[c(1:10,3100:3110),]
# then do the for loop
for (i in 1:nrow(human_merged_all_data)){
  for (j in 1:nrow(human_qpcr_data)){
    if (human_merged_all_data$`Sample Name`[i]==human_qpcr_data$`Sample Name`[j] & !(is.na(human_merged_all_data$`Sample Name`[i])) & !(is.na(human_qpcr_data$`Sample Name`[j]))){
      human_merged_all_data[i,133:135] = human_qpcr_data[j,1:3]
    } else if (str_detect(human_qpcr_data$`Sample Name`[j],"R") & 
               str_detect(human_merged_all_data$`Sample Name`[i],"R") & 
               (human_qpcr_data$mem_id[j]==human_merged_all_data$unq_memID[i]) & 
               (as.numeric(human_merged_all_data$sample_id_date[i]-human_qpcr_data$qpcr_new_date[j])<=6) &
               (as.numeric(human_merged_all_data$sample_id_date[i]-human_qpcr_data$qpcr_new_date[j])>0) & 
               !(is.na(human_merged_all_data$`Sample Name`[i])) & !(is.na(human_qpcr_data$`Sample Name`[j]))) {
      human_merged_all_data[i,133:135] = human_qpcr_data[j,1:3]
    } else if (!(str_detect(human_qpcr_data$`Sample Name`[j],"R")) & 
               !(str_detect(human_merged_all_data$`Sample Name`[i],"R")) & 
               (human_qpcr_data$mem_id[j]==human_merged_all_data$unq_memID[i]) & 
               (as.numeric(human_merged_all_data$sample_id_date[i]-human_qpcr_data$qpcr_new_date[j])<=6) &
               (as.numeric(human_merged_all_data$sample_id_date[i]-human_qpcr_data$qpcr_new_date[j])>0) & 
               !(is.na(human_merged_all_data$`Sample Name`[i])) & !(is.na(human_qpcr_data$`Sample Name`[j]))) {
      human_merged_all_data[i,133:135] = human_qpcr_data[j,1:3]
    }
  }
}
test_data = human_merged_all_data
# check the data merge
# see how many ids didn't merge
length(which(is.na(human_merged_all_data$sample_name_from_merge))) # 708 missing
length(which(!(is.na(human_merged_all_data$sample_name_from_merge)))) # 5977 total merged
# 6685-6189=496 should not have merged
# had 708 not merge so: 708-496 = 212 that didn't merge
# check if all merged ids unique
length(unique(human_merged_all_data$sample_name_from_merge)) # 5975 unique
count_table = table(human_merged_all_data$sample_name_from_merge, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 3 duplicates
length(dups_table)
dups_table
# fix these three duplicates
# K05-021117-4-R
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$sample_name_from_merge=="K05-021117-4-R" & human_merged_all_data$`Sample Name` == "K05-031117-4-R")] = NA
human_merged_all_data$pf_pcr_infection_status[which(human_merged_all_data$sample_name_from_merge=="K05-021117-4-R" & human_merged_all_data$`Sample Name` == "K05-031117-4-R")] = NA
# S02-120719-1-R
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$sample_name_from_merge=="S02-120719-1-R" & human_merged_all_data$`Sample Name` == "S02-180719-1-R")] = NA
human_merged_all_data$pf_pcr_infection_status[which(human_merged_all_data$sample_name_from_merge=="S02-120719-1-R" & human_merged_all_data$`Sample Name` == "S02-180719-1-R")] = NA
human_merged_all_data$pfr364Q_std_combined[which(human_merged_all_data$sample_name_from_merge=="S02-120719-1-R" & human_merged_all_data$`Sample Name` == "S02-180719-1-R")] = NA
# S10-260719-6-R
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$sample_name_from_merge=="S10-260719-6-R" & human_merged_all_data$`Sample Name` == "S10-300719-6-R")] = NA
human_merged_all_data$pf_pcr_infection_status[which(human_merged_all_data$sample_name_from_merge=="S10-260719-6-R" & human_merged_all_data$`Sample Name` == "S10-300719-6-R")] = NA
human_merged_all_data$pfr364Q_std_combined[which(human_merged_all_data$sample_name_from_merge=="S10-260719-6-R" & human_merged_all_data$`Sample Name` == "S10-300719-6-R")] = NA
# recheck if all merged ids unique
length(unique(human_merged_all_data$sample_name_from_merge)) # 5175 unique
length(which(is.na(human_merged_all_data$sample_name_from_merge))) # 711 missing now which is correct
count_table = table(human_merged_all_data$sample_name_from_merge, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table
# no duplicates now

# figure out what qpcr results didn't merge in
length(intersect(human_merged_all_data$sample_name_from_merge,human_qpcr_data$`Sample Name`)) # 5974 samples merged
length(setdiff(human_merged_all_data$sample_name_from_merge,human_qpcr_data$`Sample Name`)) # 1: missing value NA
length(setdiff(human_qpcr_data$`Sample Name`,human_merged_all_data$sample_name_from_merge)) # 215, which is how many didn't merge
nomerge_list = setdiff(human_qpcr_data$`Sample Name`,human_merged_all_data$sample_name_from_merge)
# look at those that didn't merge and try to figure out why they didn't merge
nomerge = human_qpcr_data[which(human_qpcr_data$`Sample Name` %in% nomerge_list),]
# export to decide how to change each sample
# write_csv(nomerge,"Desktop/phase2_human_qpcr_no_merge.csv")
# look at those that didn't merge in the large human social demograhpic data set
nomerge_social = human_merged_all_data[which(is.na(human_merged_all_data$sample_name_from_merge)),]
nomerge_social %>%
  select(`Sample Name`,unq_memID) %>%
  View()

# write out the merged data set before making changes
# write_rds(human_merged_all_data,"Desktop/phase2_qpcr_merged_prelim.rds")
human_merged_all_data = read_rds("Desktop/phase2_qpcr_merged_prelim.rds")

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
# count = 5974 so the merge worked correctly!

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
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="K07-100119-13")] = "K07-100191-13"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S05-130617-2")] = "S05-140617-2"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S09-130617-2")] = "S09-140617-2"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S02-130917-3")] = "S02-210917-3"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="M13-111018-8")] = "M13-011018-8"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S10-100119-4-R")] = "S10-110119-4-R"
human_merged_all_data$sample_name_from_merge[which(human_merged_all_data$`Sample Name`=="S10-300719-6-R")] = "S10-300719-6-R"

# check the data merge
# see how many ids didn't merge
length(which(is.na(human_merged_all_data$sample_name_from_merge))) # 645 missing, looks like worked correctly
length(which(!(is.na(human_merged_all_data$sample_name_from_merge)))) # 6040
# check if all merged ids unique
length(unique(human_merged_all_data$sample_name_from_merge)) # 6041 unique
count_table = table(human_merged_all_data$sample_name_from_merge, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table

# now merge in the rest of the qpcr result information for the 58 new samples
qpcr_small_data = human_qpcr_data[,1:3]
qpcr_small_data = rename(qpcr_small_data,"sample_name_from_merge"="Sample Name")
qpcr_small_data = qpcr_small_data[which(qpcr_small_data$sample_name_from_merge %in% nomerge$`Sample Name`),]
human_merged_all_data = left_join(human_merged_all_data,qpcr_small_data,by = "sample_name_from_merge")
# check the merge
length(which(!(is.na(human_merged_all_data$pf_pcr_infection_status.y)))) # 66
# look correct

# remove merge_date column
human_merged_all_data$merge_date <- NULL

# make a separate data set in case messes up in for loop
orig_data = human_merged_all_data

# if messes up do this
# human_merged_all_data = orig_data


# now loop through each row and move over those two columns
# check colnames
colnames(human_merged_all_data)
# start for loop to combine qpcr results
for (i in 1:nrow(human_merged_all_data)){
  if (is.na(human_merged_all_data[i,134])){
    for (k in 1:2){   # this is for all data that is present in .y files but not in .x
      startpoint = 133 + k
      human_merged_all_data[i,startpoint] = human_merged_all_data[i,startpoint+2]
      human_merged_all_data[i,startpoint+2] <- NA
    }
  } else if (is.na(human_merged_all_data[i,136])){
    for (k in 1:2){ # this is for all data that is present in .x files but not in .y -> just make it NULL
      startpoint = 133 + k
      human_merged_all_data[i,startpoint+2] <- NA
    }
  } else {
    for (k in 1:2){ # both data sets are missing -> just make it NULL at .y location
      startpoint = 133 + k
      human_merged_all_data[i,startpoint+2] <- NA
    }
  } 
}
# check the output
length(which(is.na(human_merged_all_data$pf_pcr_infection_status.x))) # 650 missing (remember that there were 7 Hb missing so Pf results missing but merged in)
length(which(is.na(human_merged_all_data$pf_pcr_infection_status.y))) # 6685 missing
# looks like it worked correctly
small_check = human_merged_all_data[,133:137]
# remove the .y columns
human_merged_all_data_final = human_merged_all_data[,-c(136:137)]
human_merged_all_data_final = rename(human_merged_all_data_final,"pf_pcr_infection_status"="pf_pcr_infection_status.x","pfr364Q_std_combined"="pfr364Q_std_combined.x")
colnames(human_merged_all_data_final)

# now move up the columns for those ids that had the monthly follow-up and sick visit on the same day
# re-add the double visit test names to the data set to point out people who had the sick and monthly visit on the same day
# create a new sample name variable
# merge together the monthly_unq_memID and sick_unq_memID columns
length(which(is.na(human_merged_all_data_final$monthly_unq_memID))) # 1066 - all the sick IDs
length(which(is.na(human_merged_all_data_final$sick_unq_memID))) # 5619 - all the monthly IDs
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
length(unique(double_visit_test)) # 6318 unique/6685 obs
length(which(is.na(double_visit_test) == T)) # 0 missing
count_table = table(double_visit_test, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 367 duplicates
length(dups_table)
dups_table_df = data.frame(dups_table)

# for all these IDs, remove the asymptomatic visit information 
human_merged_all_data_final$double_visit_test = double_visit_test
head(human_merged_all_data_final$double_visit_test,20)
small_check = human_merged_all_data_final[,130:137]

# sort the double_visit_test variable
human_merged_all_data_final = human_merged_all_data_final[order(human_merged_all_data_final$double_visit_test),]
small_check = human_merged_all_data_final[,130:137]

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
length(which(is.na(human_merged_all_data_final$monthly_unq_memID))) # 699 - (1066-367 = 699)
length(which(is.na(human_merged_all_data_final$sick_unq_memID))) # 5252 - (5619-367 = 5252)
# looks like it is working correctly
# delete the rows that are now duplicates
for (i in 1:nrow(human_merged_all_data_final)){
  if ((human_merged_all_data_final$double_visit_test[i] == human_merged_all_data_final$double_visit_test[i+1]) & !(is.na(human_merged_all_data_final$double_visit_test[i])) & !(is.na(human_merged_all_data_final$double_visit_test[i+1])) & i != nrow(human_merged_all_data_final)){
    human_merged_all_data_final = human_merged_all_data_final[-i,]
  }
}
# check the output
length(which(is.na(human_merged_all_data_final$monthly_unq_memID))) # 699 - (1066-367 = 699)
length(which(is.na(human_merged_all_data_final$sick_unq_memID))) # 5252 - (5619-367 = 5252)
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

# recode pf_pcr_infection_status as a factor
table(human_merged_all_data_final$pf_pcr_infection_status, useNA = "always")
human_merged_all_data_final$pf_pcr_infection_status = factor(human_merged_all_data_final$pf_pcr_infection_status)
table(human_merged_all_data_final$pf_pcr_infection_status, useNA = "always")
length(which(human_merged_all_data_final$pfr364Q_std_combined > 0))

# export as a CSV and RDS file
write_csv(human_merged_all_data_final,"Desktop/phase3_spat21_human_merged_all_data_18AUG2020.csv")
write_rds(human_merged_all_data_final,"Desktop/phase3_spat21_human_merged_all_data_18AUG2020.rds")

