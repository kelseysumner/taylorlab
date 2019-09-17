# ----------------------------------------- #
#   Spat21 Data Set Cleaning Merge Data     #
#                Human Data                 #
#             Mozzie Phase 2                #
#           September 17, 2019              #
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


#### --------- read in the data sets ----------------- ####

# human data sets
hum_monthly_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/clean_data/hum_monthly_data_v2_17SEP2019.RDS")
hum_sick_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/clean_data/hum_sick_data_v2_17SEP2019.RDS")
hum_table_household_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_table_household_data_19DEC2018.RDS")


#### -------- merge together data sets ------------ ####

# check if data sets in wide/long format for unq_memID
table(hum_monthly_data$unq_memID, useNA = "always") # long format
table(hum_table_household_data$unq_memID, useNA = "always") # wide format, should have 268 people total (so 268 observations)
table(hum_sick_data$unq_memID, useNA = "always") # long format

# need to create an additional unique identifying ID for the human monthly and sick data that incorporates the date
# create new ID for human monthly data
# format: HHH-DDMMYY-X with HHH being the household ID and X being the participant number in the household
monthly_date = rep(NA,nrow(hum_monthly_data))
for (i in 1:nrow(hum_monthly_data)){
  part = strsplit(as.character(hum_monthly_data$today_hum_monthly_data[i]),"-")[[1]]
  part1 = strsplit(part[1],"")[[1]]
  part1_pasted = paste0(part1[3],part1[4])
  monthly_date[i] = paste0(part[3],part[2],part1_pasted)
}
# check the new date format
head(monthly_date)
head(hum_monthly_data$today_hum_monthly_data)
table(nchar(monthly_date),useNA = "always")
# now add the new date format to the unqiue member ID
monthly_unq_memID = rep(NA,nrow(hum_monthly_data))
for (k in 1:nrow(hum_monthly_data)){
  id_split = strsplit(hum_monthly_data$unq_memID[k],"_")[[1]]
  monthly_unq_memID[k] = paste0(id_split[1],"-",monthly_date[k],"-",id_split[2])
}
# check the new unique ID
head(monthly_unq_memID)
head(hum_monthly_data$today_hum_monthly_data)
head(hum_monthly_data$unq_memID)
tail(monthly_unq_memID)
tail(hum_monthly_data$today_hum_monthly_data)
tail(hum_monthly_data$unq_memID)
table(nchar(monthly_unq_memID),useNA = "always")
# add the variable to the new data set
hum_monthly_data$monthly_unq_memID = monthly_unq_memID

# create a new unique identifying ID for the sick data set
# format: HHH-DDMMYY-X-R with HHH being the household ID and X being the participant number in the household
sick_date = rep(NA,nrow(hum_sick_data))
for (i in 1:nrow(hum_sick_data)){
  part = strsplit(as.character(hum_sick_data$today_hum_sick_data[i]),"-")[[1]]
  part1 = strsplit(part[1],"")[[1]]
  part1_pasted = paste0(part1[3],part1[4])
  sick_date[i] = paste0(part[3],part[2],part1_pasted)
}
# check the new date format
head(sick_date)
head(hum_sick_data$today_hum_sick_data)
table(nchar(sick_date),useNA = "always")
# now add the new date format to the unqiue member ID
sick_unq_memID = rep(NA,nrow(hum_sick_data))
for (k in 1:nrow(hum_sick_data)){
  id_split = strsplit(hum_sick_data$unq_memID[k],"_")[[1]]
  sick_unq_memID[k] = paste0(id_split[1],"-",sick_date[k],"-",id_split[2],"-","R")
}
# check the new unique ID
head(sick_unq_memID)
head(hum_sick_data$today_hum_sick_data)
head(hum_sick_data$unq_memID)
table(nchar(sick_unq_memID),useNA = "always")
# add the variable to the new data set
hum_sick_data$sick_unq_memID = sick_unq_memID

# create a variable that is the month and tally up how many times each person was visited in a month
# each person should have only received this monthly follow up visit once per month
hum_monthly_data$month_hum_monthly_data = month(hum_monthly_data$today_hum_monthly_data)
head(hum_monthly_data$today_hum_monthly_data)
head(hum_monthly_data$month_hum_monthly_data)
# also pull out the year
hum_monthly_data$year_hum_monthly_data = year(hum_monthly_data$today_hum_monthly_data)
head(hum_monthly_data$today_hum_monthly_data)
head(hum_monthly_data$year_hum_monthly_data)
# make a month and year combo
hum_monthly_data$month_year_combo_monthly_data = paste0(hum_monthly_data$month_hum_monthly_data,"-",hum_monthly_data$year_hum_monthly_data)
head(hum_monthly_data$month_year_combo_monthly_data)
head(hum_monthly_data$today_hum_monthly_data)
head(hum_monthly_data$month_hum_monthly_data)
head(hum_monthly_data$year_hum_monthly_data)

# check if any of the monthly_unq_memIDs occur twice
length(unique(hum_monthly_data$monthly_unq_memID)) # 4910 unique 
length(which(is.na(hum_monthly_data$monthly_unq_memID) == T)) # 0 missing
count_table = table(hum_monthly_data$monthly_unq_memID, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 175 duplicates, mostly from June 2019
# remove the duplicate rows from the table
dup_rows_removed = distinct(hum_monthly_data)
length(which(is.na(dup_rows_removed$monthly_unq_memID) == T)) # 0 missing
count_table = table(dup_rows_removed$monthly_unq_memID, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 1 duplicate: S08-200719-8
# looks good so rename as hum_monthly_data
hum_monthly_data = dup_rows_removed
# look at this case
duplicate_id = hum_monthly_data[which(hum_monthly_data$monthly_unq_memID == "S08-200719-8"),]
# only difference is that one is that one is missing slept under net info, remove this one
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$monthly_unq_memID == "S08-200719-8" & is.na(hum_monthly_data$time_under_net)),]
# check for duplicates one more time
length(which(is.na(hum_monthly_data$monthly_unq_memID) == T)) # 0 missing
count_table = table(hum_monthly_data$monthly_unq_memID, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 1 duplicate: S08-200719-8

# check if a month and year combo occurs twice for any unq_memID
id_count_table = table(hum_monthly_data$unq_memID,hum_monthly_data$month_year_combo_monthly_data, useNA = "always")
dups_id = id_count_table[which(id_count_table > 1)] # looks like 0 households were visited more than once 
id_count_table = data.frame(id_count_table)
summary(id_count_table$Freq)

# check if there are duplicates in the human sick data set for sick_unq_memID
length(unique(hum_sick_data$sick_unq_memID)) # 521 unique 
length(which(is.na(hum_sick_data$sick_unq_memID) == T)) # 0 missing
count_table = table(hum_sick_data$sick_unq_memID, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates

# check if there are duplicates in unq_memID in the table household data set
count_table = table(hum_table_household_data$unq_memID, useNA = "always")
dups = count_table[which(count_table > 1)]
# looks like no duplicates in the unq_memID for this data set

# remove unq_memID "S12_1" from hum_monthly_data because not in hum_table_household_data and no longer in study
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$unq_memID == "S12_1"),]

# merge together the monthly and table household data sets
hum_monthly_merged_data = full_join(hum_monthly_data,hum_table_household_data, by = "unq_memID")
# check the merge
table(hum_monthly_merged_data$unq_memID,useNA = "always")
table(hum_monthly_data$unq_memID,useNA = "always")
# looks good

# check to see if there were any ids that didn't merge in one data set but did in the other
length(unique(hum_monthly_data$unq_memID)) # 272 unique
length(unique(hum_table_household_data$unq_memID)) # 268 unique
length(unique(hum_monthly_merged_data$unq_memID)) # 279 unique
# looks like we didn't get monthly follow-up for everyone in the study
# look at what ids differ
uniquelist_monthly = unique(hum_monthly_data$unq_memID)
uniquelist_household = unique(hum_table_household_data$unq_memID)
# how many are in both
length(intersect(uniquelist_monthly,uniquelist_household)) # 261 in both
setdiff(uniquelist_monthly,uniquelist_household) # all of these are within the table household data set
setdiff(uniquelist_household,uniquelist_monthly)

# look at columns that are the same between populations
colnames(hum_monthly_merged_data)

# now make sure all the village names correspond with the household IDs
# check first for monthly data
sample_hh_id = sapply(strsplit(hum_monthly_merged_data$HH_ID.x,""),head,1)
sample_village_id = sapply(strsplit(as.character(hum_monthly_merged_data$village_name_hum_monthly_data),""),head,1)
all.equal(sample_hh_id,sample_village_id)  
length(which(sample_hh_id != sample_village_id))
# looks like there are 0 instances where the village doesn't match the household ID
# then check for table data
sample_hh_id = sapply(strsplit(hum_monthly_merged_data$HH_ID.y,""),head,1)
sample_village_id = sapply(strsplit(as.character(hum_monthly_merged_data$village_name_hum_monthly_data),""),head,1)
all.equal(sample_hh_id,sample_village_id)  
length(which(sample_hh_id != sample_village_id))
# aren't any mismatches per se but are some missing values where the table id must not have merged in
# check where have missing village for monthly because didn't have follow-up for those participants 
length(which(is.na(hum_monthly_merged_data$village_name_hum_monthly_data))) # 15 missing
test_missing = hum_monthly_merged_data[which(is.na(hum_monthly_merged_data$village_name_hum_monthly_data)),]
# some of these participants were less than 1 year old

# see missingess for HH_ID and memID variables
length(which(is.na(hum_monthly_merged_data$HH_ID.x))) # 15 missing
length(which(is.na(hum_monthly_merged_data$HH_ID.y))) # 0 missing
length(which(is.na(hum_monthly_merged_data$memID.x))) # 15 missing
length(which(is.na(hum_monthly_merged_data$memID.y))) # 0 missing

# remove the HH_ID.y and memID.y variables
hum_monthly_merged_data$HH_ID.x <- NULL
hum_monthly_merged_data$memID.x <- NULL

# rename the remaining HH_ID and memID
hum_monthly_merged_data = rename(hum_monthly_merged_data, HH_ID = HH_ID.y, memID = memID.y)
colnames(hum_monthly_merged_data)

# check that each participant has the same gender listed throughout the study
gendertable = table(hum_monthly_merged_data$gender_hum_monthly_data,hum_monthly_merged_data$village_name_hum_monthly_data)
prop.table(gendertable,2)
# another way that's at the participant level
participant_data = hum_monthly_merged_data %>%
  group_by(unq_memID) %>%
  summarize(n=n(), n_gender = n_distinct(gender_hum_monthly_data))
# look at those where there are two gender entries
twoentries = participant_data$unq_memID[participant_data$n_gender>1]
twogenders = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID %in% twoentries),]
# choose the gender that occurs the most often for the person that has multiple gender entries
# K05_4
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K05_4"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K05_4" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K05_4"),]
table(subset_1$gender_hum_monthly_data)
# K07_6
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K07_6"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K07_6" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K07_6"),]
table(subset_1$gender_hum_monthly_data)
# K09_5
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K09_5"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K09_5" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K09_5"),]
table(subset_1$gender_hum_monthly_data)
# K10_1
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K10_1"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K10_1" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K10_1"),]
table(subset_1$gender_hum_monthly_data)
# K11_2
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K11_2"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K11_2" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K11_2"),]
table(subset_1$gender_hum_monthly_data)
# K11_4
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K11_4"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K11_4" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K11_4"),]
table(subset_1$gender_hum_monthly_data)
# K11_5
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K11_5"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K11_5" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K11_5"),]
table(subset_1$gender_hum_monthly_data)
# K13_3
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K13_3"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K13_3" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K13_3"),]
table(subset_1$gender_hum_monthly_data)
# K13_5
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K13_5"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K13_5" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K13_5"),]
table(subset_1$gender_hum_monthly_data)
# M05_3
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M05_3"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M05_3" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M05_3"),]
table(subset_1$gender_hum_monthly_data)
# M09_2
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M09_2"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M09_2" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M09_2"),]
table(subset_1$gender_hum_monthly_data)
# M09_3
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M09_3"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M09_3" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M09_3"),]
table(subset_1$gender_hum_monthly_data)
# M12_5
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M12_5"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M12_5" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M12_5"),]
table(subset_1$gender_hum_monthly_data)
# M13_12
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M13_12"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M13_12" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M13_12"),]
table(subset_1$gender_hum_monthly_data)
# M13_13
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M13_13"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M13_13" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M13_13"),]
table(subset_1$gender_hum_monthly_data)
# M13_8
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M13_8"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M13_8" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M13_8"),]
table(subset_1$gender_hum_monthly_data)
# M15_3
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M15_3"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M15_3" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M15_3"),]
table(subset_1$gender_hum_monthly_data)
# M16_4
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M16_4"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M16_4" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M16_4"),]
table(subset_1$gender_hum_monthly_data)
# S01_2
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S01_2"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S01_2" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S01_2"),]
table(subset_1$gender_hum_monthly_data)
# S01_6
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S01_6"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S01_6" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S01_6"),]
table(subset_1$gender_hum_monthly_data)
# S01_8
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S01_8"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S01_8" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S01_8"),]
table(subset_1$gender_hum_monthly_data)
# S02_4
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S02_4"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S02_4" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S02_4"),]
table(subset_1$gender_hum_monthly_data)
# S08_1
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S08_1"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S08_1" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S08_1"),]
table(subset_1$gender_hum_monthly_data)
# S09_10
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S09_10"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S09_10" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S09_10"),]
table(subset_1$gender_hum_monthly_data)
# S09_3
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S09_3"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S09_3" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S09_3"),]
table(subset_1$gender_hum_monthly_data)
# S10_2
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S10_2"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S10_2" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S10_2"),]
table(subset_1$gender_hum_monthly_data)
# S11_3
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S11_3"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S11_3" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S11_3"),]
table(subset_1$gender_hum_monthly_data)
# S12_4
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S12_4"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S12_4" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S12_4"),]
table(subset_1$gender_hum_monthly_data)
# check to see if any other participants with multiple entries
participant_data = hum_monthly_merged_data %>%
  group_by(unq_memID) %>%
  summarize(n=n(), n_gender = n_distinct(gender_hum_monthly_data))
# look at those where there are two gender entries
twoentries = participant_data$unq_memID[participant_data$n_gender>1]
# looks like none are left which is good

# check to see if anywhere where the age differed over follow-up for a participant
participant_data = hum_monthly_merged_data %>%
  group_by(unq_memID) %>%
  summarize(n=n(), n_age = n_distinct(age_cat))
# look at those where there are two gender entries
twoentries = participant_data$unq_memID[participant_data$n_age>1]
# looks like none are miscoded which is good

# check to see if anywhere where the educ_level differed over follow-up for a participant
participant_data = hum_monthly_merged_data %>%
  group_by(unq_memID) %>%
  summarize(n=n(), n_educ_level = n_distinct(educ_level))
# look at those where there are two gender entries
twoentries = participant_data$unq_memID[participant_data$n_educ_level>1]
# looks like none are miscoded which is good

# check to see if anywhere where the employment differed over follow-up for a participant
participant_data = hum_monthly_merged_data %>%
  group_by(unq_memID) %>%
  summarize(n=n(), n_employment = n_distinct(employment))
# look at those where there are two gender entries
twoentries = participant_data$unq_memID[participant_data$n_employment>1]
# looks like none are miscoded which is good

# looks at participants from the data set who were less than 1 year old
table(hum_monthly_merged_data$age_type, useNA = "always") # 52 observations were less than 1 year old
lessthan1 = hum_monthly_merged_data[-which(hum_monthly_merged_data$age_type == "below one year (<1 year)"),]
length(unique(lessthan1$unq_memID)) # 257
length(which(hum_monthly_merged_data$age_type == "below one year (<1 year)")) # 52
lessthan1_people = hum_monthly_merged_data[which(hum_monthly_merged_data$age_type == "below one year (<1 year)"),]
length(unique(lessthan1_people$unq_memID)) # 11 people less than 1
# look at what ids differ
uniquelist_no1s = unique(lessthan1$unq_memID)
uniquelist_monthlymerged = unique(hum_monthly_merged_data$unq_memID)
# how many are in both
length(intersect(uniquelist_no1s,uniquelist_monthlymerged)) # 257 in both
setdiff(uniquelist_no1s,uniquelist_monthlymerged) # all monthly merged here
setdiff(uniquelist_monthlymerged,uniquelist_no1s)
# decide to have keep those participants in for now and decide on their follow-up later on

# export the data set
# write_csv(hum_monthly_merged_data, "hum_monthly_merged_with_table_data_4FEB2019.csv")
# write_rds(hum_monthly_merged_data, "hum_monthly_merged_with_table_data_4FEB2019.RDS")


#### ----- decide who to exclude from longitudinal analyses ----- ####

# read in the merged human monthly and table data set
human_monthly_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/hum_monthly_merged_with_table_data_4FEB2019.RDS")

# look at summaries of the people that will be excluded from longitudinal analyses

# create a household summary
household_summary = human_monthly_merged_data %>%
  group_by(HH_ID) %>%
  summarize(n_person_months = n(), n_households = n_distinct(HH_ID),
            n_participants = n_distinct(unq_memID))
# look at just a few households
m15 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="M15"),]
m16 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="M16"),]
k13 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="K13"),]
k14 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="K14"),]
s12 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="S12"),]
s13 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="S13"),]
# S13 and M16 entered study late
# look for how many people were <1
under1 = human_monthly_merged_data[which(!(is.na(human_monthly_merged_data$age_m))),]
length(unique(under1$unq_memID)) # 11
undernames = unique(under1$unq_memID)
# look for how many participants did not have monthly follow-up and weren't 1
nofolloup = human_monthly_merged_data[which(is.na(human_monthly_merged_data$gender_hum_monthly_data) & is.na(human_monthly_merged_data$age_m)),]
length(unique(nofolloup$unq_memID)) # 10
nofollowuptime = nofolloup$unq_memID
# look for how many participants had < 2 months follow-up
# create a new variable that has the village name for all data (not just those with monthly follow-up)
village_all_data = sapply(strsplit(human_monthly_merged_data$HH_ID,""),head,1)
table(village_all_data, useNA = "always")
# add variable to data set
human_monthly_merged_data$village_all_data = village_all_data
# calculate average person-months per participant
participant_data = human_monthly_merged_data %>%
  group_by(village_all_data,unq_memID) %>%
  summarize(n_count=n())
length(which(participant_data$n_count < 2))
lessthan2 = participant_data$unq_memID[which(participant_data$n_count < 2)]
# 25 participants had < 2 months follow-up
lessthan2
undernames
intersect(lessthan2,undernames)
length(intersect(lessthan2,undernames))
# 5 participants <1 and less than 2 months follow-up
lessthan2
nofollowuptime
intersect(lessthan2,nofollowuptime)
length(intersect(lessthan2,nofollowuptime))
# 10 participants declined monthly follow-up and had less than 2 months follow-up
# 25-15 = 10 participants had < 2 months follow-up
# 268 - 10 -5 - 10 = 237 participants in final data set

# now exclude those participants that fit those exclusion criteria
# create a new variable of unq_memIDs of everyone that fits exclusion criteria
firstcomparison = intersect(lessthan2,undernames)
exclude_ids_1 = union(firstcomparison,lessthan2)
exclude_ids_2 = union(exclude_ids_1,nofollowuptime)
length(exclude_ids_2)
length(which(human_monthly_merged_data$unq_memID %in% exclude_ids_2)) # 25 times
# remove all occurrences of those 25 unq_memIDs
test_data = human_monthly_merged_data[-which(human_monthly_merged_data$unq_memID %in% exclude_ids_2),]
# check the results
nrow(test_data)
nrow(human_monthly_merged_data)
2646-2621 # = 25
# looks good
human_monthly_merged_data = test_data


#### ------ now merge in the sick data with the monthly and table data ------ ####

# look at the colnames in the human merged and human sick data
colnames(hum_sick_data)
colnames(human_monthly_merged_data)

# merge in the sick data to the monthly data
human_merged_all_data = bind_rows(human_monthly_merged_data,hum_sick_data)
# check the merge
colnames(human_merged_all_data)
nrow(hum_sick_data) # 521
nrow(human_monthly_merged_data) # 2621
nrow(human_merged_all_data) # 3142
# looks good

# remove participant S12_1 from sick data because no longer in study
length(which(human_merged_all_data$unq_memID == "S12_1")) # 1 in sick data
human_merged_all_data = human_merged_all_data[-which(human_merged_all_data$unq_memID == "S12_1"),]
# now have 3141 in human_merged_all_data

# check that there are all Rs at the end of the DBS sick data and none in monthly data
# test monthly data
test_rs = rep(NA,nrow(human_merged_all_data))
for (i in 1:nrow(human_merged_all_data)){
  test_rs[i] = str_detect(human_merged_all_data$monthly_unq_memID[i],"R")
}
sum(test_rs,na.rm=T) # no Rs in monthly IDs which is good
# test sick data
test_rs = rep(NA,nrow(human_merged_all_data))
for (i in 1:nrow(human_merged_all_data)){
  test_rs[i] = str_detect(human_merged_all_data$sick_unq_memID[i],"R")
}
sum(test_rs,na.rm=T) # 520 Rs in sick IDs which is good

# merge together the monthly_unq_memID and sick_unq_memID columns
length(which(is.na(human_merged_all_data$monthly_unq_memID))) # 520 - all the sick IDs
length(which(is.na(human_merged_all_data$sick_unq_memID))) # 2621 - all the monthly IDs
monthly_or_sick_unq_memID = ifelse(is.na(human_merged_all_data$sick_unq_memID),human_merged_all_data$monthly_unq_memID,human_merged_all_data$sick_unq_memID)
length(which(is.na(monthly_or_sick_unq_memID))) # no missing, which is good
# add to the data set and compare with the monthly and sick ID columns
human_merged_all_data$`Sample Name` = monthly_or_sick_unq_memID
comparison_df = data.frame(human_merged_all_data$`Sample Name`,human_merged_all_data$monthly_unq_memID, human_merged_all_data$sick_unq_memID)

# remove participant S11_4 because only in sick visit data set
human_merged_all_data = human_merged_all_data[-which(human_merged_all_data$unq_memID == "S11_4"),]

# write out the merged human monthly, table, and sick data
write_csv(human_merged_all_data, "hum_monthly_merged_with_table_and_sick_with_exclusion_data_12APR2019.csv")
write_rds(human_merged_all_data, "hum_monthly_merged_with_table_and_sick_with_exclusion_data_12APR2019.RDS")

