# ----------------------------------------- #
#   Spat21 Data Set Cleaning Merge Data     #
#                Human Data                 #
#            December 19, 2018              #
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
hum_ann_household_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_ann_household_data_19DEC2018.RDS")
hum_monthly_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_monthly_data_19DEC2018.RDS")
hum_sick_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_sick_data_19DEC2018.RDS")
hum_sleeping_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_sleeping_data_19DEC2018.RDS")
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
length(unique(hum_monthly_data$monthly_unq_memID)) # 2640 unique 
length(which(is.na(hum_monthly_data$monthly_unq_memID) == T)) # 0 missing
count_table = table(hum_monthly_data$monthly_unq_memID, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 1 duplicate: S05-130617-4
# look at this case
duplicate_id = hum_monthly_data[which(hum_monthly_data$monthly_unq_memID == "S05-130617-4"),]
# only difference is that one is labeled as a "male" and one is labeled as a "female" for the gender
# tabulate the gender distribution for S05_4
gender_tab = hum_monthly_data[which(hum_monthly_data$unq_memID == "S05_4"),]
table(gender_tab$gender_hum_monthly_data, useNA = "always")
# only 1 female observation, so assume that was done in error and only remove that duplicate row
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$monthly_unq_memID == "S05-130617-4" & hum_monthly_data$gender_hum_monthly_data == "female"),]
# check it
gender_tab = hum_monthly_data[which(hum_monthly_data$unq_memID == "S05_4"),]
table(gender_tab$gender_hum_monthly_data, useNA = "always")

# check if a month and year combo occurs twice for any unq_memID
id_count_table = table(hum_monthly_data$unq_memID,hum_monthly_data$month_year_combo_monthly_data, useNA = "always")
dups_id = id_count_table[which(id_count_table > 1)] # looks like 7 households were visited more than once (Wendy found this, too)
id_count_table = data.frame(id_count_table)
duplicate_id_names = id_count_table[which(id_count_table$Freq > 1),]
# pull each of these entries out from the data set
# K04_3
multiple_visits = hum_monthly_data[which(hum_monthly_data$unq_memID == "K04_3" & hum_monthly_data$month_year_combo_monthly_data == "1-2018"),]
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$monthly_unq_memID == "K04-190118-3"),]
# S05_2
multiple_visits = hum_monthly_data[which(hum_monthly_data$unq_memID == "S05_2" & hum_monthly_data$month_year_combo_monthly_data == "6-2017"),]
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$monthly_unq_memID == "S05-130617-2"),]
# S05_5
multiple_visits = hum_monthly_data[which(hum_monthly_data$unq_memID == "S05_5" & hum_monthly_data$month_year_combo_monthly_data == "6-2017"),]
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$monthly_unq_memID == "S05-170617-5"),]
# S09_2
multiple_visits = hum_monthly_data[which(hum_monthly_data$unq_memID == "S09_2" & hum_monthly_data$month_year_combo_monthly_data == "6-2017"),]
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$monthly_unq_memID == "S09-130617-2"),]
# K01_9
multiple_visits = hum_monthly_data[which(hum_monthly_data$unq_memID == "K01_9" & hum_monthly_data$month_year_combo_monthly_data == "7-2017"),]
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$monthly_unq_memID == "K01-060717-9"),]
# K10_1
multiple_visits = hum_monthly_data[which(hum_monthly_data$unq_memID == "K10_1" & hum_monthly_data$month_year_combo_monthly_data == "7-2017"),]
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$monthly_unq_memID == "K10-070717-1"),]
# S02_3
multiple_visits = hum_monthly_data[which(hum_monthly_data$unq_memID == "S02_3" & hum_monthly_data$month_year_combo_monthly_data == "9-2017"),]
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$monthly_unq_memID == "S02-130917-3"),]
# I kept the entry that was present in the lab inventory or, if not in the lab inventory, I chose the one that said it had more data or had a DBS

# check if there are duplicates in the human sick data set for sick_unq_memID
length(unique(hum_sick_data$sick_unq_memID)) # 521 unique 
length(which(is.na(hum_sick_data$sick_unq_memID) == T)) # 0 missing
count_table = table(hum_sick_data$sick_unq_memID, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates

# check if there are duplicates in unq_memID in the table household data set
count_table = table(hum_table_household_data$unq_memID, useNA = "always")
dups = count_table[which(count_table > 1)]
# looks like no duplicates in the unq_memID for this data set

# for this data set, create a variable that is the age categories:
# 1-5 years is 1, 6-10 years is 2, 11-17 years is 3, 18 and above is 4
hum_table_household_data$age_cat = ifelse(hum_table_household_data$age_y >= 1 & hum_table_household_data$age_y <= 5,1,
                                          ifelse(hum_table_household_data$age_y >= 6 & hum_table_household_data$age_y <= 10,2,
                                                 ifelse(hum_table_household_data$age_y >= 11 & hum_table_household_data$age_y <= 17,3,
                                                        ifelse(hum_table_household_data$age_y >=18,4,NA))))
# make age_cat a factor
hum_table_household_data$age_cat = factor(hum_table_household_data$age_cat,levels = c(1,2,3,4), labels = c("1-5","6-10","11-17","18+"))
# check the variable coding
table(hum_table_household_data$age_cat,hum_table_household_data$age_y)

# remove unq_memID "S12_1" from hum_monthly_data because not in hum_table_household_data and no longer in study
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$unq_memID == "S12_1"),]

# merge together the monthly and table household data sets
hum_monthly_merged_data = full_join(hum_monthly_data,hum_table_household_data, by = "unq_memID")
# check the merge
table(hum_monthly_merged_data$unq_memID,useNA = "always")
table(hum_monthly_data$unq_memID,useNA = "always")
# looks good

# check to see if there were any ids that didn't merge in one data set but did in the other
length(unique(hum_monthly_data$unq_memID)) # 253 unique
length(unique(hum_table_household_data$unq_memID)) # 268 unique
length(unique(hum_monthly_merged_data$unq_memID)) # 268 unique
# looks like we didn't get monthly follow-up for everyone in the study
# look at what ids differ
uniquelist_monthly = unique(hum_monthly_data$unq_memID)
uniquelist_household = unique(hum_table_household_data$unq_memID)
# how many are in both
length(intersect(uniquelist_monthly,uniquelist_household)) # 253 in both
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

# write out the merged human monthly, table, and sick data
# write_csv(human_merged_all_data, "hum_monthly_merged_with_table_and_sick_with_exclusion_data_27FEB2019.csv")
# write_rds(human_merged_all_data, "hum_monthly_merged_with_table_and_sick_with_exclusion_data_27FEB2019.RDS")


#### ------- now merge in the qpcr data with the human monthly, table, and sick data ------ ####

# read in the merged human monthly and table data set
human_merged_all_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/hum_monthly_merged_with_table_and_sick_with_exclusion_data_27FEB2019.RDS")

# read in the preliminary qpcr data
human_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/spat21_qpcr_data_clean_human_dbs_16JAN2019.RDS")

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
length(unique(human_merged_all_data$`Sample Name`)) # 3141 unique 
length(which(is.na(human_merged_all_data$`Sample Name`) == T)) # 0 missing
count_table = table(human_merged_all_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates

# merge the qpcr data with the human_merged_all_data
human_merged_all_data$`Sample Name` = as.character(human_merged_all_data$`Sample Name`)
human_qpcr_data$`Sample Name` = as.character(human_qpcr_data$`Sample Name`)
human_merged_all_data_qpcr = left_join(human_merged_all_data,human_qpcr_data,by = "Sample Name")
# check the merge
length(which(is.na(human_qpcr_data$`Experiment Name`))) # 0 missing
length(which(is.na(human_merged_all_data_qpcr$`Experiment Name`))) # 1061 missing
# looks like a lot of sample IDs didn't merge
length(intersect(human_merged_all_data$`Sample Name`, human_qpcr_data$`Sample Name`)) # 2086 samples merged
length(setdiff(human_merged_all_data$`Sample Name`,human_qpcr_data$`Sample Name`)) # 1055
length(setdiff(human_qpcr_data$`Sample Name`,human_merged_all_data$`Sample Name`)) # 804
# all 804 from the qpcr didn't merge into full data set 
# 3141-2890 = didn't have 252 dbs from follow-ups
# look at the outersect
outersect <- function(x, y, ...) {
  big.vec <- c(x, y, ...)
  duplicates <- big.vec[duplicated(big.vec)]
  setdiff(big.vec, unique(duplicates))
}
length(outersect(human_merged_all_data$`Sample Name`,human_qpcr_data$`Sample Name`)) # 1868
# look at which qpcr ids did not merge in with the other data set
setdiff(human_qpcr_data$`Sample Name`,human_merged_all_data$`Sample Name`) # 808
# check S08-250118-8 and see if similar to S08-270118-8
test = human_merged_all_data_qpcr[which(human_merged_all_data_qpcr$`Sample Name` == "S08-270118-8"),]
length(which(is.na(test$`Experiment Name`))) # didn't merge in!!!
# check S11-240817-8 and see if similar to S11-250817-8
test = human_merged_all_data_qpcr[which(human_merged_all_data_qpcr$`Sample Name` == "S11-250817-8"),]
length(which(is.na(test$`Experiment Name`))) # didn't merge in!!!
test = human_merged_all_data_qpcr[which(human_merged_all_data_qpcr$unq_memID == "S11_8"),]
test$monthly_unq_memID
test$sick_unq_memID
test$`Sample Name`
test$`Experiment Name`
# it looks like sometimes the follow-up date and the DBS date are off by a few days

# for all the samples that didn't merge, try to merge them based on the month and year
# decide which variables to use
length(which(is.na(human_merged_all_data_qpcr$month_year_combo_monthly_data))) # 520 missing
length(which(is.na(human_merged_all_data_qpcr$HH_ID))) # 0 missing
length(which(is.na(human_merged_all_data_qpcr$memID))) # 0 missing
# create a second variable for the unqmemID, HH_ID, and MM-YY
new_sample_id = rep(NA,nrow(human_merged_all_data_qpcr))
for (i in 1:nrow(human_merged_all_data_qpcr)){
  if (!(is.na(human_merged_all_data_qpcr$month_year_combo_monthly_data[i]))){
    monthfollowup1 = as.character(month(human_merged_all_data_qpcr$today_hum_monthly_data[i]))
    if (nchar(monthfollowup1) == 1){
      monthfollowup = paste0("0",as.character(monthfollowup1))
    } else {
      monthfollowup = monthfollowup1
    }
    yearfollowup1 = strsplit(as.character(year(human_merged_all_data_qpcr$today_hum_monthly_data[i])),"")[[1]]
    yearfollowup = paste0(yearfollowup1[c(3,4)], collapse="")
    month_year_short_2 = paste0(monthfollowup,yearfollowup)
    new_sample_id[i] = paste0(human_merged_all_data_qpcr$HH_ID[i],"-",month_year_short_2,"-",human_merged_all_data_qpcr$memID[i])
  } else {
    monthsick1 = as.character(month(human_merged_all_data_qpcr$today_hum_sick_data[i]))
    if (nchar(monthsick1) == 1){
      monthsick = paste0("0",as.character(monthsick1))
    } else {
      monthsick = monthsick1
    }
    yearsick1 = strsplit(as.character(year(human_merged_all_data_qpcr$today_hum_sick_data[i])),"")[[1]]
    yearsick = paste0(yearsick1[c(3,4)], collapse="")
    month_year_short_2 = paste0(monthsick,yearsick)
    new_sample_id[i] = paste0(human_merged_all_data_qpcr$HH_ID[i],"-",month_year_short_2,"-",human_merged_all_data_qpcr$memID[i],"-R")
  }
}
# check the output
length(which(is.na(new_sample_id))) # 0 missing
head(human_merged_all_data_qpcr$month_year_combo_monthly_data)
head(human_merged_all_data_qpcr$HH_ID)
head(human_merged_all_data$memID)
head(new_sample_id)
table(nchar(new_sample_id),useNA = "always")
tail(human_merged_all_data_qpcr$today_hum_sick_data)
tail(human_merged_all_data_qpcr$HH_ID)
tail(human_merged_all_data$memID)
tail(new_sample_id)
# add the new sample id to the data set
human_merged_all_data_qpcr$new_sample_id = new_sample_id

# now merge in those qPCR DBS that didn't already merge into the data set
# pull out those that didn't merge
dbs_no_merge = setdiff(human_qpcr_data$`Sample Name`,human_merged_all_data$`Sample Name`) 
dbs_no_merge_df = human_qpcr_data[which(human_qpcr_data$`Sample Name` %in% dbs_no_merge),]
# create a second variable for the unqmemID, HH_ID, and MM-YY for the qpcr data
new_sample_id = rep(NA,nrow(dbs_no_merge_df))
for (i in 1:nrow(dbs_no_merge_df)){
    firstsplit = strsplit(dbs_no_merge_df$`Sample Name`[i],"-")[[1]]
    if (length(firstsplit) == 4){
      secondsplit = as.character(strsplit(firstsplit[2],"")[[1]])
      remergedates = paste0(secondsplit[c(3,4,5,6)],collapse="")
      new_sample_id[i] = paste0(firstsplit[1],"-",remergedates,"-",firstsplit[3],"-R")
    } else {
      secondsplit = as.character(strsplit(firstsplit[2],"")[[1]])
      remergedates = paste0(secondsplit[c(3,4,5,6)],collapse="")
      new_sample_id[i] = paste0(firstsplit[1],"-",remergedates,"-",firstsplit[3])
    }
}
# check the output
length(which(is.na(new_sample_id))) # 0 missing
head(dbs_no_merge_df$`Sample Name`)
head(new_sample_id)
table(nchar(new_sample_id),useNA = "always")
tail(dbs_no_merge_df$`Sample Name`)
tail(new_sample_id)
# add the new sample id to the data set
dbs_no_merge_df$new_sample_id = new_sample_id

# check for duplicates in new_sample_id for the human_merged_all_data_qpcr data set
length(unique(human_merged_all_data_qpcr$new_sample_id)) # 3109 unique 
length(which(is.na(human_merged_all_data_qpcr$new_sample_id) == T)) # 0 missing
count_table = table(human_merged_all_data_qpcr$new_sample_id, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 31 duplicates
# check to see if any of these duplicates were among those not matched, and there aren't any

# check for duplicates in new_sample_id for the dbs_no_merge_df data set
length(unique(dbs_no_merge_df$new_sample_id)) # 804 unique 
length(which(is.na(dbs_no_merge_df$new_sample_id) == T)) # 0 missing
count_table = table(dbs_no_merge_df$new_sample_id, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates left

# now merge in the dbs_no_merge_df to the human_merged_all_data_qpcr by new_sample_id
test_merge_all = left_join(human_merged_all_data_qpcr,dbs_no_merge_df,by="new_sample_id")
colnames(test_merge_all)
# check the merge
length(intersect(human_merged_all_data_qpcr$new_sample_id,dbs_no_merge_df$new_sample_id)) # 725/804 samples merged
length(setdiff(human_merged_all_data_qpcr$new_sample_id,dbs_no_merge_df$new_sample_id)) # 2383
length(setdiff(dbs_no_merge_df$new_sample_id,human_merged_all_data_qpcr$new_sample_id)) # 79
# still have 79 samples to match
setdiff(dbs_no_merge_df$new_sample_id,human_merged_all_data_qpcr$new_sample_id)
# looks like the R wasn't always added to the DBS for the sick data
# note that 3 of these are S12_1 which was removed from that study and 4 of these are duplicates so really only 72 left to match

# first remove the columns that are not needed (standards)
cols_to_remove = c("HbtubStd1a.x","HbtubStd1b.x","HbtubStd2a.x","HbtubStd2b.x","HbtubStd3a.x","HbtubStd3b.x",
                   "HbtubStd4a.x","HbtubStd4b.x","HbtubStd5a.x","HbtubStd5b.x","HbtubStd6a.x","HbtubStd6b.x",
                   "HbtubStd7a.x","HbtubStd7b.x","HbtubStd8a.x","HbtubStd8b.x","HbtubStd9a.x","HbtubStd9b.x",
                   "HbtubStd10a.x","HbtubStd10b.x","pfr364Std1a.x","pfr364Std1b.x","pfr364Std2a.x","pfr364Std2b.x",
                   "pfr364Std3a.x","pfr364Std3b.x","pfr364Std4a.x","pfr364Std4b.x","pfr364Std5a.x","pfr364Std5b.x",
                   "pfr364Std6a.x","pfr364Std6b.x","pfr364Std7a.x","pfr364Std7b.x","pfr364Std8a.x","pfr364Std8b.x",
                   "pfr364Std9a.x","pfr364Std9b.x","pfr364Std10a.x","pfr364Std10b.x","r_value_std.x","intercept_std.x",
                   "slope_std.x", "HbtubStd1a.y","HbtubStd1b.y","HbtubStd2a.y","HbtubStd2b.y","HbtubStd3a.y","HbtubStd3b.y",
                   "HbtubStd4a.y","HbtubStd4b.y","HbtubStd5a.y","HbtubStd5b.y","HbtubStd6a.y","HbtubStd6b.y",
                   "HbtubStd7a.y","HbtubStd7b.y","HbtubStd8a.y","HbtubStd8b.y","HbtubStd9a.y","HbtubStd9b.y",
                   "HbtubStd10a.y","HbtubStd10b.y","pfr364Std1a.y","pfr364Std1b.y","pfr364Std2a.y","pfr364Std2b.y",
                   "pfr364Std3a.y","pfr364Std3b.y","pfr364Std4a.y","pfr364Std4b.y","pfr364Std5a.y","pfr364Std5b.y",
                   "pfr364Std6a.y","pfr364Std6b.y","pfr364Std7a.y","pfr364Std7b.y","pfr364Std8a.y","pfr364Std8b.y",
                   "pfr364Std9a.y","pfr364Std9b.y","pfr364Std10a.y","pfr364Std10b.y","r_value_std.y","intercept_std.y",
                   "slope_std.y")
test_merge_all = test_merge_all[,!(colnames(test_merge_all) %in% cols_to_remove)]

# merge together the qpcr columns
#  sample name
colnames(test_merge_all)
length(which(is.na(test_merge_all$`Sample Name.y`))) # 2416
length(which(is.na(test_merge_all$`Sample Name.x`))) # 0
# not going to change for now

# experiment name
length(which(is.na(test_merge_all$`Experiment Name.y`))) # 2416
length(which(is.na(test_merge_all$`Experiment Name.x`))) # 1055
experiment_name = ifelse(is.na(test_merge_all$`Experiment Name.y`),test_merge_all$`Experiment Name.x`,test_merge_all$`Experiment Name.y`)
length(which(is.na(experiment_name))) # still 333 missing
test_merge_all$experiment_name = experiment_name
# 3141-2890 = 251 that should be missing at the end
# 76 DBS samples didn't merge
# 79+251 = 330 samples
# create test data set where experiment name is missing that indicates it didn't merge (so three need to be accounted for)
#test = test_merge_all[which(is.na(test_merge_all$experiment_name)),]
# create a test data set that indicates which qpcr data did not merge
#dbs_no_merge_2 = setdiff(dbs_no_merge_df$new_sample_id,human_merged_all_data_qpcr$new_sample_id) 
#dbs_no_merge_2_df = dbs_no_merge_df[which(dbs_no_merge_df$new_sample_id %in% dbs_no_merge_2),]
#dbs_no_merge_df_2 = human_qpcr_data[which(human_qpcr_data$`Sample Name` %in% dbs_no_merge_2_df$`Sample Name`),]

# export the data sets to figure out where Rs were left off of DBS data set
# write_csv(test,"human_descriptive_no_merge_with_qpcr.csv")
# write_csv(dbs_no_merge_df_2,"qpcr_no_merge_with_human_descriptive.csv")

# while still trying to figure out why those 72 DBS didn't merge, stick with data set that did merge
human_merged_all_data_final = test_merge_all

# check for participants who a symptomatic visit on the same day as an asymptomatic visit
# take off R for sick data and test to see if any IDs match monthly data
double_visit_test = rep(NA,nrow(human_merged_all_data_final))
for (i in 1:nrow(human_merged_all_data_final)){
  if (str_detect(human_merged_all_data_final$`Sample Name.x`[i],"R")){
    firstsplit = strsplit(human_merged_all_data_final$`Sample Name.x`[i],"-")[[1]]
    double_visit_test[i] = paste0(firstsplit[1],"-",firstsplit[2],"-",firstsplit[3],collapse="")
  } else {
    double_visit_test[i] = human_merged_all_data_final$`Sample Name.x`[i]
  }
}
# look for duplicates in double visit_test
length(unique(double_visit_test)) # 2920 unique/3141 obs
length(which(is.na(double_visit_test) == T)) # 0 missing
count_table = table(double_visit_test, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 221 duplicates
length(dups_table)
dups_table_df = data.frame(dups_table)
# for all these IDs, remove the asymptomatic visit information
human_merged_all_data_final$double_visit_test = double_visit_test
# pull out the duplicate visits
duplicate_visits = human_merged_all_data_final[which(human_merged_all_data_final$double_visit_test %in% dups_table_df$double_visit_test),]
# quickly figure how many of these sick visits on monthly data collection days had a positive RDT
table(duplicate_visits$rdt_rst, useNA = "always") # 71 positive, 150 negative

# check to see if any people who had qpcr data for both monthly follow-up and sick visit
length(which(!(is.na(duplicate_visits$today_hum_monthly_data)) & (!(is.na(duplicate_visits$`Experiment Name.x`)) | !(is.na(duplicate_visits$`Experiment Name.y`)))))
length(which(!(is.na(duplicate_visits$today_hum_sick_data)) & (!(is.na(duplicate_visits$`Experiment Name.x`)) | !(is.na(duplicate_visits$`Experiment Name.y`)))))
length(which(is.na(duplicate_visits$`Experiment Name.x`) & is.na(duplicate_visits$`Experiment Name.y`))) # 219 missing both so theoretically 2 that have two DBS
# look at a dataframe of merge results
small_df_for_merge_results = data.frame(duplicate_visits$double_visit_test, duplicate_visits$`Sample Name.x`,duplicate_visits$`Experiment Name.x`,duplicate_visits$`Experiment Name.y`)
# 7 had asymp and symp DBS collected on same day:
# K01-030817-2, K01-110717-9, K01-120617-4, K02-050817-9, M06-130617-4, M13-130717-1, S01-240817-6
# check qpcr results to see if discrepant
# K01-030817-2 - both positive
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "K01-030817-2"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y
# K01-110717-9 - both positive
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "K01-110717-9"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y
# K01-120617-4 - both positive
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "K01-120617-4"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y
# K02-050817-9 - both positive
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "K02-050817-9"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y
# M06-130617-4 - both positive
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "M06-130617-4"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y
# M13-130717-1 - both negative
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "M13-130717-1"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y
# S01-240817-6 - both negative
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "S01-240817-6"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y

# for each duplicate visit, write some code that pulls the duplicate sick visit information into the corresponding sick columns for the monthly visit
# column where sick visit info starts:
# FINISH!!!




