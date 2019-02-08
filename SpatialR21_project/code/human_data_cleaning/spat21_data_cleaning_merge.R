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
write_csv(hum_monthly_merged_data, "hum_monthly_merged_with_table_data_4FEB2019.csv")
write_rds(hum_monthly_merged_data, "hum_monthly_merged_with_table_data_4FEB2019.RDS")








