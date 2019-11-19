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
hum_monthly_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/clean_data/hum_monthly_data_p2_14NOV2019.RDS")
hum_sick_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/clean_data/hum_sick_data_p2_14NOV2019.RDS")
hum_table_household_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_table_household_data_19DEC2018.RDS")
hum_table_household_data_p2= readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Cohort data through August July 2019/clean_data/hum_socdem2_data_p2_14NOV2019.rds")


#### -------- merge together data sets ------------ ####

# check if data sets in wide/long format for unq_memID
table(hum_monthly_data$unq_memID, useNA = "always") # long format
table(hum_table_household_data$unq_memID, useNA = "always") # wide format, should have 268 people total (so 268 observations)
table(hum_sick_data$unq_memID, useNA = "always") # long format
table(hum_table_household_data_p2$unq_memID, useNA = "always") # wide format, should have 284 people total (so 268 observations)
# note: some people in hum_table_household_data but not hum_table_household_data_p2 and vice versa

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
dups_table = count_table[which(count_table > 1)] # 0 duplicates
# check for duplicates one more time
length(which(is.na(hum_monthly_data$monthly_unq_memID) == T)) # 0 missing
count_table = table(hum_monthly_data$monthly_unq_memID, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no duplicates

# check if a month and year combo occurs twice for any unq_memID
id_count_table = table(hum_monthly_data$unq_memID,hum_monthly_data$month_year_combo_monthly_data, useNA = "always")
dups_id = id_count_table[which(id_count_table > 1)] # looks like 0 households were visited more than once 
id_count_table = data.frame(id_count_table)
summary(id_count_table$Freq)

# check if there are duplicates in the human sick data set for sick_unq_memID
length(unique(hum_sick_data$sick_unq_memID)) # 908 unique 
length(which(is.na(hum_sick_data$sick_unq_memID) == T)) # 0 missing
count_table = table(hum_sick_data$sick_unq_memID, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates

# check if there are duplicates in unq_memID in the table household data set
count_table = table(hum_table_household_data$unq_memID, useNA = "always")
dups = count_table[which(count_table > 1)]
# looks like no duplicates in the unq_memID for this data set

# remove unq_memID "S12_1" from hum_monthly_data because not in hum_table_household_data and no longer in study
hum_monthly_data = hum_monthly_data[-which(hum_monthly_data$unq_memID == "S12_1"),]

# remove unq_memID "S12_1" from hum_sick_data because not in hum_table_household_data and no longer in study
hum_sick_data = hum_sick_data[-which(hum_sick_data$unq_memID == "S12_1"),]

# pull out what variables you need from the table household p1 and p2 data sets
colnames(hum_table_household_data)
colnames(hum_table_household_data_p2)
# need: age and unq_memID
hum_table_household_data = hum_table_household_data %>%
  select(age_y,age_m,age_type,unq_memID)
hum_table_household_data_p2 = hum_table_household_data_p2 %>%
  select(age,unq_memID)
# merge together by unq_memID and default to p1 age
hum_table_household_data_all = full_join(hum_table_household_data,hum_table_household_data_p2,by="unq_memID")
# make one age variable
age_all = ifelse(!(is.na(hum_table_household_data_all$age_y)),hum_table_household_data_all$age_y,
                 ifelse(!(is.na(hum_table_household_data_all$age_m)),paste0(hum_table_household_data_all$age_m,"mos"),hum_table_household_data_all$age))
table(age_all, useNA = "always")
# all that are 0 at the second annual visit did not age into the study because it only lasted up to 12 months after that and we don't know their months so remove
# all that have missing age (one participant), we don't know the age so remove
hum_table_household_data_all$age_all = age_all
hum_table_household_data_all = hum_table_household_data_all %>%
  filter(!(is.na(age_all))) %>%
  filter(age_all != "0") %>%
  select(unq_memID,age_all) %>%
  rename(age_all_baseline = age_all)
table(hum_table_household_data_all$age_all_baseline, useNA = "always")
length(unique(hum_table_household_data_all$unq_memID))
count_table = table(hum_table_household_data_all$unq_memID, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates

# merge together the monthly and table household data sets
str(hum_monthly_data$unq_memID)
str(hum_table_household_data_all$unq_memID)
hum_table_household_data_all$unq_memID = as.character(hum_table_household_data_all$unq_memID)
hum_monthly_merged_data = full_join(hum_monthly_data,hum_table_household_data_all, by = "unq_memID")
# check the merge
table(hum_monthly_merged_data$unq_memID,useNA = "always")
table(hum_monthly_data$unq_memID,useNA = "always")
# looks good

# check to see if there were any ids that didn't merge in one data set but did in the other
length(unique(hum_monthly_data$unq_memID)) # 272 unique
length(unique(hum_table_household_data_all$unq_memID)) # 279 unique
length(unique(hum_monthly_merged_data$unq_memID)) # 280 unique
# looks like we didn't get monthly follow-up for everyone in the study
# look at what ids differ
uniquelist_monthly = unique(hum_monthly_data$unq_memID)
uniquelist_household = unique(hum_table_household_data_all$unq_memID)
# how many are in both
length(intersect(uniquelist_monthly,uniquelist_household)) # 271 in both
setdiff(uniquelist_monthly,uniquelist_household) # all of these are within the table household data set except S04_9 which was a baby
setdiff(uniquelist_household,uniquelist_monthly)
# now remove observations that didn't have monthly visit or were S04_9
hum_monthly_merged_data = hum_monthly_merged_data[-which(hum_monthly_merged_data$unq_memID == "S04_9"),]
hum_monthly_merged_data = hum_monthly_merged_data[-which(hum_monthly_merged_data$unq_memID == "K13_6"),]
hum_monthly_merged_data = hum_monthly_merged_data[-which(hum_monthly_merged_data$unq_memID == "K07_12"),]
hum_monthly_merged_data = hum_monthly_merged_data[-which(hum_monthly_merged_data$unq_memID == "K09_11"),]
hum_monthly_merged_data = hum_monthly_merged_data[-which(hum_monthly_merged_data$unq_memID == "K12_6"),]
hum_monthly_merged_data = hum_monthly_merged_data[-which(hum_monthly_merged_data$unq_memID == "M07_4"),]
hum_monthly_merged_data = hum_monthly_merged_data[-which(hum_monthly_merged_data$unq_memID == "M07_5"),]
hum_monthly_merged_data = hum_monthly_merged_data[-which(hum_monthly_merged_data$unq_memID == "S06_8"),]
hum_monthly_merged_data = hum_monthly_merged_data[-which(hum_monthly_merged_data$unq_memID == "K09_15"),]
# look at what ids differ
uniquelist_monthly = unique(hum_monthly_merged_data$unq_memID)
uniquelist_household = unique(hum_table_household_data_all$unq_memID)
# how many are in both
length(intersect(uniquelist_monthly,uniquelist_household)) # 271 in both
setdiff(uniquelist_monthly,uniquelist_household) # all of these are within the table household data
setdiff(uniquelist_household,uniquelist_monthly)
length(which(is.na(hum_monthly_merged_data$age_all_baseline)))
# looks good

# look at columns that are the same between populations
colnames(hum_monthly_merged_data)

# now make sure all the village names correspond with the household IDs
# check first for monthly data
sample_hh_id = sapply(strsplit(hum_monthly_merged_data$HH_ID,""),head,1)
sample_village_id = sapply(strsplit(as.character(hum_monthly_merged_data$village_name_hum_monthly_data),""),head,1)
all.equal(sample_hh_id,sample_village_id)  
length(which(sample_hh_id != sample_village_id))
# looks like there are 0 instances where the village doesn't match the household ID

# see missingess for HH_ID and memID variables
length(which(is.na(hum_monthly_merged_data$HH_ID))) # 0 missing
length(which(is.na(hum_monthly_merged_data$memID))) # 0 missing

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
# K01_6
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K01_6"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K01_6" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K01_6"),]
table(subset_1$gender_hum_monthly_data)
# K02_5
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K02_5"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K02_5" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K02_5"),]
table(subset_1$gender_hum_monthly_data)
# K03_4
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K03_4"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K03_4" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K03_4"),]
table(subset_1$gender_hum_monthly_data)
# K05_4
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K05_4"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K05_4" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K05_4"),]
table(subset_1$gender_hum_monthly_data)
# K07_4
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K07_4"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "K07_4" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "K07_4"),]
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
# M09_4
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M09_4"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M09_4" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M09_4"),]
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
# M14_5
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M14_5"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M14_5" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M14_5"),]
table(subset_1$gender_hum_monthly_data)
# M15_1
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M15_1"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M15_1" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M15_1"),]
table(subset_1$gender_hum_monthly_data)
# M15_3
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M15_3"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M15_3" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M15_3"),]
table(subset_1$gender_hum_monthly_data)
# M15_4
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M15_4"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M15_4" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M15_4"),]
table(subset_1$gender_hum_monthly_data)
# M15_5
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M15_5"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M15_5" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M15_5"),]
table(subset_1$gender_hum_monthly_data)
# M16_4
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M16_4"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "M16_4" & hum_monthly_merged_data$gender_hum_monthly_data == "male")] = "female"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "M16_4"),]
table(subset_1$gender_hum_monthly_data)
# S01_12
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S01_12"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S01_12" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S01_12"),]
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
# S03_5
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S03_5"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S03_5" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S03_5"),]
table(subset_1$gender_hum_monthly_data)
# S05_5
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S05_5"),]
table(subset_1$gender_hum_monthly_data)
hum_monthly_merged_data$gender_hum_monthly_data[which(hum_monthly_merged_data$unq_memID == "S05_5" & hum_monthly_merged_data$gender_hum_monthly_data == "female")] = "male"
subset_1 = hum_monthly_merged_data[which(hum_monthly_merged_data$unq_memID == "S05_5"),]
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
  summarize(n=n(), n_age = n_distinct(age_all_baseline))
# look at those where there are two gender entries
twoentries = participant_data$unq_memID[participant_data$n_age>1]
# looks like none are miscoded which is good

# remove other codings of age for now
hum_monthly_merged_data$age_m_hum_monthly_data <- NULL
hum_monthly_merged_data$age_type <- NULL
hum_monthly_merged_data$age_y_hum_monthly_data <- NULL




#### ------ now merge in the sick data with the monthly and table data ------ ####

# look at the colnames in the human merged and human sick data
colnames(hum_sick_data)
colnames(hum_monthly_merged_data)

# merge in the sick data to the monthly data
human_merged_all_data = bind_rows(hum_monthly_merged_data,hum_sick_data)
# check the merge
colnames(human_merged_all_data)
nrow(hum_sick_data) # 907
nrow(hum_monthly_merged_data) # 4902
nrow(human_merged_all_data) # 5809 (907+4902=5809)
# looks good

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
sum(test_rs,na.rm=T) # 907 Rs in sick IDs which is good

# merge together the monthly_unq_memID and sick_unq_memID columns
length(which(is.na(human_merged_all_data$monthly_unq_memID))) # 907 - all the sick IDs
length(which(is.na(human_merged_all_data$sick_unq_memID))) # 4902 - all the monthly IDs
monthly_or_sick_unq_memID = ifelse(is.na(human_merged_all_data$sick_unq_memID),human_merged_all_data$monthly_unq_memID,human_merged_all_data$sick_unq_memID)
length(which(is.na(monthly_or_sick_unq_memID))) # no missing, which is good
# add to the data set and compare with the monthly and sick ID columns
human_merged_all_data$`Sample Name` = monthly_or_sick_unq_memID
comparison_df = data.frame(human_merged_all_data$`Sample Name`,human_merged_all_data$monthly_unq_memID, human_merged_all_data$sick_unq_memID)

# compare unq_memIDs in monthly and sick data sets and make sure all in the sick data set are found in monthly data set
monthly_ids = hum_monthly_merged_data$unq_memID
sick_ids = hum_sick_data$unq_memID
setdiff(sick_ids,monthly_ids) # all sick ids found in monthly data set except S04_9 (baby - remove)
setdiff(monthly_ids,sick_ids)
# looks good

# removed baby S04_9
human_merged_all_data = human_merged_all_data[-which(human_merged_all_data$unq_memID == "S04_9"),]
length(unique(human_merged_all_data$unq_memID))

# look one last time at number of participants
length(human_merged_all_data$unq_memID)
length(unique(human_merged_all_data$unq_memID)) # 271 participants

# note: haven't removed people with <2 months follow-up and those who didn't age in yet

# write out the merged human monthly, table, and sick data
write_csv(human_merged_all_data, "Desktop/phase2_hum_monthly_merged_with_table_and_sick_19NOV2019.csv")
write_rds(human_merged_all_data, "Desktop/phase2_hum_monthly_merged_with_table_and_sick_19NOV2019.RDS")

