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

# merge together the monthly and table household data sets
hum_monthly_merged_data = left_join(hum_monthly_data,hum_table_household_data, by = "unq_memID")
# check the merge
table(hum_monthly_merged_data$unq_memID,useNA = "always")
table(hum_monthly_data$unq_memID,useNA = "always")
# looks good














