# ----------------------------------------- #
#   Spat21 Data Set Cleaning - qPCR Data    #
#                Human Data                 #
#             January 14, 2019              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(stringr)


#### ---------- load in the qpcr data set ---------- ####

# read in the human DBS qpcr data set
qpcr_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/raw_files/qpcr_data/Mozzie DBS compiled detection results 04Jan2019.csv")

# look at a summary of the data set
summary(qpcr_data)
str(qpcr_data)


#### ------- clean the qpcr data set ---------- ####

# look at all the column names
colnames(qpcr_data)

# Sample Name
table(qpcr_data$`Sample Name`, useNA = "always")
str(qpcr_data$`Sample Name`)
# check for duplicate sample names
length(unique(qpcr_data$`Sample Name`)) # 2894 unique 
length(which(is.na(qpcr_data$`Sample Name`) == T)) # 0 missing
count_table = table(qpcr_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 33 duplicates
# pull out those that are duplicates
# look where these duplicates occurred
dup_data = qpcr_data[which(qpcr_data$`Sample Name` %in% names(dups_table)),]
# for duplicates on plate 1 that are also found on plate 2, will remove samples on plate 2
# K01-030817-1
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-030817-1" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-030817-2
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-030817-2" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-030817-3
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-030817-3" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-030817-4
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-030817-4" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-030817-5
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-030817-5" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-030817-6
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-030817-6" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-030817-7
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-030817-7" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-030817-8
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-030817-8" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-060717-1
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-060717-1" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-060717-2
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-060717-2" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-060717-3
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-060717-3" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-060717-4
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-060717-4" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-060717-5
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-060717-5" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-060717-6
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-060717-6" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-060717-7
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-060717-7" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-060717-8
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-060717-8" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# K01-130717-9
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K01-130717-9" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# R-030817-K01-2
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "R-030817-K01-2" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# R-080717-K01-3
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "R-080717-K01-3" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# R-080717-K01-5
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "R-080717-K01-5" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# R-090817-K01-1
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "R-090817-K01-1" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# R-110717-K01-9
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "R-110717-K01-9" & qpcr_data$`Experiment Name` == "Mozzie DBS B22-B23 Taqman duplex 12-22-18"),]
# R-210817-K01-5
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "R-210817-K01-5" & qpcr_data$`Experiment Name` == "Mozzie DBS B2-B3 Taqman duplex 12-17-18"),]
# recheck for duplicate sample names
length(unique(qpcr_data$`Sample Name`)) # 2894 unique 
length(which(is.na(qpcr_data$`Sample Name`) == T)) # 0 missing
count_table = table(qpcr_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 10 duplicates left
# the duplicates left are the ones I'm going to pull out and rerun the qpcr on because are duplicates on the same plate
# for now, remove these 10 duplicates from this data set because will have new qpcr data for them
# S12-240817-4
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "S12-240817-4"),]
# S02-211217-3
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "S02-211217-3"),]
# M09-130717-5
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "M09-130717-5"),]
# S09-161117-2
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "S09-161117-2"),]
# S09-161117-3
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "S09-161117-3"),]
# S09-161117-4
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "S09-161117-4"),]
# S09-161117-5
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "S09-161117-5"),]
# S09-161117-6
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "S09-161117-6"),]
# S09-161117-7
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "S09-161117-7"),]
# S09-161117-8
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "S09-161117-8"),]
# recheck for duplicate sample names
length(unique(qpcr_data$`Sample Name`)) # 2884 unique 
length(which(is.na(qpcr_data$`Sample Name`) == T)) # 0 missing
count_table = table(qpcr_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates left
# will need to add in the 10 duplicates you just deleted when have new qpcr results
# now clean up the lab id for Sample Name
table(qpcr_data$`Sample Name`, useNA = "always")
table(nchar(qpcr_data$`Sample Name`), useNA = "always")
length(which(is.na(qpcr_data$`Sample Name`))) # 0 missing IDs
# most sample names are 12 characters long but range from 10 to 15
# clean up the Sample Names
clean_sample_id = rep(NA,nrow(qpcr_data))
for (i in 1:nrow(qpcr_data)){
  if (qpcr_data$`Sample Name`[i] == "2-260618-M03-2" & !(is.na(qpcr_data$`Sample Name`[i]))){
    clean_sample_id[i] = "M03-260618-2"
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 10 & !(is.na(qpcr_data$`Sample Name`[i]))){
    clean_sample_id[i] = qpcr_data$`Sample Name`[i]
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 11 & !(is.na(qpcr_data$`Sample Name`[i]))){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[1],"-","0",parts[2],"-",parts[3]))
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 12 & !(is.na(qpcr_data$`Sample Name`[i]))){
    clean_sample_id[i] = toupper(qpcr_data$`Sample Name`[i])
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 13 & str_count(qpcr_data$`Sample Name`[i], "-") == 2 & !(is.na(qpcr_data$`Sample Name`[i]))){
    clean_sample_id[i] = toupper(qpcr_data$`Sample Name`[i])
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 14 & !(is.na(qpcr_data$`Sample Name`[i])) & qpcr_data$`Sample Name`[i] != "2-260618-M03-2"){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 15 & !(is.na(qpcr_data$`Sample Name`[i]))){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
  }
}
# check the recode
table(clean_sample_id, useNA = "always")
length(which(is.na(clean_sample_id))) # 1 missing
summary(nchar(clean_sample_id))
# add the clean Sample Name to the data set
qpcr_data$`Sample Name` = clean_sample_id
# there are a few ids that still need to be recoded
qpcr_data$`Sample Name`[qpcr_data$`Sample Name` == "418-270-M14-R"] = "M14-270418-6-R"
qpcr_data$`Sample Name`[qpcr_data$`Sample Name` == "K02-030817-08"] = "K02-030817-8"
qpcr_data$`Sample Name`[qpcr_data$`Sample Name` == "K02-030817-09"] = "K02-030817-9"
qpcr_data$`Sample Name`[qpcr_data$`Sample Name` == "K05-030817-06"] = "K05-030817-6"
qpcr_data$`Sample Name`[is.na(qpcr_data$`Sample Name`)] = "M01-051217-8-R"
# check the recode
table(qpcr_data$`Sample Name`, useNA = "always")
length(which(is.na(qpcr_data$`Sample Name`))) # 0 missing
summary(nchar(qpcr_data$`Sample Name`))
# looks good

# Experiment Name
table(qpcr_data$`Experiment Name`, useNA = "always")
str(qpcr_data$`Experiment Name`)
# looks good, clean

# Well Position
table(qpcr_data$`Well Position`, useNA = "always")
str(qpcr_data$`Well Position`)
# looks good, clean

# HbtubCT1 & HbtubCT2
table(qpcr_data$HbtubCT1, useNA = "always")
table(qpcr_data$HbtubCT2, useNA = "always")
# some changes need to be made, making below with the Pf CT values

# Pfr364CT1 & Pfr364CT2
table(qpcr_data$pfr364CT1, useNA = "always")
table(qpcr_data$pfr364CT2, useNA = "always")
# some changes need to be made, making below with the Hb CT values

# make sure the qpcr values are numeric and change "Undetermined" to NA to represent missing because undetectable
# make sure all the qpcr values with CT values ==0  are also changed to NA
# first check how many are 0, undetermined, and missing



