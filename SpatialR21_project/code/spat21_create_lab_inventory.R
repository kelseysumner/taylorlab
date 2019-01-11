# ----------------------------------------- #
#        Spat21 Data Set Cleaning           #
#         Creating lab inventory            #
#            January 9, 2019              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(stringr)


#### --------- read in lab inventories ----------------- ####
# read in the original master inventory
dbs_master <- read_csv("~/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/lab_inventories/csv files/Spatial R21 Master Inventory_DBS.csv")
mosquito_master <-  read_csv("~/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/lab_inventories/csv files/Spatial R21 Master Inventory_mosquito.csv")

# read in the dbs update
dbs_update <- read_csv("~/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/lab_inventories/csv files/SPAT_7B+DBS_Database.csv")

# read in the mosquito update
mosquito_update <- read_csv("~/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/lab_inventories/csv files/mosquito_Spatial R21_Inventory_20181130.csv")


#### --------- clean inventories ----------------- ####

## ------------ dbs_master inventory

# clean the dbs_master inventory
summary(dbs_master)

# Sample ID
table(dbs_master$`Sample ID`, useNA = "always")
table(nchar(dbs_master$`Sample ID`), useNA = "always")
length(which(is.na(dbs_master$`Sample ID`))) # 1 missing ID
nchar("3D7 culture")
nchar("M01-170817-1")
# most IDs are 12 characters long
# clean up the sample IDs
clean_sample_id = rep(NA,nrow(dbs_master))
for (i in 1:nrow(dbs_master)){
  if (dbs_master$`Sample ID`[i] == "3D7 culture" & !(is.na(dbs_master$`Sample ID`[i]))){
    clean_sample_id[i] = "3D7 CULTURE"
  }
  if (nchar(dbs_master$`Sample ID`[i]) == 11 & dbs_master$`Sample ID`[i] != "3D7 culture" & !(is.na(dbs_master$`Sample ID`[i]))){
    parts = strsplit(dbs_master$`Sample ID`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[1],"-","0",parts[2],"-",parts[3]))
  }
  if (nchar(dbs_master$`Sample ID`[i]) == 12 & !(is.na(dbs_master$`Sample ID`[i]))){
    clean_sample_id[i] = toupper(dbs_master$`Sample ID`[i])
  }
  if (nchar(dbs_master$`Sample ID`[i]) == 13 & str_count(dbs_master$`Sample ID`[i], "-") == 2 & !(is.na(dbs_master$`Sample ID`[i]))){
    clean_sample_id[i] = toupper(dbs_master$`Sample ID`[i])
  }
  if (nchar(dbs_master$`Sample ID`[i]) == 13 & str_count(dbs_master$`Sample ID`[i], "-") == 3 & !(is.na(dbs_master$`Sample ID`[i]))){
    parts = strsplit(dbs_master$`Sample ID`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[1],parts[2],"-",parts[3],"-",parts[4]))
  }
  if (nchar(dbs_master$`Sample ID`[i]) == 14 & !(is.na(dbs_master$`Sample ID`[i]))){
    parts = strsplit(dbs_master$`Sample ID`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
  }
  if (nchar(dbs_master$`Sample ID`[i]) == 15 & !(is.na(dbs_master$`Sample ID`[i]))){
    parts = strsplit(dbs_master$`Sample ID`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
  }
}
# check the recode
table(clean_sample_id, useNA = "always")
length(which(is.na(clean_sample_id))) # 1 missing
# looks good
# add the clean sample ID to the data set
dbs_master$`Sample ID` = clean_sample_id

# Shipment Date
table(dbs_master$`Shipment Date`, useNA = "always")
str(dbs_master$`Shipment Date`)
# looks good, clean

# Sample Received
table(dbs_master$`Sample Received`, useNA = "always")
str(dbs_master$`Sample Received`)
# make a factor
# 1 is yes, 0 no
# code as a factor
dbs_master$`Sample Received` = factor(dbs_master$`Sample Received`,levels = c(1,0), labels = c("yes", "no"))
table(dbs_master$`Sample Received`, useNA = "always")

# DBS Plate #
table(dbs_master$`DBS Plate #`, useNA = "always")
str(dbs_master$`DBS Plate #`)
# looks good, clean

# DBS Column #
table(dbs_master$`DBS Column #`, useNA = "always")
str(dbs_master$`DBS Column #`)
# looks good, clean

# DBS Row #
table(dbs_master$`DBS Row #`, useNA = "always")
str(dbs_master$`DBS Row #`)
# looks good, clean

# gDNA Extraction Date
table(dbs_master$`gDNA Extraction Date`, useNA = "always")
str(dbs_master$`gDNA Extraction Date`)
# put in date format
newdate = mdy(dbs_master$`gDNA Extraction Date`)
head(newdate)
head(dbs_master$`gDNA Extraction Date`)
dbs_master$`gDNA Extraction Date` = newdate
table(dbs_master$`gDNA Extraction Date`, useNA = "always")

# gDNA Plate #
table(dbs_master$`gDNA Plate #`, useNA = "always")
str(dbs_master$`gDNA Plate #`)
# looks good, clean

# gDNA Column #
table(dbs_master$`gDNA Column #`, useNA = "always")
str(dbs_master$`gDNA Column #`)
# looks good, clean

# gDNA Row #
table(dbs_master$`gDNA Row #`, useNA = "always")
str(dbs_master$`gDNA Row #`)
# looks good, clean

# Comment
table(dbs_master$Comment, useNA = "always")
str(dbs_master$Comment)
# keep in the data set


## --------- mosquito_master inventory

# look at an overall summary
summary(mosquito_master)

# Sample ID
table(mosquito_master$`Sample ID`, useNA = "always")
table(nchar(mosquito_master$`Sample ID`), useNA = "always")
length(which(is.na(mosquito_master$`Sample ID`))) # 0 missing IDs
# looks good, clean

# Shipment Date
table(mosquito_master$`Shipment Date`, useNA = "always")
table(nchar(mosquito_master$`Shipment Date`), useNA = "always")
# looks good, clean

# Sample Received
table(mosquito_master$`Sample Received`, useNA = "always")
str(mosquito_master$`Sample Received`)
# make a factor
# 1 is yes, 0 no
# code as a factor
mosquito_master$`Sample Received` = factor(mosquito_master$`Sample Received`,levels = c(1,0), labels = c("yes", "no"))
table(mosquito_master$`Sample Received`, useNA = "always")

# gDNA Extraction Date
table(mosquito_master$`gDNA Extraction Date`, useNA = "always")
str(mosquito_master$`gDNA Extraction Date`)
# put in date format
newdate = mdy(mosquito_master$`gDNA Extraction Date`)
head(newdate)
head(mosquito_master$`gDNA Extraction Date`)
mosquito_master$`gDNA Extraction Date` = newdate
table(mosquito_master$`gDNA Extraction Date`, useNA = "always")

# gDNA Plate #
table(mosquito_master$`gDNA Plate #`, useNA= "always")
str(mosquito_master$`gDNA Plate #`)
# looks good, clean

# gDNA Column #
table(mosquito_master$`gDNA Column #`, useNA= "always")
str(mosquito_master$`gDNA Column #`)
# looks good, clean

# gDNA Row #
table(mosquito_master$`gDNA Row #`, useNA= "always")
str(mosquito_master$`gDNA Row #`)
# looks good, clean

# Extracted
table(mosquito_master$Extracted, useNA= "always")
str(mosquito_master$Extracted)
# looks good, clean

# Comment
table(mosquito_master$Comment, useNA= "always")
str(mosquito_master$Comment)
# looks good, clean


## ------------ dbs_update inventory

# look at the overall data set summary
summary(dbs_update)

# ID Number
table(dbs_update$`ID Number`, useNA = "always")
table(nchar(dbs_update$`ID Number`), useNA = "always")
length(which(is.na(dbs_update$`ID Number`))) # 0 missing IDs
# most IDs are 12 characters long
# first rename ID Number to Sample ID
names(dbs_update)[names(dbs_update) == 'ID Number'] <- 'Sample ID'
# clean up the sample IDs
clean_sample_id = rep(NA,nrow(dbs_update))
for (i in 1:nrow(dbs_update)){
  if (dbs_update$`Sample ID`[i] == "2-260618-M03-2" & !(is.na(dbs_update$`Sample ID`[i]))){
    clean_sample_id[i] = "M03-260618-2"
  }
  if (nchar(dbs_update$`Sample ID`[i]) == 10 & !(is.na(dbs_update$`Sample ID`[i]))){
    clean_sample_id[i] = dbs_update$`Sample ID`[i]
  }
  if (dbs_update$`Sample ID`[i] == "3D7 culture" & !(is.na(dbs_update$`Sample ID`[i]))){
    clean_sample_id[i] = "3D7 CULTURE"
  }
  if (nchar(dbs_update$`Sample ID`[i]) == 11 & dbs_update$`Sample ID`[i] != "3D7 culture" & !(is.na(dbs_update$`Sample ID`[i]))){
    parts = strsplit(dbs_update$`Sample ID`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[1],"-","0",parts[2],"-",parts[3]))
  }
  if (nchar(dbs_update$`Sample ID`[i]) == 12 & !(is.na(dbs_update$`Sample ID`[i]))){
    clean_sample_id[i] = toupper(dbs_update$`Sample ID`[i])
  }
  if (nchar(dbs_update$`Sample ID`[i]) == 13 & str_count(dbs_update$`Sample ID`[i], "-") == 2 & !(is.na(dbs_update$`Sample ID`[i]))){
    clean_sample_id[i] = toupper(dbs_update$`Sample ID`[i])
  }
  if (nchar(dbs_update$`Sample ID`[i]) == 13 & str_count(dbs_update$`Sample ID`[i], "-") == 3 & !(is.na(dbs_update$`Sample ID`[i]))){
    parts = strsplit(dbs_update$`Sample ID`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[3],"-","0",parts[2],"-",parts[4],"-",parts[1]))
  }
  if (nchar(dbs_update$`Sample ID`[i]) == 14 & !(is.na(dbs_update$`Sample ID`[i]))){
    parts = strsplit(dbs_update$`Sample ID`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
  }
  if (nchar(dbs_update$`Sample ID`[i]) == 15 & !(is.na(dbs_update$`Sample ID`[i]))){
    parts = strsplit(dbs_update$`Sample ID`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
  }
}
# check the recode
table(clean_sample_id, useNA = "always")
length(which(is.na(clean_sample_id))) # 0 missing
# looks good
# add the clean sample ID to the data set
dbs_update$`Sample ID` = clean_sample_id

# Row
table(dbs_update$Row, useNA = "always")
length(which(is.na(dbs_update$Row))) # 0 missing
# looks good, clean

# Column
table(dbs_update$Column, useNA = "always")
length(which(is.na(dbs_update$Column))) # 0 missing

# Plate #
table(dbs_update$`Plate #`, useNA = "always")
length(which(is.na(dbs_update$`Plate #`))) # 0 missing


## ------------ mosquito_update inventory

# look at the overall data set summary
summary(mosquito_update)

# Sample ID
table(mosquito_update$`Sample ID`, useNA = "always")
table(nchar(mosquito_update$`Sample ID`), useNA = "always")
length(which(is.na(mosquito_update$`Sample ID`))) # 0 missing IDs
str(mosquito_update$`Sample ID`)
# add a space between all the ID numbers
new_clean_id = rep(NA,nrow(mosquito_update))
for (i in 1:nrow(mosquito_update)){
  new_name_head = strsplit(mosquito_update$`Sample ID`[i],"")[[1]]
  new_name_p1_head = paste(new_name_head[1:3], collapse="")
  new_name_p2_head = paste(new_name_head[4:9], collapse="")
  new_clean_id[i] = paste0(new_name_p1_head," ",new_name_p2_head)
}
# add the new ID to the data set
mosquito_update$`Sample ID` = new_clean_id
table(nchar(mosquito_update$`Sample ID`))
table(mosquito_update$`Sample ID`, useNA = "always")
length(which(is.na(mosquito_update$`Sample ID`))) # 0 missing

# gDNA Extraction Date
table(mosquito_update$`gDNA Extraction Date`, useNA = "always")
str(mosquito_update$`gDNA Extraction Date`)
# put in date format
newdate = mdy(mosquito_update$`gDNA Extraction Date`) # note: 7 failed to parse, there were all coded as N/A originally so are now correctly coded as missing
head(newdate)
head(mosquito_update$`gDNA Extraction Date`)
mosquito_update$`gDNA Extraction Date` = newdate
table(mosquito_update$`gDNA Extraction Date`, useNA = "always")

# gDNA Plate #
table(mosquito_update$`gDNA Plate #`, useNA= "always")
str(mosquito_update$`gDNA Plate #`)
# change the 7 N/A to NA
mosquito_update$`gDNA Plate #`[mosquito_update$`gDNA Plate #` == "N/A"] = NA
# check the recode
table(mosquito_update$`gDNA Plate #`, useNA= "always")
str(mosquito_update$`gDNA Plate #`)

# gDNA Column #
table(mosquito_update$`gDNA Column #`, useNA= "always")
str(mosquito_update$`gDNA Column #`)
# change the 7 N/A to NA
mosquito_update$`gDNA Column #`[mosquito_update$`gDNA Column #` == "N/A"] = NA
# check the recode
table(mosquito_update$`gDNA Column #`, useNA= "always")
str(mosquito_update$`gDNA Column #`)

# gDNA Row #
table(mosquito_update$`gDNA Row #`, useNA= "always")
str(mosquito_update$`gDNA Row #`)
# change the 7 N/A to NA
mosquito_update$`gDNA Row #`[mosquito_update$`gDNA Row #` == "N/A"] = NA
# check the recode
table(mosquito_update$`gDNA Row #`, useNA= "always")
str(mosquito_update$`gDNA Row #`)

# Extracted
table(mosquito_update$Extracted, useNA= "always")
str(mosquito_update$Extracted)
# looks good, clean

# Comment
table(mosquito_update$Comment, useNA= "always")
str(mosquito_update$Comment)
# looks good, clean


#### -------------- merge inventories -------------- ####

## ----- first the human dbs inventories

# look at summaries again
summary(dbs_master)
summary(dbs_update)

# rename some of the Sample IDs
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "418-270-M14-R"] = "M14-418270-R"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "K02-030817-08"] = "K02-030817-8"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "K02-030817-09"] = "K02-030817-9"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "K05-030817-06"] = "K05-030817-6"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "R05127-M01-8"] = "M01-005127-8-R"
dbs_master$`Sample ID`[dbs_master$`Sample ID` == "418-270-M14-R"] = "M14-418270-R"
dbs_master$`Sample ID`[dbs_master$`Sample ID` == "K02-030817-08"] = "K02-030817-8"
dbs_master$`Sample ID`[dbs_master$`Sample ID` == "K02-030817-09"] = "K02-030817-9"
dbs_master$`Sample ID`[dbs_master$`Sample ID` == "K05-030817-06"] = "K05-030817-6"
dbs_master$`Sample ID`[dbs_master$`Sample ID` == "R05127-M01-8"] = "M01-005127-8-R"
# haven't cleaned M15-311017-P-R because not sure what it should be changed to

# check for duplicates before the merge in the dbs_master data set
length(unique(dbs_master$`Sample ID`)) # 2917 unique 
length(which(is.na(dbs_master$`Sample ID`) == T)) # 1 missing
count_table = table(dbs_master$`Sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are 2 sample IDs that are duplicated

# have two duplicate IDs but looks like everything is missing for each of the rows
# K02-030817-8, K02-030817-9
dbs_master$`Sample ID`[dbs_master$`Sample ID` == "K02-030817-8" & is.na(dbs_master$`Sample Received`)] = NA
dbs_master$`Sample ID`[dbs_master$`Sample ID` == "K02-030817-9" & is.na(dbs_master$`Sample Received`)] = NA
# remove these rows with sample ID missing
dbs_master = dbs_master[-which(is.na(dbs_master$`Sample ID`)),]

# recheck for duplicates in dbs_master data set
length(unique(dbs_master$`Sample ID`)) # 2916 unique 
length(which(is.na(dbs_master$`Sample ID`) == T)) # 0 missing
count_table = table(dbs_master$`Sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are no more duplicates

# check for duplicates in the data set for the dbs_update
length(unique(dbs_update$`Sample ID`)) # 2453 unique 
length(which(is.na(dbs_update$`Sample ID`) == T)) # 0 missing
count_table = table(dbs_update$`Sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are no more duplicates

# recode the duplicates in dbs_update to be unique (add a A or B to the end of the ID number)
# M09-130717-5
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "M09-130717-5" & dbs_update$Row == "F"] = "M09-130717-5A"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "M09-130717-5" & dbs_update$Row == "G"] = "M09-130717-5B"
# S02-211217-3
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S02-211217-3" & dbs_update$Column == 3] = "S02-211217-3A"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S02-211217-3" & dbs_update$Column == 7] = "S02-211217-3B"
# S09-161117-2
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-2" & dbs_update$Column == 10] = "S09-161117-2A"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-2" & dbs_update$Column == 6] = "S09-161117-2B"
# S09-161117-3
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-3" & dbs_update$Column == 11] = "S09-161117-3A"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-3" & dbs_update$Column == 7] = "S09-161117-3B"
# S09-161117-4
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-4" & dbs_update$Column == 12] = "S09-161117-4A"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-4" & dbs_update$Column == 8] = "S09-161117-4B"
# S09-161117-5
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-5" & dbs_update$Column == 1] = "S09-161117-5A"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-5" & dbs_update$Column == 9] = "S09-161117-5B"
# S09-161117-6 
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-6" & dbs_update$Column == 2] = "S09-161117-6A"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-6" & dbs_update$Column == 11] = "S09-161117-6B"
# S09-161117-7
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-7" & dbs_update$Column == 3] = "S09-161117-7A"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-7" & dbs_update$Column == 10] = "S09-161117-7B"
# S09-161117-8
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-8" & dbs_update$Column == 4] = "S09-161117-8A"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S09-161117-8" & dbs_update$Column == 12] = "S09-161117-8B"
# S12-240817-4
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S12-240817-4" & dbs_update$Column == 12] = "S12-240817-4A"
dbs_update$`Sample ID`[dbs_update$`Sample ID` == "S12-240817-4" & dbs_update$Column == 1] = "S12-240817-4B"

# now recheck for duplicates
length(unique(dbs_update$`Sample ID`)) # 2463 unique 
length(which(is.na(dbs_update$`Sample ID`) == T)) # 0 missing
count_table = table(dbs_update$`Sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are no more duplicates

# do a full join by Sample ID
dbs_merged = full_join(dbs_master,dbs_update, by = "Sample ID")

# check for duplicates
length(unique(dbs_merged$`Sample ID`)) # 2951 unique 
length(which(is.na(dbs_merged$`Sample ID`) == T)) # 0 missing
count_table = table(dbs_merged$`Sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no duplicates

# those that were duplicates and had to be recoded, clean up in inventory
# M09-130717-5
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "M09-130717-5A"] = "yes"
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "M09-130717-5B"] = "yes"
dbs_merged = dbs_merged[-which(dbs_merged$`Sample ID` == "M09-130717-5"),]
# S02-211217-3
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S02-211217-3A"] = "yes"
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S02-211217-3B"] = "yes"
dbs_merged$`Shipment Date`[dbs_merged$`Sample ID` == "S02-211217-3A"] = "Apr-18"
dbs_merged$`Shipment Date`[dbs_merged$`Sample ID` == "S02-211217-3B"] = "Apr-18"
dbs_merged = dbs_merged[-which(dbs_merged$`Sample ID` == "S02-211217-3"),]
# S09-161117-2
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-2A"] = "yes"
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-2B"] = "yes"
dbs_merged = dbs_merged[-which(dbs_merged$`Sample ID` == "S09-161117-2"),]
# S09-161117-3
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-3A"] = "yes"
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-3B"] = "yes"
dbs_merged = dbs_merged[-which(dbs_merged$`Sample ID` == "S09-161117-3"),]
# S09-161117-4
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-4A"] = "yes"
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-4B"] = "yes"
dbs_merged = dbs_merged[-which(dbs_merged$`Sample ID` == "S09-161117-4"),]
# S09-161117-5
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-5A"] = "yes"
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-5B"] = "yes"
dbs_merged = dbs_merged[-which(dbs_merged$`Sample ID` == "S09-161117-5"),]
# S09-161117-6 
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-6A"] = "yes"
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-6B"] = "yes"
dbs_merged = dbs_merged[-which(dbs_merged$`Sample ID` == "S09-161117-6"),]
# S09-161117-7
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-7A"] = "yes"
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-7B"] = "yes"
dbs_merged = dbs_merged[-which(dbs_merged$`Sample ID` == "S09-161117-7"),]
# S09-161117-8
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-8A"] = "yes"
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S09-161117-8B"] = "yes"
dbs_merged = dbs_merged[-which(dbs_merged$`Sample ID` == "S09-161117-8"),]
# S12-240817-4
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S12-240817-4A"] = "yes"
dbs_merged$`Sample Received`[dbs_merged$`Sample ID` == "S12-240817-4B"] = "yes"
dbs_merged = dbs_merged[-which(dbs_merged$`Sample ID` == "S12-240817-4"),]

# last check for duplicates
length(unique(dbs_merged$`Sample ID`)) # 2941 unique 
length(which(is.na(dbs_merged$`Sample ID`) == T)) # 0 missing
count_table = table(dbs_merged$`Sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no duplicates

# rename the additional row, column, plate # columns with _addition
names(dbs_merged)[names(dbs_merged) == 'Row'] <- 'Row_addition'
names(dbs_merged)[names(dbs_merged) == 'Column'] <- 'Column_addition'
names(dbs_merged)[names(dbs_merged) == 'Plate #'] <- 'Plate_number_addition'

# make all column names lowercase and snake case
names(dbs_merged) = tolower(names(dbs_merged))
names(dbs_merged)[names(dbs_merged) == 'sample id'] <- 'sample_id'
names(dbs_merged)[names(dbs_merged) == 'shipment date'] <- 'shipment_date'
names(dbs_merged)[names(dbs_merged) == 'sample received'] <- 'sample_received'
names(dbs_merged)[names(dbs_merged) == 'dbs plate #'] <- 'dbs_plate_number'
names(dbs_merged)[names(dbs_merged) == 'dbs column #'] <- 'dbs_column_number'
names(dbs_merged)[names(dbs_merged) == 'dbs row #'] <- 'dbs_row_number'
names(dbs_merged)[names(dbs_merged) == 'gdna extraction date'] <- 'gdna_extraction_date'
names(dbs_merged)[names(dbs_merged) == 'gdna plate #'] <- 'gdna_plate_number'
names(dbs_merged)[names(dbs_merged) == 'gdna column #'] <- 'gdna_column_number'
names(dbs_merged)[names(dbs_merged) == 'gdna row #'] <- 'gdna_row_number'
names(dbs_merged)[names(dbs_merged) == 'comment'] <- 'comment'
names(dbs_merged)[names(dbs_merged) == 'row'] <- 'row_addition'
# check the change
names(dbs_merged)

# export the file
# write_csv(dbs_merged,"dbs_lab_inventory_final_9JAN2019.csv")


## ----- second the mosquito inventories

# look at summaries again
summary(mosquito_master)
summary(mosquito_update)

# check for duplicates before the merge in the mosquito_master data set
length(unique(mosquito_master$`Sample ID`)) # 2076 unique 
length(which(is.na(mosquito_master$`Sample ID`) == T)) # 0 missing
count_table = table(mosquito_master$`Sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are 14 sample IDs that are duplicated

# recode the duplicates to be unique 
# K05 A00029
mosquito_master$`Shipment Date`[mosquito_master$`Sample ID` == "K05 A00029" & is.na(mosquito_master$`Shipment Date`)] = "18-Apr"
mosquito_master$`Sample Received`[mosquito_master$`Sample ID` == "K05 A00029" & is.na(mosquito_master$`Sample Received`)] = "yes"
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "K05 A00029" & is.na(mosquito_master$`gDNA Plate #`)),]
# K05 H00029
mosquito_master$`Shipment Date`[mosquito_master$`Sample ID` == "K05 H00029" & is.na(mosquito_master$`Shipment Date`)] = "18-Apr"
mosquito_master$`Sample Received`[mosquito_master$`Sample ID` == "K05 H00029" & is.na(mosquito_master$`Sample Received`)] = "yes"
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "K05 H00029" & is.na(mosquito_master$`gDNA Plate #`)),]
# K09 A00010
mosquito_master$`Shipment Date`[mosquito_master$`Sample ID` == "K09 A00010" & is.na(mosquito_master$`Shipment Date`)] = "18-Apr"
mosquito_master$`Sample Received`[mosquito_master$`Sample ID` == "K09 A00010" & is.na(mosquito_master$`Sample Received`)] = "yes"
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "K09 A00010" & is.na(mosquito_master$`gDNA Plate #`)),]
# K10 A00007
mosquito_master$`Shipment Date`[mosquito_master$`Sample ID` == "K10 A00007" & is.na(mosquito_master$`Shipment Date`)] = "18-Apr"
mosquito_master$`Sample Received`[mosquito_master$`Sample ID` == "K10 A00007" & is.na(mosquito_master$`Sample Received`)] = "yes"
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "K10 A00007" & is.na(mosquito_master$`gDNA Plate #`)),]
# K10 A00008
mosquito_master$`Shipment Date`[mosquito_master$`Sample ID` == "K10 A00008" & is.na(mosquito_master$`Shipment Date`)] = "18-Apr"
mosquito_master$`Sample Received`[mosquito_master$`Sample ID` == "K10 A00008" & is.na(mosquito_master$`Sample Received`)] = "yes"
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "K10 A00008" & is.na(mosquito_master$`gDNA Plate #`)),]
# K10 H00008
mosquito_master$`Shipment Date`[mosquito_master$`Sample ID` == "K10 H00008" & is.na(mosquito_master$`Shipment Date`)] = "18-Apr"
mosquito_master$`Sample Received`[mosquito_master$`Sample ID` == "K10 H00008" & is.na(mosquito_master$`Sample Received`)] = "yes"
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "K10 H00008" & is.na(mosquito_master$`gDNA Plate #`)),]
# K13 A00020
mosquito_master$`Shipment Date`[mosquito_master$`Sample ID` == "K13 A00020" & is.na(mosquito_master$`Shipment Date`)] = "18-Apr"
mosquito_master$`Sample Received`[mosquito_master$`Sample ID` == "K13 A00020" & is.na(mosquito_master$`Sample Received`)] = "yes"
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "K13 A00020" & is.na(mosquito_master$`gDNA Plate #`)),]
# K13 H00020
mosquito_master$`Shipment Date`[mosquito_master$`Sample ID` == "K13 H00020" & is.na(mosquito_master$`Shipment Date`)] = "18-Apr"
mosquito_master$`Sample Received`[mosquito_master$`Sample ID` == "K13 H00020" & is.na(mosquito_master$`Sample Received`)] = "yes"
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "K13 H00020" & is.na(mosquito_master$`gDNA Plate #`)),]
# K14 A00038
mosquito_master$`Shipment Date`[mosquito_master$`Sample ID` == "K14 A00038" & is.na(mosquito_master$`Shipment Date`)] = "18-Apr"
mosquito_master$`Sample Received`[mosquito_master$`Sample ID` == "K14 A00038" & is.na(mosquito_master$`Sample Received`)] = "yes"
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "K14 A00038" & is.na(mosquito_master$`gDNA Plate #`)),]
# K14 A00040
mosquito_master$`Shipment Date`[mosquito_master$`Sample ID` == "K14 A00040" & is.na(mosquito_master$`Shipment Date`)] = "18-Apr"
mosquito_master$`Sample Received`[mosquito_master$`Sample ID` == "K14 A00040" & is.na(mosquito_master$`Sample Received`)] = "yes"
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "K14 A00040" & is.na(mosquito_master$`gDNA Plate #`)),]
# K14 H00038
mosquito_master$`Shipment Date`[mosquito_master$`Sample ID` == "K14 H00038" & is.na(mosquito_master$`Shipment Date`)] = "18-Apr"
mosquito_master$`Sample Received`[mosquito_master$`Sample ID` == "K14 H00038" & is.na(mosquito_master$`Sample Received`)] = "yes"
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "K14 H00038" & is.na(mosquito_master$`gDNA Plate #`)),]
# K14 H00039
mosquito_master$`Shipment Date`[mosquito_master$`Sample ID` == "K14 H00039" & is.na(mosquito_master$`Shipment Date`)] = "18-Apr"
mosquito_master$`Sample Received`[mosquito_master$`Sample ID` == "K14 H00039" & is.na(mosquito_master$`Sample Received`)] = "yes"
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "K14 H00039" & is.na(mosquito_master$`gDNA Plate #`)),]
# M06 H00048
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "M06 H00048" & is.na(mosquito_master$`gDNA Plate #`)),]
# S06 A00014
mosquito_master = mosquito_master[-which(mosquito_master$`Sample ID` == "S06 A00014" & is.na(mosquito_master$`gDNA Plate #`)),]

# check again to see if duplicates before the merge in the mosquito_master data set
length(unique(mosquito_master$`Sample ID`)) # 2076 unique 
length(which(is.na(mosquito_master$`Sample ID`) == T)) # 0 missing
count_table = table(mosquito_master$`Sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are no more duplicates

# check for duplicates in the data set for the mosquito_update
length(unique(mosquito_update$`Sample ID`)) # 940 unique 
length(which(is.na(mosquito_update$`Sample ID`) == T)) # 0 missing
count_table = table(mosquito_update$`Sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are no more duplicates

# do a full join by Sample ID
mosquito_merged = full_join(mosquito_master,mosquito_update, by = "Sample ID")

# check for duplicates
length(unique(mosquito_merged$`Sample ID`)) # 2977 unique 
length(which(is.na(mosquito_merged$`Sample ID`) == T)) # 0 missing
count_table = table(mosquito_merged$`Sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no duplicates

# move all the columns that are repeated to be in one column
new_mosquito_merged = c(rep(0,9))
for (i in 1:nrow(mosquito_merged)){
  if (is.na(mosquito_merged$`gDNA Extraction Date.x`[i]) & is.na(mosquito_merged$`gDNA Plate #.x`[i]) & is.na(mosquito_merged$`gDNA Column #.x`[i]) & is.na(mosquito_merged$`gDNA Row #.x`[i]) & is.na(mosquito_merged$Extracted.x[i]) & is.na(mosquito_merged$Comment.x[i])){
    new_mosquito_row = c(mosquito_merged[i,1:3],mosquito_merged[i,10:15])
  }
  if (is.na(mosquito_merged$`gDNA Extraction Date.y`[i]) & is.na(mosquito_merged$`gDNA Plate #.y`[i]) & is.na(mosquito_merged$`gDNA Column #.y`[i]) & is.na(mosquito_merged$`gDNA Row #.y`[i]) & is.na(mosquito_merged$Extracted.y[i]) & is.na(mosquito_merged$Comment.y[i])){
    new_mosquito_row = c(mosquito_merged[i,1:9])
  }
  new_mosquito_merged = rbind(new_mosquito_merged,new_mosquito_row)
}
# remove the first row
new_mosquito_merged = new_mosquito_merged[-1,]
# make it a data frame
new_mosquito_merged = as_data_frame(new_mosquito_merged)
str(new_mosquito_merged)
colnames(new_mosquito_merged)

# change the column names
colnames(new_mosquito_merged)
new_mosquito_merged = rename(new_mosquito_merged, "sample_id_mosquito" = "Sample ID", "shipment_date" = "Shipment Date",
                       "sample_received" = "Sample Received", "gdna_extraction_date" = "gDNA Extraction Date.x", 
                       "gdna_plate_number" = "gDNA Plate #.x", "gdna_column_number" = "gDNA Column #.x", 
                       "gdna_row_number" = "gDNA Row #.x", "extracted" = "Extracted.x", "comment" = "Comment.x")
colnames(new_mosquito_merged)

# make all columns a specific data type
new_mosquito_merged$sample_id_mosquito = as.character(new_mosquito_merged$sample_id_mosquito)
new_mosquito_merged$shipment_date = as.numeric(new_mosquito_merged$shipment_date)
new_mosquito_merged$sample_received = factor(new_mosquito_merged$sample_received, levels = c(1,0), labels = c("yes", "no"))
new_mosquito_merged$gdna_extraction_date = as.character(new_mosquito_merged$gdna_extraction_date)
new_mosquito_merged$gdna_plate_number = as.character(new_mosquito_merged$gdna_plate_number)
new_mosquito_merged$gdna_column_number = as.numeric(new_mosquito_merged$gdna_column_number)
new_mosquito_merged$gdna_row_number = as.character(new_mosquito_merged$gdna_row_number)
new_mosquito_merged$extracted = as.character(new_mosquito_merged$extracted)
new_mosquito_merged$comment = as.character(new_mosquito_merged$comment)

# change the date format
newdate = origin + days(new_mosquito_merged$gdna_extraction_date)
length(which(is.na(newdate))) # 30
length(which(is.na(mosquito_merged$`gDNA Extraction Date.x`) & is.na(mosquito_merged$`gDNA Extraction Date.y`))) # 30
new_mosquito_merged$gdna_extraction_date = newdate

# check the merge
length(which(is.na(new_mosquito_merged$gdna_plate_number))) # 30
length(which(is.na(mosquito_merged$`gDNA Plate #.x`) & is.na(mosquito_merged$`gDNA Plate #.y`))) # 30
length(which(is.na(new_mosquito_merged$gdna_column_number))) # 59
length(which(is.na(mosquito_merged$`gDNA Column #.x`) & is.na(mosquito_merged$`gDNA Column #.y`))) # 44
# note sure what caused this difference in the column numbers

# write out the file as a csv file
write_csv(new_mosquito_merged, "mosquito_lab_inventory_final_11JAN2019.csv")









