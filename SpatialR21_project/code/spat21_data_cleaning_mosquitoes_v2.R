# ----------------------------------------- #
#        Spat21 Data Set Cleaning           #
#              Mosquito Data                #
#            December 18, 2018              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


#### --------- read in mosquito data ----------------- ####

# read in the mosquito descriptive data sets
# read in the data set with all mosquito species
allspecies_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/raw data/MOZZIECollectionSummary_June2017_July2018.csv")
# read in the data set with only anopheles mosquitoes (Wendy's version that's already converted to long format)
# in stata format
anopheles_data_long = Individual_female_anoph_long
# this is the original version
anopheles_data_original = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/raw data/MOZZIEFemaleAnophele_Jun2017_Jul2018.csv")
# anopheles_data_original = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/raw data/old/MOZZIEFemaleAnophele_June2017_July2018.csv")

# read in the mosquito qpcr data sets
qpcr_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/raw data/Mozzie mosquito compiled detection results 18Dec2018.csv")

# look at summaries of all the data sets
summary(allspecies_data)
summary(anopheles_data_original)
summary(anopheles_data_long)
summary(qpcr_data)
str(allspecies_data)
str(anopheles_data)
str(qpcr_data)

# output a csv file of all the variable names
names1 = names(allspecies_data)
names2 = names(anopheles_data)
names3 = names(qpcr_data)
allnames = c(names1,names2,names3)
allnames = data.frame(allnames)
# write_csv(allnames,"spat21_data_mosquito_dictionary.csv")



## ------------ anopheles data

# take out the variables that aren't needed in the wide anopheles data set (anopheles_data_original)
vars_to_exclude = c("repeatinstrument","collectiontime","collectiondoneby","samplespreparedby",
                    "speciesiddoneby","comment1","comment2","comment3","comment4","comment5",
                    "comment6","comment7","comment8","comment9","comment10","comment11","comment12",
                    "comment13","comment14","comment15","comment16","formcheckedby","formcheckeddate",
                    "formenteredby","formenteredon","complete")
short_data = anopheles_data_original[,-which(colnames(anopheles_data_original) %in% vars_to_exclude)]

# switch from wide to long format using a form loop
new_long_data = c(rep(0,10))
for (i in 1:nrow(short_data)){
  new_row_1 = c(short_data[i,1:5],short_data[i,6:10])
  new_row_2 = c(short_data[i,1:5],short_data[i,11:15])
  new_row_3 = c(short_data[i,1:5],short_data[i,16:20])
  new_row_4 = c(short_data[i,1:5],short_data[i,21:25])
  new_row_5 = c(short_data[i,1:5],short_data[i,26:30])
  new_row_6 = c(short_data[i,1:5],short_data[i,31:35])
  new_row_7 = c(short_data[i,1:5],short_data[i,36:40])
  new_row_8 = c(short_data[i,1:5],short_data[i,41:45])
  new_row_9 = c(short_data[i,1:5],short_data[i,46:50])
  new_row_10 = c(short_data[i,1:5],short_data[i,51:55])
  new_row_11 = c(short_data[i,1:5],short_data[i,56:60])
  new_row_12 = c(short_data[i,1:5],short_data[i,61:65])
  new_row_13 = c(short_data[i,1:5],short_data[i,66:70])
  new_row_14 = c(short_data[i,1:5],short_data[i,71:75])
  new_row_15 = c(short_data[i,1:5],short_data[i,76:80])
  new_row_16 = c(short_data[i,1:5],short_data[i,81:85])
  new_long_data = rbind(new_long_data,new_row_1, new_row_2, new_row_3, new_row_4, new_row_5, new_row_6, new_row_7, new_row_8, 
                        new_row_9, new_row_10, new_row_11, new_row_12, new_row_13, new_row_14, new_row_15, new_row_16)
}
# remove the first row
new_long_data = new_long_data[-1,]
# make it a data frame
new_long_data = as_data_frame(new_long_data)
str(new_long_data)
colnames(new_long_data)
# remove rows with all missing observations for columns 6:10
test = new_long_data[-which(is.na(new_long_data$sampleidhead1) & is.na(new_long_data$sampleidhead1) & is.na(new_long_data$abdominalstatus1) & is.na(new_long_data$speciestype1) & is.na(new_long_data$specifyspecies1)),]
length(which(is.na(new_long_data$sampleidhead1) & is.na(new_long_data$sampleidabdomen1) & is.na(new_long_data$abdominalstatus1) & is.na(new_long_data$speciestype1) & is.na(new_long_data$specifyspecies1)))
# 9209 observations should have been removed from the original for loop output for having all missing
# looks like those were removed correctly
new_long_data = test

# change the column names
colnames(new_long_data)
new_long_data = rename(new_long_data, "HH_ID" = "householdid", "repeat_instance" = "repeatinstance",
                       "collection_date" = "collectiondate", "village" = "village", "total_num_mosq_in_hh" = "totalnumberofmosquitosinthehouse",
                       "sample_id_head" = "sampleidhead1", "sample_id_abdomen" = "sampleidabdomen1","abdominal_status" = "abdominalstatus1",
                       "species_type" = "speciestype1", "specify_species" = "specifyspecies1")
colnames(new_long_data)

# make all the columns a specific data type
new_long_data$HH_ID = as.character(new_long_data$HH_ID)
new_long_data$repeat_instance = as.numeric(new_long_data$repeat_instance)
new_long_data$collection_date = as.character(new_long_data$collection_date)
new_long_data$village = as.character(new_long_data$village)
new_long_data$total_num_mosq_in_hh = as.numeric(new_long_data$total_num_mosq_in_hh)
new_long_data$sample_id_head = as.character(new_long_data$sample_id_head)
new_long_data$sample_id_abdomen = as.character(new_long_data$sample_id_abdomen)
new_long_data$abdominal_status = as.character(new_long_data$abdominal_status)
new_long_data$species_type = as.character(new_long_data$species_type)
new_long_data$specify_species = as.character(new_long_data$specify_species)

# look at each variable in the long data set
colnames(new_long_data)

# HH_ID
table(new_long_data$HH_ID, useNA = "always")
str(new_long_data$HH_ID)
# looks good, clean

# repeat_instance
table(new_long_data$repeat_instance, useNA = "always")
str(new_long_data$repeat_instance)
# looks good, clean

# collection_date
table(new_long_data$collection_date, useNA = "always")
str(new_long_data$collection_date)
# change to date format
test_date = mdy(new_long_data$collection_date)
head(test_date)
head(new_long_data$collection_date)
tail(test_date)
tail(new_long_data$collection_date)
table(test_date, useNA = "always")
new_long_data$collection_date = test_date
str(new_long_data$collection_date)

# create week, month, and year variables from collection_date
# for week
collection_week = epiweek(new_long_data$collection_date)
head(collection_week)
head(new_long_data$collection_date)
new_long_data$collection_week = collection_week
# for month
collection_month = month(new_long_data$collection_date)
head(collection_month)
head(new_long_data$collection_date)
new_long_data$collection_month = collection_month
summary(new_long_data$collection_month)
table(new_long_data$collection_month, useNA = "always")
# create a factor 
new_long_data$collection_month = factor(new_long_data$collection_month, levels = c(1:12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
table(new_long_data$collection_month, useNA = "always")
# for year
collection_year = epiyear(new_long_data$collection_date)
head(collection_year)
head(new_long_data$collection_date)
new_long_data$collection_year = collection_year
summary(new_long_data$collection_year)
# create a variable that is month and year combined
collection_month_year_combo = paste0(collection_month,"-",collection_year)
head(collection_month_year_combo)
head(new_long_data$collection_date)
new_long_data$collection_month_year_combo = collection_month_year_combo
table(new_long_data$collection_month_year_combo, useNA = "always")

# village
table(new_long_data$village, useNA = "always")
str(new_long_data$village)
# make a factor
new_long_data$village = as.factor(new_long_data$village)
str(new_long_data$village)

# total_num_mosq_in_hh
table(new_long_data$total_num_mosq_in_hh, useNA = "always")
str(new_long_data$total_num_mosq_in_hh)
summary(new_long_data$total_num_mosq_in_hh)
# looks good, clean

# sample_id_head
table(new_long_data$sample_id_head, useNA = "always")
str(new_long_data$sample_id_head)
# check for duplicate sample names
length(unique(new_long_data$sample_id_head)) # 1494 unique 
length(which(is.na(new_long_data$sample_id_head) == T)) # 0 missing
count_table = table(new_long_data$sample_id_head, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there is one duplicate
# the two duplicates: M03 H00021
# M03 H00021 looks like it was just entered twice, will remove one row
# look where these duplicates occurred
dup_data = new_long_data[which(new_long_data$sample_id_head == "M03 H00021"),]
new_long_data = new_long_data[-which(new_long_data$sample_id_head == "M03 H00021" & new_long_data$repeat_instance == 9),]
# check to see if any duplicates are left
length(unique(new_long_data$sample_id_head)) # 1494 unique 
length(which(is.na(new_long_data$sample_id_head) == T)) # 0 missing
count_table = table(new_long_data$sample_id_head, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are no duplicates left
# clean up the sample names
new_sample_name = rep(NA,nrow(new_long_data))
for (i in 1:nrow(new_long_data)){
  if (nchar(new_long_data$sample_id_head[i]) == 9){
    new_name = strsplit(new_long_data$sample_id_head[i],"")[[1]]
    new_name_p1 = paste(new_name[1:3], collapse="")
    new_name_p2 = paste(new_name[4:9], collapse="")
    new_sample_name[i] = paste0(new_name_p1," ",new_name_p2)
  }
  if (nchar(new_long_data$sample_id_head[i]) == 10 & new_long_data$sample_id_head[i] != "K1 H 00027"){
    new_name = strsplit(new_long_data$sample_id_head[i]," ")[[1]]
    new_sample_name[i] = paste0(new_name[1]," ",new_name[2])
  }
  if (nchar(new_long_data$sample_id_head[i]) == 11){
    new_name = strsplit(new_long_data$sample_id_head[i]," ")[[1]]
    new_sample_name[i] = paste0(new_name[1]," ",new_name[2],new_name[3])
  }
  if (nchar(new_long_data$sample_id_head[i]) == 12 & new_long_data$sample_id_head[i] == "K14  H 00027"){
    new_sample_name[i] = "K14 H00027"
  }
}
# look at the new_sample_name
table(new_sample_name, useNA = "always")
length(which(is.na(new_sample_name))) # 0 missing
table(nchar(new_sample_name)) # all 10 characters long
# now add the new_sample name to the data set
new_long_data$sample_id_head = new_sample_name
head(new_long_data$sample_id_head)
str(new_long_data$sample_id_head)
summary(nchar(new_long_data$sample_id_head))
# keep as a character

# sample_id_abdomen
table(new_long_data$sample_id_abdomen, useNA = "always")
str(new_long_data$sample_id_abdomen)
# check for duplicate sample names
length(unique(new_long_data$sample_id_abdomen)) # 1494 unique
length(which(is.na(new_long_data$sample_id_abdomen) == T)) # 1 missing
count_table = table(new_long_data$sample_id_abdomen, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no duplicates
# missing value should be missing, so keep missing
# put in a placeholder for row 85 which is the missing adomen
new_long_data$sample_id_abdomen[new_long_data$sample_id_head == "S02 H00001" & new_long_data$repeat_instance == 6] = "PLA CEHOLD"
# clean up the sample names
new_sample_name = rep(NA,nrow(new_long_data))
for (i in 1:nrow(new_long_data)){
  if (nchar(new_long_data$sample_id_abdomen[i]) == 9 & !(is.na(new_long_data$sample_id_abdomen))){
    new_name = strsplit(new_long_data$sample_id_abdomen[i],"")[[1]]
    new_name_p1 = paste(new_name[1:3], collapse="")
    new_name_p2 = paste(new_name[4:9], collapse="")
    new_sample_name[i] = paste0(new_name_p1," ",new_name_p2)
  } 
  if (nchar(new_long_data$sample_id_abdomen[i]) == 10 & !(is.na(new_long_data$sample_id_abdomen))){
    new_name = strsplit(new_long_data$sample_id_abdomen[i]," ")[[1]]
    new_sample_name[i] = paste0(new_name[1]," ",new_name[2])
  } 
  if (nchar(new_long_data$sample_id_abdomen[i]) == 11 & !(is.na(new_long_data$sample_id_abdomen))){
    new_name = strsplit(new_long_data$sample_id_abdomen[i]," ")[[1]]
    new_sample_name[i] = paste0(new_name[1]," ",new_name[2],new_name[3])
  }
}
# look at the new_sample_name
table(new_sample_name, useNA = "always")
length(which(is.na(new_sample_name))) # 0 missing (because of placeholder at row 85)
table(nchar(new_sample_name)) # all 10 characters long
# now add the new_sample name to the data set
new_long_data$sample_id_abdomen = new_sample_name
head(new_long_data$sample_id_abdomen)
str(new_long_data$sample_id_abdomen)
summary(nchar(new_long_data$sample_id_abdomen))
# keep as a character

# abdominal_status and species_type
table(new_long_data$abdominal_status, useNA = "always")
str(new_long_data$abdominal_status)
# looks like some species info in this column
status_testdata = new_long_data[which(new_long_data$abdominal_status == "An. funestus" | new_long_data$abdominal_status == "An. gambiae"),]
# looks like the abdominal status and species information were switched for some cases
# fix these columns
# abdominal_status
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00031" & new_long_data$repeat_instance == 4] = "Gravid"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M14 H00018" & new_long_data$repeat_instance == 3] = "Blood Fed"
new_long_data$abdominal_status[new_long_data$sample_id_head == "K01 H00026" & new_long_data$repeat_instance == 3] = "Gravid"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00047" & new_long_data$repeat_instance == 5] = "Blood Fed"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M09 H00038" & new_long_data$repeat_instance == 4] = "Blood Fed"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M14 H00031" & new_long_data$repeat_instance == 4] = "Unfed"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00062" & new_long_data$repeat_instance == 6] = "Half Gravid"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00092" & new_long_data$repeat_instance == 9] = "Blood Fed"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00109" & new_long_data$repeat_instance == 11] = "Half Gravid"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00128" & new_long_data$repeat_instance == 14] = "Half Gravid"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M09 H00103" & new_long_data$repeat_instance == 26] = "Gravid"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00185" & new_long_data$repeat_instance == 33] = "Unfed"
# species_type
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00031" & new_long_data$repeat_instance == 4] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M14 H00018" & new_long_data$repeat_instance == 3] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "K01 H00026" & new_long_data$repeat_instance == 3] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00047" & new_long_data$repeat_instance == 5] = "An. funestus"
new_long_data$species_type[new_long_data$sample_id_head == "M09 H00038" & new_long_data$repeat_instance == 4] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M14 H00031" & new_long_data$repeat_instance == 4] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00062" & new_long_data$repeat_instance == 6] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00092" & new_long_data$repeat_instance == 9] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00109" & new_long_data$repeat_instance == 11] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00128" & new_long_data$repeat_instance == 14] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M09 A00103" & new_long_data$repeat_instance == 26] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00185" & new_long_data$repeat_instance == 33] = "An. gambiae"
# look at tables of both sepcies_type and abdominal_status
table(new_long_data$abdominal_status, useNA = "always") # abdominal status looks good
table(new_long_data$species_type, useNA = "always") # species type still needs to be cleaned up some
# check these entries
species_testdata = new_long_data[which(new_long_data$species_type == "Blood Fed" | new_long_data$species_type == "Gravid" | new_long_data$species_type == "Half Gravid"),]
# looks like abdominal status was double entered into both the abdominal_status and species_type columns for three mosquitoes
# defaulted to what was in the abdominal status column and changed those entries in the species_type to missing
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00011" & new_long_data$repeat_instance == 1] = NA
new_long_data$species_type[new_long_data$sample_id_head == "M09 H00018" & new_long_data$repeat_instance == 2] = NA
new_long_data$species_type[new_long_data$sample_id_head == "M09 H00103" & new_long_data$repeat_instance == 26] = NA
# check species_type again
table(new_long_data$species_type, useNA = "always") # looks good
# make abdominal_status a factor
new_long_data$abdominal_status = as.factor(new_long_data$abdominal_status)
str(new_long_data$abdominal_status)
table(new_long_data$abdominal_status, useNA = "always")
# specify_species
table(new_long_data$specify_species, useNA = "always")
# the 13 "Other, Specify" species in species_type wre all An. Pretoriensis
# make these "Other, Specify" entries for species_type An. pretoriensis
new_long_data$species_type[new_long_data$species_type == "Other, Specify"] = "An. pretoriensis"
table(new_long_data$species_type, useNA = "always")
# make species_type a factor
new_long_data$species_type = as.factor(new_long_data$species_type)
str(new_long_data$species_type)
table(new_long_data$species_type, useNA = "always")

# remove specify_species column because no longer needed
table(new_long_data$specify_species, useNA = "always")
new_long_data$specify_species <- NULL

# remove collection month and collection year columns
new_long_data$collection_month <- NULL
new_long_data$collection_year <- NULL

# check that all the head and sample IDs are the same and create sample_id_mosquito variable
split_check_head = rep(NA,nrow(new_long_data))
split_check_abdomen = rep(NA,nrow(new_long_data))
for (i in 1:nrow(new_long_data)){
  new_name_head = strsplit(new_long_data$sample_id_head[i],"")[[1]]
  new_name_p1_head = paste(new_name_head[1:3], collapse="")
  new_name_p2_head = paste(new_name_head[6:10], collapse="")
  split_check_head[i] = paste0(new_name_p1_head," ",new_name_p2_head)
  
  new_name_abdomen = strsplit(new_long_data$sample_id_abdomen[i],"")[[1]]
  new_name_p1_abdomen = paste(new_name_abdomen[1:3], collapse="")
  new_name_p2_abdomen = paste(new_name_abdomen[6:10], collapse="")
  split_check_abdomen[i] = paste0(new_name_p1_abdomen," ",new_name_p2_abdomen)
}
table(split_check_head, useNA = "always")
table(split_check_abdomen, useNA = "always")
length(which(is.na(split_check_head))) # no missing, good
length(which(is.na(split_check_abdomen))) # no missing, good
# check if the two vectors are the same
all.equal(split_check_head, split_check_abdomen)
length(which(split_check_head == split_check_abdomen))
# looks like one row is not exactly the same (and there's the placeholder row - 86)
# examine this row
which(split_check_head != split_check_abdomen)
split_check_head[1116]
split_check_abdomen[1116]
# looks like M09 A0097 should be recoded to M09 A00097
new_long_data$sample_id_abdomen[new_long_data$sample_id_abdomen == "M09  A0097" & new_long_data$repeat_instance == 26] = "M09 A00097"
new_long_data$sample_id_abdomen[1116]
# recheck that the head and abdomen vectors are the same
split_check_head = rep(NA,nrow(new_long_data))
split_check_abdomen = rep(NA,nrow(new_long_data))
for (i in 1:nrow(new_long_data)){
  new_name_head = strsplit(new_long_data$sample_id_head[i],"")[[1]]
  new_name_p1_head = paste(new_name_head[1:3], collapse="")
  new_name_p2_head = paste(new_name_head[6:10], collapse="")
  split_check_head[i] = paste0(new_name_p1_head," ",new_name_p2_head)
  
  new_name_abdomen = strsplit(new_long_data$sample_id_abdomen[i],"")[[1]]
  new_name_p1_abdomen = paste(new_name_abdomen[1:3], collapse="")
  new_name_p2_abdomen = paste(new_name_abdomen[6:10], collapse="")
  split_check_abdomen[i] = paste0(new_name_p1_abdomen," ",new_name_p2_abdomen)
}
table(split_check_head, useNA = "always")
table(split_check_abdomen, useNA = "always")
length(which(is.na(split_check_head))) # no missing, good
length(which(is.na(split_check_abdomen))) # no missing, good
# check if the two vectors are the same
all.equal(split_check_head, split_check_abdomen)
length(which(split_check_head == split_check_abdomen))
which(split_check_head != split_check_abdomen) # good, just place holder row that is still different
# the two vectors are the same now so that's all good
# add the new variable created to the data set as sample_id_mosquito
new_long_data$sample_id_mosquito = split_check_head

# remove the place holder row for sample_id_abdomen (row 86) (change back to missing)
new_long_data$sample_id_abdomen[new_long_data$sample_id_head == "S02 H00001" & new_long_data$repeat_instance == 6] = NA
length(which(is.na(new_long_data$sample_id_abdomen))) # 1 missing, correct
length(which(is.na(new_long_data$sample_id_head))) # 0 missing, correct

# check one last time for duplicates sample IDs
# look for duplicates in sample_id_head
length(unique(new_long_data$sample_id_head)) # 1494 unique 
length(which(is.na(new_long_data$sample_id_head) == T)) # 0 missing
count_table = table(new_long_data$sample_id_head, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are no duplicates left
# look for duplicates in sample_id_abdomen
length(unique(new_long_data$sample_id_abdomen)) # 1494 unique 
length(which(is.na(new_long_data$sample_id_abdomen) == T)) # 1 missing
count_table = table(new_long_data$sample_id_abdomen, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are no duplicates left
# check if sample_id_mosquito ever occurred more than 2 times
count_table = table(new_long_data$sample_id_mosquito, useNA = "always")
greater_dups_table = count_table[which(count_table > 2)] # none > 2 so good

# check that the HH ID matched the sample ID
sample_hh_id = sapply(strsplit(new_long_data$sample_id_mosquito," "),head,1)
all.equal(new_long_data$HH_ID, sample_hh_id) 
length(which(new_long_data$HH_ID != sample_hh_id))
# looks like there are 22 instances of mismatches
# when there are mismatches, go back to the original data set and see what happened
mismatched_hhs = new_long_data[which(new_long_data$HH_ID != sample_hh_id),]
# make the changes in the data set
# M16 H00035 and M16 A00035 - change household ID manually to M16
new_long_data$HH_ID[new_long_data$sample_id_head == "M16 H00035" & new_long_data$repeat_instance == 22] = "M16"
# M15 H00016 - changed household ID to M15
new_long_data$HH_ID[new_long_data$sample_id_head == "M15 H00016" & new_long_data$repeat_instance == 3] = "M15"
# S02 H00005 - changed household ID to S02
new_long_data$HH_ID[new_long_data$sample_id_head == "S02 H00005" & new_long_data$repeat_instance == 9] = "S02"
# S02 H00006 - changed household ID to S02
new_long_data$HH_ID[new_long_data$sample_id_head == "S02 H00006" & new_long_data$repeat_instance == 9] = "S02"
# S02 H00007 - changed household ID to S02
new_long_data$HH_ID[new_long_data$sample_id_head == "S02 H00007" & new_long_data$repeat_instance == 9] = "S02"
# S02 H00008 - changed household ID to S02
new_long_data$HH_ID[new_long_data$sample_id_head == "S02 H00008" & new_long_data$repeat_instance == 9] = "S02"
# S12 H00005 - changed household ID to S12
new_long_data$HH_ID[new_long_data$sample_id_head == "S12 H00005" & new_long_data$repeat_instance == 2] = "S12"
# S08 H00004 - changed household ID to S08
new_long_data$HH_ID[new_long_data$sample_id_head == "S08 H00004" & new_long_data$repeat_instance == 4] = "S08"
# K07 H00010 - changed household ID to K07
new_long_data$HH_ID[new_long_data$sample_id_head == "K07 H00010" & new_long_data$repeat_instance == 4] = "K07"
# K05 H00013 - changed household ID to K05
new_long_data$HH_ID[new_long_data$sample_id_head == "K05 H00013" & new_long_data$repeat_instance == 3] = "K05"
# M12 H00002 - changed household ID to M12
new_long_data$HH_ID[new_long_data$sample_id_head == "M12 H00002" & new_long_data$repeat_instance == 5] = "M12"
# M12 H00003 - changed household ID to M12
new_long_data$HH_ID[new_long_data$sample_id_head == "M12 H00003" & new_long_data$repeat_instance == 5] = "M12"
# M12 H00004 - changed household ID to M12
new_long_data$HH_ID[new_long_data$sample_id_head == "M12 H00004" & new_long_data$repeat_instance == 5] = "M12"
# M12 H00005 - changed household ID to M12
new_long_data$HH_ID[new_long_data$sample_id_head == "M12 H00005" & new_long_data$repeat_instance == 5] = "M12"
# M05 H00023 - changed household ID to M05
new_long_data$HH_ID[new_long_data$sample_id_head == "M05 H00023" & new_long_data$repeat_instance == 23] = "M05"
# S06 H00015 - changed household ID to S06
new_long_data$HH_ID[new_long_data$sample_id_head == "S06 H00015" & new_long_data$repeat_instance == 9] = "S06"
# S05 H00014 - changed household ID to S05
new_long_data$HH_ID[new_long_data$sample_id_head == "S05 H00014" & new_long_data$repeat_instance == 10] = "S05"
# S05 H00014 - changed household ID to S05
new_long_data$HH_ID[new_long_data$sample_id_head == "S05 H00014" & new_long_data$repeat_instance == 10] = "S05"
# K05 H00035 - changed household ID to K05
new_long_data$HH_ID[new_long_data$sample_id_head == "K05 H00035" & new_long_data$repeat_instance == 7] = "K05"
# M16 H00033 - changed household ID to M16
new_long_data$HH_ID[new_long_data$sample_id_head == "M16 H00033" & new_long_data$repeat_instance == 22] = "M16"
# M16 H00034 - changed household ID to M16
new_long_data$HH_ID[new_long_data$sample_id_head == "M16 H00034" & new_long_data$repeat_instance == 22] = "M16"
# M16 H00035 - changed household ID to M16
new_long_data$HH_ID[new_long_data$sample_id_head == "M16 H00035" & new_long_data$repeat_instance == 22] = "M16"
# M16 H00036 - changed household ID to M16
new_long_data$HH_ID[new_long_data$sample_id_head == "M16 H00036" & new_long_data$repeat_instance == 22] = "M16"
# M05 H00040 - changed household ID to M05
new_long_data$HH_ID[new_long_data$sample_id_head == "M05 H00040" & new_long_data$repeat_instance == 20] = "M05"
# recheck that the HH ID matched the sample ID
sample_hh_id = sapply(strsplit(new_long_data$sample_id_mosquito," "),head,1)
all.equal(new_long_data$HH_ID, sample_hh_id) 
length(which(new_long_data$HH_ID != sample_hh_id))
# they all look like they've been successfully changed

# now make sure all the village names correspond with the household IDs
sample_hh_id = sapply(strsplit(new_long_data$sample_id_mosquito,""),head,1)
sample_village_id = sapply(strsplit(as.character(new_long_data$village),""),head,1)
all.equal(sample_hh_id,sample_village_id)  
length(which(sample_hh_id != sample_village_id))
# looks like there are 7 instances where the village doesn't match the hosuehold ID
mismatched_villages = new_long_data[which(sample_hh_id != sample_village_id),]
# M04 H00015 - changed village to Maruti
new_long_data$village[new_long_data$sample_id_head == "M04 H00015" & new_long_data$repeat_instance == 5] = "Maruti"
# S05 H00011 - changed village to Sitabicha
new_long_data$village[new_long_data$sample_id_head == "S05 H00011" & new_long_data$repeat_instance == 7] = "Sitabicha"
# S08 H00009 - changed village to Sitabicha
new_long_data$village[new_long_data$sample_id_head == "S08 H00009" & new_long_data$repeat_instance == 7] = "Sitabicha"
# S08 H00010 - changed village to Sitabicha
new_long_data$village[new_long_data$sample_id_head == "S08 H00010" & new_long_data$repeat_instance == 7] = "Sitabicha"
# K07 H00047 - changed village to Kinesamo
new_long_data$village[new_long_data$sample_id_head == "K07 H00047" & new_long_data$repeat_instance == 18] = "Kinesamo"
# K07 H00048 - changed village to Kinesamo
new_long_data$village[new_long_data$sample_id_head == "K07 H00048" & new_long_data$repeat_instance == 18] = "Kinesamo"
# K07 H00049 - changed village to Kinesamo
new_long_data$village[new_long_data$sample_id_head == "K07 H00049" & new_long_data$repeat_instance == 18] = "Kinesamo"
# now recheck to see if all villages equal the hh ids now
sample_hh_id = sapply(strsplit(new_long_data$sample_id_mosquito,""),head,1)
sample_village_id = sapply(strsplit(as.character(new_long_data$village),""),head,1)
all.equal(sample_hh_id,sample_village_id)  
length(which(sample_hh_id != sample_village_id))
# looks like they do, looks good, clean

# save the cleaned anopheles mosquito descriptive data set as a csv file and an rds file
write_csv(new_long_data,"spat21_mosquito_anopheles_descriptive_long_data_4JAN2019_v2.csv")
write_rds(new_long_data,"spat21_mosquito_anopheles_descriptive_long_data_4JAN2019_v2.RDS")









