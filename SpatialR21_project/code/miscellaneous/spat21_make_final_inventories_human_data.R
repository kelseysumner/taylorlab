# ----------------------------------------- #
#           Make Final Inventories          #
#                  Human Data               #
#            Mozzie Phase 1 Data            #
#              July 30, 2019                #
#                K. Sumner                  #
# ----------------------------------------- #


#### --------- load packages ----------------- ####
library(tidyverse)



#### -------- load in the data sets ---------- ####

# read in the original inventories
# read in the human dbs inventory
human_inventory_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/lab_inventories/clean files/dbs_lab_inventory_final_with_re-extraction_29APR2019.csv")

# read in the final merged data sets
# read in the human data set
human_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_16JUL2019.rds")


#### -------- HUMAN DATA: merge in the information with the inventory ----------- ####

# select variables of interest for human data
colnames(human_data)
human_data = human_data %>%
  select(sample_id_date,sample_name_dbs,pf_pcr_infection_status,pfr364Q_std_combined,village_name,HH_ID)

# recode some of the human inventory entries
human_inventory_data = rename(human_inventory_data,"sample_name_dbs"="sample_id")
# fix typos
# add an "R" to the end of M03-0618-2-R for the one with M03-260618-2 because was actually a sick visit for the qpcr data
human_inventory_data$sample_name_dbs[human_inventory_data$sample_name_dbs == "M03-260618-2"] = "M03-260618-2-R" 
# change M16-270618-P-R to M16-270618-4-R in the qpcr data
human_inventory_data$sample_name_dbs[human_inventory_data$sample_name_dbs == "M16-270618-P-R"] = "M16-270618-4-R" 
# change M15-311017-P-R to M15-311017-6-R in the qpcr data
human_inventory_data$sample_name_dbs[human_inventory_data$sample_name_dbs == "M15-311017-P-R"] = "M15-311017-6-R" 
# change K07-030817-08 to K07-030817-8 in the qpcr data
human_inventory_data$sample_name_dbs[human_inventory_data$sample_name_dbs == "K07-030817-08"] = "K07-030817-8" 
# change K07-030817-09 to K07-030817-9 in the qpcr data
human_inventory_data$sample_name_dbs[human_inventory_data$sample_name_dbs == "K07-030817-09"] = "K07-030817-9" 
# remove ID because incorrect
human_inventory_data = human_inventory_data[-which(human_inventory_data$sample_name_dbs == "S09-071717-9"),]

# check for duplicates in the inventory
length(unique(human_inventory_data$sample_name_dbs)) # 2925 unique 
length(which(is.na(human_inventory_data$sample_name_dbs) == T)) # 0 missing
count_table = table(human_inventory_data$sample_name_dbs, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 26 duplicates
length(dups_table)
dups_table = data.frame(dups_table)

# remove the rows where the inventory is a duplicate and plate is 2
small_data = human_inventory_data[which(human_inventory_data$sample_name_dbs %in% dups_table$Var1 & human_inventory_data$sample_name_dbs != "3D7 CULTURE"),]

# only keep plate 1
human_inventory_data = human_inventory_data[-which(human_inventory_data$sample_name_dbs %in% dups_table$Var1 & human_inventory_data$sample_name_dbs != "3D7 CULTURE" & human_inventory_data$dbs_plate_number == "Spat 2B-punches"),]
human_inventory_data = human_inventory_data[-which(human_inventory_data$sample_name_dbs == "K01-110717-9-R" & is.na(human_inventory_data$gdna_plate_number)),]
human_inventory_data = human_inventory_data[-which(human_inventory_data$sample_name_dbs == "K07-030817-8" & human_inventory_data$sample_received=="yes"),]
human_inventory_data = human_inventory_data[-which(human_inventory_data$sample_name_dbs == "K07-030817-9" & human_inventory_data$sample_received=="yes"),]

# check for duplicates in human inventory data
length(unique(human_inventory_data$sample_name_dbs)) # 2925 unique 
length(which(is.na(human_inventory_data$sample_name_dbs) == T)) # 0 missing
count_table = table(human_inventory_data$sample_name_dbs, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 1 duplicates
length(dups_table)
dups_table

# check for duplicates in human dbs data
length(unique(human_data$sample_name_dbs)) # 2813 unique 
length(which(is.na(human_data$sample_name_dbs) == T)) # 0 missing
count_table = table(human_data$sample_name_dbs, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 1 duplicates
length(dups_table)
dups_table

# remove the human dbs duplicate
human_data = human_data[-which(human_data$sample_name_dbs=="K05-021117-4-R" & human_data$sample_id_date == "2017-11-03"),]

# merge together the human data with human inventory
human_merged_data = full_join(human_inventory_data,human_data,by="sample_name_dbs")

# check the human data merge
setdiff(human_data$sample_name_dbs,human_inventory_data$sample_name_dbs)
setdiff(human_inventory_data$sample_name_dbs,human_data$sample_name_dbs)
length(intersect(human_inventory_data$sample_name_dbs,human_data$sample_name_dbs))

# write some code to clean the files yourself
# now loop through each row and move over those two columns
# check colnames
colnames(human_merged_data)
# change column order
human_merged_data = human_merged_data[,c(1,15,18,19,2,3,7,16,11,4,5,6,8,9,10,14,13,12)]
# remove columns 13-15
human_merged_data = human_merged_data[,-c(13,14,15)]
colnames(human_merged_data)
# look at input
length(which(is.na(human_merged_data$dbs_plate_number))) # 2446 missing
length(which(is.na(human_merged_data$plate_number_addition))) # 485 missing

# start for loop to combine qpcr results
for (i in 1:nrow(human_merged_data)){
  if (is.na(human_merged_data[i,10])){
    for (k in 1:3){   # this is for all data that is present in .y files but not in .x
      startpoint = 9 + k
      human_merged_data[i,startpoint] = human_merged_data[i,startpoint+3]
      human_merged_data[i,startpoint+3] <- NA
    }
  } else if (is.na(human_merged_data[i,13])){
    for (k in 1:3){ # this is for all data that is present in .x files but not in .y -> just make it NULL
      startpoint = 9 + k
      human_merged_data[i,startpoint+3] <- NA
    }
  } else {
    for (k in 1:3){ # both data sets are missing -> just make it NULL at .y location
      startpoint = 9 + k
      human_merged_data[i,startpoint+3] <- NA
    }
  } 
}
# check the output
length(which(is.na(human_merged_data$dbs_plate_number))) # 14 missing
length(which(is.na(human_merged_data$plate_number_addition))) # 3034 missing
# looks like it worked correctly

# change the data frame name for cleaning
final_data = human_merged_data

# clean up the column names
colnames(final_data)
final_data$plate_number_addition <- NULL
final_data$column_addition <- NULL
final_data$row_addition <- NULL

# change the dbs_plate_number entries
table(final_data$dbs_plate_number)
final_data$dbs_plate_number[which(final_data$dbs_plate_number == "Spat 2B-punches")] = 2
final_data$dbs_plate_number[which(final_data$dbs_plate_number == "Spat 4B-punches")] = 4
final_data$dbs_plate_number[which(final_data$dbs_plate_number == "Spat 3B-punches")] = 3
final_data$dbs_plate_number[which(final_data$dbs_plate_number == "Spat 5B-punches")] = 5
final_data$dbs_plate_number[which(final_data$dbs_plate_number == "Spat 6B-punches")] = 6
table(final_data$dbs_plate_number)

# for each entry that is missing put in a value
colnames(final_data)
# sample_name_dbs
length(which(is.na(final_data$sample_name_dbs)))
# sample_id_date
length(which(is.na(final_data$sample_id_date)))
for(i in 1:nrow(final_data)){
  if(is.na(final_data$sample_id_date[i]) & final_data$sample_name_dbs[i] != "3D7 CULTURE"){
    final_data$sample_id_date[i]=lubridate::dmy(str_split(final_data$sample_name_dbs[i],"-")[[1]][2])
  } else {
    final_data$sample_id_date[i] = final_data$sample_id_date[i]
  }
}
length(which(is.na(final_data$sample_id_date)))
length(which(final_data$sample_name_dbs=="3D7 CULTURE"))
final_data$sample_id_date = as.character(final_data$sample_id_date)
final_data$sample_id_date[which(final_data$sample_name_dbs=="3D7 CULTURE")] = "Not applicable"
length(which(is.na(final_data$sample_id_date)))
# village name
table(final_data$village_name, useNA="always")
length(which(is.na(final_data$village_name)))
for(i in 1:nrow(final_data)){
  if (is.na(final_data$village_name[i]) & final_data$sample_name_dbs[i] != "3D7 CULTURE"){
     if (str_detect(final_data$sample_name_dbs[i],"K")){
      final_data$village_name[i] = "Kinesamo"
    } else if (str_detect(final_data$sample_name_dbs[i],"M")){
      final_data$village_name[i] = "Maruti"
    } else if (str_detect(final_data$sample_name_dbs[i],"S")) {
      final_data$village_name[i] = "Sitabicha"
    } 
  } else if (is.na(final_data$village_name[i]) & final_data$sample_name_dbs[i] == "3D7 CULTURE") {
    final_data$village_name[i] = "Not applicable"
  } else {
    final_data$village_name[i] = final_data$village_name[i]
  }
}
length(which(is.na(final_data$village_name)))
table(final_data$village_name, useNA = "always")
# HH_ID
table(final_data$HH_ID, useNA="always")
length(which(is.na(final_data$HH_ID)))
for(i in 1:nrow(final_data)){
  if (is.na(final_data$HH_ID[i]) & final_data$sample_name_dbs[i] != "3D7 CULTURE"){
    final_data$HH_ID[i]=str_split(final_data$sample_name_dbs[i],"-")[[1]][1]
  } else if (is.na(final_data$HH_ID[i]) & final_data$sample_name_dbs[i] == "3D7 CULTURE") {
    final_data$HH_ID[i] = "Not applicable"
  } else {
    final_data$HH_ID[i] = final_data$HH_ID[i]
  }
}
length(which(is.na(final_data$HH_ID)))
table(final_data$HH_ID, useNA = "always")
# shipment_date
length(which(is.na(final_data$shipment_date)))
str(final_data$shipment_date)
for (i in 1:nrow(final_data)){
  if (is.na(final_data$shipment_date[i])){
    final_data$shipment_date[i] = "Not recorded"
  } else {
    final_data$shipment_date[i] = final_data$shipment_date[i]
  }
}
length(which(is.na(final_data$shipment_date)))
# sample_received
length(which(is.na(final_data$sample_received)))
str(final_data$sample_received)
for (i in 1:nrow(final_data)){
  if (is.na(final_data$sample_received[i])){
    final_data$sample_received[i] = "Not recorded"
  } else {
    final_data$sample_received[i] = final_data$sample_received[i]
  }
}
length(which(is.na(final_data$sample_received)))
# gdna_extraction_date
length(which(is.na(final_data$gdna_extraction_date)))
str(final_data$gdna_extraction_date)
for (i in 1:nrow(final_data)){
  if (is.na(final_data$gdna_extraction_date[i])){
    final_data$gdna_extraction_date[i] = "Not recorded"
  } else {
    final_data$gdna_extraction_date[i] = final_data$gdna_extraction_date[i]
  }
}
length(which(is.na(final_data$gdna_extraction_date)))
# comment
length(which(is.na(final_data$comment)))
str(final_data$comment)
for (i in 1:nrow(final_data)){
  if (is.na(final_data$comment[i])){
    final_data$comment[i] = "Not recorded"
  } else {
    final_data$comment[i] = final_data$comment[i]
  }
}
length(which(is.na(final_data$comment)))
# dbs_plate_number
table(final_data$dbs_plate_number, useNA = "always")
final_data$dbs_plate_number[which(is.na(final_data$dbs_plate_number))] = "Not recorded"
table(final_data$dbs_plate_number, useNA = "always")
# dbs_columns_number
table(final_data$dbs_column_number, useNA = "always")
final_data$dbs_column_number[which(is.na(final_data$dbs_column_number))] = "Not recorded"
table(final_data$dbs_column_number, useNA = "always")
# dbs_row_number
table(final_data$dbs_row_number, useNA = "always")
final_data$dbs_row_number[which(is.na(final_data$dbs_row_number))] = "Not recorded"
table(final_data$dbs_row_number, useNA = "always")

# now read in the recent dbs information to add CT values and experiment names


#### ----------- HUMAN DATA: set up qpcr data ---------------------- ####

# read in the preliminary qpcr data
human_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/spat21_qpcr_data_clean_human_dbs_16JAN2019.RDS")

# read in the redo qpcr data
redo_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/spat21_qpcr_data_B8-10-11redo.rds")

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

# first remove the columns that are not needed (standards)
cols_to_remove = c("HbtubStd1a","HbtubStd1b","HbtubStd2a","HbtubStd2b","HbtubStd3a","HbtubStd3b",
                   "HbtubStd4a","HbtubStd4b","HbtubStd5a","HbtubStd5b","HbtubStd6a","HbtubStd6b",
                   "HbtubStd7a","HbtubStd7b","HbtubStd8a","HbtubStd8b","HbtubStd9a","HbtubStd9b",
                   "HbtubStd10a","HbtubStd10b","pfr364Std1a","pfr364Std1b","pfr364Std2a","pfr364Std2b",
                   "pfr364Std3a","pfr364Std3b","pfr364Std4a","pfr364Std4b","pfr364Std5a","pfr364Std5b",
                   "pfr364Std6a","pfr364Std6b","pfr364Std7a","pfr364Std7b","pfr364Std8a","pfr364Std8b",
                   "pfr364Std9a","pfr364Std9b","pfr364Std10a","pfr364Std10b","r_value_std","intercept_std",
                   "slope_std", "Well Position","pfr364Q1","pfr364Q2")
human_qpcr_data = human_qpcr_data[,!(colnames(human_qpcr_data) %in% cols_to_remove)]
redo_qpcr_data = redo_qpcr_data[,!(colnames(redo_qpcr_data) %in% cols_to_remove)]

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

# change sample name
human_qpcr_data = rename(human_qpcr_data, "sample_name_dbs" = "Sample Name")

# write out the qpcr data set
# write_csv(human_qpcr_data,"spat21_final_qpcr_data_5AUG2019.csv")
# write_rds(human_qpcr_data,"spat21_final_qpcr_data_5AUG2019.rds")


#### ------------------ HUMAN DATA: now merge the qpcr data into the inventory ----------------- ####

# look at both data set
colnames(final_data)
colnames(human_qpcr_data)

# first remove pf_pcr_infection status from final_data
final_data$pf_pcr_infection_status <- NULL

# merge the qpcr and merged inventory data sets together
final_data_2 = left_join(final_data,human_qpcr_data,by="sample_name_dbs")
length(intersect(final_data$sample_name_dbs,human_qpcr_data$sample_name_dbs)) # looks like all merged
setdiff(human_qpcr_data$sample_name_dbs,final_data$sample_name_dbs)
final_data = final_data_2

# now clean up the remaining variables
colnames(final_data)
# Experiment Name
final_data = rename(final_data,"experiment_name" = "Experiment Name")
table(final_data$experiment_name, useNA = "always")
final_data$experiment_name[which(is.na(final_data$experiment_name))] = "qPCR not done"
table(final_data$experiment_name, useNA = "always")
# HbtubCT1
length(which(is.na(final_data$HbtubCT1)))
final_data$HbtubCT1[which(is.na(final_data$HbtubCT1) & final_data$experiment_name == "qPCR not done")] = "qPCR not done"
final_data$HbtubCT1[which(is.na(final_data$HbtubCT1) & final_data$experiment_name != "qPCR not done")] = "Undefined"
length(which(is.na(final_data$HbtubCT1)))
# HbtubCT1
length(which(is.na(final_data$HbtubCT2)))
final_data$HbtubCT2[which(is.na(final_data$HbtubCT2) & final_data$experiment_name == "qPCR not done")] = "qPCR not done"
final_data$HbtubCT2[which(is.na(final_data$HbtubCT2) & final_data$experiment_name != "qPCR not done")] = "Undefined"
length(which(is.na(final_data$HbtubCT2)))
# pfr364CT1
length(which(is.na(final_data$pfr364CT1)))
final_data$pfr364CT1[which(is.na(final_data$pfr364CT1) & final_data$experiment_name == "qPCR not done")] = "qPCR not done"
final_data$pfr364CT1[which(is.na(final_data$pfr364CT1) & final_data$experiment_name != "qPCR not done")] = "Undefined"
length(which(is.na(final_data$pfr364CT1)))
# pfr364CT2
length(which(is.na(final_data$pfr364CT2)))
final_data$pfr364CT2[which(is.na(final_data$pfr364CT2) & final_data$experiment_name == "qPCR not done")] = "qPCR not done"
final_data$pfr364CT2[which(is.na(final_data$pfr364CT2) & final_data$experiment_name != "qPCR not done")] = "Undefined"
length(which(is.na(final_data$pfr364CT2)))
# pfr364Q1_std
length(which(is.na(final_data$pfr364Q1_std)))
final_data$pfr364Q1_std[which(is.na(final_data$pfr364Q1_std) & final_data$experiment_name == "qPCR not done")] = "qPCR not done"
final_data$pfr364Q1_std[which(is.na(final_data$pfr364Q1_std) & final_data$experiment_name != "qPCR not done")] = "Undefined"
length(which(is.na(final_data$pfr364Q1_std)))
# pfr364Q2_std
length(which(is.na(final_data$pfr364Q2_std)))
final_data$pfr364Q2_std[which(is.na(final_data$pfr364Q2_std) & final_data$experiment_name == "qPCR not done")] = "qPCR not done"
final_data$pfr364Q2_std[which(is.na(final_data$pfr364Q2_std) & final_data$experiment_name != "qPCR not done")] = "Undefined"
length(which(is.na(final_data$pfr364Q2_std)))
# pf_pcr_infection_status
table(final_data$pf_pcr_infection_status, useNA = "always")
final_data$pf_pcr_infection_status = as.character(final_data$pf_pcr_infection_status)
final_data$pf_pcr_infection_status[which(is.na(final_data$pf_pcr_infection_status))] = "qPCR not done"
table(final_data$pf_pcr_infection_status, useNA = "always")
# pfr364Q_std_combined
length(which(is.na(final_data$pfr364Q_std_combined)))
final_data$pfr364Q_std_combined[which(is.na(final_data$pfr364Q_std_combined) & final_data$experiment_name == "qPCR not done")] = "qPCR not done"
final_data$pfr364Q_std_combined[which(is.na(final_data$pfr364Q_std_combined) & final_data$experiment_name != "qPCR not done")] = "Undefined"
length(which(is.na(final_data$pfr364Q_std_combined)))



#### -------------- HUMAN DATA: now read in the samples that were pf positive and merged into the data set so were used for sequencing ---------- ####

# read in the samples pulled for sequencing
sequencing_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Human DBS Samples to Sequence/spat21_human_dbs_positive_final_22MAY2019_clean.csv")

# add a column to the sequencing data to indicate that it was sequenced
sequencing_data$sequenced = rep("yes",nrow(sequencing_data))

# add a column to the sequencing data to indicate that it has no extracted material left
sequencing_data$all_extraction_material_used = rep("yes",nrow(sequencing_data))

# now remove all other variables aside from the two you added and sample_dbs_name
sequencing_data$pfr364CT1 <- NULL
sequencing_data$pfr364CT2 <- NULL
sequencing_data$dbs_plate_number <- NULL
sequencing_data$dbs_column_number <- NULL
sequencing_data$dbs_row_number <- NULL
colnames(sequencing_data)

# merge in the sequencing data with the final data set
final_data_2 = left_join(final_data,sequencing_data,by="sample_name_dbs")
length(intersect(final_data$sample_name_dbs,sequencing_data$sample_name_dbs))
setdiff(sequencing_data$sample_name_dbs,final_data$sample_name_dbs)
# looks like all merge in which is good
final_data = final_data_2

# clean up the two remaining variables
# sequenced
length(which(is.na(final_data$sequenced)))
final_data$sequenced[which(is.na(final_data$sequenced))] = "Not sequenced"
table(final_data$sequenced,useNA = "always")
# all_extraction_material_used
length(which(is.na(final_data$all_extraction_material_used)))
final_data$all_extraction_material_used[which(is.na(final_data$all_extraction_material_used) & final_data$experiment_name != "qPCR not done")] = "no"
final_data$all_extraction_material_used[which(is.na(final_data$all_extraction_material_used) & final_data$experiment_name == "qPCR not done")] = "extraction not done"
table(final_data$all_extraction_material_used,useNA = "always")

# remove all the 3D7 culture rows
length(which(final_data$sample_name_dbs == "3D7 CULTURE")) # 112 samples
final_data = final_data[-which(final_data$sample_name_dbs == "3D7 CULTURE"),]
length(which(final_data$sample_name_dbs == "3D7 CULTURE")) # all missing

# add in some rows for the samples that were removed for not knowing what sample they were
# look at the number within each plate
table(final_data$dbs_plate_number, useNA = "always")
# look at plates 10, 11, 2, 3, 8

# remove the sample_received and comment columns
final_data$sample_received <- NULL
final_data$comment <- NULL

# add in samples for plate 2
colnames(final_data)
plate_2_add_df = data.frame(sample_name_dbs = rep("plate_id_mislabeled",22), sample_id_date = rep("Not applicable",22), village_name = rep("Not applicable",22),
                            HH_ID=rep("Not applicable",22), shipment_date=rep("Not recorded",22), gdna_extraction_date = rep("Not recorded",22),
                            dbs_plate_number=rep(2,22),dbs_column_number=c(1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10),dbs_row_number=c("A","A","A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","B","B"),
                            experiment_name = rep("qPCR not done",22), HbtubCT1 = rep("qPCR not done",22), HbtubCT2 = rep("qPCR not done",22), pfr364CT1 = rep("qPCR not done",22), pfr364CT2 = rep("qPCR not done",22),
                            pfr364Q1_std = rep("qPCR not done",22), pfr364Q2_std = rep("qPCR not done",22), pf_pcr_infection_status = rep("qPCR not done",22),
                            pfr364Q_std_combined = rep("qPCR not done",22), sequenced = rep("Not sequenced",22), all_extraction_material_used = rep("mislabeled sample",22))

# add in samples for plate 3
plate_3_add_df = data.frame(sample_name_dbs = rep("plate_id_mislabeled",3), sample_id_date = rep("Not applicable",3), village_name = rep("Not applicable",3),
                            HH_ID=rep("Not applicable",3), shipment_date=rep("Not recorded",3), gdna_extraction_date = rep("Not recorded",3),
                            dbs_plate_number=rep(3,3),dbs_column_number=c(1,2,12),dbs_row_number=c("A","A","B"),
                            experiment_name = rep("qPCR not done",3), HbtubCT1 = rep("qPCR not done",3), HbtubCT2 = rep("qPCR not done",3), pfr364CT1 = rep("qPCR not done",3), pfr364CT2 = rep("qPCR not done",3),
                            pfr364Q1_std = rep("qPCR not done",3), pfr364Q2_std = rep("qPCR not done",3), pf_pcr_infection_status = rep("qPCR not done",3),
                            pfr364Q_std_combined = rep("qPCR not done",3), sequenced = rep("Not sequenced",3), all_extraction_material_used = rep("mislabeled sample",3))

# add in samples for plate 8
plate_8_add_df = data.frame(sample_name_dbs = rep("plate_id_mislabeled",21), sample_id_date = rep("Not applicable",21), village_name = rep("Not applicable",21),
                            HH_ID=rep("Not applicable",21), shipment_date=rep("Not recorded",21), gdna_extraction_date = rep("Not recorded",21),
                            dbs_plate_number=rep(8,21),dbs_column_number=c(1,10,11,12,1,2,3,4,6,7,8,9,10,11,12,1,2,3,4,6,4),dbs_row_number=c("A","A","A","A","B","B","B","B","B","B","B","B","B","B","B","H","H","H","H","F","G"),
                            experiment_name = rep("qPCR not done",21), HbtubCT1 = rep("qPCR not done",21), HbtubCT2 = rep("qPCR not done",21), pfr364CT1 = rep("qPCR not done",21), pfr364CT2 = rep("qPCR not done",21),
                            pfr364Q1_std = rep("qPCR not done",21), pfr364Q2_std = rep("qPCR not done",21), pf_pcr_infection_status = rep("qPCR not done",21),
                            pfr364Q_std_combined = rep("qPCR not done",21), sequenced = rep("Not sequenced",21), all_extraction_material_used = rep("mislabeled sample",21))

# add in samples for plate 10
plate_10_add_df = data.frame(sample_name_dbs = rep("plate_id_mislabeled",4), sample_id_date = rep("Not applicable",4), village_name = rep("Not applicable",4),
                            HH_ID=rep("Not applicable",4), shipment_date=rep("Not recorded",4), gdna_extraction_date = rep("Not recorded",4),
                            dbs_plate_number=rep(10,4),dbs_column_number=c(10,11,12,1),dbs_row_number=c("F","F","F","H"),
                            experiment_name = rep("qPCR not done",4), HbtubCT1 = rep("qPCR not done",4), HbtubCT2 = rep("qPCR not done",4), pfr364CT1 = rep("qPCR not done",4), pfr364CT2 = rep("qPCR not done",4),
                            pfr364Q1_std = rep("qPCR not done",4), pfr364Q2_std = rep("qPCR not done",4), pf_pcr_infection_status = rep("qPCR not done",4),
                            pfr364Q_std_combined = rep("qPCR not done",4), sequenced = rep("Not sequenced",4), all_extraction_material_used = rep("mislabeled sample",4))

# add in samples for plate 11
plate_11_add_df = data.frame(sample_name_dbs = rep("plate_id_mislabeled",5), sample_id_date = rep("Not applicable",5), village_name = rep("Not applicable",5),
                            HH_ID=rep("Not applicable",5), shipment_date=rep("Not recorded",5), gdna_extraction_date = rep("Not recorded",5),
                            dbs_plate_number=rep(11,5),dbs_column_number=c(1,2,3,4,7),dbs_row_number=c("F","F","F","F","F"),
                            experiment_name = rep("qPCR not done",5), HbtubCT1 = rep("qPCR not done",5), HbtubCT2 = rep("qPCR not done",5), pfr364CT1 = rep("qPCR not done",5), pfr364CT2 = rep("qPCR not done",5),
                            pfr364Q1_std = rep("qPCR not done",5), pfr364Q2_std = rep("qPCR not done",5), pf_pcr_infection_status = rep("qPCR not done",5),
                            pfr364Q_std_combined = rep("qPCR not done",5), sequenced = rep("Not sequenced",5), all_extraction_material_used = rep("mislabeled sample",5))

# bind in those new plates
final_data = rbind(final_data,plate_2_add_df,plate_3_add_df,plate_8_add_df,plate_10_add_df,plate_11_add_df)

# check that each plate is shown correctly
table(final_data$dbs_plate_number, useNA = "always")

# remove the columns all_extraction_material_used
final_data$all_extraction_material_used <- NULL

# change the not sequenced column
final_data = rename(final_data,"sent_for_sequencing"="sequenced")
table(final_data$sent_for_sequencing, useNA = "always")
final_data$sent_for_sequencing[which(final_data$sent_for_sequencing=="Not sequenced")] = "no"
table(final_data$sent_for_sequencing, useNA = "always")

# change plate names to be exactly what shows up on the plates
table(final_data$dbs_plate_number, useNA = "always")
for (i in 1:nrow(final_data)){
  final_data$dbs_plate_number[i] = paste0("Spat-",as.character(final_data$dbs_plate_number[i]),"B")
}
table(final_data$dbs_plate_number, useNA = "always")
final_data$dbs_plate_number[which(final_data$dbs_plate_number=="Spat-Not recordedB")] = "Not recorded"
table(final_data$dbs_plate_number, useNA = "always")

# look at the variables one last time
colnames(final_data)
length(which(is.na(final_data$sample_name_dbs)))
length(which(is.na(final_data$sample_id_date)))
table(final_data$village_name, useNA = "always")
table(final_data$HH_ID, useNA = "always")
table(final_data$shipment_date, useNA = "always")
table(final_data$gdna_extraction_date, useNA = "always")
table(final_data$dbs_plate_number, useNA = "always")
table(final_data$dbs_column_number, useNA = "always")
table(final_data$dbs_row_number, useNA = "always")
table(final_data$experiment_name, useNA = "always")
length(which(is.na(final_data$HbtubCT1)))
length(which(is.na(final_data$HbtubCT2)))
length(which(is.na(final_data$pfr364CT1)))
length(which(is.na(final_data$pfr364CT2)))
length(which(is.na(final_data$pfr364Q1_std)))
length(which(is.na(final_data$pfr364Q2_std)))
table(final_data$pf_pcr_infection_status,useNA = "always")
length(which(is.na(final_data$pfr364Q_std_combined)))
table(final_data$sent_for_sequencing, useNA = 'always')

# fix a few inventory entries that look like typos
# K01-110717-9-R
final_data$dbs_row_number[which(final_data$sample_name_dbs == "K01-110717-9-R")] = "A"
final_data$dbs_plate_number[which(final_data$sample_name_dbs=="K01-110717-9-R")] = "Spat-1B"
# M03-260618-2-2 and M03-260618-2-R
final_data$dbs_row_number[which(final_data$sample_name_dbs=="M03-260618-2-R")] = "C"
final_data$dbs_plate_number[which(final_data$sample_name_dbs=="M03-260618-2-R")] = "Spat-29B"
final_data$dbs_column_number[which(final_data$sample_name_dbs=="M03-260618-2-R")] = "1"
final_data = final_data[-which(final_data$sample_name_dbs=="M03-260618-2-2"),]

# check one last time for duplicate ids
length(unique(final_data$sample_name_dbs)) # 2921 unique 
length(which(is.na(final_data$sample_name_dbs) == T)) # 0 missing
count_table = table(final_data$sample_name_dbs, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 1 duplicates
length(dups_table)
dups_table # looks good, no duplicates other than "plate_id_mislabeled"

# do one last check that there's the right number of each plate
table(final_data$dbs_plate_number, useNA = "always")
table(final_data$experiment_name, useNA = "always")

# looks like there's 1 missing from spat-22B with the recode
# add in samples for plate 22
plate_22_add_df = data.frame(sample_name_dbs = rep("plate_id_mislabeled",1), sample_id_date = rep("Not applicable",1), village_name = rep("Not applicable",1),
                             HH_ID=rep("Not applicable",1), shipment_date=rep("Not recorded",1), gdna_extraction_date = rep("Not recorded",1),
                             dbs_plate_number=rep("Spat-22B",1),dbs_column_number=c(9),dbs_row_number=c("G"),
                             experiment_name = rep("qPCR not done",1), HbtubCT1 = rep("qPCR not done",1), HbtubCT2 = rep("qPCR not done",1), pfr364CT1 = rep("qPCR not done",1), pfr364CT2 = rep("qPCR not done",1),
                             pfr364Q1_std = rep("qPCR not done",1), pfr364Q2_std = rep("qPCR not done",1), pf_pcr_infection_status = rep("qPCR not done",1),
                             pfr364Q_std_combined = rep("qPCR not done",1), sent_for_sequencing = rep("no",1))

# bind in those new plates
final_data = rbind(final_data,plate_22_add_df)

# do one last check that there's the right number of each plate
table(final_data$dbs_plate_number, useNA = "always")
table(final_data$experiment_name, useNA = "always")

# write out the data set
write_csv(final_data,"mozzie_phase_1_final_human_inventory_6AUG2019_searchable.csv")
write_rds(final_data,"mozzie_phase_1_final__human_inventory_6AUG2019_searchable.rds")

# created an Excel sheet version for the lab to use that's pared down some from the searchable version





