# ----------------------------------------- #
#     Pull Human DBS Merged for Betsy       #
#                Human Data                 #
#               May 21, 2019                #
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


#### ------- read in the data sets -------- ####

# read in the human dbs inventory
human_inventory_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/lab_inventories/clean files/dbs_lab_inventory_final_with_re-extraction_29APR2019.csv")

# read in the clean merged qpcr/social data set
human_merged_all_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/spat21_human_merged_all_data_21MAY2019.RDS")


#### ------- merge in the qpcr data with the human monthly, table, and sick data ------ ####

# calculate how many RDT+ results for the same person
participant_data = human_merged_all_data %>%
  filter(rdt_rst == "positive") %>%
  group_by(unq_memID) %>%
  summarize(n=n()) 
repeated_infections = participant_data[which(participant_data$n>1),]
summary(repeated_infections$n)

# first check for duplicates in the sample name column for the human_merged_all_data data set
length(unique(human_merged_all_data$`Sample Name`)) # 3919 unique 
length(which(is.na(human_merged_all_data$`Sample Name`) == T)) # 0 missing
count_table = table(human_merged_all_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates


#### ----------- set up qpcr data ---------------------- ####

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

# first check for duplicates in the sample name column for the human_merged_all_data data set
length(unique(human_merged_all_data$`Sample Name`)) # 3140 unique 
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
                   "slope_std", "Well Position","HbtubCT1","HbtubCT2",
                   "pfr364Q1","pfr364Q2","pfr364Q1_std","pfr364Q2_std")
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

# check for duplicates in the inventory
length(unique(human_inventory_data$sample_id)) # 2931 unique 
length(which(is.na(human_inventory_data$sample_id) == T)) # 0 missing
count_table = table(human_inventory_data$sample_id, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 18 duplicates
length(dups_table)
dups_table = data.frame(dups_table)

# remove the rows where the inventory is a duplicate and plate is 2
small_data = human_inventory_data[which(human_inventory_data$sample_id %in% dups_table$Var1 & human_inventory_data$sample_id != "3D7 CULTURE"),]

# only keep plate 1
human_inventory_data = human_inventory_data[-which(human_inventory_data$sample_id %in% dups_table$Var1 & human_inventory_data$sample_id != "3D7 CULTURE" & human_inventory_data$dbs_plate_number == "Spat 2B-punches"),]

# check for duplicates in the inventory
length(unique(human_inventory_data$sample_id)) # 2925 unique 
length(which(is.na(human_inventory_data$sample_id) == T)) # 0 missing
count_table = table(human_inventory_data$sample_id, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 1 duplicates
length(dups_table)
dups_table = data.frame(dups_table)
dups_table
# remove the exgra K01-110717-9-R entry
human_inventory_data = human_inventory_data[-which(human_inventory_data$sample_id == "K01-110717-9-R" & is.na(human_inventory_data$dbs_plate_number)),]
# check for duplicates in the inventory
length(unique(human_inventory_data$sample_id)) # 2925 unique 
length(which(is.na(human_inventory_data$sample_id) == T)) # 0 missing
count_table = table(human_inventory_data$sample_id, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table = data.frame(dups_table)
dups_table

#### --------------------- pull samples for Betsy that perfectly merged ------------------------- ####

# pull samples for Betsy
samples_to_pull = human_merged_all_data[which(human_merged_all_data$pf_pcr_infection_status == "positive"),]
length(which(is.na(human_merged_all_data$pf_pcr_infection_status)))
length(which(human_merged_all_data$pf_pcr_infection_status == "negative"))
length(which(human_merged_all_data$pf_pcr_infection_status == "positive"))

# merge in the qpcr data to samples to pull
samples_to_pull = left_join(samples_to_pull,human_qpcr_data,by="sample_name_dbs")
length(which(is.na(samples_to_pull$pf_pcr_infection_status.y)))

# only pull columns of interest
colnames(samples_to_pull)
columns_to_keep = c("sample_name_dbs","pfr364CT1","pfr364CT2","Experiment Name")
samples_to_pull = samples_to_pull[,columns_to_keep]

# merge in the human inventory information
human_inventory_data = rename(human_inventory_data, "sample_name_dbs" = "sample_id")
all_sample_data = left_join(samples_to_pull,human_inventory_data,by="sample_name_dbs")

# remove the experiment name column and some other columns
all_sample_data$`Experiment Name` <- NULL
all_sample_data$gdna_extraction_date <- NULL
all_sample_data$shipment_date <- NULL
all_sample_data$sample_received <- NULL
all_sample_data$comment <- NULL

# write some code to clean the files yourself
# now loop through each row and move over those two columns
# check colnames
colnames(all_sample_data)
# remove columns 7,8,9
all_sample_data = all_sample_data[,-c(7,8,9)]
colnames(all_sample_data)
# change column order
all_sample_data = all_sample_data[,c(1,2,3,4,5,6,9,8,7)]
colnames(all_sample_data)
# look at input
length(which(is.na(all_sample_data$dbs_plate_number))) # 1006 missing
length(which(is.na(all_sample_data$plate_number_addition))) # 171 missing

# start for loop to combine qpcr results
for (i in 1:nrow(all_sample_data)){
  if (is.na(all_sample_data[i,4])){
    for (k in 1:3){   # this is for all data that is present in .y files but not in .x
      startpoint = 3 + k
      all_sample_data[i,startpoint] = all_sample_data[i,startpoint+3]
      all_sample_data[i,startpoint+3] <- NA
    }
  } else if (is.na(all_sample_data[i,7])){
    for (k in 1:3){ # this is for all data that is present in .x files but not in .y -> just make it NULL
      startpoint = 3 + k
      all_sample_data[i,startpoint+3] <- NA
    }
  } else {
    for (k in 1:3){ # both data sets are missing -> just make it NULL at .y location
      startpoint = 3 + k
      all_sample_data[i,startpoint+3] <- NA
    }
  } 
}
# check the output
length(which(is.na(all_sample_data$dbs_plate_number))) # 2 missing
length(which(is.na(all_sample_data$plate_number_addition))) # 1178 missing
# looks like it worked correctly

# change the data frame name for cleaning
final_data = all_sample_data

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

# export as a csv file
write_csv(final_data,"spat21_human_dbs_positive_final_22MAY2019.csv")

# check why 2 pilot study positive samples no longer in data set
plate_1 = human_inventory_data[which(human_inventory_data$dbs_plate_number == 1),]
intersect(human_merged_all_data$sample_name_dbs,plate_1$sample_name_dbs)
length(intersect(human_merged_all_data$sample_name_dbs,plate_1$sample_name_dbs)) # 21, so 2 positives weren't in data set
plate_1_final_merge = final_data[which(final_data$dbs_plate_number == 1),]
plate_1_human_merge = human_merged_all_data[which(human_merged_all_data$sample_name_dbs %in% plate_1$sample_name_dbs),]
table(plate_1_human_merge$pf_pcr_infection_status, useNA = "always")
# looks like 2 positives didn't merge into the human merge data set



#### ------- clear working directory and read in what already merged -------- ####

# full merged data set (all samples)
all_samples = read_csv("Desktop/spat21_human_dbs_positive_final_22MAY2019_clean.csv")

# read in data set you sent Betsy
original_samples = read_csv("Desktop/spat21_human_dbs_positive_perfect_matches_final.csv")

# check the intersect
length(intersect(all_samples$sample_name_dbs,original_samples$`Sample Name`)) # 849 samples intersect
# 2 samples do not
# what are the 2 samples in the original samples pulled that are not in the all_samples data set
setdiff(original_samples$`Sample Name`,all_samples$sample_name_dbs)
# didn't have in final data set with all samples: K01-120617-4, M06-130617-4

# read in the human dbs inventory
human_inventory_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/lab_inventories/clean files/dbs_lab_inventory_final_with_re-extraction_29APR2019.csv")

# read in the clean merged qpcr/social data set
human_merged_all_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/spat21_human_merged_all_data_21MAY2019.RDS")

# look at the samples that didn't merge
human_merged_test = human_merged_all_data[which(human_merged_all_data$sample_name_dbs == "K01-120617-4"),]
table(human_merged_test$pf_pcr_infection_status, useNA = "always")
# everything is good, it was because sometimes the monthly and sick visits occurred on same day and DBS were collected at both visits

# now create data set of remaining samples to pull out for sequencing
length(setdiff(all_samples$sample_name_dbs,original_samples$`Sample Name`)) # 329 left 
rest_to_pull = setdiff(all_samples$sample_name_dbs,original_samples$`Sample Name`)
rest_to_pull_data = all_samples[which(all_samples$sample_name_dbs %in% rest_to_pull),]

# double check that samples pulled correctly
intersect(rest_to_pull_data$sample_name_dbs,original_samples$`Sample Name`)
# no intersect so looks good
# check numbers: 329+851-2=1178, correct!

# write out the data set
write_csv(rest_to_pull_data,"spat21_human_positive_dbs_rest_of_samples.csv")

# now also read in the mosquito inventory and pcr results
# read in the data set
qpcr_clean_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/old/spat21_mosquito_qpcr_data_22DEC2018.RDS")
# pull out the samples that are positive for pf_pcr_infection_status_sample_level
table(qpcr_clean_data$pf_pcr_infection_status_sample_level, useNA = "always")
colnames(qpcr_clean_data)
qpcr_clean_data = rename(qpcr_clean_data,"sample_name" = "Sample Name")
pf_positives = qpcr_clean_data[which(qpcr_clean_data$pf_pcr_infection_status_sample_level == "positive"),]
pf_positives = pf_positives[,c(1,6,7,25)]

# create a list of the samples in the pilot study
pilot_study = read_csv("Desktop/pilot_study_mosquitoes.csv")
colnames(pilot_study)

# now merge in the pcr results with the mosquito pilot study
pilot_study = left_join(pilot_study,pf_positives,by="sample_name")

# remove rows that are not positive
pilot_study = pilot_study[which(pilot_study$pf_pcr_infection_status_sample_level == "positive"),]






