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

# write out as a csv file
write_csv(all_sample_data,"Spat21_human_dbs_positive_samples_for_sequencing_ALL_SAMPLES.csv")


# now read back in the data set that you manually cleaned and clean up the plate numbers
final_data = read_csv("/Users/kelseysumner/Desktop/Spat21_human_dbs_positive_samples_for_sequencing_perfect_matches_clean.csv")

# clean up the column names
final_data$X7 <- NULL
final_data$X8 <- NULL
final_data$X9 <- NULL
final_data$X10 <- NULL
final_data$X11 <- NULL
final_data$X12 <- NULL

# change the dbs_plate_number entries
final_data$dbs_plate_number[which(final_data$dbs_plate_number == "Spat 2B-punches")] = 2
final_data$dbs_plate_number[which(final_data$dbs_plate_number == "Spat 4B-punches")] = 4
final_data$dbs_plate_number[which(final_data$dbs_plate_number == "Spat 3B-punches")] = 3
final_data$dbs_plate_number[which(final_data$dbs_plate_number == "Spat 5B-punches")] = 5
final_data$dbs_plate_number[which(final_data$dbs_plate_number == "Spat 6B-punches")] = 6
table(final_data$dbs_plate_number)

# export as a csv file
write_csv(final_data,"spat21_human_dbs_positive_perfect_matches_final.csv")




