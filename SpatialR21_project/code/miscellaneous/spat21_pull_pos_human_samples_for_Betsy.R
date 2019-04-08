# ----------------------------------------- #
# Pull Human DBS Perfectly Merged for Betsy #
#                Human Data                 #
#              April 5, 2019                #
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

# read in the merged human monthly and table data set
human_merged_all_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/hum_monthly_merged_with_table_and_sick_with_exclusion_data_27FEB2019.RDS")

# read in the preliminary qpcr data
human_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/spat21_qpcr_data_clean_human_dbs_16JAN2019.RDS")

# read in the human dbs inventory
human_inventory_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/lab_inventories/clean files/dbs_lab_inventory_final_9JAN2019.csv")

#### ------- merge in the qpcr data with the human monthly, table, and sick data ------ ####

# calculate how many RDT+ results for the same person
participant_data = human_merged_all_data %>%
  filter(rdt_rst == "positive") %>%
  group_by(unq_memID) %>%
  summarize(n=n()) 
repeated_infections = participant_data[which(participant_data$n>1),]
summary(repeated_infections$n)

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



#### --------------------- pull samples for Betsy that perfectly merged ------------------------- ####

# pull samples for Betsy
samples_to_pull = human_merged_all_data_qpcr[which(human_merged_all_data_qpcr$pf_pcr_infection_status == "positive"),]
length(which(is.na(human_merged_all_data_qpcr$pf_pcr_infection_status)))
length(which(human_merged_all_data_qpcr$pf_pcr_infection_status == "negative"))
length(which(human_merged_all_data_qpcr$pf_pcr_infection_status == "positive"))

# only pull columns of interest
colnames(samples_to_pull)
columns_to_keep = c("Sample Name","pfr364CT1","pfr364CT2","Experiment Name")
samples_to_pull = samples_to_pull[,columns_to_keep]

# remove all rows where the experiment name had plate 1
length(which(samples_to_pull$`Experiment Name` == "Mozzie DBS B1 Taqman duplex 1-3-19")) # 18
samples_to_pull = samples_to_pull[-which(samples_to_pull$`Experiment Name` == "Mozzie DBS B1 Taqman duplex 1-3-19"),]

# merge in the human inventory information
human_inventory_data = rename(human_inventory_data, "Sample Name" = "sample_id")
all_sample_data = left_join(samples_to_pull,human_inventory_data,by="Sample Name")

# remove the experiment name column and some other columns
all_sample_data$`Experiment Name` <- NULL
all_sample_data$gdna_extraction_date <- NULL
all_sample_data$shipment_date <- NULL
all_sample_data$sample_received <- NULL
all_sample_data$comment <- NULL

# write out as a csv file
# write_csv(all_sample_data,"Spat21_human_dbs_positive_samples_for_sequencing_perfect_matches.csv")


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




