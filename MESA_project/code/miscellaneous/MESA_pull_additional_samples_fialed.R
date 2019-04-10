# ----------------------------------------- #
# Pull out MESA data that failed sequencing #
#             March 29, 2019                #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(tableone)


#### ------- read in data sets ---------- ####

# read in the all failed data set
all_failed = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/MESA_extra_Betsy/MESA_all_failed_samples_28MAR2019.csv")

# read in the all passed data set
all_passed = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/MESA_extra_Betsy/MESA_all_passed_samples_28MAR2019.csv")

# read in the all sample data set
all_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/FINAL_DATA/final_results_20DEC2018.csv")


#### --------- pull samples that were successfully sequenced for ama -------- ####

# now subset the columns to only the columns of interest
# all_data
colnames(all_data)
cols_to_keep = c("labid_new","pfr364CT1","pfr364CT2")
all_data = all_data[,colnames(all_data) %in% cols_to_keep]
# rename labid_new to labid
all_data = rename(all_data,"labid" = "labid_new")

# now merge together the two data sets for those that failed
failed_merged_data = left_join(all_failed,all_data,by="labid")

# now merge together the two data sets for those that passed
passed_merged_data = left_join(all_passed,all_data,by="labid")

# write out the data set
# write_csv(failed_merged_data,"MESA_all_failed_sequencing_29MAR2019.csv")
# write_csv(passed_merged_data,"MESA_all_pass_sequencing_29MAR2019.csv")

# create a new variable for if it passed for failed sequencing
failed_merged_data$passed_or_failed_sequencing = rep("failed",nrow(failed_merged_data))
passed_merged_data$passed_or_failed_sequencing = rep("passed",nrow(passed_merged_data))

# combine the passed and failed sequencing data
all_merged_data = rbind(failed_merged_data,passed_merged_data)

# create a new variable for if both CT values >= 31
all_merged_data$both_cts_over_30 = rep(NA,nrow(all_merged_data))
all_merged_data$both_cts_over_30[all_merged_data$pfr364CT1>=31 & all_merged_data$pfr364CT2>=31] = "yes"
all_merged_data$both_cts_over_30[all_merged_data$pfr364CT1>=31 & is.na(all_merged_data$pfr364CT2)] = "yes"
all_merged_data$both_cts_over_30[all_merged_data$pfr364CT2>=31 & is.na(all_merged_data$pfr364CT1)] = "yes"
all_merged_data$both_cts_over_30[is.na(all_merged_data$pfr364CT1) & is.na(all_merged_data$pfr364CT2)] = "yes"

 # check the output
length(which((all_merged_data$pfr364CT1 >= 31 & all_merged_data$pfr364CT2 >= 31) | 
         (all_merged_data$pfr364CT1>=31 & is.na(all_merged_data$pfr364CT2)) |
         (all_merged_data$pfr364CT2>=31 & is.na(all_merged_data$pfr364CT1)) |
         (is.na(all_merged_data$pfr364CT1) & is.na(all_merged_data$pfr364CT2))))
table(all_merged_data$both_cts_over_30, useNA = "always")

# see how many samples had CT values that were both >= 31
table(all_merged_data$both_cts_over_30, useNA = "always")
table(all_merged_data$both_cts_over_30,all_merged_data$passed_or_failed_sequencing)


