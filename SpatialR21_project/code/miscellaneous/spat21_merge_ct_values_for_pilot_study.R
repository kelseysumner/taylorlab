# ----------------------------------------- #
#       Spat21 Pilot Study CT Values        #
#               May 6, 2019                 #
#                K. Sumner                  #
# ----------------------------------------- #


#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(tableone)
library(MHCtools)
library(dada2)
library(ggplot2)
library(stringr)



#### ---------- read in the data sets ----------- ####

miseq_inventory_human = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Primers Information/MiSeq order #4747 - Spatial R21 Pilot Study 3-2018.csv")

qpcr_clean_data_human = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/spat21_qpcr_data_clean_human_dbs_16JAN2019.RDS")

qpcr_clean_data_mosquito = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/old/spat21_mosquito_qpcr_data_22DEC2018.RDS")



#### ---------- now merge the ct values with the lab data set for sequencing ------------- ####

# look at the column names for both data sets
colnames(miseq_inventory_human)
colnames(qpcr_clean_data_human)

# change the sample_id_mosquito to sample ID in the qpcr data set
qpcr_clean_data_human = rename(qpcr_clean_data_human, "sample_id" = "Sample Name")

# change sample id column for miseq inventory
miseq_inventory_human = rename(miseq_inventory_human, "sample_id" = "MiSeq order #4747 - Spatial R21 Pilot study  3/18")

# subset the data set to just the columns of interest
cols_of_interest = c("sample_id", "Experiment Name", "pfr364CT1", "pfr364CT2")
qpcr_clean_data_human = qpcr_clean_data_human[,cols_of_interest]

# clean the sample ids for the miseq inventory
miseq_inventory_human = miseq_inventory_human[-c(1:2),]
miseq_inventory_human$sample_id[miseq_inventory_human$sample_id == "R-21087-K01-5"] = "K01-210817-5-R"
miseq_inventory_human$sample_id[miseq_inventory_human$sample_id == "R-080717-K01-3"] = "K01-080717-3-R"
miseq_inventory_human$sample_id[miseq_inventory_human$sample_id == "R-080717-K01-5"] = "K01-080717-5-R"
miseq_inventory_human$sample_id[miseq_inventory_human$sample_id == "R-030817-K01-2"] = "K01-030817-2-R"
miseq_inventory_human$sample_id[miseq_inventory_human$sample_id == "R-090817-K01-1"] = "K01-090817-1-R"
miseq_inventory_human$sample_id[miseq_inventory_human$sample_id == "R-110717-K01-9"] = "K01-110717-9-R"

# merge the two data sets by sample ID
merged_data_human = left_join(miseq_inventory_human, qpcr_clean_data_human, by = "sample_id")

# export as a CSV file
# write_csv(merged_data_human, "spat21_pilot_study_inventory_with_CT_values.csv")

# now look at the mosquito qpcr results
colnames(qpcr_clean_data_mosquito)

# change the sample_id to sample ID in the qpcr data set
qpcr_clean_data_mosquito = rename(qpcr_clean_data_mosquito, "sample_id" = "Sample Name")

# subset the data set to just the columns of interest
cols_of_interest = c("sample_id", "Experiment Name", "pfr364CT1", "pfr364CT2")
qpcr_clean_data_mosquito = qpcr_clean_data_mosquito[,cols_of_interest]

# merge the two data sets by sample ID
merged_data_mosquito = left_join(miseq_inventory_human, qpcr_clean_data_mosquito, by = "sample_id")

# export the data set
write_csv(merged_data_mosquito,"spat21_piloty_study_inventory_part2.csv")






