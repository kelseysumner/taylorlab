# ----------------------------------------- #
#       Spat21 Data Set for Betsy           #
#      Merge sequence data with CT values   #
#              Mosquito Data                #
#              April 8, 2019                #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


#### ---------- read in the data sets ----------- ####

lab_data = read_csv("Desktop/Mozzie mosquito ama csp MiSeq order #5531.csv")

qpcr_clean_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/old/spat21_mosquito_qpcr_data_22DEC2018.RDS")


#### ---------- now merge the ct values with the lab data set for sequencing ------------- ####

# look at the column names for both data sets
colnames(lab_data)
colnames(qpcr_clean_data)

# change the sample_id_mosquito to sample ID in the qpcr data set
qpcr_clean_data = rename(qpcr_clean_data, "Sample ID_1" = "Sample Name")

# subset the data set to just the columns of interest
cols_of_interest = c("Sample ID_1", "pfr364CT1", "pfr364CT2")
qpcr_clean_data = qpcr_clean_data[,cols_of_interest]

# merge the two data sets by sample ID
merged_data = left_join(lab_data, qpcr_clean_data, by = "Sample ID_1")

# export as a CSV file
write_csv(merged_data, "spat21_mosquito_sequencing_inventory_with_CT_values.csv")






