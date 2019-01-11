# ----------------------------------------- #
#   Spat21 Data Set Cleaning Merge Data     #
#                Human Data                 #
#            December 19, 2018              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(tableone)


#### --------- read in the data sets ----------------- ####

# human data sets
hum_ann_household_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_ann_household_data_19DEC2018.RDS")
hum_monthly_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_monthly_data_19DEC2018.RDS")
hum_sick_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_sick_data_19DEC2018.RDS")
hum_sleeping_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_sleeping_data_19DEC2018.RDS")
hum_table_household_data = readRDS("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/hum_table_household_data_19DEC2018.RDS")


#### -------- merge together data sets ------------ ####

# check if data sets in wide/long format for unq_memID
table(hum_monthly_data$unq_memID, useNA = "always") # long format
table(hum_table_household_data$unq_memID, useNA = "always") # wide format, should have 268 people total (so 268 observations)
table(hum_sick_data$unq_memID, useNA = "always") # long format

# convert hum_monthly_data and hum_sick_data from long to wide format
# first convert hum_monthly_data
# way one
long_hum_monthly_data = spread(data=hum_monthly_data, key = today_hum_monthly_data, value = unq_memID)
# way two
long_hum_monthly_data = dcast(hum_monthly_data, unq_memID ~ today_hum_monthly_data)

# don't merge data sets for the moment

