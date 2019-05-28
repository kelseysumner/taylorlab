# ----------------------------------------- #
#  Clean Final Merged Data Set for Spat21   #
#                Human Data                 #
#               May 28, 2019                #
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


#### -------- read in the final merged data set ------------- ####

human_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/spat21_human_merged_all_data_21MAY2019.RDS")



#### -------- look at the remaining variables and clean ------- ####

# look at the column names
colnames(human_merged_data)
# 135 variables in final data set

# make a data frame of the colnames
colname_df = data.frame(colnames(human_merged_data))
colname_df
write_csv(colname_df,"colname_df.csv")









