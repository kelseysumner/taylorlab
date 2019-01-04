# ----------------------------------------- #
#        Spat21 Data Set Merging            #
#              Mosquito Data                #
#             January 4, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


#### --------- read in mosquito data ----------------- ####

# read in the anopheles mosquito data sets
# first the cleaned descriptive data set
anoph_descriptive_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/spat21_mosquito_anopheles_descriptive_long_data_4JAN2019.RDS")
# next the cleaned qpcr data set
anoph_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/spat21_mosquito_qpcr_data_wide_3JAN2019.RDS")

# look at the new data sets
summary(anoph_descriptive_data)
summary(anoph_qpcr_data)


#### ------- merge the anopheles mosquito data together by sample_id_mosquito ---------- ####





