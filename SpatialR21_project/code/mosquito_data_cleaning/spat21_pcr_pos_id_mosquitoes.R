# ----------------------------------------- #
#    Spat21 Data Set PCR + Identification   #
#              Mosquito Data                #
#            December 21, 2018              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)


#### --------- load in the qpcr cleaned data set ----------- ####

# read in the data set
qpcr_clean_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/spat21_mosquito_qpcr_data_22DEC2018.RDS")

# pull out the samples that are positive for pf_pcr_infection_status_sample_level
pf_positives = qpcr_clean_data[which(qpcr_clean_data$pf_pcr_infection_status_sample_level == "positive"),]

# write out as a CSV file
write_csv(pf_positives, "spat21_mosquito_qpcr_data_22DEC2018_pf_positives_for_BF.csv")


