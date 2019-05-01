# ----------------------------------------- #
#   Spat21 Merging Repeated qPCR Samples    #
#                Human Data                 #
#             April 30, 2019                #
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
library(magrittr)
library(reshape2)


#### ------- read in the data sets -------- ####

# read in the human merged data set (social and qpcr)
human_merged_all_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/spat21_human_merged_all_data_29APR2019.rds")

# read in the qpcr data set with the 34 samples that were re-run






