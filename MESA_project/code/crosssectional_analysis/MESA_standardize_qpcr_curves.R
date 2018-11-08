# ----------------------------------------- #
#       MESA Standardize qPCR curves        #
#            November 7, 2018               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)


#### ------ standardize the qpcr curves ------ ####

# read in the merged data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")

# look at the column names
names(merged_data)







