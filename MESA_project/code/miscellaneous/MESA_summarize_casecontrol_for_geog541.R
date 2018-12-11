# ------------------------------------ #
#   MESA Summarize Case/Control Data   #
#         for GEOG541 Project          #
#           December 3, 2018           #
#             K. Sumner                #
# ------------------------------------ #

#### ----- load the necessary libraries ----- ####
library(tidyverse)
library(tableone)
library(foreign)


#### ----- read in data set ----- ####
# read in the merged data set for the geog 541 project
merged_data = read.dbf("/Users/kelseysumner/Desktop/Fall 2018 Classes/GEOG 541/Class Project/final_qgis_file.dbf")

# create a new variable for if had pfcsp or not in household (binary)
pfcsp_binary = ifelse(merged_data$csp_num>0,1,0)
table(pfcsp_binary, useNA = "always")
length(which(merged_data$csp_num>0))
merged_data$pfcsp_binary = pfcsp_binary

# make a tableone of the data separated out by sample type
CreateTableOne(vars = c("month","gps_coordi","total_anop","Join_Count"),strata = c("pfcsp_binary"),data=merged_data)

# read in the final qpcr merged data set
final_results = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/qPCR_results/final_qpcr_merge.csv")

# calculate the total malaria prevalence based on RDT results
table(final_results$rdt_positive, useNA = "always")
length(which(final_results$rdt_positive=="yes"))/length(which(final_results$rdt_positive=="no" | final_results$rdt_positive == "yes"))

# calculate the total malaria prevalence based on qPCR results
pcr_positive = ifelse(is.na(final_results$pfr364Q1_std) & is.na(final_results$pfr364Q2_std),NA,ifelse(final_results$pfr364Q1_std > 0 | final_results$pfr364Q2_std > 0,"positive","negative"))
table(pcr_positive,useNA = "always")
length(which(final_results$pfr364Q1_std > 0 | final_results$pfr364Q2_std > 0))
length(which(is.na(final_results$pfr364Q1_std) & is.na(final_results$pfr364Q2_std)))
length(which(final_results$pfr364Q1_std > 0 & final_results$pfr364Q2_std > 0))
length(which(pcr_positive == "positive"))/length(which(pcr_positive == "positive" | pcr_positive == "negative"))

# percentage of population that is male
prop.table(table(final_results$sex, useNA = "always"))

# mean age of study population 
mean(final_results$mem_age,na.rm=T)
summary(final_results$mem_age)
