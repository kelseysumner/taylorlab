# ----------------------------------------- #
#        Spat21 Data Set Manipulation       #
#        Pulling May-July for Verona        #
#              Mosquito Data                #
#            January 18, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


#### --------- read in merged female anoph mosquito data ----------------- ####

# read in the female anopheles merged data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the all species data
allspecies_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/spat21_allspecies_mosquito_data_18JAN2019.RDS")


#### ------- subset the data to just May to July 2018 -------- ####

# first do the female anopheles data

# look at how many observations fall within the collection month/year combo
table(anoph_merged_data$collection_month_year_combo, useNA = "always") # 394 mosquitoes

# subset the data
may_july_subset = anoph_merged_data[which(anoph_merged_data$collection_month_year_combo == "5-2018" |
                                            anoph_merged_data$collection_month_year_combo == "6-2018" | 
                                            anoph_merged_data$collection_month_year_combo == "7-2018"),]

# looks like 394 mosquitoes were pulled out, which is correct
# will look at a quick summary of the output
summary(may_july_subset)
table(may_july_subset$collection_month_year_combo, useNA = "always")
# looks good

# export the data set
write_csv(may_july_subset,"spat21_female_anopheles_merged_data_forVerona_MaytoJuly_2018.csv")
write_rds(may_july_subset,"spat21_female_anopheles_merged_data_forVerona_MaytoJuly_2018.RDS")


