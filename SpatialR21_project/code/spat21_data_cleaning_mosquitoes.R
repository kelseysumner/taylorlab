# ----------------------------------------- #
#        Spat21 Data Set Cleaning           #
#              Mosquito Data                #
#            December 18, 2018              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)


#### --------- read in mosquito data ----------------- ####

# read in the mosquito descriptive data sets
# read in the data set with all mosquito species
allspecies_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/MOZZIECollectionSummary_June2017_July2018.csv")
# read in the data set with only anopheles mosquitoes (Wendy's version that's already converted to long format)
# in stata format
anopheles_data = individual_female_anoph_long

# read in the mosquito qpcr data sets
qpcr_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/Mozzie mosquito compiled detection results 18Dec2018.csv")

# look at summaries of all the data sets
summary(allspecies_data)
summary(anopheles_data)
summary(qpcr_data)
str(allspecies_data)
str(anopheles_data)
str(qpcr_data)

# output a csv file of all the variable names
names1 = names(allspecies_data)
names2 = names(anopheles_data)
names3 = names(qpcr_data)
allnames = c(names1,names2,names3)
allnames = data.frame(allnames)
write_csv(allnames,"spat21_data_mosquito_dictionary.csv")


#### ------------- clean each variable in mosquito data sets ---------------- ####

## -------- allspecies_data

# Household ID
table(anopheles_data$`Household ID`, useNA = "always")
str(anopheles_data$`Household ID`)
attr(anopheles_data$`Household ID`, "labels") 

















