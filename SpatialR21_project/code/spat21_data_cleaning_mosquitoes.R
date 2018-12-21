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
# write_csv(allnames,"spat21_data_mosquito_dictionary.csv")


#### ------------- clean each variable in mosquito data sets ---------------- ####

## ----------- qpcr_data

# look at the overall data set
summary(qpcr_data)

# Sample Name
table(qpcr_data$`Sample Name`, useNA = "always")
str(qpcr_data$`Sample Name`)
# check for duplicate sample names
length(unique(qpcr_data$`Sample Name`)) # 2914 unique 
length(which(is.na(qpcr_data$`Sample Name`) == T)) # 0 missing
count_table = table(qpcr_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no duplicates
# clean up the sample names
new_sample_name = rep(NA,nrow(qpcr_data))
for (i in 1:nrow(qpcr_data)){
  if (nchar(qpcr_data$`Sample Name`[i]) == 9){
    new_name = strsplit(qpcr_data$`Sample Name`[i],"")[[1]]
    new_name_p1 = paste(new_name[1:3], collapse="")
    new_name_p2 = paste(new_name[4:9], collapse="")
    new_sample_name[i] = paste0(new_name_p1," ",new_name_p2)
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 10){
    new_name = strsplit(qpcr_data$`Sample Name`[i]," ")[[1]]
    new_sample_name[i] = paste0(new_name[1]," ",new_name[2])
  }
}
# look at the new_sample_name
table(new_sample_name, useNA = "always")
length(which(is.na(new_sample_name))) # 0 missing
summary(nchar(new_sample_name)) # all sample names 10 characters long as they should be
# add the new sample name to the data set
qpcr_data$`Sample Name` = new_sample_name
head(qpcr_data$`Sample Name`)
str(qpcr_data$`Sample Name`)
# keep as a character

# Experiment Name
table(qpcr_data$`Experiment Name`, useNA = "always")
str(qpcr_data$`Experiment Name`)
# looks good, clean

# Well Position
table(qpcr_data$`Well Position`, useNA = "always")
str(qpcr_data$`Well Position`)
# looks good, clean

# HbtubCT1 & HbtubCT2
table(qpcr_data$HbtubCT1, useNA = "always")
table(qpcr_data$HbtubCT2, useNA = "always")
# some changes need to be made, making below with the Pf CT values

# Pfr364CT1 & Pfr364CT2
table(qpcr_data$pfr364CT1, useNA = "always")
table(qpcr_data$pfr364CT2, useNA = "always")
# some changes need to be made, making below with the Hb CT values

# make sure the qpcr values are numeric and change "Undetermined" to NA to represent missing because undetectable
# make sure all the qpcr values with CT values ==0  are also changed to NA
# first check how many are 0, undetermined, and missing
length(which(qpcr_data$HbtubCT1 == "Undetermined" | qpcr_data$HbtubCT1 == "0" | qpcr_data$HbtubCT1 == "0.000")) # 1742
length(which(qpcr_data$HbtubCT2 == "Undetermined" | qpcr_data$HbtubCT2 == "0" | qpcr_data$HbtubCT2 == "0.000")) # 1742
length(which(qpcr_data$pfr364CT1 == "Undetermined" | qpcr_data$pfr364CT1 == "0" | qpcr_data$pfr364CT1 == "0.000")) # 2653
length(which(qpcr_data$pfr364CT2 == "Undetermined" | qpcr_data$pfr364CT2 == "0" | qpcr_data$pfr364CT2 == "0.000")) # 2646
length(which(is.na(qpcr_data$HbtubCT1))) # 0
length(which(is.na(qpcr_data$HbtubCT2))) # 0
length(which(is.na(qpcr_data$pfr364CT1))) # 0
length(which(is.na(qpcr_data$pfr364CT2))) # 0
# then change the values
qpcr_data$HbtubCT1[qpcr_data$HbtubCT1 == "Undetermined" | qpcr_data$HbtubCT1 == "0" | qpcr_data$HbtubCT1 == "0.000"] = NA
qpcr_data$HbtubCT2[qpcr_data$HbtubCT2 == "Undetermined" | qpcr_data$HbtubCT2 == "0" | qpcr_data$HbtubCT2 == "0.000"] = NA
qpcr_data$pfr364CT1[qpcr_data$pfr364CT1 == "Undetermined" | qpcr_data$pfr364CT1 == "0" | qpcr_data$pfr364CT1 == "0.000"] = NA
qpcr_data$pfr364CT2[qpcr_data$pfr364CT2 == "Undetermined" | qpcr_data$pfr364CT2 == "0" | qpcr_data$pfr364CT2 == "0.000"] = NA
# then check how many are now missing
length(which(qpcr_data$HbtubCT1 == "Undetermined" | qpcr_data$HbtubCT1 == "0" | qpcr_data$HbtubCT1 == "0.000")) # 0
length(which(qpcr_data$HbtubCT2 == "Undetermined" | qpcr_data$HbtubCT2 == "0" | qpcr_data$HbtubCT2 == "0.000")) # 0
length(which(qpcr_data$pfr364CT1 == "Undetermined" | qpcr_data$pfr364CT1 == "0" | qpcr_data$pfr364CT1 == "0.000")) # 0
length(which(qpcr_data$pfr364CT2 == "Undetermined" | qpcr_data$pfr364CT2 == "0" | qpcr_data$pfr364CT2 == "0.000")) # 0
length(which(is.na(qpcr_data$HbtubCT1))) # 1742
length(which(is.na(qpcr_data$HbtubCT2))) # 1742
length(which(is.na(qpcr_data$pfr364CT1))) # 2653
length(which(is.na(qpcr_data$pfr364CT2))) # 2646

# make sure all the CT variables are numeric
qpcr_data$HbtubCT1 = as.numeric(qpcr_data$HbtubCT1)
qpcr_data$HbtubCT2 = as.numeric(qpcr_data$HbtubCT2)
qpcr_data$pfr364CT1 = as.numeric(qpcr_data$pfr364CT1)
qpcr_data$pfr364CT2 = as.numeric(qpcr_data$pfr364CT2)

# look at final summaries of the CT variables
summary(qpcr_data$HbtubCT1)
summary(qpcr_data$HbtubCT2)
summary(qpcr_data$pfr364CT1)
summary(qpcr_data$pfr364CT2)

# pfr364Q1 & pfr364Q2
summary(qpcr_data$pfr364Q1)
summary(qpcr_data$pfr364Q2)
str(qpcr_data$pfr364Q1)
str(qpcr_data$pfr364Q2)
# looks clean but very very low values
# change all the values of Q1 & Q2 that are 0 to missing
length(which(qpcr_data$pfr364Q1 == 0)) # 2653
length(which(qpcr_data$pfr364Q2 == 0)) # 2647
qpcr_data$pfr364Q1[qpcr_data$pfr364Q1 == 0] = NA
qpcr_data$pfr364Q2[qpcr_data$pfr364Q2 == 0] = NA
length(which(qpcr_data$pfr364Q1 == 0)) # 0
length(which(qpcr_data$pfr364Q2 == 0)) # 0
summary(qpcr_data$pfr364Q1)
summary(qpcr_data$pfr364Q2)

# pfr364Y-Intercept
summary(qpcr_data$`pfr364Y-Intercept`)
str(qpcr_data$`pfr364Y-Intercept`)
# looks good, clean

# pfr364R_
summary(qpcr_data$pfr364R_)
str(qpcr_data$pfr364R_)
# look good, clean
# overall, high R2 value for the standards

# pfr364Slope
summary(qpcr_data$pfr364Slope)
str(qpcr_data$pfr364Slope)
# looks good, clean

# Hb standards
# looks like there aren't any Hb standards in the data set

# Pf standards
summary(qpcr_data$pfr364Std1a)
summary(qpcr_data$pfr364Std1b)
summary(qpcr_data$pfr364Std2a)
summary(qpcr_data$pfr364Std2b)
summary(qpcr_data$pfr364Std3a)
summary(qpcr_data$pfr364Std3b)
summary(qpcr_data$pfr364Std4a)
summary(qpcr_data$pfr364Std4b)
summary(as.numeric(qpcr_data$pfr364Std5a))
summary(as.numeric(qpcr_data$pfr364Std5b))
summary(as.numeric(qpcr_data$pfr364Std6a))
summary(as.numeric(qpcr_data$pfr364Std6b))

# create a variable that indicates whether the sample is positive or negative for Pf malaria infection
# if at least 1 duplicate has parasitemia > 0, then saying sample is positive
# 1 is positive, 0 negative
pf_infection_status_sample_level = rep(NA,nrow(qpcr_data))
for (i in 1:nrow(qpcr_data)){
  if (is.na(qpcr_data$pfr364Q1[i]) & is.na(qpcr_data$pfr364Q2[i])){
    pf_infection_status_sample_level[i] = 0
  } else {
    pf_infection_status_sample_level[i] = 1
  }
}
table(pf_infection_status_sample_level,useNA = "always")
# check the output
length(which(qpcr_data$pfr364Q1 > 0 | qpcr_data$pfr364Q2 > 0)) # 333
length(which((is.na(qpcr_data$pfr364Q1) & is.na(qpcr_data$pfr364Q2)))) # 2581
# make a factor
qpcr_data$pf_pcr_infection_status_sample_level = factor(pf_infection_status_sample_level,levels = c(0,1), labels = c("negative", "positive"))
# look at the output
table(qpcr_data$pf_pcr_infection_status_sample_level,useNA = "always")

# check to make sure there's never a time where Pf CT 1 is positive and Q1 isn't positive and Pf CT2 is positive and Q2 isn't positive
length(which(qpcr_data$pfr364CT1 > 0 & is.na(qpcr_data$pfr364Q1))) # 0 times, looks good
length(which(qpcr_data$pfr364Q1 > 0 & is.na(qpcr_data$pfr364CT1))) # 0 times, looks good
length(which(qpcr_data$pfr364CT2 > 0 & is.na(qpcr_data$pfr364Q2))) # 1 time, check this case
length(which(qpcr_data$pfr364Q2 > 0 & is.na(qpcr_data$pfr364CT2))) # 0 times, looks good
checkdata = qpcr_data[which(as.numeric(qpcr_data$pfr364CT2) > 0 & is.na(qpcr_data$pfr364Q2)),]
# a Q parasitemia value of 0 was calculated for this sample, count this as negative for P. falciparum
# this sample was M06 A00026

# create a combined parasitemia new variable (combining Q1 and Q2)
# if both positive parasitemia for both, then is an average of the two parasitemia for the duplicates.
# if only 1 positive parasitemia then is the value of the positive parasitemia
pfr364Q_combined = rep(NA,nrow(qpcr_data))
for (k in 1:nrow(qpcr_data)){
  if (qpcr_data$pf_pcr_infection_status_sample_level[k] == "positive" & !(is.na(qpcr_data$pf_pcr_infection_status_sample_level[k]))){
    pfr364Q_combined[k] = (sum(qpcr_data$pfr364Q1[k],qpcr_data$pfr364Q2[k],na.rm = T))/(2-(is.na(qpcr_data$pfr364Q1[k])+is.na(qpcr_data$pfr364Q2[k])))
  } else
    pfr364Q_combined[k] = NA
}
summary(pfr364Q_combined,useNA = "always")
# add to the data set
qpcr_data$pfr364Q_combined = pfr364Q_combined
# check the output
summary(qpcr_data$pfr364Q1)
summary(qpcr_data$pfr364Q2)
summary(qpcr_data$pfr364Q_combined)
checkdata = qpcr_data[which(!(is.na(qpcr_data$pfr364Q_combined))),]
head(checkdata$HbtubCT1)
head(checkdata$HbtubCT2)
head(checkdata$pfr364CT1)
head(checkdata$pfr364CT2)
head(checkdata$pf_pcr_infection_status_sample_level)
head(checkdata$pfr364Q_combined)

# final check for samples to change to missing
# read back in the original qpcr data set but rename to merged_data
merged_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/Mozzie mosquito compiled detection results 18Dec2018.csv")
# clean up the sample names
new_sample_name = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (nchar(merged_data$`Sample Name`[i]) == 9){
    new_name = strsplit(merged_data$`Sample Name`[i],"")[[1]]
    new_name_p1 = paste(new_name[1:3], collapse="")
    new_name_p2 = paste(new_name[4:9], collapse="")
    new_sample_name[i] = paste0(new_name_p1," ",new_name_p2)
  }
  if (nchar(merged_data$`Sample Name`[i]) == 10){
    new_name = strsplit(merged_data$`Sample Name`[i]," ")[[1]]
    new_sample_name[i] = paste0(new_name[1]," ",new_name[2])
  }
}
# look at the new_sample_name
table(new_sample_name, useNA = "always")
length(which(is.na(new_sample_name))) # 0 missing
summary(nchar(new_sample_name)) # all sample names 10 characters long as they should be
# add the new sample name to the data set
merged_data$`Sample Name` = new_sample_name
# now pull out the samples that had CT values of 0, indicting run failure
orig_zeroes_1 = merged_data[which(as.numeric(merged_data$pfr364CT1) == 0 | as.numeric(merged_data$HbtubCT1) == 0),]
orig_zeroes_2 = merged_data[which(as.numeric(merged_data$pfr364CT2) == 0 | as.numeric(merged_data$HbtubCT2) == 0),]
# pull out the sample name for the samples with CT values of 0 for Pf and Hb indicating it was probably a run failure
orig_zeroes_1_labid = orig_zeroes_1$`Sample Name`
orig_zeroes_2_labid = orig_zeroes_2$`Sample Name`
# subset the standardized parasitemia data set to look at these labid values
subset1 = qpcr_data[which(qpcr_data$`Sample Name` %in% orig_zeroes_1_labid),]
subset2 = qpcr_data[which(qpcr_data$`Sample Name` %in% orig_zeroes_2_labid),]
# pull out the values that had 0s for both CT values
orig_zeroes_all = merged_data[which(as.numeric(merged_data$pfr364CT1) == 0 & as.numeric(merged_data$HbtubCT1) == 0 & as.numeric(merged_data$pfr364CT2) == 0 & as.numeric(merged_data$HbtubCT2) == 0),]
# these values were all changed to negative for pf_pcr_infection_status but 
# one value had zeroes for all Hb and Pf CT 1 and 2 values, which means they did not amplify correctly 
# and have been changed from "negative" to "missing" for the pf_pcr_infection_status column
# this value is M15 H00056
qpcr_data$pf_pcr_infection_status_sample_level[qpcr_data$`Sample Name` == "M15 H00056"] = NA
# check the change
qpcr_data$pf_pcr_infection_status_sample_level[which(qpcr_data$`Sample Name` == "M15 H00056")]

# look at the new summary of pf_pcr_infection_status_sample_level
summary(qpcr_data$pf_pcr_infection_status_sample_level)
table(qpcr_data$pf_pcr_infection_status_sample_level, useNA = "always")

# export the data set as a CSV file and RDS file
write_csv(qpcr_data,"spat21_mosquito_qpcr_data_21DEC2018.csv")
write_rds(qpcr_data,"spat21_mosquito_qpcr_data_21DEC2018.RDS")
