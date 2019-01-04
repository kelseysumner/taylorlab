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
library(stringr)


#### --------- read in mosquito data ----------------- ####

# read in the mosquito descriptive data sets
# read in the data set with all mosquito species
allspecies_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/raw data/MOZZIECollectionSummary_June2017_July2018.csv")
# read in the data set with only anopheles mosquitoes (Wendy's version that's already converted to long format)
# in stata format
anopheles_data_long = Individual_female_anoph_long
# this is the original version
anopheles_data_original = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/raw data/old/MOZZIEFemaleAnophele_June2017_July2018.csv")

# read in the mosquito qpcr data sets
qpcr_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/raw data/Mozzie mosquito compiled detection results 18Dec2018.csv")

# look at summaries of all the data sets
summary(allspecies_data)
summary(anopheles_data_original)
summary(anopheles_data_long)
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
merged_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/raw data/Mozzie mosquito compiled detection results 18Dec2018.csv")
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

# create a variable that indicates whether the sample is positive or negative for Human beta-tubulin
# if at least 1 duplicate has Hb CT > 0, then saying sample is positive
# 1 is positive, 0 negative
hb_status_sample_level = rep(NA,nrow(qpcr_data))
for (i in 1:nrow(qpcr_data)){
  if (is.na(qpcr_data$HbtubCT1[i]) & is.na(qpcr_data$HbtubCT2[i])){
    hb_status_sample_level[i] = 0
  } else {
    hb_status_sample_level[i] = 1
  }
}
table(hb_status_sample_level,useNA = "always")
# check the output
length(which(qpcr_data$HbtubCT1 > 0 | qpcr_data$HbtubCT2 > 0)) # 1372
length(which((is.na(qpcr_data$HbtubCT1) & is.na(qpcr_data$HbtubCT2)))) # 1542
# make a factor
qpcr_data$hb_status_sample_level = factor(hb_status_sample_level,levels = c(0,1), labels = c("negative", "positive"))
# look at the output
table(qpcr_data$hb_status_sample_level,useNA = "always")

# these values were all changed to negative for hb_status_sample_level but 
# one value had zeroes for all Hb and Pf CT 1 and 2 values, which means they did not amplify correctly 
# and have been changed from "negative" to "missing" for the hb_status_sample_level column
# this value is M15 H00056
qpcr_data$hb_status_sample_level[qpcr_data$`Sample Name` == "M15 H00056"] = NA
# check the change
qpcr_data$hb_status_sample_level[which(qpcr_data$`Sample Name` == "M15 H00056")]

# look at the new summary of pf_pcr_infection_status_sample_level and hb_status_sample_level
summary(qpcr_data$pf_pcr_infection_status_sample_level)
table(qpcr_data$pf_pcr_infection_status_sample_level, useNA = "always")
summary(qpcr_data$hb_status_sample_level)
table(qpcr_data$hb_status_sample_level, useNA = "always")

# look at summaries of the CT values
summary(qpcr_data$HbtubCT1)
summary(qpcr_data$HbtubCT2)
summary(qpcr_data$pfr364CT1)
summary(qpcr_data$pfr364CT2)

# export the data set as a CSV file and RDS file
# write_csv(qpcr_data,"spat21_mosquito_qpcr_data_22DEC2018.csv")
# write_rds(qpcr_data,"spat21_mosquito_qpcr_data_22DEC2018.RDS")

# create another variable that is the sample_id_mosquito
table(nchar(qpcr_data$`Sample Name`))
new_sample_name = rep(NA,nrow(qpcr_data))
for (i in 1:nrow(qpcr_data)){
    new_name = strsplit(qpcr_data$`Sample Name`[i],"")[[1]]
    new_name_p1 = paste(new_name[1:3], collapse="")
    new_name_p2 = paste(new_name[6:10], collapse="")
    new_sample_name[i] = paste0(new_name_p1," ",new_name_p2)
}
# look at the new_sample_name
table(new_sample_name, useNA = "always")
length(which(is.na(new_sample_name))) # no missing, which is good
# add the variable to the data set as sample_id_mosquito
qpcr_data$sample_id_mosquito = new_sample_name

# take out the standards, experiment name, well position, and Intercept, R, and Slope information because no longer needed
colnames_to_exclude_in_qpcr = c("Experiment Name","Well Position","pfr364Y-Intercept","pfr364R_","pfr364Slope","pfr364Std1a",
                                "pfr364Std1b","pfr364Std2a","pfr364Std2b","pfr364Std3a","pfr364Std3b","pfr364Std4a","pfr364Std4b",
                                "pfr364Std5a","pfr364Std5b","pfr364Std6a","pfr364Std6b")
qpcr_data = qpcr_data[,-which(colnames(qpcr_data) %in% colnames_to_exclude_in_qpcr)]

# check the number of sample ids that only occur once (only received a head or an abdomen)
number_1 = table(qpcr_data$sample_id_mosquito, useNA = "always")
number_1[which(number_1 == 1)]
length(number_1[which(number_1 == 1)]) # 46 sample ids only occur once
once_table = number_1[which(number_1 == 1)]
length(number_1[which(number_1 == 2)]) # 1434 sample ids occur twice
twice_table = number_1[which(number_1 == 2)]
length(number_1[which(number_1 > 2)]) # 0 sample ids occur more than twice
# looks like we have 1480 unique sample ids in this data set
# when switch the data to wide format should have 1480 rows

# now make the data in wide format by sample_id_mosquito
# first sort the data set by the sample_id_mosquito and create index number
result <- qpcr_data %>% 
  group_by(sample_id_mosquito) %>%
  arrange(desc(`Sample Name`)) %>%
  mutate(eval_index = row_number()) %>%
  ungroup %>%
  arrange(sample_id_mosquito)
# loop through each row, and check if the corresponding sample id
new_row = rep(0,21)
for (i in 1:nrow(result)){
  if (result$eval_index[i] == 1 & result$eval_index[i+1] == 2 & str_count(result$`Sample Name`[i], "H") > 0){
    next_row = c(result[i,11],result[i,1:10],result[i+1,1:10])
  } else if (result$eval_index[i] == 1 & result$eval_index[i+1] != 2 & str_count(result$`Sample Name`[i], "H") > 0) {
    next_row = c(result[i,11],result[i,1:10],rep(NA,10))
  } else if (result$eval_index[i] == 1 & result$eval_index[i+1] != 2 & str_count(result$`Sample Name`[i], "A") > 0){
    next_row = c(result[i,11],rep(NA,10),result[i,1:10])
  } else {
    next
  }
  new_row = rbind(new_row,next_row)
}
# remove the first row
new_row = new_row[-1,]
# make it a data frame
qpcr_data_wide = as_data_frame(new_row)
str(qpcr_data_wide)
colnames(qpcr_data_wide)

# rename the column names
qpcr_data_wide = rename(qpcr_data_wide, "sample_id_head" = "Sample Name", "HbtubCT1_h" = "HbtubCT1", "HbtubCT2_h" = "HbtubCT2",
                        "pfr364CT1_h" = "pfr364CT1", "pfr364CT2_h" = "pfr364CT2", "pfr364Q1_h" = "pfr364Q1", "pfr364Q2_h" = "pfr364Q2",
                        "pf_pcr_infection_status_sample_level_h" = "pf_pcr_infection_status_sample_level", "pfr364Q_combined_h" = "pfr364Q_combined",
                        "hb_status_sample_level_h" = "hb_status_sample_level","sample_id_abdomen" = "Sample Name1", "HbtubCT1_a" = "HbtubCT11", "HbtubCT2_a" = "HbtubCT21",
                        "pfr364CT1_a" = "pfr364CT11", "pfr364CT2_a" = "pfr364CT21", "pfr364Q1_a" = "pfr364Q11", "pfr364Q2_a" = "pfr364Q21",
                        "pf_pcr_infection_status_sample_level_a" = "pf_pcr_infection_status_sample_level1", "pfr364Q_combined_a" = "pfr364Q_combined1",
                        "hb_status_sample_level_a" = "hb_status_sample_level1")
colnames(qpcr_data_wide)

# reformat the variables because formating was messed up in the processing
qpcr_data_wide$sample_id_mosquito = as.character(qpcr_data_wide$sample_id_mosquito)
qpcr_data_wide$sample_id_abdomen = as.character(qpcr_data_wide$sample_id_abdomen)
qpcr_data_wide$HbtubCT1_a = as.numeric(qpcr_data_wide$HbtubCT1_a)
qpcr_data_wide$HbtubCT2_a = as.numeric(qpcr_data_wide$HbtubCT2_a)
qpcr_data_wide$pfr364CT1_a = as.numeric(qpcr_data_wide$pfr364CT1_a)
qpcr_data_wide$pfr364CT2_a = as.numeric(qpcr_data_wide$pfr364CT2_a)
qpcr_data_wide$pfr364Q1_a = as.numeric(qpcr_data_wide$pfr364Q1_a)
qpcr_data_wide$pfr364Q2_a = as.numeric(qpcr_data_wide$pfr364Q2_a)
qpcr_data_wide$pf_pcr_infection_status_sample_level_a = factor(qpcr_data_wide$pf_pcr_infection_status_sample_level_a, levels = c(1,2), labels = c("negative", "positive"))
qpcr_data_wide$pfr364Q_combined_a = as.numeric(qpcr_data_wide$pfr364Q_combined_a)
qpcr_data_wide$hb_status_sample_level_a = factor(qpcr_data_wide$hb_status_sample_level_a, levels = c(1,2), labels = c("negative", "positive"))
qpcr_data_wide$sample_id_head = as.character(qpcr_data_wide$sample_id_head)
qpcr_data_wide$HbtubCT1_h = as.numeric(qpcr_data_wide$HbtubCT1_h)
qpcr_data_wide$HbtubCT2_h = as.numeric(qpcr_data_wide$HbtubCT2_h)
qpcr_data_wide$pfr364CT1_h = as.numeric(qpcr_data_wide$pfr364CT1_h)
qpcr_data_wide$pfr364CT2_h = as.numeric(qpcr_data_wide$pfr364CT2_h)
qpcr_data_wide$pfr364Q1_h = as.numeric(qpcr_data_wide$pfr364Q1_h)
qpcr_data_wide$pfr364Q2_h = as.numeric(qpcr_data_wide$pfr364Q2_h)
qpcr_data_wide$pf_pcr_infection_status_sample_level_h = factor(qpcr_data_wide$pf_pcr_infection_status_sample_level_h, levels = c(1,2), labels = c("negative", "positive"))
qpcr_data_wide$pfr364Q_combined_h = as.numeric(qpcr_data_wide$pfr364Q_combined_h)
qpcr_data_wide$hb_status_sample_level_h = factor(qpcr_data_wide$hb_status_sample_level_h, levels = c(1,2), labels = c("negative", "positive"))

# check missingness in the IDs
length(which(is.na(qpcr_data_wide$sample_id_mosquito))) # no missing, good
length(which(qpcr_data_wide$sample_id_mosquito == "NA"))
length(which(is.na(qpcr_data_wide$sample_id_abdomen))) 
length(which(qpcr_data_wide$sample_id_abdomen == "NA")) # 30 NAs needs to be recoded
length(which(is.na(qpcr_data_wide$sample_id_head))) 
length(which(qpcr_data_wide$sample_id_head == "NA")) # 16 NAs need to be recoded
# recode NAs to missing 
qpcr_data_wide$sample_id_head[qpcr_data_wide$sample_id_head == "NA"] = NA
qpcr_data_wide$sample_id_abdomen[qpcr_data_wide$sample_id_abdomen == "NA"] = NA
length(which(is.na(qpcr_data_wide$sample_id_head))) # 16 missing, correct
length(which(is.na(qpcr_data_wide$sample_id_abdomen))) # 30 missing, correct

# check how many heads and abdomens were in the original data set
sum(str_count(qpcr_data$`Sample Name`, "H")) # 1464 heads out of 1480 mosquitoes (16 missing)
sum(str_count(qpcr_data$`Sample Name`, "A")) # 1450 abdomens out of 1480 mosquitoes (30 missing)

# check if any heads and abdomens in the wrong columns
sum(str_count(qpcr_data_wide$sample_id_abdomen, "H"))
sum(str_count(qpcr_data_wide$sample_id_head, "A"))
# looks good

# create a new column that tells whether or not the mosquito is positive for malaria
# if at least 1 mosquito part (head or abdomen) is positive, then saying mosquito is positive
# 1 is positive, 0 negative
pf_infection_status_mosquito_level = ifelse((qpcr_data_wide$pf_pcr_infection_status_sample_level_a == "negative" & qpcr_data_wide$pf_pcr_infection_status_sample_level_h == "negative") |
                                              (qpcr_data_wide$pf_pcr_infection_status_sample_level_a == "negative" & is.na(qpcr_data_wide$pf_pcr_infection_status_sample_level_h)) |
                                              (qpcr_data_wide$pf_pcr_infection_status_sample_level_h == "negative" & is.na(qpcr_data_wide$pf_pcr_infection_status_sample_level_a)),0,1)
table(pf_infection_status_mosquito_level,useNA = "always")
# make a factor
qpcr_data_wide$pf_infection_status_mosquito_level = factor(pf_infection_status_mosquito_level, levels = c(0,1), labels = c("negative", "positive"))
# check the recode
table(qpcr_data_wide$pf_infection_status_mosquito_level, useNA = "always")
length(which(is.na(qpcr_data_wide$pf_pcr_infection_status_sample_level_a))) # 30
length(which(is.na(qpcr_data_wide$pf_pcr_infection_status_sample_level_h))) # 17
length(which(is.na(qpcr_data_wide$pf_pcr_infection_status_sample_level_a) & is.na(qpcr_data_wide$pf_pcr_infection_status_sample_level_h))) # 0
length(which(qpcr_data_wide$pf_pcr_infection_status_sample_level_a == "positive" | qpcr_data_wide$pf_pcr_infection_status_sample_level_h == "positive")) # 244

# create a new column that tells whether or not the mosquito is positive for human beta tubulin
# if at least 1 mosquito part (head or abdomen) is positive, then saying mosquito is positive
# 1 is positive, 0 negative
hb_status_mosquito_level = ifelse((qpcr_data_wide$hb_status_sample_level_a == "negative" & qpcr_data_wide$hb_status_sample_level_h == "negative") |
                                              (qpcr_data_wide$hb_status_sample_level_a == "negative" & is.na(qpcr_data_wide$hb_status_sample_level_h)) |
                                              (qpcr_data_wide$hb_status_sample_level_h == "negative" & is.na(qpcr_data_wide$hb_status_sample_level_a)),0,1)
table(hb_status_mosquito_level,useNA = "always")
# make a factor
qpcr_data_wide$hb_status_mosquito_level = factor(hb_status_mosquito_level, levels = c(0,1), labels = c("negative", "positive"))
# check the recode
table(qpcr_data_wide$hb_status_mosquito_level, useNA = "always")
length(which(is.na(qpcr_data_wide$hb_status_sample_level_h))) # 30
length(which(is.na(qpcr_data_wide$hb_status_sample_level_a))) # 17
length(which(is.na(qpcr_data_wide$hb_status_sample_level_h) & is.na(qpcr_data_wide$hb_status_sample_level_a))) # 0
length(which(qpcr_data_wide$hb_status_sample_level_h == "positive" | qpcr_data_wide$hb_status_sample_level_a == "positive")) # 244

# export the data set as a CSV file and RDS file
# write_csv(qpcr_data_wide,"spat21_mosquito_qpcr_data_wide_3JAN2019.csv")
# write_rds(qpcr_data_wide,"spat21_mosquito_qpcr_data_wide_3JAN2019.RDS")  
  
  
  
  
## ------------ anopheles data

# take out the variables that aren't needed in the wide anopheles data set (anopheles_data_original)
vars_to_exclude = c("Repeat Instrument","Collection Time","Collection done by:","Samples prepared by:",
                    "Species ID done by","Comment 1","Comment 2","Comment 3","Comment 4","Comment 5",
                    "Comment 6","Comment 7","Comment 8","Comment 9","Comment 10","Comment 11","Comment 12",
                    "Comment 13","Comment 14","Comment 15","Comment 16","Form Checked by:","Form checked date",
                    "Form Entered By:","Form Entered on:","Complete?")
short_data = anopheles_data_original[,-which(colnames(anopheles_data_original) %in% vars_to_exclude)]

# switch from wide to long format using a form loop
new_long_data = c(rep(0,10))
for (i in 1:nrow(short_data)){
  new_row_1 = c(short_data[i,1:5],short_data[i,6:10])
  new_row_2 = c(short_data[i,1:5],short_data[i,11:15])
  new_row_3 = c(short_data[i,1:5],short_data[i,16:20])
  new_row_4 = c(short_data[i,1:5],short_data[i,21:25])
  new_row_5 = c(short_data[i,1:5],short_data[i,26:30])
  new_row_6 = c(short_data[i,1:5],short_data[i,31:35])
  new_row_7 = c(short_data[i,1:5],short_data[i,36:40])
  new_row_8 = c(short_data[i,1:5],short_data[i,41:45])
  new_row_9 = c(short_data[i,1:5],short_data[i,46:50])
  new_row_10 = c(short_data[i,1:5],short_data[i,51:55])
  new_row_11 = c(short_data[i,1:5],short_data[i,56:60])
  new_row_12 = c(short_data[i,1:5],short_data[i,61:65])
  new_row_13 = c(short_data[i,1:5],short_data[i,66:70])
  new_row_14 = c(short_data[i,1:5],short_data[i,71:75])
  new_row_15 = c(short_data[i,1:5],short_data[i,76:80])
  new_row_16 = c(short_data[i,1:5],short_data[i,81:85])
  new_long_data = rbind(new_long_data,new_row_1, new_row_2, new_row_3, new_row_4, new_row_5, new_row_6, new_row_7, new_row_8, 
                        new_row_9, new_row_10, new_row_11, new_row_12, new_row_13, new_row_14, new_row_15, new_row_16)
}
# remove the first row
new_long_data = new_long_data[-1,]
# make it a data frame
new_long_data = as_data_frame(new_long_data)
str(new_long_data)
colnames(new_long_data)
# remove rows with all missing observations for columns 6:10
test = new_long_data[-which(is.na(new_long_data$`Sample ID Head #1`) & is.na(new_long_data$`Sample ID Abdomen#1`) & is.na(new_long_data$`Abdominal status #1`) & is.na(new_long_data$`Species type #1`) & is.na(new_long_data$`Specify Species #1`)),]
length(which(is.na(new_long_data$`Sample ID Head #1`) & is.na(new_long_data$`Sample ID Abdomen#1`) & is.na(new_long_data$`Abdominal status #1`) & is.na(new_long_data$`Species type #1`) & is.na(new_long_data$`Specify Species #1`)))
# 9209 observations should have been removed from the original for loop output for having all missing
# looks like those were removed correctly
new_long_data = test

# change the column names
colnames(new_long_data)
new_long_data = rename(new_long_data, "HH_ID" = "Household ID", "repeat_instance" = "Repeat Instance",
                       "collection_date" = "Collection Date", "village" = "Village", "total_num_mosq_in_hh" = "Total Number of mosquitos in the household",
                       "sample_id_head" = "Sample ID Head #1", "sample_id_abdomen" = "Sample ID Abdomen#1","abdominal_status" = "Abdominal status #1",
                       "species_type" = "Species type #1", "specify_species" = "Specify Species #1")
colnames(new_long_data)

# make all the columns a specific data type
new_long_data$HH_ID = as.character(new_long_data$HH_ID)
new_long_data$repeat_instance = as.numeric(new_long_data$repeat_instance)
new_long_data$collection_date = as.character(new_long_data$collection_date)
new_long_data$village = as.character(new_long_data$village)
new_long_data$total_num_mosq_in_hh = as.numeric(new_long_data$total_num_mosq_in_hh)
new_long_data$sample_id_head = as.character(new_long_data$sample_id_head)
new_long_data$sample_id_abdomen = as.character(new_long_data$sample_id_abdomen)
new_long_data$abdominal_status = as.character(new_long_data$abdominal_status)
new_long_data$species_type = as.character(new_long_data$species_type)
new_long_data$specify_species = as.character(new_long_data$specify_species)

# look at each variable in the long data set
colnames(new_long_data)

# HH_ID
table(new_long_data$HH_ID, useNA = "always")
str(new_long_data$HH_ID)
# looks good, clean

# repeat_instance
table(new_long_data$repeat_instance, useNA = "always")
str(new_long_data$repeat_instance)
# looks good, clean

# collection_date
table(new_long_data$collection_date, useNA = "always")
str(new_long_data$collection_date)
# change to date format
test_date = mdy(new_long_data$collection_date)
head(test_date)
head(new_long_data$collection_date)
tail(test_date)
tail(new_long_data$collection_date)
table(test_date, useNA = "always")
new_long_data$collection_date = test_date
str(new_long_data$collection_date)

# create week, month, and year variables from collection_date
# for week
collection_week = epiweek(new_long_data$collection_date)
head(collection_week)
head(new_long_data$collection_date)
new_long_data$collection_week = collection_week
# for month
collection_month = month(new_long_data$collection_date)
head(collection_month)
head(new_long_data$collection_date)
new_long_data$collection_month = collection_month
summary(new_long_data$collection_month)
table(new_long_data$collection_month, useNA = "always")
# create a factor 
new_long_data$collection_month = factor(new_long_data$collection_month, levels = c(1:12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
table(new_long_data$collection_month, useNA = "always")
# for year
collection_year = epiyear(new_long_data$collection_date)
head(collection_year)
head(new_long_data$collection_date)
new_long_data$collection_year = collection_year
summary(new_long_data$collection_year)
# create a variable that is month and year combined
collection_month_year_combo = paste0(collection_month,"-",collection_year)
head(collection_month_year_combo)
head(new_long_data$collection_date)
new_long_data$collection_month_year_combo = collection_month_year_combo
table(new_long_data$collection_month_year_combo, useNA = "always")

# village
table(new_long_data$village, useNA = "always")
str(new_long_data$village)
# make a factor
new_long_data$village = as.factor(new_long_data$village)
str(new_long_data$village)

# total_num_mosq_in_hh
table(new_long_data$total_num_mosq_in_hh, useNA = "always")
str(new_long_data$total_num_mosq_in_hh)
summary(new_long_data$total_num_mosq_in_hh)
# looks good, clean

# sample_id_head
table(new_long_data$sample_id_head, useNA = "always")
str(new_long_data$sample_id_head)
# check for duplicate sample names
length(unique(new_long_data$sample_id_head)) # 1493 unique 
length(which(is.na(new_long_data$sample_id_head) == T)) # 0 missing
count_table = table(new_long_data$sample_id_head, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are two duplicates
# the two duplicates: M03 H00021, M13 H00035
# M03 H00021 looks like it was just entered twice, will remove one row
# M13 H00035 looks like it should be M16 H00035 in one of the rows (the blood fed one), changed this in the long data set
# look where these duplicates occurred
dup_data = new_long_data[which(new_long_data$sample_id_head == "M03 H00021" | new_long_data$sample_id_head == "M13 H00035"),]
new_long_data$sample_id_head[new_long_data$sample_id_head == "M13 H00035" & new_long_data$repeat_instance == 22] = "M16 H00035"
# also did made the change for the sample_id_abdomen for M13 H00035
new_long_data$sample_id_abdomen[new_long_data$sample_id_abdomen == "M13 A00035" & new_long_data$repeat_instance == 22] = "M16 A00035"
new_long_data = new_long_data[-which(new_long_data$sample_id_head == "M03 H00021" & new_long_data$repeat_instance == 9),]
# check to see if any duplicates are left
length(unique(new_long_data$sample_id_head)) # 1494 unique 
length(which(is.na(new_long_data$sample_id_head) == T)) # 0 missing
count_table = table(new_long_data$sample_id_head, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are no duplicates left
# clean up the sample names
new_sample_name = rep(NA,nrow(new_long_data))
for (i in 1:nrow(new_long_data)){
  if (nchar(new_long_data$sample_id_head[i]) == 9){
    new_name = strsplit(new_long_data$sample_id_head[i],"")[[1]]
    new_name_p1 = paste(new_name[1:3], collapse="")
    new_name_p2 = paste(new_name[4:9], collapse="")
    new_sample_name[i] = paste0(new_name_p1," ",new_name_p2)
  }
  if (nchar(new_long_data$sample_id_head[i]) == 10 & new_long_data$sample_id_head[i] != "K1 H 00027"){
    new_name = strsplit(new_long_data$sample_id_head[i]," ")[[1]]
    new_sample_name[i] = paste0(new_name[1]," ",new_name[2])
  }
  if (nchar(new_long_data$sample_id_head[i]) == 11){
    new_name = strsplit(new_long_data$sample_id_head[i]," ")[[1]]
    new_sample_name[i] = paste0(new_name[1]," ",new_name[2],new_name[3])
  }
  if (nchar(new_long_data$sample_id_head[i]) == 10 & new_long_data$sample_id_head[i] == "K1 H 00027"){
    new_sample_name[i] = "K14 H00027"
  }
}
# look at the new_sample_name
table(new_sample_name, useNA = "always")
length(which(is.na(new_sample_name))) # 0 missing
table(nchar(new_sample_name)) # 1 sample is 4 characters long
# pull out the sample that is 4 characters long
new_sample_name[nchar(new_sample_name)==4]
table(nchar(new_long_data$sample_id_head))
# add the new sample name to the data set
new_long_data$new_sample_name = new_sample_name
# figure out where the 4 character sample id head is coming in
four_character = new_long_data[which(nchar(new_long_data$new_sample_name) == 4),]
# K1 H was the four character ID
# sample ID head K1 H 00027 seems to be a typo. Changed to K14 H00027 to match the household and abdomen IDs
new_long_data$new_sample_name[new_long_data$new_sample_name == "K1 H"] = "K14 H00027"
summary(nchar(new_long_data$new_sample_name)) # all sample names 10 characters long as they should be
# now add the new_sample name to the data set
new_long_data$sample_id_head = new_long_data$new_sample_name
head(new_long_data$sample_id_head)
str(new_long_data$sample_id_head)
summary(nchar(new_long_data$sample_id_head))
# keep as a character

# sample_id_abdomen
table(new_long_data$sample_id_abdomen, useNA = "always")
str(new_long_data$sample_id_abdomen)
# check for duplicate sample names
length(unique(new_long_data$sample_id_abdomen)) # 1493 unique (remember already fixed M13 A00035 duplicate)
length(which(is.na(new_long_data$sample_id_abdomen) == T)) # 1 missing
count_table = table(new_long_data$sample_id_abdomen, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there is 1 duplicate left
# the duplicate is: K05 A 00005
# look where these duplicates occurred
dup_data2 = new_long_data[which(new_long_data$sample_id_abdomen == "K05 A 00005"),]
new_long_data$sample_id_abdomen[new_long_data$sample_id_abdomen == "K05 A 00005" & new_long_data$repeat_instance == 4] = "K05 A00008"
# missing value should be missing, so keep missing
# check to see if any duplicates are left
length(unique(new_long_data$sample_id_abdomen)) # 1494 unique 
length(which(is.na(new_long_data$sample_id_abdomen) == T)) # 1 missing, correct
count_table = table(new_long_data$sample_id_abdomen, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are no duplicates left
# put in a placeholder for row 85 which is the missing adomen
new_long_data$sample_id_abdomen[new_long_data$sample_id_head == "S02 H00001" & new_long_data$repeat_instance == 6] = "PLA CEHOLD"
# clean up the sample names
new_sample_name = rep(NA,nrow(new_long_data))
for (i in 1:nrow(new_long_data)){
  if (nchar(new_long_data$sample_id_abdomen[i]) == 9 & !(is.na(new_long_data$sample_id_abdomen))){
    new_name = strsplit(new_long_data$sample_id_abdomen[i],"")[[1]]
    new_name_p1 = paste(new_name[1:3], collapse="")
    new_name_p2 = paste(new_name[4:9], collapse="")
    new_sample_name[i] = paste0(new_name_p1," ",new_name_p2)
  } 
  if (nchar(new_long_data$sample_id_abdomen[i]) == 10 & !(is.na(new_long_data$sample_id_abdomen))){
    new_name = strsplit(new_long_data$sample_id_abdomen[i]," ")[[1]]
    new_sample_name[i] = paste0(new_name[1]," ",new_name[2])
  } 
  if (nchar(new_long_data$sample_id_abdomen[i]) == 11 & !(is.na(new_long_data$sample_id_abdomen))){
    new_name = strsplit(new_long_data$sample_id_abdomen[i]," ")[[1]]
    new_sample_name[i] = paste0(new_name[1]," ",new_name[2],new_name[3])
  }
}
# look at the new_sample_name
table(new_sample_name, useNA = "always")
length(which(is.na(new_sample_name))) # 0 missing (because of placeholder at row 85)
table(nchar(new_sample_name)) # all 10 characters long
# now add the new_sample name to the data set
new_long_data$sample_id_abdomen = new_sample_name
head(new_long_data$sample_id_abdomen)
str(new_long_data$sample_id_abdomen)
summary(nchar(new_long_data$sample_id_abdomen))
# keep as a character

# abdominal_status and species_type
table(new_long_data$abdominal_status, useNA = "always")
str(new_long_data$abdominal_status)
# looks like some species info in this column
status_testdata = new_long_data[which(new_long_data$abdominal_status == "An. funestus" | new_long_data$abdominal_status == "An. gambiae"),]
# looks like the abdominal status and species information were switched for some cases
# fix these columns
# abdominal_status
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00031" & new_long_data$repeat_instance == 4] = "Gravid"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M14 H00018" & new_long_data$repeat_instance == 3] = "Blood Fed"
new_long_data$abdominal_status[new_long_data$sample_id_head == "K01 H00026" & new_long_data$repeat_instance == 3] = "Gravid"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00047" & new_long_data$repeat_instance == 5] = "Blood Fed"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M09 H00038" & new_long_data$repeat_instance == 4] = "Blood Fed"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M14 H00031" & new_long_data$repeat_instance == 4] = "Unfed"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00062" & new_long_data$repeat_instance == 6] = "Half Gravid"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00092" & new_long_data$repeat_instance == 9] = "Blood Fed"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00109" & new_long_data$repeat_instance == 11] = "Half Gravid"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00128" & new_long_data$repeat_instance == 14] = "Half Gravid"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M09 H00103" & new_long_data$repeat_instance == 26] = "Gravid"
new_long_data$abdominal_status[new_long_data$sample_id_head == "M07 H00185" & new_long_data$repeat_instance == 33] = "Unfed"
# species_type
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00031" & new_long_data$repeat_instance == 4] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M14 H00018" & new_long_data$repeat_instance == 3] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "K01 H00026" & new_long_data$repeat_instance == 3] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00047" & new_long_data$repeat_instance == 5] = "An. funestus"
new_long_data$species_type[new_long_data$sample_id_head == "M09 H00038" & new_long_data$repeat_instance == 4] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M14 H00031" & new_long_data$repeat_instance == 4] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00062" & new_long_data$repeat_instance == 6] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00092" & new_long_data$repeat_instance == 9] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00109" & new_long_data$repeat_instance == 11] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00128" & new_long_data$repeat_instance == 14] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M09 A00103" & new_long_data$repeat_instance == 26] = "An. gambiae"
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00185" & new_long_data$repeat_instance == 33] = "An. gambiae"
# look at tables of both sepcies_type and abdominal_status
table(new_long_data$abdominal_status, useNA = "always") # abdominal status looks good
table(new_long_data$species_type, useNA = "always") # species type still needs to be cleaned up some
# check these entries
species_testdata = new_long_data[which(new_long_data$species_type == "Blood Fed" | new_long_data$species_type == "Gravid" | new_long_data$species_type == "Half Gravid"),]
# looks like abdominal status was double entered into both the abdominal_status and species_type columns for three mosquitoes
# defaulted to what was in the abdominal status column and changed those entries in the species_type to missing
new_long_data$species_type[new_long_data$sample_id_head == "M07 H00011" & new_long_data$repeat_instance == 1] = NA
new_long_data$species_type[new_long_data$sample_id_head == "M09 H00018" & new_long_data$repeat_instance == 2] = NA
new_long_data$species_type[new_long_data$sample_id_head == "M09 H00103" & new_long_data$repeat_instance == 26] = NA
# check species_type again
table(new_long_data$species_type, useNA = "always") # looks good
# make abdominal_status a factor
new_long_data$abdominal_status = as.factor(new_long_data$abdominal_status)
str(new_long_data$abdominal_status)
table(new_long_data$abdominal_status, useNA = "always")
# specify_species
table(new_long_data$specify_species, useNA = "always")
# the 13 "Other, Specify" species in species_type wre all An. Pretoriensis
# make these "Other, Specify" entries for species_type An. pretoriensis
new_long_data$species_type[new_long_data$species_type == "Other, Specify"] = "An. pretoriensis"
table(new_long_data$species_type, useNA = "always")
# make species_type a factor
new_long_data$species_type = as.factor(new_long_data$species_type)
str(new_long_data$species_type)
table(new_long_data$species_type, useNA = "always")

# remove specify_species column because no longer needed
table(new_long_data$specify_species, useNA = "always")
new_long_data$specify_species <- NULL

# remove the new_sample_name column from the data set
new_long_data$new_sample_name <- NULL

# remove collection month and collection year columns
new_long_data$collection_month <- NULL
new_long_data$collection_year <- NULL

# check that all the head and sample IDs are the same and create sample_id_mosquito variable
split_check_head = rep(NA,nrow(new_long_data))
split_check_abdomen = rep(NA,nrow(new_long_data))
for (i in 1:nrow(new_long_data)){
  new_name_head = strsplit(new_long_data$sample_id_head[i],"")[[1]]
  new_name_p1_head = paste(new_name_head[1:3], collapse="")
  new_name_p2_head = paste(new_name_head[6:10], collapse="")
  split_check_head[i] = paste0(new_name_p1_head," ",new_name_p2_head)
  
  new_name_abdomen = strsplit(new_long_data$sample_id_abdomen[i],"")[[1]]
  new_name_p1_abdomen = paste(new_name_abdomen[1:3], collapse="")
  new_name_p2_abdomen = paste(new_name_abdomen[6:10], collapse="")
  split_check_abdomen[i] = paste0(new_name_p1_abdomen," ",new_name_p2_abdomen)
}
table(split_check_head, useNA = "always")
table(split_check_abdomen, useNA = "always")
length(which(is.na(split_check_head))) # no missing, good
length(which(is.na(split_check_abdomen))) # no missing, good
# check if the two vectors are the same
all.equal(split_check_head, split_check_abdomen)
length(which(split_check_head == split_check_abdomen))
# looks like one row is not exactly the same (and there's the placeholder row - 85)
# examine this row
which(split_check_head != split_check_abdomen)
split_check_head[1124]
split_check_abdomen[1124]
# looks like M09 A0097 should be recoded to M09 A00097
new_long_data$sample_id_abdomen[new_long_data$sample_id_abdomen == "M09  A0097" & new_long_data$repeat_instance == 26] = "M09 A00097"
new_long_data$sample_id_abdomen[1124]
# recheck that the head and abdomen vectors are the same
split_check_head = rep(NA,nrow(new_long_data))
split_check_abdomen = rep(NA,nrow(new_long_data))
for (i in 1:nrow(new_long_data)){
  new_name_head = strsplit(new_long_data$sample_id_head[i],"")[[1]]
  new_name_p1_head = paste(new_name_head[1:3], collapse="")
  new_name_p2_head = paste(new_name_head[6:10], collapse="")
  split_check_head[i] = paste0(new_name_p1_head," ",new_name_p2_head)
  
  new_name_abdomen = strsplit(new_long_data$sample_id_abdomen[i],"")[[1]]
  new_name_p1_abdomen = paste(new_name_abdomen[1:3], collapse="")
  new_name_p2_abdomen = paste(new_name_abdomen[6:10], collapse="")
  split_check_abdomen[i] = paste0(new_name_p1_abdomen," ",new_name_p2_abdomen)
}
table(split_check_head, useNA = "always")
table(split_check_abdomen, useNA = "always")
length(which(is.na(split_check_head))) # no missing, good
length(which(is.na(split_check_abdomen))) # no missing, good
# check if the two vectors are the same
all.equal(split_check_head, split_check_abdomen)
length(which(split_check_head == split_check_abdomen))
which(split_check_head != split_check_abdomen) # good, just place holder row that is still different
# the two vectors are the same now so that's all good
# add the new variable created to the data set as sample_id_mosquito
new_long_data$sample_id_mosquito = split_check_head

# remove the place holder row for sample_id_abdomen (row 85) (change back to missing)
new_long_data$sample_id_abdomen[new_long_data$sample_id_head == "S02 H00001" & new_long_data$repeat_instance == 6] = NA
length(which(is.na(new_long_data$sample_id_abdomen))) # 1 missing, correct
length(which(is.na(new_long_data$sample_id_head))) # 0 missing, correct

# check one last time for duplicates sample IDs
# look for duplicates in sample_id_head
length(unique(new_long_data$sample_id_head)) # 1494 unique 
length(which(is.na(new_long_data$sample_id_head) == T)) # 0 missing
count_table = table(new_long_data$sample_id_head, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are no duplicates left
# look for duplicates in sample_id_abdomen
length(unique(new_long_data$sample_id_abdomen)) # 1494 unique 
length(which(is.na(new_long_data$sample_id_abdomen) == T)) # 1 missing
count_table = table(new_long_data$sample_id_abdomen, useNA = "always")
dups_table = count_table[which(count_table > 1)] # looks like there are no duplicates left
# check if sample_id_mosquito ever occurred more than 2 times
count_table = table(new_long_data$sample_id_mosquito, useNA = "always")
greater_dups_table = count_table[which(count_table > 2)] # none > 2 so good

# check that the HH ID matched the sample ID
sample_hh_id = sapply(strsplit(new_long_data$sample_id_mosquito," "),head,1)
all.equal(new_long_data$HH_ID, sample_hh_id) 
length(which(new_long_data$HH_ID != sample_hh_id))
# looks like there are 23 instances of mismatches
# when there are mismatches, go back to the original data set and see what happened
mismatched_hhs = new_long_data[which(new_long_data$HH_ID != sample_hh_id),]
# make the changes in the data set
# M16 H00035 and M16 A00035 - change household ID manually to M16
new_long_data$HH_ID[new_long_data$sample_id_head == "M16 H00035" & new_long_data$repeat_instance == 22] = "M16"
# S04 H00001 - changed household ID to S04
new_long_data$HH_ID[new_long_data$sample_id_head == "S04 H00001" & new_long_data$repeat_instance == 5] = "S04"
# M15 H00016 - changed household ID to M15
new_long_data$HH_ID[new_long_data$sample_id_head == "M15 H00016" & new_long_data$repeat_instance == 3] = "M15"
# S02 H00005 - changed household ID to S02
new_long_data$HH_ID[new_long_data$sample_id_head == "S02 H00005" & new_long_data$repeat_instance == 9] = "S02"
# S02 H00006 - changed household ID to S02
new_long_data$HH_ID[new_long_data$sample_id_head == "S02 H00006" & new_long_data$repeat_instance == 9] = "S02"
# S02 H00007 - changed household ID to S02
new_long_data$HH_ID[new_long_data$sample_id_head == "S02 H00007" & new_long_data$repeat_instance == 9] = "S02"
# S02 H00008 - changed household ID to S02
new_long_data$HH_ID[new_long_data$sample_id_head == "S02 H00008" & new_long_data$repeat_instance == 9] = "S02"
# S12 H00005 - changed household ID to S12
new_long_data$HH_ID[new_long_data$sample_id_head == "S12 H00005" & new_long_data$repeat_instance == 2] = "S12"
# S08 H00004 - changed household ID to S08
new_long_data$HH_ID[new_long_data$sample_id_head == "S08 H00004" & new_long_data$repeat_instance == 4] = "S08"
# K07 H00010 - changed household ID to K07
new_long_data$HH_ID[new_long_data$sample_id_head == "K07 H00010" & new_long_data$repeat_instance == 4] = "K07"
# K05 H00013 - changed household ID to K05
new_long_data$HH_ID[new_long_data$sample_id_head == "K05 H00013" & new_long_data$repeat_instance == 3] = "K05"
# M12 H00002 - changed household ID to M12
new_long_data$HH_ID[new_long_data$sample_id_head == "M12 H00002" & new_long_data$repeat_instance == 5] = "M12"
# M12 H00003 - changed household ID to M12
new_long_data$HH_ID[new_long_data$sample_id_head == "M12 H00003" & new_long_data$repeat_instance == 5] = "M12"
# M12 H00004 - changed household ID to M12
new_long_data$HH_ID[new_long_data$sample_id_head == "M12 H00004" & new_long_data$repeat_instance == 5] = "M12"
# M12 H00005 - changed household ID to M12
new_long_data$HH_ID[new_long_data$sample_id_head == "M12 H00005" & new_long_data$repeat_instance == 5] = "M12"
# M05 H00023 - changed household ID to M05
new_long_data$HH_ID[new_long_data$sample_id_head == "M05 H00023" & new_long_data$repeat_instance == 23] = "M05"
# S06 H00015 - changed household ID to S06
new_long_data$HH_ID[new_long_data$sample_id_head == "S06 H00015" & new_long_data$repeat_instance == 9] = "S06"
# S05 H00014 - changed household ID to S05
new_long_data$HH_ID[new_long_data$sample_id_head == "S05 H00014" & new_long_data$repeat_instance == 10] = "S05"
# S05 H00014 - changed household ID to S05
new_long_data$HH_ID[new_long_data$sample_id_head == "S05 H00014" & new_long_data$repeat_instance == 10] = "S05"
# K05 H00035 - changed household ID to K05
new_long_data$HH_ID[new_long_data$sample_id_head == "K05 H00035" & new_long_data$repeat_instance == 7] = "K05"
# M16 H00033 - changed household ID to M16
new_long_data$HH_ID[new_long_data$sample_id_head == "M16 H00033" & new_long_data$repeat_instance == 22] = "M16"
# M16 H00034 - changed household ID to M16
new_long_data$HH_ID[new_long_data$sample_id_head == "M16 H00034" & new_long_data$repeat_instance == 22] = "M16"
# M16 H00035 - changed household ID to M16
new_long_data$HH_ID[new_long_data$sample_id_head == "M16 H00035" & new_long_data$repeat_instance == 22] = "M16"
# M16 H00036 - changed household ID to M16
new_long_data$HH_ID[new_long_data$sample_id_head == "M16 H00036" & new_long_data$repeat_instance == 22] = "M16"
# M05 H00040 - changed household ID to M05
new_long_data$HH_ID[new_long_data$sample_id_head == "M05 H00040" & new_long_data$repeat_instance == 20] = "M05"
# recheck that the HH ID matched the sample ID
sample_hh_id = sapply(strsplit(new_long_data$sample_id_mosquito," "),head,1)
all.equal(new_long_data$HH_ID, sample_hh_id) 
length(which(new_long_data$HH_ID != sample_hh_id))
# they all look like they've been successfully changed


# save the cleaned anopheles mosquito descriptive data set as a csv file and an rds file
write_csv(new_long_data,"spat21_mosquito_anopheles_descriptive_long_data_4JAN2019.csv")
write_rds(new_long_data,"spat21_mosquito_anopheles_descriptive_long_data_4JAN2019.RDS")

