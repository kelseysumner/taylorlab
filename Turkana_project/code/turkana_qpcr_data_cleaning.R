# ----------------------------------------- #
#   qPCR data cleaning for lab purposes     #
#         Turkana EMBATALK Data             #
#             January 9, 2020               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(stringr)
library(broom)
library(ggplot2)


#### ---------- load in the data sets ---------- ####

# read in the human DBS qpcr data set
qpcr_data = read_csv("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Original data/EMBATALK DBS compiled 8Jan2020.csv")

# read in the lab inventory of samples received and punched
inventory_data = read_csv("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Original data/EMBATALK Database.csv")



#### -------- clean up the qpcr data ---------- ####

# look at the qpcr data
summary(qpcr_data)

# remove all the standards
qpcr_data = qpcr_data %>%
  mutate(Contaminated = rep(NA,nrow(qpcr_data))) %>%
  select("Sample Name","Experiment Name","Well Position","Contaminated","pfr364R²",`pfr364Y-Intercept`,pfr364Slope,HbtubCT1,HbtubCT2,pfr364CT1,pfr364CT2,pfr364Q1,pfr364Q2)

# change all the undetermined CT values to missing
qpcr_data$HbtubCT1[which(qpcr_data$HbtubCT1 == "Undetermined")] = NA
qpcr_data$HbtubCT2[which(qpcr_data$HbtubCT2 == "Undetermined")] = NA
qpcr_data$pfr364CT1[which(qpcr_data$pfr364CT1 == "Undetermined")] = NA
qpcr_data$pfr364CT2[which(qpcr_data$pfr364CT2 == "Undetermined")] = NA

# change all the 0 CT values to missing
qpcr_data$HbtubCT1[which(qpcr_data$HbtubCT1 == "0")] = NA
qpcr_data$HbtubCT2[which(qpcr_data$HbtubCT2 == "0")] = NA
qpcr_data$pfr364CT1[which(qpcr_data$pfr364CT1 == "0")] = NA
qpcr_data$pfr364CT2[which(qpcr_data$pfr364CT2 == "0")] = NA

# make sure all the CT values are numeric
qpcr_data$HbtubCT1 = as.numeric(qpcr_data$HbtubCT1)
qpcr_data$HbtubCT2 = as.numeric(qpcr_data$HbtubCT2)
qpcr_data$pfr364CT1 = as.numeric(qpcr_data$pfr364CT1)
qpcr_data$pfr364CT2 = as.numeric(qpcr_data$pfr364CT2)

# look at original summaries of the CT values and Q values
summary(qpcr_data$HbtubCT1)
summary(qpcr_data$HbtubCT2)
summary(qpcr_data$pfr364CT1)
summary(qpcr_data$pfr364CT2)
summary(qpcr_data$pfr364Q1)
summary(qpcr_data$pfr364Q2)



#### ------- clean the inventory data -------- ####

# look at the inventory data
summary(inventory_data)

# look at levels of some of the categorical variables
levels(as.factor(inventory_data$`DBSplate ID`))
levels(as.factor(inventory_data$column))
levels(as.factor(inventory_data$row))
levels(as.factor(inventory_data$`gDNA plate ID`))

# remove x6 and x7 columns because empty
inventory_data$X6 <- NULL
inventory_data$X7 <- NULL

# clean up the gDNA plate ID entries
table(inventory_data$`gDNA plate ID`, useNA = "always")
for (i in 1:nrow(inventory_data)){
  if (!(str_detect(inventory_data$`gDNA plate ID`[i]," "))){
    splitted = str_split(inventory_data$`gDNA plate ID`[i],"-")[[1]]
    inventory_data$`gDNA plate ID`[i] = paste(splitted[1],"-",splitted[2])
  } else {
    inventory_data$`gDNA plate ID`[i] = inventory_data$`gDNA plate ID`[i]
  }
}
table(inventory_data$`gDNA plate ID`, useNA = "always")


#### -------- merge in the inventory data and qpcr data -------- ####

# before merging, need to check that all ids in the inventory and qpcr data are unique

# check that all ids in the inventory are unique
length(unique(inventory_data$`sample ID`)) # 5735 unique 
length(which(is.na(inventory_data$`sample ID`) == T)) # 0 missing
count_table = table(inventory_data$`sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # blanks are duplicates but also some sample ids are duplicates
dups_table
# remove the blanks entries
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "Blank" | inventory_data$`sample ID` == "BLANK"),]
# K0040A is duplicated 89 times (an entire plate) - remove this plate for now (EMB - 28B) while we sort it out, will add back in later
inventory_data = inventory_data[-which(inventory_data$`gDNA plate ID` == "EMB - 28B"),]
# for each of the duplicates that only appear twice, choose the first plate or instance the sample appears on to keep, the rest remove
# G0009B
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0009B" & inventory_data$column=="3"),]
# G0015
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0015" & inventory_data$column=="8"),]
# G0033
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0033" & inventory_data$column=="11"),]
# G0066C
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0066C" & inventory_data$column=="10"),]
# G0125C
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0125C" & inventory_data$column=="2"),]
# G0136
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0136" & inventory_data$column=="6"),]
# G0261
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0261" & inventory_data$column=="11"),]
# G0265A
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0265A" & inventory_data$column=="10"),]
# G0276B
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0276B" & inventory_data$column=="10"),]
# G0293A
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0293A" & inventory_data$column=="2"),]
# G0296A
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0296A" & inventory_data$`gDNA plate ID`=="EMB - 23B"),]
# G0296B
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0296B" & inventory_data$`gDNA plate ID`=="EMB - 23B"),]
# G0306A
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0306A" & inventory_data$`gDNA plate ID`=="EMB - 23B"),]
# G0339C
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0339C" & inventory_data$column == "5"),]
# G0393B
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0393B" & inventory_data$column == "11"),]
# G0397D
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0397D" & inventory_data$column == "2"),]
# G0501
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="G0501" & inventory_data$column == "7"),]
# GO233A
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="GO233A" & inventory_data$`gDNA plate ID`=="EMB - 23B"),]
# K0070
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="K0070" & inventory_data$column=="1"),]
# K0105
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="K0105" & inventory_data$row=="C"),]
# K0119C
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="K0119C" & inventory_data$column=="3"),]
# K0225A
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="K0225A" & inventory_data$column=="4"),]
# K0226A
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="K0226A" & inventory_data$column=="11"),]
# K0313
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="K0313" & inventory_data$column=="12"),]
# K0321
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="K0321" & inventory_data$column=="10"),]
# K0325
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="K0325" & inventory_data$column=="11"),]
# K0344
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="K0344" & inventory_data$`gDNA plate ID`=="EMB - 41B"),]
# M0024
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="M0024" & inventory_data$column=="8"),]
# M0046E
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="M0046E" & inventory_data$column=="7"),]
# M0214
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="M0214" & inventory_data$column=="11"),]
# M0322
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="M0322" & inventory_data$column=="8"),]
# N0020
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="N0020" & inventory_data$column=="1"),]
# N0048H
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="N0048H" & inventory_data$column=="9"),]
# N0069
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="N0069" & inventory_data$column=="3"),]
# N0084
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="N0084" & inventory_data$column=="2"),]
# P0033
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="P0033" & inventory_data$column=="4"),]
# P0059
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="P0059" & inventory_data$column=="11"),]
# P0156
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="P0156" & inventory_data$column=="5"),]
# S0024B
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="S0024B" & inventory_data$column=="8"),]
# S0037
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="S0037" & inventory_data$column=="5"),]
# S0145
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="S0145" & inventory_data$column=="6"),]
# S0233C
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="S0233C" & inventory_data$column=="10"),]
# T0117
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="T0117" & inventory_data$row=="F"),]
# T0315
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="T0315" & inventory_data$column=="8"),]
# T0316
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="T0316" & inventory_data$column=="2"),]
# T0317
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="T0317" & inventory_data$column=="11"),]
# T0318
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="T0318" & inventory_data$column=="7"),]
# T0319
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="T0319" & inventory_data$column=="6"),]
# T0320
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="T0320" & inventory_data$column=="10"),]
# T0764
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="T0764" & inventory_data$row=="F"),]
# T1152
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="T1152" & inventory_data$column=="9"),]
# T1494
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="T1494" & inventory_data$column=="6"),]
# T1503
inventory_data = inventory_data[-which(inventory_data$`sample ID`=="T1503" & inventory_data$column=="12"),]
# check again for duplicates
length(unique(inventory_data$`sample ID`)) # 5732 unique 
length(which(is.na(inventory_data$`sample ID`) == T)) # 0 missing
count_table = table(inventory_data$`sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no more duplicates
dups_table


# check for duplicates in the qpcr data
length(unique(qpcr_data$`Sample Name`)) # 1587 unique 
length(which(is.na(qpcr_data$`Sample Name`) == T)) # 0 missing
count_table = table(qpcr_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 14 duplicates
dups_table
# choose the first instance of these samples as well, discard second instance
# G0015
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="G0015" & qpcr_data$`Well Position`=="O7"),]
# G0033
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="G0033" & qpcr_data$`Well Position`=="F21"),]
# G0125C
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="G0125C" & qpcr_data$`Well Position`=="C23"),]
# K0070
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="K0070" & qpcr_data$`Well Position`=="J3"),]
# K0105
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="K0105" & qpcr_data$`Well Position`=="K5"),]
# P0033
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="P0033" & qpcr_data$`Well Position`=="E9"),]
# T0117
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="T0117" & qpcr_data$`Well Position`=="F23"),]
# T0315
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="T0315" & qpcr_data$`Well Position`=="C15"),]
# T0316
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="T0316" & qpcr_data$`Well Position`=="C3"),]
# T0317
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="T0317" & qpcr_data$`Well Position`=="C21"),]
# T0318
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="T0318" & qpcr_data$`Well Position`=="C13"),]
# T0319
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="T0319" & qpcr_data$`Well Position`=="C11"),]
# T0320
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="T0320" & qpcr_data$`Well Position`=="C19"),]
# T0764
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name`=="T0764" & qpcr_data$`Well Position`=="F15"),]
# check one more time for duplicates 
length(unique(qpcr_data$`Sample Name`)) # 1587 unique 
length(which(is.na(qpcr_data$`Sample Name`) == T)) # 0 missing
count_table = table(qpcr_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no more duplicates
dups_table


# change sample name to sample id in the qpcr data set for merging
qpcr_data = qpcr_data %>%
  rename("sample ID"="Sample Name")


# now merge the qpcr data into the inventory data set 
merged_data = left_join(inventory_data,qpcr_data,by="sample ID")


# check the merge
merged_data %>%
  filter(is.na(`Well Position`)) %>%
  View()
# 4,415 empty (5732-1587 = 4145, so looks like all merged in)


# check for duplicates in the merged data just in case
length(unique(merged_data$`sample ID`)) # 5732 unique 
length(which(is.na(merged_data$`sample ID`) == T)) # 0 missing
count_table = table(merged_data$`sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no more duplicates
dups_table



#### ---------------- create a composite parasitemia value (combine Q1 & Q2) ------------------ ####

# determine Sample Name that have human beta tubulin missing and need to be excluded
hbcriteria_1 = merged_data[which(is.na(merged_data$HbtubCT1) & !(is.na(merged_data$`pfr364R²`))),]
# pull out the vector of Sample Name
hbcriteria_1_ids = hbcriteria_1$`sample ID`
# now the second CT value for Hb
hbcriteria_2 = merged_data[which(is.na(merged_data$HbtubCT2) & !(is.na(merged_data$`pfr364R²`))),]
# pull out the vector of Sample Name
hbcriteria_2_ids = hbcriteria_2$`sample ID`

# look at original summaries of pfr364Q variables
summary(merged_data$pfr364Q1) # 4145 missing
summary(merged_data$pfr364Q2) # 4145 missing

# make a variable that censors for human beta tublin CT values missing
merged_data$pfr364Q1_std_censored = ifelse(merged_data$`sample ID` %in% hbcriteria_1_ids,NA,merged_data$pfr364Q1)
merged_data$pfr364Q2_std_censored = ifelse(merged_data$`sample ID` %in% hbcriteria_2_ids,NA,merged_data$pfr364Q2)
summary(merged_data$pfr364Q1_std_censored) # 4159 missing
summary(merged_data$pfr364Q2_std_censored) # 4152 missing
hbcriteria_1$pfr364Q1
hbcriteria_2$pfr364Q2
# did had some new missingness

# Q values that are missing for pf CT values
merged_data$pfr364Q1_std_censored = ifelse(is.na(merged_data$pfr364CT1),NA,merged_data$pfr364Q1_std_censored)
merged_data$pfr364Q2_std_censored = ifelse(is.na(merged_data$pfr364CT2),NA,merged_data$pfr364Q2_std_censored)
summary(merged_data$pfr364Q1_std_censored) # 5104
summary(merged_data$pfr364Q2_std_censored) # 5098


# build off that variable to now make a variable that censors for pf CT values >38 and other replicate missing and rename to pfr364Q_std_censored_v2
merged_data$pfr364Q1_std_censored_v2 = ifelse(merged_data$pfr364CT1 >= 38 & is.na(merged_data$pfr364CT2),NA,merged_data$pfr364Q1_std_censored)
merged_data$pfr364Q2_std_censored_v2 = ifelse(merged_data$pfr364CT2 >= 38 & is.na(merged_data$pfr364CT1),NA,merged_data$pfr364Q2_std_censored)
summary(merged_data$pfr364Q1_std_censored_v2) # 5106 missing
summary(merged_data$pfr364Q2_std_censored_v2) # 5098 missing
# check the output one more time
length(which(merged_data$pfr364CT1 >= 38 & is.na(merged_data$pfr364CT2))) # 2 observations
length(which(merged_data$pfr364CT2 >= 38 & is.na(merged_data$pfr364CT1))) # 0 observations
# look at the original data sets with this criteria
test1 = merged_data[which(merged_data$pfr364CT1 >= 38 & is.na(merged_data$pfr364CT2)),]
test2 = merged_data[which(merged_data$pfr364CT2 >= 38 & is.na(merged_data$pfr364CT1)),]


# create a variable that indicates whether the sample is positive or negative for Pf malaria infection
# if at least 1 duplicate has parasitemia > 0 after criteria enforced (ie. in pfr364Q_std_censored_v2 variable), then saying sample is positive
# 1 is positive, 0 negative
pf_infection_status = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (!(is.na(merged_data$"pfr364R²"[i])) & (is.na(merged_data$pfr364Q1_std_censored_v2[i]) & is.na(merged_data$pfr364Q2_std_censored_v2[i]))){
    pf_infection_status[i] = 0
  } else if ((merged_data$pfr364Q1_std_censored_v2[i] > 0 | merged_data$pfr364Q2_std_censored_v2[i] > 0) & !(is.na(merged_data$"pfr364R²"[i])) & (!(is.na(merged_data$pfr364Q1_std_censored_v2[i])) | !(is.na(merged_data$pfr364Q2_std_censored_v2[i])))){
    pf_infection_status[i] = 1
  } else {
    pf_infection_status[i] = NA
  }
}
table(pf_infection_status,useNA = "always")
# check the output
length(which(merged_data$pfr364Q1_std_censored_v2 > 0 | merged_data$pfr364Q2_std_censored_v2 > 0))
length(which(!(is.na(merged_data$"pfr364R²")))) - length(which(merged_data$pfr364Q1_std_censored_v2 > 0 | merged_data$pfr364Q2_std_censored_v2 > 0))
length(which((is.na(merged_data$"pfr364R²"))))
# make a factor
merged_data$pf_pcr_infection_status = factor(pf_infection_status,levels = c(0,1), labels = c("negative", "positive"))
# look at the output
table(merged_data$pf_pcr_infection_status,useNA = "always")

# create a combined standardized new variable (combining Q1 and Q2) that was standardized from the 8 standards (1-2000 p/uL)
# if both positive parasitemia for both, then is an average of the two parasitemia for the duplicates.
# if only 1 positive parasitemia then is the value of the positive parasitemia
pfr364Q_std_combined = rep(NA,nrow(merged_data))
for (k in 1:nrow(merged_data)){
  if (merged_data$pf_pcr_infection_status[k] == "positive" & !(is.na(merged_data$pf_pcr_infection_status[k]))){
    pfr364Q_std_combined[k] = (sum(merged_data$pfr364Q1_std_censored_v2[k],merged_data$pfr364Q2_std_censored_v2[k],na.rm = T))/(2-(is.na(merged_data$pfr364Q1_std_censored_v2[k])+is.na(merged_data$pfr364Q2_std_censored_v2[k])))
  } else
    pfr364Q_std_combined[k] = NA
}
summary(pfr364Q_std_combined,useNA = "always")
# add to the data set
merged_data$pfr364Q_std_combined = pfr364Q_std_combined
# check the output
summary(merged_data$pfr364Q1)
summary(merged_data$pfr364Q2)
summary(merged_data$pfr364Q_std_combined)
checkdata = merged_data[which(!(is.na(merged_data$pfr364Q_std_combined))),]
head(checkdata$HbtubCT1)
head(checkdata$HbtubCT2)
head(checkdata$pfr364CT1)
head(checkdata$pfr364CT2)
head(checkdata$pfr364Q1_std_censored_v2)
head(checkdata$pfr364Q2_std_censored_v2)
head(checkdata$pf_pcr_infection_status)
head(checkdata$pfr364Q_std_combined)


#### ---------- do final checks to make sure data processed correctly -------- ####


# final check through all the data processing for the qPCR data
summary(checkdata$HbtubCT1)
summary(checkdata$HbtubCT2)
summary(checkdata$pfr364CT1)
summary(checkdata$pfr364CT2)
summary(merged_data$pfr364Q_std_combined)
table(merged_data$pf_pcr_infection_status, useNA = "always")


# final check for samples to change to missing
orig_zeroes_1 = qpcr_data[which(qpcr_data$pfr364CT1 == 0 | qpcr_data$HbtubCT1 == 0),]
orig_zeroes_2 = qpcr_data[which(qpcr_data$pfr364CT2 == 0 | qpcr_data$HbtubCT2 == 0),]
# pull out the labid_new for the samples with CT values of 0 for Pf
orig_zeroes_1_labid = orig_zeroes_1$`sample ID`
orig_zeroes_2_labid = orig_zeroes_2$`sample ID`
# subset the standardized parasitemia data set to look at these labid values
subset1 = merged_data[which(merged_data$`sample ID` %in% orig_zeroes_1_labid),]
subset2 = merged_data[which(merged_data$`sample ID` %in% orig_zeroes_2_labid),]
# none had zeroes for all Hb CT values

# also need to change values that have a hb CT value NA from negative to missing in pf_pcr_infection_status
# do this for labid_new observations that have both Hb CT values as NA
hbctbothmissing = merged_data[which(is.na(merged_data$HbtubCT1) & is.na(merged_data$HbtubCT2) & !(is.na(merged_data$"pfr364R²"))),]
# looks like we had 4 of these
merged_data$pf_pcr_infection_status[merged_data$`sample ID`=="M0213"] = NA
merged_data$pf_pcr_infection_status[merged_data$`sample ID`=="G0293"] = NA
merged_data$pf_pcr_infection_status[merged_data$`sample ID`=="T0204"] = NA
merged_data$pf_pcr_infection_status[merged_data$`sample ID`=="T0505"] = NA
# check the change
table(merged_data$pf_pcr_infection_status, useNA = "always")
# looks good


# now remove the extraneous Q standardization columns
colnames(merged_data)
merged_data = merged_data %>%
  select(-c(pfr364Q1_std_censored,pfr364Q2_std_censored,pfr364Q1_std_censored_v2,pfr364Q2_std_censored_v2))
colnames(merged_data)



#### --------- export the new data set --------- ####

# export the data set as CSV and RDS files
write_csv(merged_data, "Desktop/EMBATALK_inventory_with_qpcr_9JAN2020.csv")
write_rds(merged_data, "Desktop/EMBATALK_inventory_with_qpcr_9JAN2020.RDS")


# pull out the positive samples for Betsy
positive_data = merged_data %>%
  filter(pf_pcr_infection_status == "positive")
write_csv(positive_data,"Desktop/EMBATALK_positive_samples_9JAN2020.csv")























