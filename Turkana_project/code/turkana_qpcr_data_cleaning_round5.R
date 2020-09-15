# ----------------------------------------- #
#   qPCR data cleaning for lab purposes     #
#         Turkana EMBATALK Data             #
#               Round 5 qPCR                #
#             September 8, 2020             #
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
qpcr_data = read_csv("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Original data/Round 5/EMBATALK DBS compiled 4Sept2020.csv")

# read in the lab inventory of samples received and punched
inventory_data = read_csv("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Original data/Round 5/EMBATALK Database.csv")
inventory_data_new = read_csv("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Original data/Round 5/EMBATALK Database_new.csv")


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

# delete the blank row
inventory_data = inventory_data %>%
  filter(!(is.na(row)))

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

# delete all rows where sample id == "Blank"
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "Blank"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "BLANK"),]
inventory_data = inventory_data[-which(is.na(inventory_data$`sample ID`)),]


#### --------- clean the new inventory data -------- ####

# look at the inventory data
summary(inventory_data_new)

# look at levels of some of the categorical variables
levels(as.factor(inventory_data_new$`DBSplate ID`))
levels(as.factor(inventory_data_new$column))
levels(as.factor(inventory_data_new$row))
levels(as.factor(inventory_data_new$`gDNA plate ID`))

# delete the blank row
inventory_data_new = inventory_data_new %>%
  filter(!(is.na(row)))

# clean up the gDNA plate ID entries
table(inventory_data_new$`gDNA plate ID`, useNA = "always")
for (i in 1:nrow(inventory_data_new)){
  if (!(str_detect(inventory_data_new$`gDNA plate ID`[i]," "))){
    splitted = str_split(inventory_data_new$`gDNA plate ID`[i],"-")[[1]]
    inventory_data_new$`gDNA plate ID`[i] = paste(splitted[1],"-",splitted[2])
  } else {
    inventory_data_new$`gDNA plate ID`[i] = inventory_data_new$`gDNA plate ID`[i]
  }
}
table(inventory_data_new$`gDNA plate ID`, useNA = "always")

# delete all rows where sample id == "Blank"
inventory_data_new = inventory_data_new[-which(inventory_data_new$`sample ID` == "Blank"),]
inventory_data_new = inventory_data_new[-which(inventory_data_new$`sample ID` == "BLANK"),]
inventory_data_new = inventory_data_new[-which(is.na(inventory_data_new$`sample ID`)),]

# cut this inventory data down to just plates 82-86 to add onto the other inventory after remove other duplicates
inventory_data_new = inventory_data_new %>% filter(`gDNA plate ID` == "EMB - 82B" | `gDNA plate ID` == "EMB - 83B" | `gDNA plate ID` == "EMB - 84B" |
                                                     `gDNA plate ID` == "EMB - 85B" | `gDNA plate ID` == "EMB - 86B")
table(inventory_data_new$`gDNA plate ID`)


#### -------- merge in the inventory data and qpcr data -------- ####

# before merging, need to check that all ids in the inventory and qpcr data are unique

# check that all ids in the inventory are unique
length(unique(inventory_data$`sample ID`)) # 6888 unique 
length(which(is.na(inventory_data$`sample ID`) == T)) # 0 missing
count_table = table(inventory_data$`sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # blanks are duplicates but also some sample ids are duplicates
dups_table
inventory_data = inventory_data[-which(inventory_data$`sample ID` %in% names(dups_table)),]
ids_to_remove = names(dups_table)
# check one more time for duplicates
length(unique(inventory_data$`sample ID`)) # 6822 unique 
length(which(is.na(inventory_data$`sample ID`) == T)) # 0 missing
count_table = table(inventory_data$`sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no more duplicates
dups_table

# now merge in the new inventory information
colnames(inventory_data)
colnames(inventory_data_new)
inventory_data = rbind(inventory_data,inventory_data_new)

# now check once more for duplicates in the inventory data
length(unique(inventory_data$`sample ID`)) # 7082 unique 
length(which(is.na(inventory_data$`sample ID`) == T)) # 0 missing
count_table = table(inventory_data$`sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # blanks are duplicates but also some sample ids are duplicates
dups_table # 58 duplicates
# now keep the first instance for each of these duplicates
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "G0142" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "G0223A" & inventory_data$`gDNA plate ID` == "EMB - 84B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "G0280" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "G0291" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "G0293" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "G0398B" & inventory_data$`gDNA plate ID` == "EMB - 84B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "G0506A" & inventory_data$`gDNA plate ID` == "EMB - 85B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "G0506B" & inventory_data$`gDNA plate ID` == "EMB - 85B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "G0545" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "G0575B" & inventory_data$`gDNA plate ID` == "EMB - 84B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "G0645" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "K0028" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "K0143D" & inventory_data$row == "E"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "K0407" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "K0422" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "M0001" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "M0308" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "M0398B" & inventory_data$`gDNA plate ID` == "EMB - 84B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "N0084H" & inventory_data$`gDNA plate ID` == "EMB - 85B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "P0119" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "S0145" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "S0209" & inventory_data$`gDNA plate ID` == "EMB - 82B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "S0223D" & inventory_data$`gDNA plate ID` == "EMB - 84B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0028" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0204" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0231" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0232" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0238" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0239" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0240" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0505" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0513" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0515" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0516" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0886" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0887" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0888" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0962" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0968" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T0969" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1042" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1043" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1044" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1097" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1098" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1101" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1102" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1103" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1106" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1107" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1108" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1109" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1110" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1144" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1147" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1170" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1171" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
inventory_data = inventory_data[-which(inventory_data$`sample ID` == "T1552" & inventory_data$`gDNA plate ID` == "EMB - 83B"),]
# check one more time for duplicates
length(unique(inventory_data$`sample ID`)) # 7082 unique 
length(which(is.na(inventory_data$`sample ID`) == T)) # 0 missing
count_table = table(inventory_data$`sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no more duplicates
dups_table

# check for duplicates in the qpcr data
length(unique(qpcr_data$`Sample Name`)) # 320 unique 
length(which(is.na(qpcr_data$`Sample Name`) == T)) # 87 missing
qpcr_data = qpcr_data %>% filter(!(is.na(`Sample Name`)))
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "0"),]
count_table = table(qpcr_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 4 duplicates
dups_table
# choose one of each of these duplicates to keep 
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "K0143D" & qpcr_data$`Well Position` == "E19"),]
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "N0084H" & qpcr_data$`Well Position` == "I1"),]
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "S0145" & qpcr_data$`Well Position` == "B9"),]
qpcr_data = qpcr_data[-which(qpcr_data$`Sample Name` == "T1552" & qpcr_data$`Well Position` == "I1"),]
# check one more time for duplicates 
length(unique(qpcr_data$`Sample Name`)) # 318 unique 
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
length(which(is.na(merged_data$`Well Position`))) # 7082-318=6764, good 
setdiff(qpcr_data$`sample ID`,inventory_data$`sample ID`)
# looks good

# check for duplicates in the merged data just in case
length(unique(merged_data$`sample ID`)) # 7082 unique 
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
summary(merged_data$pfr364Q1) # 6764 missing
summary(merged_data$pfr364Q2) # 6764 missing

# make a variable that censors for human beta tublin CT values missing
merged_data$pfr364Q1_std_censored = ifelse(merged_data$`sample ID` %in% hbcriteria_1_ids,NA,merged_data$pfr364Q1)
merged_data$pfr364Q2_std_censored = ifelse(merged_data$`sample ID` %in% hbcriteria_2_ids,NA,merged_data$pfr364Q2)
summary(merged_data$pfr364Q1_std_censored) # 6772 missing
summary(merged_data$pfr364Q2_std_censored) # 6767 missing
hbcriteria_1$pfr364Q1
hbcriteria_2$pfr364Q2
# did had some new missingness

# Q values that are missing for pf CT values
merged_data$pfr364Q1_std_censored = ifelse(is.na(merged_data$pfr364CT1),NA,merged_data$pfr364Q1_std_censored)
merged_data$pfr364Q2_std_censored = ifelse(is.na(merged_data$pfr364CT2),NA,merged_data$pfr364Q2_std_censored)
summary(merged_data$pfr364Q1_std_censored) # 6991
summary(merged_data$pfr364Q2_std_censored) # 6987


# build off that variable to now make a variable that censors for pf CT values >38 and other replicate missing and rename to pfr364Q_std_censored_v2
merged_data$pfr364Q1_std_censored_v2 = ifelse(merged_data$pfr364CT1 >= 38 & is.na(merged_data$pfr364CT2),NA,merged_data$pfr364Q1_std_censored)
merged_data$pfr364Q2_std_censored_v2 = ifelse(merged_data$pfr364CT2 >= 38 & is.na(merged_data$pfr364CT1),NA,merged_data$pfr364Q2_std_censored)
summary(merged_data$pfr364Q1_std_censored_v2) # 6991 missing
summary(merged_data$pfr364Q2_std_censored_v2) # 6987 missing
# check the output one more time
length(which(merged_data$pfr364CT1 >= 38 & is.na(merged_data$pfr364CT2))) # 0 observation
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
# looks like we had 2 of these
merged_data$pf_pcr_infection_status[which(merged_data$`sample ID` == "G0545")] = NA
merged_data$pf_pcr_infection_status[which(merged_data$`sample ID` == "T0231")] = NA
# check the change
table(merged_data$pf_pcr_infection_status, useNA = "always")
# looks good


# now remove the extraneous Q standardization columns
colnames(merged_data)
merged_data = merged_data %>%
  select(-c(pfr364Q1_std_censored,pfr364Q2_std_censored,pfr364Q1_std_censored_v2,pfr364Q2_std_censored_v2))
colnames(merged_data)



#### --------- export the new data set --------- ####

# remove the samples that were double tested from merged data
merged_data = merged_data[-which(merged_data$`sample ID` == "S0037"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "K0157"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "G0261"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "G0033"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "G0015"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "S0145"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "P0059"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "G0125C"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "P0033"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "K0070"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "M0214"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "G0136"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "G0223A"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "M0308"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "G0506B"),]
merged_data = merged_data[-which(merged_data$`sample ID` == "K0224"),]

# now read back in the first round 1 and 2 pcr results
merged_data_round1 = read_rds("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Merged data/All merged pcr results - round 4/EMBATALK_inventory_with_qpcr_7JUL2020.RDS")

# row bind all these data sets
merged_data_round1_have_v1 = merged_data_round1[-which(is.na(merged_data_round1$`pfr364R²`)),] # 6918
merged_data_round2_have_v1 = merged_data[-which(is.na(merged_data$`pfr364R²`)),] # 302
merged_data_test = rbind(merged_data_round1_have_v1,merged_data_round2_have_v1) # 7220 variables

# check the combined data set
length(unique(merged_data_test$`sample ID`)) # 7165 unique 
length(which(is.na(merged_data_test$`sample ID`) == T)) # 0 missing
count_table = table(merged_data_test$`sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no more duplicates
dups_table

# then get rid of remaining duplicates
length(intersect(merged_data$`sample ID`,names(dups_table)))
length(dups_table)
merged_data = merged_data[-which(merged_data$`sample ID` %in% names(dups_table)),]

# pull out the positive samples for Betsy
positive_data = merged_data %>%
  filter(pf_pcr_infection_status == "positive") %>%
  select(`sample ID`,`gDNA plate ID`,row,column,pfr364CT1,pfr364CT2)
write_csv(positive_data,"Desktop/EMBATALK_positive_samples_round5_15SEP2020.csv")


# compare these positive samples with the ones that betsy previously had
positive_data_round1 = read_csv("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Merged data/9JAN2020 Positive sample - round 1/EMBATALK_positive_samples_9JAN2020.csv")
positive_data_round2 = read_csv("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Merged data/14FEB2020 Positive sample - round 2/EMBATALK_positive_samples_round2_14FEB2020.csv")
positive_data_round3 = read_csv("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Merged data/21MAY2020 Positive sample - round 3/EMBATALK_positive_samples_round3_21MAY2020.csv")
positive_data_round4 = read_csv("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Merged data/7JUL2020 Postivie sample - round 4/EMBATALK_positive_samples_round4_7JUL2020.csv")
intersect(positive_data$`sample ID`,positive_data_round1$`sample ID`) # overlap in some samples - go with original results
length(setdiff(positive_data$`sample ID`,positive_data_round1$`sample ID`)) # 72, good
length(setdiff(positive_data_round1$`sample ID`,positive_data$`sample ID`)) # 661, good
intersect(positive_data$`sample ID`,positive_data_round2$`sample ID`) # overlap in some samples - go with original results
length(setdiff(positive_data$`sample ID`,positive_data_round2$`sample ID`)) # 72, good
length(setdiff(positive_data_round2$`sample ID`,positive_data$`sample ID`)) # 1380, good
intersect(positive_data$`sample ID`,positive_data_round3$`sample ID`) # no overlap, good
length(setdiff(positive_data$`sample ID`,positive_data_round3$`sample ID`)) # 72, good
length(setdiff(positive_data_round3$`sample ID`,positive_data$`sample ID`)) # 518, good
intersect(positive_data$`sample ID`,positive_data_round4$`sample ID`) # no overlap, good
length(setdiff(positive_data$`sample ID`,positive_data_round4$`sample ID`)) # 72, good
length(setdiff(positive_data_round4$`sample ID`,positive_data$`sample ID`)) # 400, good

# split up the data sets based on what is merged
merged_data_round1_have = merged_data_round1[-which(is.na(merged_data_round1$`pfr364R²`)),] # 6918
merged_data_round2_have = merged_data[-which(is.na(merged_data$`pfr364R²`)),] # 247
merged_data_missing = merged_data[-which(merged_data$`sample ID` %in% merged_data_round1_have$`sample ID` | 
                                           merged_data$`sample ID`%in% merged_data_round2_have$`sample ID`),] # 2: called SPOILT so guess spoiled? and one that is N0071C
# row bind all these data sets
merged_data_all = rbind(merged_data_round1_have,merged_data_round2_have,merged_data_missing) # 7167 variables

# check the combined data set
length(unique(merged_data_all$`sample ID`)) # 7167 unique 
length(which(is.na(merged_data_all$`sample ID`) == T)) # 0 missing
count_table = table(merged_data_all$`sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # no more duplicates
dups_table

# check that the number of qpcr positive samples was maintained
table(merged_data_all$pf_pcr_infection_status, useNA = "always") # 3018 positive
table(merged_data_round1_have$pf_pcr_infection_status, useNA = "always") # 2946
table(merged_data_round2_have$pf_pcr_infection_status, useNA = "always") # 72

# export the data set as CSV and RDS files
write_csv(merged_data_all, "Desktop/EMBATALK_inventory_with_qpcr_15SEP2020.csv")
write_rds(merged_data_all, "Desktop/EMBATALK_inventory_with_qpcr_15SEP2020.RDS")
















