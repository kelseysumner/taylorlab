# ----------------------------------------- #
#   Spat21 Cleaning Repeated qPCR Samples   #
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
library(stringr)
library(broom)
library(ggplot2)



#### ------- read in the data sets -------- ####

# read in the human merged data set (social and qpcr)
human_merged_all_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/spat21_human_merged_all_data_29APR2019.rds")

# read in the qpcr data set with the 34 samples that were re-run
qpcr_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/raw_files/qpcr_data/Mozzie DBS B8-10-11redo TaqMan duplex 5-20-19_clean.csv")


#### ------- clean up qpcr data for the 34 samples that were re-run -------- ####


# look at the column names
names(qpcr_data)

# pull out a table of each of the qpcr plates and their correspond Pf standards
# note: only pulled out standards 1-8 because don't want to use 1-10
qpcr_plates_table = qpcr_data %>%
  filter(!(is.na(`Experiment Name`))) %>%
  group_by(`Experiment Name`) %>%
  select(`Experiment Name`,pfr364Std1a,pfr364Std1b,pfr364Std2a,pfr364Std2b,pfr364Std3a,pfr364Std3b,
         pfr364Std4a,pfr364Std4b,pfr364Std5a,pfr364Std5b,pfr364Std6a,pfr364Std6b,pfr364Std7a,pfr364Std7b,
         pfr364Std8a,pfr364Std8b)
qpcr_plates_table = unique(qpcr_plates_table)

# add columns to the table with the concentration of the parasites
qpcr_plates_table$Std1a_x = 2000
qpcr_plates_table$Std1b_x = 2000
qpcr_plates_table$Std2a_x = 1000
qpcr_plates_table$Std2b_x = 1000
qpcr_plates_table$Std3a_x = 200
qpcr_plates_table$Std3b_x = 200
qpcr_plates_table$Std4a_x = 100
qpcr_plates_table$Std4b_x = 100
qpcr_plates_table$Std5a_x = 20
qpcr_plates_table$Std5b_x = 20
qpcr_plates_table$Std6a_x = 10
qpcr_plates_table$Std6b_x = 10
qpcr_plates_table$Std7a_x = 2
qpcr_plates_table$Std7b_x = 2
qpcr_plates_table$Std8a_x = 1
qpcr_plates_table$Std8b_x = 1

# recode everything labeled "undetermined" or 0 as NA
qpcr_plates_table[qpcr_plates_table == "Undetermined" | qpcr_plates_table == 0] = NA

# run a linear regression model with the concentrations as the x values and ct values as the y value for the standards
# run this model for each plate
model1 = lm(as.numeric(unlist(qpcr_plates_table[1,2:17])) ~ log10(as.numeric(unlist(qpcr_plates_table[1,18:33]))), data=qpcr_plates_table)

# combine the models in a data frame
m1_df = bind_cols(tidy(model1), confint_tidy(model1)) %>% mutate(model_name="M1: plate8-10-11redo") %>% mutate(r_value = summary(model1)$r.squared)
model_results_df = bind_rows(m1_df)

# rename the slope value
model_results_df[2,1] <- "slope"

# remove the columns that aren't needed
model_results_df$std.error <- NULL
model_results_df$statistic <- NULL
model_results_df$p.value <- NULL
model_results_df$conf.low <- NULL
model_results_df$conf.high <- NULL

# switch this dataframe from long to wide format
model_results_simplified = spread(data=model_results_df,key=term,value=estimate)

# rename the intercept column
colnames(model_results_simplified)[colnames(model_results_simplified) == '(Intercept)'] <- 'intercept'
model_results = model_results_simplified

# change the column names for model_results to represent the new standardization
colnames(model_results)[colnames(model_results) == 'r_value'] <- 'r_value_std'
colnames(model_results)[colnames(model_results) == 'intercept'] <- 'intercept_std'
colnames(model_results)[colnames(model_results) == 'slope'] <- 'slope_std'

# change the column names for qpcr_data
qpcr_data$model_name = rep("M1: plate8-10-11redo",nrow(qpcr_data))

# now merge in the model_results with the qpcr human data for spat21
final_qpcr_merge = left_join(qpcr_data,model_results,by="model_name")

# check the merge
table(final_qpcr_merge$r_value_std, useNA = "always")
table(final_qpcr_merge$pfr364R_, useNA = "always")
table(final_qpcr_merge$intercept_std, useNA = "always")
table(final_qpcr_merge$`pfr364Y-Intercept`,useNA = "always")

# make sure the qpcr values are numeric and change "Undetermined" to NA to represent missing because undetectable
# make sure all the qpcr values with CT values ==0  are also changed to NA
# first check how many are 0, undetermined, and missing
length(which(final_qpcr_merge$HbtubCT1 == "Undetermined" | final_qpcr_merge$HbtubCT1 == "0" | final_qpcr_merge$HbtubCT1 == "0.000")) # 0
length(which(final_qpcr_merge$HbtubCT2 == "Undetermined" | final_qpcr_merge$HbtubCT2 == "0" | final_qpcr_merge$HbtubCT2 == "0.000")) # 0
length(which(final_qpcr_merge$pfr364CT1 == "Undetermined" | final_qpcr_merge$pfr364CT1 == "0" | final_qpcr_merge$pfr364CT1 == "0.000")) # 24
length(which(final_qpcr_merge$pfr364CT2 == "Undetermined" | final_qpcr_merge$pfr364CT2 == "0" | final_qpcr_merge$pfr364CT2 == "0.000")) # 19
length(which(is.na(final_qpcr_merge$HbtubCT1))) # 0
length(which(is.na(final_qpcr_merge$HbtubCT2))) # 0
length(which(is.na(final_qpcr_merge$pfr364CT1))) # 0
length(which(is.na(final_qpcr_merge$pfr364CT2))) # 0
# then change the values
final_qpcr_merge$HbtubCT1[final_qpcr_merge$HbtubCT1 == "Undetermined" | final_qpcr_merge$HbtubCT1 == "0" | final_qpcr_merge$HbtubCT1 == "0.000"] = NA
final_qpcr_merge$HbtubCT2[final_qpcr_merge$HbtubCT2 == "Undetermined" | final_qpcr_merge$HbtubCT2 == "0" | final_qpcr_merge$HbtubCT2 == "0.000"] = NA
final_qpcr_merge$pfr364CT1[final_qpcr_merge$pfr364CT1 == "Undetermined" | final_qpcr_merge$pfr364CT1 == "0" | final_qpcr_merge$pfr364CT1 == "0.000"] = NA
final_qpcr_merge$pfr364CT2[final_qpcr_merge$pfr364CT2 == "Undetermined" | final_qpcr_merge$pfr364CT2 == "0" | final_qpcr_merge$pfr364CT2 == "0.000"] = NA
# then check how many are now missing
length(which(final_qpcr_merge$HbtubCT1 == "Undetermined" | final_qpcr_merge$HbtubCT1 == "0" | final_qpcr_merge$HbtubCT1 == "0.000")) # 0
length(which(final_qpcr_merge$HbtubCT2 == "Undetermined" | final_qpcr_merge$HbtubCT2 == "0" | final_qpcr_merge$HbtubCT2 == "0.000")) # 0
length(which(final_qpcr_merge$pfr364CT1 == "Undetermined" | final_qpcr_merge$pfr364CT1 == "0" | final_qpcr_merge$pfr364CT1 == "0.000")) # 0
length(which(final_qpcr_merge$pfr364CT2 == "Undetermined" | final_qpcr_merge$pfr364CT2 == "0" | final_qpcr_merge$pfr364CT2 == "0.000")) # 0
length(which(is.na(final_qpcr_merge$HbtubCT1))) # 0
length(which(is.na(final_qpcr_merge$HbtubCT2))) # 0
length(which(is.na(final_qpcr_merge$pfr364CT1))) # 24
length(which(is.na(final_qpcr_merge$pfr364CT2))) # 19

# make sure the values for the standardization formula are numeric
final_qpcr_merge$pfr364CT1 = as.numeric(final_qpcr_merge$pfr364CT1)
final_qpcr_merge$pfr364CT2 = as.numeric(final_qpcr_merge$pfr364CT2)
final_qpcr_merge$HbtubCT1 = as.numeric(final_qpcr_merge$HbtubCT1)
final_qpcr_merge$HbtubCT2 = as.numeric(final_qpcr_merge$HbtubCT2)
final_qpcr_merge$intercept_std = as.numeric(final_qpcr_merge$intercept_std)
final_qpcr_merge$slope_std = as.numeric(final_qpcr_merge$slope_std)

# look at summaries of all the ct values
summary(final_qpcr_merge$HbtubCT1)
summary(final_qpcr_merge$HbtubCT2)
summary(final_qpcr_merge$pfr364CT1)
summary(final_qpcr_merge$pfr364CT2)

# create two new columns for each of the replicates that are restandardized to the new slope and intercept for standards 1-2000 p/uL
# will use the columns pfr364CT1 and pfr364CT2 as the observed y-values
# plug in those to y=mx+b equation solved for x -> x = (y-b)/m
pfr364Q1_std = rep(NA,nrow(final_qpcr_merge))
pfr364Q2_std = rep(NA,nrow(final_qpcr_merge))
for (i in 1:nrow(final_qpcr_merge)){
  if (is.na(final_qpcr_merge$pfr364CT1[i])){
    pfr364Q1_std[i] = NA
  } else {
    pfr364Q1_std[i] = 10^((final_qpcr_merge$pfr364CT1[i] - final_qpcr_merge$intercept_std[i])/final_qpcr_merge$slope_std[i])
  }
}
for (i in 1:nrow(final_qpcr_merge)){
  if (is.na(final_qpcr_merge$pfr364CT2[i])){
    pfr364Q2_std[i] = NA
  } else {
    pfr364Q2_std[i] = 10^((final_qpcr_merge$pfr364CT2[i] - final_qpcr_merge$intercept_std[i])/final_qpcr_merge$slope_std[i])
  }
}
final_qpcr_merge$pfr364Q1_std = pfr364Q1_std
final_qpcr_merge$pfr364Q2_std = pfr364Q2_std

# compare the output
summary(final_qpcr_merge$pfr364Q1_std)
summary(final_qpcr_merge$pfr364Q2_std)
summary(final_qpcr_merge$slope_std)
summary(final_qpcr_merge$intercept_std)
summary(final_qpcr_merge$pfr364CT1)
summary(final_qpcr_merge$pfr364CT2)
summary(final_qpcr_merge$pfr364Q1)
summary(final_qpcr_merge$pfr364Q2)

# change all the original Q1 and Q2 values that are 0 to missing
final_qpcr_merge$pfr364Q1[final_qpcr_merge$pfr364Q1 == 0] = NA
final_qpcr_merge$pfr364Q2[final_qpcr_merge$pfr364Q2 == 0] = NA


#### ---------------- create a composite parasitemia value (combine Q1 & Q2) ------------------ ####

# change final_qpcr_merge to final_results
final_results = final_qpcr_merge

# make pfr364CT numeric
summary(final_results$pfr364CT1)
summary(final_results$pfr364CT2)
final_results$pfr364CT1 = as.numeric(final_results$pfr364CT1)
final_results$pfr364CT2 = as.numeric(final_results$pfr364CT2)

# determine Sample Name that have human beta tubulin missing and need to be excluded
hbcriteria_1 = final_results[which(is.na(final_results$HbtubCT1) & !(is.na(final_results$r_value_std))),]
# pull out the vector of Sample Name
hbcriteria_1_ids = hbcriteria_1$`Sample Name`
# now the second CT value for Hb
hbcriteria_2 = final_results[which(is.na(final_results$HbtubCT2) & !(is.na(final_results$r_value_std))),]
# pull out the vector of Sample Name
hbcriteria_2_ids = hbcriteria_2$`Sample Name`

# look at original summaries of pfr364Q variables
summary(final_results$pfr364Q1_std) # 24 missing
summary(final_results$pfr364Q2_std) # 19 missing

# make a variable that censors for human beta tublin CT values missing
final_results$pfr364Q1_std_censored = ifelse(final_results$`Sample Name` %in% hbcriteria_1_ids,NA,final_results$pfr364Q1_std)
final_results$pfr364Q2_std_censored = ifelse(final_results$`Sample Name` %in% hbcriteria_2_ids,NA,final_results$pfr364Q2_std)
summary(final_results$pfr364Q1_std_censored) # 1827 missing
summary(final_results$pfr364Q2_std_censored) # 1817 missing
hbcriteria_1$pfr364Q1_std
hbcriteria_2$pfr364Q2_std
# didn't add any new missingness

# build off that variable to now make a variable that censors for pf CT values >38 and other replicate missing and rename to pfr364Q_std_censored_v2
final_results$pfr364Q1_std_censored_v2 = ifelse(final_results$pfr364CT1 >= 38 & is.na(final_results$pfr364CT2),NA,final_results$pfr364Q1_std_censored)
final_results$pfr364Q2_std_censored_v2 = ifelse(final_results$pfr364CT2 >= 38 & is.na(final_results$pfr364CT1),NA,final_results$pfr364Q2_std_censored)
summary(final_results$pfr364Q1_std_censored_v2) # 24 missing
summary(final_results$pfr364Q2_std_censored_v2) # 21 missing
# check the output one more time
length(which(final_results$pfr364CT1 >= 38 & is.na(final_results$pfr364CT2))) # 0 observations
length(which(final_results$pfr364CT2 >= 38 & is.na(final_results$pfr364CT1))) # 2 observations
# look at the original data sets with this criteria
test1 = final_results[which(final_results$pfr364CT1 >= 38 & is.na(final_results$pfr364CT2)),]
test2 = final_results[which(final_results$pfr364CT2 >= 38 & is.na(final_results$pfr364CT1)),]

# create a variable that indicates whether the sample is positive or negative for Pf malaria infection
# if at least 1 duplicate has parasitemia > 0 after criteria enforced (ie. in pfr364Q_std_censored_v2 variable), then saying sample is positive
# 1 is positive, 0 negative
pf_infection_status = rep(NA,nrow(final_results))
for (i in 1:nrow(final_results)){
  if (!(is.na(final_results$r_value_std[i])) & (is.na(final_results$pfr364Q1_std_censored_v2[i]) & is.na(final_results$pfr364Q2_std_censored_v2[i]))){
    pf_infection_status[i] = 0
  } else if ((final_results$pfr364Q1_std_censored_v2[i] > 0 | final_results$pfr364Q2_std_censored_v2[i] > 0) & !(is.na(final_results$r_value_std[i])) & (!(is.na(final_results$pfr364Q1_std_censored_v2[i])) | !(is.na(final_results$pfr364Q2_std_censored_v2[i])))){
    pf_infection_status[i] = 1
  } else {
    pf_infection_status[i] = NA
  }
}
table(pf_infection_status,useNA = "always")
# check the output
length(which(final_results$pfr364Q1_std_censored_v2 > 0 | final_results$pfr364Q2_std_censored_v2 > 0))
length(which(!(is.na(final_results$r_value_std)))) - length(which(final_results$pfr364Q1_std_censored_v2 > 0 | final_results$pfr364Q2_std_censored_v2 > 0))
length(which((is.na(final_results$r_value_std))))
# make a factor
final_results$pf_pcr_infection_status = factor(pf_infection_status,levels = c(0,1), labels = c("negative", "positive"))
# look at the output
table(final_results$pf_pcr_infection_status,useNA = "always")

# create a combined standardized new variable (combining Q1 and Q2) that was standardized from the 8 standards (1-2000 p/uL)
# if both positive parasitemia for both, then is an average of the two parasitemia for the duplicates.
# if only 1 positive parasitemia then is the value of the positive parasitemia
pfr364Q_std_combined = rep(NA,nrow(final_results))
for (k in 1:nrow(final_results)){
  if (final_results$pf_pcr_infection_status[k] == "positive" & !(is.na(final_results$pf_pcr_infection_status[k]))){
    pfr364Q_std_combined[k] = (sum(final_results$pfr364Q1_std_censored_v2[k],final_results$pfr364Q2_std_censored_v2[k],na.rm = T))/(2-(is.na(final_results$pfr364Q1_std_censored_v2[k])+is.na(final_results$pfr364Q2_std_censored_v2[k])))
  } else
    pfr364Q_std_combined[k] = NA
}
summary(pfr364Q_std_combined,useNA = "always")
# add to the data set
final_results$pfr364Q_std_combined = pfr364Q_std_combined
# check the output
summary(final_results$pfr364Q1_std)
summary(final_results$pfr364Q2_std)
summary(final_results$pfr364Q_std_combined)
checkdata = final_results[which(!(is.na(final_results$pfr364Q_std_combined))),]
head(checkdata$HbtubCT1)
head(checkdata$HbtubCT2)
head(checkdata$pfr364CT1)
head(checkdata$pfr364CT2)
head(checkdata$pfr364Q1_std_censored_v2)
head(checkdata$pfr364Q2_std_censored_v2)
head(checkdata$pf_pcr_infection_status)
head(checkdata$pfr364Q_std_combined)

# final check through all the data processing for the qPCR data
summary(checkdata$HbtubCT1)
summary(checkdata$HbtubCT2)
summary(checkdata$pfr364CT1)
summary(checkdata$pfr364CT2)
summary(final_results$pfr364Q_std_combined)
table(final_results$pf_pcr_infection_status, useNA = "always")

# final check for samples to change to missing
orig_zeroes_1 = qpcr_data[which(qpcr_data$pfr364CT1 == 0 | qpcr_data$HbtubCT1 == 0),]
orig_zeroes_2 = qpcr_data[which(qpcr_data$pfr364CT2 == 0 | qpcr_data$HbtubCT2 == 0),]
# pull out the labid_new for the samples with CT values of 0 for Pf
orig_zeroes_1_labid = orig_zeroes_1$`Sample Name`
orig_zeroes_2_labid = orig_zeroes_2$`Sample Name`
# subset the standardized parasitemia data set to look at these labid values
subset1 = final_results[which(final_results$`Sample Name` %in% orig_zeroes_1_labid),]
subset2 = final_results[which(final_results$`Sample Name` %in% orig_zeroes_2_labid),]
# none had zeroes for all Hb CT values

# also need to change values that have a hb CT value NA from negative to missing in pf_pcr_infection_status
# do this for labid_new observations that have both Hb CT values as NA
hbctbothmissing = final_results[which(is.na(final_results$HbtubCT1) & is.na(final_results$HbtubCT2) & !(is.na(final_results$r_value_std))),]
# looks like we didn't have any with all 0 CT values missing for Hb

#### ------- clean the qpcr data set ---------- ####

# now rename the final_results data set back to qpcr_data and clean it
qpcr_data = final_results

# look at all the column names
colnames(qpcr_data)

# Sample Name
table(qpcr_data$`Sample Name`, useNA = "always")
str(qpcr_data$`Sample Name`)
# check for duplicate sample names
length(unique(qpcr_data$`Sample Name`)) # 34 unique 
length(which(is.na(qpcr_data$`Sample Name`) == T)) # 0 missing
count_table = table(qpcr_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
# looks like no duplicates

# now clean up the lab id for Sample Name
table(qpcr_data$`Sample Name`, useNA = "always")
table(nchar(qpcr_data$`Sample Name`), useNA = "always")
length(which(is.na(qpcr_data$`Sample Name`))) # 0 missing IDs
# most sample names are 12 characters long but range from 10 to 15
# clean up the Sample Names
clean_sample_id = rep(NA,nrow(qpcr_data))
for (i in 1:nrow(qpcr_data)){
  if (qpcr_data$`Sample Name`[i] == "2-260618-M03-2" & !(is.na(qpcr_data$`Sample Name`[i]))){
    clean_sample_id[i] = "M03-260618-2"
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 10 & !(is.na(qpcr_data$`Sample Name`[i]))){
    clean_sample_id[i] = qpcr_data$`Sample Name`[i]
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 11 & !(is.na(qpcr_data$`Sample Name`[i]))){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[1],"-","0",parts[2],"-",parts[3]))
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 12 & !(is.na(qpcr_data$`Sample Name`[i]))){
    clean_sample_id[i] = toupper(qpcr_data$`Sample Name`[i])
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 13 & str_count(qpcr_data$`Sample Name`[i], "-") == 2 & !(is.na(qpcr_data$`Sample Name`[i]))){
    clean_sample_id[i] = toupper(qpcr_data$`Sample Name`[i])
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 14 & !(is.na(qpcr_data$`Sample Name`[i])) & qpcr_data$`Sample Name`[i] != "2-260618-M03-2"){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
  }
  if (nchar(qpcr_data$`Sample Name`[i]) == 15 & !(is.na(qpcr_data$`Sample Name`[i]))){
    parts = strsplit(qpcr_data$`Sample Name`[i],"-")[[1]]
    clean_sample_id[i] = toupper(paste0(parts[3],"-",parts[2],"-",parts[4],"-",parts[1]))
  }
}
# check the recode
table(clean_sample_id, useNA = "always")
length(which(is.na(clean_sample_id))) # 1 missing
summary(nchar(clean_sample_id))
# add the clean Sample Name to the data set
qpcr_data$`Sample Name` = clean_sample_id

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
length(which(qpcr_data$HbtubCT1 == "Undetermined" | qpcr_data$HbtubCT1 == "0" | qpcr_data$HbtubCT1 == "0.000")) # 0
length(which(qpcr_data$HbtubCT2 == "Undetermined" | qpcr_data$HbtubCT2 == "0" | qpcr_data$HbtubCT2 == "0.000")) # 0
length(which(qpcr_data$pfr364CT1 == "Undetermined" | qpcr_data$pfr364CT1 == "0" | qpcr_data$pfr364CT1 == "0.000")) # 0
length(which(qpcr_data$pfr364CT2 == "Undetermined" | qpcr_data$pfr364CT2 == "0" | qpcr_data$pfr364CT2 == "0.000")) # 0
length(which(is.na(qpcr_data$HbtubCT1))) # 0
length(which(is.na(qpcr_data$HbtubCT2))) # 0
length(which(is.na(qpcr_data$pfr364CT1))) # 24
length(which(is.na(qpcr_data$pfr364CT2))) # 19
# still looks good

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

# pfr364Y-Intercept
summary(qpcr_data$`pfr364Y-Intercept`)
str(qpcr_data$`pfr364Y-Intercept`)
# looks good, clean
# no longer need because have standardized one
qpcr_data$`pfr364Y-Intercept` <- NULL

# pfr364R_
summary(qpcr_data$pfr364R_)
str(qpcr_data$pfr364R_)
# look good, clean
# no longer need because have standardized one
qpcr_data$pfr364R_ <- NULL

# pfr364Slope
summary(qpcr_data$pfr364Slope)
str(qpcr_data$pfr364Slope)
# looks good, clean
# no longer need because have standardized one
qpcr_data$pfr364Slope <- NULL

# Hb standards
summary(qpcr_data$HbtubStd1a)
summary(qpcr_data$HbtubStd1b)
summary(qpcr_data$HbtubStd2a)
summary(qpcr_data$HbtubStd2b)
summary(qpcr_data$HbtubStd3a)
summary(qpcr_data$HbtubStd3b)
summary(qpcr_data$HbtubStd4a)
summary(qpcr_data$HbtubStd4b)
summary(qpcr_data$HbtubStd5a)
summary(qpcr_data$HbtubStd5b)
summary(qpcr_data$HbtubStd6a)
summary(qpcr_data$HbtubStd6b)
summary(qpcr_data$HbtubStd7a)
summary(qpcr_data$HbtubStd7b)
summary(qpcr_data$HbtubStd8a)
summary(qpcr_data$HbtubStd8b)
summary(qpcr_data$HbtubStd9a)
summary(qpcr_data$HbtubStd9b)
summary(qpcr_data$HbtubStd10a)
summary(qpcr_data$HbtubStd10b)

# Pf standards
summary(qpcr_data$pfr364Std1a)
summary(qpcr_data$pfr364Std1b)
summary(qpcr_data$pfr364Std2a)
summary(qpcr_data$pfr364Std2b)
summary(qpcr_data$pfr364Std3a)
summary(qpcr_data$pfr364Std3b)
summary(qpcr_data$pfr364Std4a)
summary(qpcr_data$pfr364Std4b)
summary(qpcr_data$pfr364Std5a)
summary(qpcr_data$pfr364Std5b)
summary(qpcr_data$pfr364Std6a)
summary(qpcr_data$pfr364Std6b)
summary(qpcr_data$pfr364Std7a)
summary(qpcr_data$pfr364Std7b)
summary(as.numeric(qpcr_data$pfr364Std8a))
summary(as.numeric(qpcr_data$pfr364Std8b))
summary(qpcr_data$pfr364Std9a)
summary(qpcr_data$pfr364Std9b)
summary(qpcr_data$pfr364Std10a)
summary(as.numeric(qpcr_data$pfr364Std10b))

# model_name
table(qpcr_data$model_name, useNA = "always")
str(qpcr_data$model_name)
# don't need anymore, remove
qpcr_data$model_name <- NULL

# r_value_std
summary(qpcr_data$r_value_std)
str(qpcr_data$r_value_std)
# looks good, high R2 values now

# intercept_std
summary(qpcr_data$intercept_std)
str(qpcr_data$intercept_std)
# looks good

# slope_std
summary(qpcr_data$slope_std)
str(qpcr_data$slope_std)
# looks good

# pfr364Q1_std
summary(qpcr_data$pfr364Q1_std)
str(qpcr_data$pfr364Q1_std)
# don't need anymore, remove
qpcr_data$pfr364Q1_std <- NULL

# pfr364Q2_std
summary(qpcr_data$pfr364Q2_std)
str(qpcr_data$pfr364Q2_std)
# don't need anymore, remove
qpcr_data$pfr364Q2_std <- NULL

# pfr364Q1_std_censored
summary(qpcr_data$pfr364Q1_std_censored)
str(qpcr_data$pfr364Q1_std_censored)
# don't need anymore, remove
qpcr_data$pfr364Q1_std_censored <- NULL

# pfr364Q2_std_censored
summary(qpcr_data$pfr364Q2_std_censored)
str(qpcr_data$pfr364Q2_std_censored)
# don't need anymore, remove
qpcr_data$pfr364Q2_std_censored <- NULL

# pfr364Q1_std_censored_v2
summary(qpcr_data$pfr364Q1_std_censored_v2)
str(qpcr_data$pfr364Q1_std_censored_v2)
# rename the variable to pfr364Q1_std
colnames(qpcr_data)[colnames(qpcr_data) == 'pfr364Q1_std_censored_v2'] <- 'pfr364Q1_std'

# pfr364Q2_std_censored_v2
summary(qpcr_data$pfr364Q2_std_censored_v2)
str(qpcr_data$pfr364Q2_std_censored_v2)
# rename the variable to pfr364Q2_std
colnames(qpcr_data)[colnames(qpcr_data) == 'pfr364Q2_std_censored_v2'] <- 'pfr364Q2_std'

# pf_pcr_infection_status
table(qpcr_data$pf_pcr_infection_status, useNA = "always")
str(qpcr_data$pf_pcr_infection_status)
# looks good, clean

# pfr364Q_std_combined
summary(qpcr_data$pfr364Q_std_combined)
str(qpcr_data$pfr364Q_std_combined)
# looks good, clean

# export the clean rerun data set
write_csv(qpcr_data,"spat21_qpcr_data_B8-10-11redo.csv")
write_rds(qpcr_data,"spat21_qpcr_data_B8-10-11redo.rds")


#### --------- now merge in the qpcr_data from the rerun (now clean!) with the already merged data set ---------- ####

# rename qpcr_data to human_qpcr_data
human_qpcr_data = qpcr_data

# first check for duplicates in the sample name column for the human_merged_all_data data set
length(unique(human_merged_all_data$`Sample Name`)) # 3919 unique 
length(which(is.na(human_merged_all_data$`Sample Name`) == T)) # 0 missing
count_table = table(human_merged_all_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates

# first remove the columns that are not needed (standards)
cols_to_remove = c("HbtubStd1a","HbtubStd1b","HbtubStd2a","HbtubStd2b","HbtubStd3a","HbtubStd3b",
                   "HbtubStd4a","HbtubStd4b","HbtubStd5a","HbtubStd5b","HbtubStd6a","HbtubStd6b",
                   "HbtubStd7a","HbtubStd7b","HbtubStd8a","HbtubStd8b","HbtubStd9a","HbtubStd9b",
                   "HbtubStd10a","HbtubStd10b","pfr364Std1a","pfr364Std1b","pfr364Std2a","pfr364Std2b",
                   "pfr364Std3a","pfr364Std3b","pfr364Std4a","pfr364Std4b","pfr364Std5a","pfr364Std5b",
                   "pfr364Std6a","pfr364Std6b","pfr364Std7a","pfr364Std7b","pfr364Std8a","pfr364Std8b",
                   "pfr364Std9a","pfr364Std9b","pfr364Std10a","pfr364Std10b","r_value_std","intercept_std",
                   "slope_std", "Well Position","HbtubCT1","HbtubCT2","pfr364CT1","pfr364CT2",
                   "pfr364Q1","pfr364Q2","pfr364Q1_std","pfr364Q2_std","Experiment Name")
human_qpcr_data = human_qpcr_data[,!(colnames(human_qpcr_data) %in% cols_to_remove)]

# check if any of the human qpcr ids are in the human merged data set
intersect(human_qpcr_data$`Sample Name`,human_merged_all_data$sample_name_dbs)
length(intersect(human_qpcr_data$`Sample Name`,human_merged_all_data$sample_name_dbs))
# looks like 15/34 ids the same
# create a data set to check this
checkdata = human_merged_all_data[which(human_merged_all_data$sample_name_dbs %in% intersect(human_qpcr_data$`Sample Name`,human_merged_all_data$sample_name_dbs)),]
checkdata = checkdata[,c(125:135)]

# best method might be to actually go through original data set and re-clean
# have decided to try this







