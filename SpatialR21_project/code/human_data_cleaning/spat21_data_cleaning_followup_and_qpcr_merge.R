# ----------------------------------------- #
#  Spat21 Merging Follow-up and qPCR Data   #
#                Human Data                 #
#               March 1, 2019               #
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


#### ------- read in the data sets -------- ####

# read in the merged human monthly and table data set
human_merged_all_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/hum_monthly_merged_with_table_and_sick_with_exclusion_data_27FEB2019.RDS")

# read in the preliminary qpcr data
human_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/spat21_qpcr_data_clean_human_dbs_16JAN2019.RDS")


#### ------- merge in the qpcr data with the human monthly, table, and sick data ------ ####

# calculate how many RDT+ results for the same person
participant_data = human_merged_all_data %>%
  filter(rdt_rst == "positive") %>%
  group_by(unq_memID) %>%
  summarize(n=n()) 
repeated_infections = participant_data[which(participant_data$n>1),]
summary(repeated_infections$n)

# add an "R" to the end of M03-0618-2-R for the one with M03-260618-2 because was actually a sick visit for the qpcr data
human_qpcr_data$`Sample Name`[human_qpcr_data$`Sample Name` == "M03-260618-2"] = "M03-260618-2-R" 
# change M16-270618-P-R to M16-270618-4-R in the qpcr data
human_qpcr_data$`Sample Name`[human_qpcr_data$`Sample Name` == "M16-270618-P-R"] = "M16-270618-4-R" 
# change M15-311017-P-R to M15-311017-6-R in the qpcr data
human_qpcr_data$`Sample Name`[human_qpcr_data$`Sample Name` == "M15-311017-P-R"] = "M15-311017-6-R" 
# change K07-030817-08 to K07-030817-8 in the qpcr data
human_qpcr_data$`Sample Name`[human_qpcr_data$`Sample Name` == "K07-030817-08"] = "K07-030817-8" 
# change K07-030817-09 to K07-030817-9 in the qpcr data
human_qpcr_data$`Sample Name`[human_qpcr_data$`Sample Name` == "K07-030817-09"] = "K07-030817-9" 

# first check for duplicates in the sample name column for the human_merged_all_data data set
length(unique(human_merged_all_data$`Sample Name`)) # 3141 unique 
length(which(is.na(human_merged_all_data$`Sample Name`) == T)) # 0 missing
count_table = table(human_merged_all_data$`Sample Name`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates

# merge the qpcr data with the human_merged_all_data
human_merged_all_data$`Sample Name` = as.character(human_merged_all_data$`Sample Name`)
human_qpcr_data$`Sample Name` = as.character(human_qpcr_data$`Sample Name`)
human_merged_all_data_qpcr = left_join(human_merged_all_data,human_qpcr_data,by = "Sample Name")
# check the merge
length(which(is.na(human_qpcr_data$`Experiment Name`))) # 0 missing
length(which(is.na(human_merged_all_data_qpcr$`Experiment Name`))) # 1061 missing
# looks like a lot of sample IDs didn't merge
length(intersect(human_merged_all_data$`Sample Name`, human_qpcr_data$`Sample Name`)) # 2086 samples merged
length(setdiff(human_merged_all_data$`Sample Name`,human_qpcr_data$`Sample Name`)) # 1055
length(setdiff(human_qpcr_data$`Sample Name`,human_merged_all_data$`Sample Name`)) # 804
# all 804 from the qpcr didn't merge into full data set 
# 3141-2890 = didn't have 252 dbs from follow-ups
# look at the outersect
outersect <- function(x, y, ...) {
  big.vec <- c(x, y, ...)
  duplicates <- big.vec[duplicated(big.vec)]
  setdiff(big.vec, unique(duplicates))
}
length(outersect(human_merged_all_data$`Sample Name`,human_qpcr_data$`Sample Name`)) # 1868
# look at which qpcr ids did not merge in with the other data set
setdiff(human_qpcr_data$`Sample Name`,human_merged_all_data$`Sample Name`) # 808
# check S08-250118-8 and see if similar to S08-270118-8
test = human_merged_all_data_qpcr[which(human_merged_all_data_qpcr$`Sample Name` == "S08-270118-8"),]
length(which(is.na(test$`Experiment Name`))) # didn't merge in!!!
# check S11-240817-8 and see if similar to S11-250817-8
test = human_merged_all_data_qpcr[which(human_merged_all_data_qpcr$`Sample Name` == "S11-250817-8"),]
length(which(is.na(test$`Experiment Name`))) # didn't merge in!!!
test = human_merged_all_data_qpcr[which(human_merged_all_data_qpcr$unq_memID == "S11_8"),]
test$monthly_unq_memID
test$sick_unq_memID
test$`Sample Name`
test$`Experiment Name`
# it looks like sometimes the follow-up date and the DBS date are off by a few days

# for all the samples that didn't merge, try to merge them based on the month and year
# decide which variables to use
length(which(is.na(human_merged_all_data_qpcr$month_year_combo_monthly_data))) # 520 missing
length(which(is.na(human_merged_all_data_qpcr$HH_ID))) # 0 missing
length(which(is.na(human_merged_all_data_qpcr$memID))) # 0 missing
# create a second variable for the unqmemID, HH_ID, and MM-YY
new_sample_id = rep(NA,nrow(human_merged_all_data_qpcr))
for (i in 1:nrow(human_merged_all_data_qpcr)){
  if (!(is.na(human_merged_all_data_qpcr$month_year_combo_monthly_data[i]))){
    monthfollowup1 = as.character(month(human_merged_all_data_qpcr$today_hum_monthly_data[i]))
    if (nchar(monthfollowup1) == 1){
      monthfollowup = paste0("0",as.character(monthfollowup1))
    } else {
      monthfollowup = monthfollowup1
    }
    yearfollowup1 = strsplit(as.character(year(human_merged_all_data_qpcr$today_hum_monthly_data[i])),"")[[1]]
    yearfollowup = paste0(yearfollowup1[c(3,4)], collapse="")
    month_year_short_2 = paste0(monthfollowup,yearfollowup)
    new_sample_id[i] = paste0(human_merged_all_data_qpcr$HH_ID[i],"-",month_year_short_2,"-",human_merged_all_data_qpcr$memID[i])
  } else {
    monthsick1 = as.character(month(human_merged_all_data_qpcr$today_hum_sick_data[i]))
    if (nchar(monthsick1) == 1){
      monthsick = paste0("0",as.character(monthsick1))
    } else {
      monthsick = monthsick1
    }
    yearsick1 = strsplit(as.character(year(human_merged_all_data_qpcr$today_hum_sick_data[i])),"")[[1]]
    yearsick = paste0(yearsick1[c(3,4)], collapse="")
    month_year_short_2 = paste0(monthsick,yearsick)
    new_sample_id[i] = paste0(human_merged_all_data_qpcr$HH_ID[i],"-",month_year_short_2,"-",human_merged_all_data_qpcr$memID[i],"-R")
  }
}
# check the output
length(which(is.na(new_sample_id))) # 0 missing
head(human_merged_all_data_qpcr$month_year_combo_monthly_data)
head(human_merged_all_data_qpcr$HH_ID)
head(human_merged_all_data$memID)
head(new_sample_id)
table(nchar(new_sample_id),useNA = "always")
tail(human_merged_all_data_qpcr$today_hum_sick_data)
tail(human_merged_all_data_qpcr$HH_ID)
tail(human_merged_all_data$memID)
tail(new_sample_id)
# add the new sample id to the data set
human_merged_all_data_qpcr$new_sample_id = new_sample_id

# now merge in those qPCR DBS that didn't already merge into the data set
# pull out those that didn't merge
dbs_no_merge = setdiff(human_qpcr_data$`Sample Name`,human_merged_all_data$`Sample Name`) 
dbs_no_merge_df = human_qpcr_data[which(human_qpcr_data$`Sample Name` %in% dbs_no_merge),]
# create a second variable for the unqmemID, HH_ID, and MM-YY for the qpcr data
new_sample_id = rep(NA,nrow(dbs_no_merge_df))
for (i in 1:nrow(dbs_no_merge_df)){
  firstsplit = strsplit(dbs_no_merge_df$`Sample Name`[i],"-")[[1]]
  if (length(firstsplit) == 4){
    secondsplit = as.character(strsplit(firstsplit[2],"")[[1]])
    remergedates = paste0(secondsplit[c(3,4,5,6)],collapse="")
    new_sample_id[i] = paste0(firstsplit[1],"-",remergedates,"-",firstsplit[3],"-R")
  } else {
    secondsplit = as.character(strsplit(firstsplit[2],"")[[1]])
    remergedates = paste0(secondsplit[c(3,4,5,6)],collapse="")
    new_sample_id[i] = paste0(firstsplit[1],"-",remergedates,"-",firstsplit[3])
  }
}
# check the output
length(which(is.na(new_sample_id))) # 0 missing
head(dbs_no_merge_df$`Sample Name`)
head(new_sample_id)
table(nchar(new_sample_id),useNA = "always")
tail(dbs_no_merge_df$`Sample Name`)
tail(new_sample_id)
# add the new sample id to the data set
dbs_no_merge_df$new_sample_id = new_sample_id

# check for duplicates in new_sample_id for the human_merged_all_data_qpcr data set
length(unique(human_merged_all_data_qpcr$new_sample_id)) # 3109 unique 
length(which(is.na(human_merged_all_data_qpcr$new_sample_id) == T)) # 0 missing
count_table = table(human_merged_all_data_qpcr$new_sample_id, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 31 duplicates
# check to see if any of these duplicates were among those not matched, and there aren't any

# check for duplicates in new_sample_id for the dbs_no_merge_df data set
length(unique(dbs_no_merge_df$new_sample_id)) # 804 unique 
length(which(is.na(dbs_no_merge_df$new_sample_id) == T)) # 0 missing
count_table = table(dbs_no_merge_df$new_sample_id, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates left

# now merge in the dbs_no_merge_df to the human_merged_all_data_qpcr by new_sample_id
test_merge_all = left_join(human_merged_all_data_qpcr,dbs_no_merge_df,by="new_sample_id")
colnames(test_merge_all)
# check the merge
length(intersect(human_merged_all_data_qpcr$new_sample_id,dbs_no_merge_df$new_sample_id)) # 725/804 samples merged
length(setdiff(human_merged_all_data_qpcr$new_sample_id,dbs_no_merge_df$new_sample_id)) # 2383
length(setdiff(dbs_no_merge_df$new_sample_id,human_merged_all_data_qpcr$new_sample_id)) # 79
# still have 79 samples to match
setdiff(dbs_no_merge_df$new_sample_id,human_merged_all_data_qpcr$new_sample_id)
# looks like the R wasn't always added to the DBS for the sick data
# note that 3 of these are S12_1 which was removed from that study and 4 of these are duplicates so really only 72 left to match

# first remove the columns that are not needed (standards)
cols_to_remove = c("HbtubStd1a.x","HbtubStd1b.x","HbtubStd2a.x","HbtubStd2b.x","HbtubStd3a.x","HbtubStd3b.x",
                   "HbtubStd4a.x","HbtubStd4b.x","HbtubStd5a.x","HbtubStd5b.x","HbtubStd6a.x","HbtubStd6b.x",
                   "HbtubStd7a.x","HbtubStd7b.x","HbtubStd8a.x","HbtubStd8b.x","HbtubStd9a.x","HbtubStd9b.x",
                   "HbtubStd10a.x","HbtubStd10b.x","pfr364Std1a.x","pfr364Std1b.x","pfr364Std2a.x","pfr364Std2b.x",
                   "pfr364Std3a.x","pfr364Std3b.x","pfr364Std4a.x","pfr364Std4b.x","pfr364Std5a.x","pfr364Std5b.x",
                   "pfr364Std6a.x","pfr364Std6b.x","pfr364Std7a.x","pfr364Std7b.x","pfr364Std8a.x","pfr364Std8b.x",
                   "pfr364Std9a.x","pfr364Std9b.x","pfr364Std10a.x","pfr364Std10b.x","r_value_std.x","intercept_std.x",
                   "slope_std.x", "HbtubStd1a.y","HbtubStd1b.y","HbtubStd2a.y","HbtubStd2b.y","HbtubStd3a.y","HbtubStd3b.y",
                   "HbtubStd4a.y","HbtubStd4b.y","HbtubStd5a.y","HbtubStd5b.y","HbtubStd6a.y","HbtubStd6b.y",
                   "HbtubStd7a.y","HbtubStd7b.y","HbtubStd8a.y","HbtubStd8b.y","HbtubStd9a.y","HbtubStd9b.y",
                   "HbtubStd10a.y","HbtubStd10b.y","pfr364Std1a.y","pfr364Std1b.y","pfr364Std2a.y","pfr364Std2b.y",
                   "pfr364Std3a.y","pfr364Std3b.y","pfr364Std4a.y","pfr364Std4b.y","pfr364Std5a.y","pfr364Std5b.y",
                   "pfr364Std6a.y","pfr364Std6b.y","pfr364Std7a.y","pfr364Std7b.y","pfr364Std8a.y","pfr364Std8b.y",
                   "pfr364Std9a.y","pfr364Std9b.y","pfr364Std10a.y","pfr364Std10b.y","r_value_std.y","intercept_std.y",
                   "slope_std.y", "Well Position.y","HbtubCT1.y","HbtubCT2.y","pfr364CT1.y","pfr364CT2.y",
                   "pfr364Q1.y","pfr364Q2.y","pfr364Q1_std.y","pfr364Q2_std.y", "Well Position.x","HbtubCT1.x","HbtubCT2.x","pfr364CT1.x","pfr364CT2.x",
                   "pfr364Q1.x","pfr364Q2.x","pfr364Q1_std.x","pfr364Q2_std.x")
test_merge_all = test_merge_all[,!(colnames(test_merge_all) %in% cols_to_remove)]

# merge together the qpcr columns
#  sample name
colnames(test_merge_all)
length(which(is.na(test_merge_all$`Sample Name.y`))) # 2416
length(which(is.na(test_merge_all$`Sample Name.x`))) # 0
# not going to change for now

# experiment name
length(which(is.na(test_merge_all$`Experiment Name.y`))) # 2416
length(which(is.na(test_merge_all$`Experiment Name.x`))) # 1055
experiment_name = ifelse(is.na(test_merge_all$`Experiment Name.y`),test_merge_all$`Experiment Name.x`,test_merge_all$`Experiment Name.y`)
length(which(is.na(experiment_name))) # still 333 missing
test_merge_all$experiment_name = experiment_name
# 3141-2890 = 251 that should be missing at the end
# 76 DBS samples didn't merge
# 79+251 = 330 samples
# create test data set where experiment name is missing that indicates it didn't merge (so three need to be accounted for)
#test = test_merge_all[which(is.na(test_merge_all$experiment_name)),]
# create a test data set that indicates which qpcr data did not merge
#dbs_no_merge_2 = setdiff(dbs_no_merge_df$new_sample_id,human_merged_all_data_qpcr$new_sample_id) 
#dbs_no_merge_2_df = dbs_no_merge_df[which(dbs_no_merge_df$new_sample_id %in% dbs_no_merge_2),]
#dbs_no_merge_df_2 = human_qpcr_data[which(human_qpcr_data$`Sample Name` %in% dbs_no_merge_2_df$`Sample Name`),]

# export the data sets to figure out where Rs were left off of DBS data set
# write_csv(test,"human_descriptive_no_merge_with_qpcr.csv")
# write_csv(dbs_no_merge_df_2,"qpcr_no_merge_with_human_descriptive.csv")

# while still trying to figure out why those 72 DBS didn't merge, stick with data set that did merge
human_merged_all_data_final = test_merge_all

# check for participants who a symptomatic visit on the same day as an asymptomatic visit
# take off R for sick data and test to see if any IDs match monthly data
double_visit_test = rep(NA,nrow(human_merged_all_data_final))
for (i in 1:nrow(human_merged_all_data_final)){
  if (str_detect(human_merged_all_data_final$`Sample Name.x`[i],"R")){
    firstsplit = strsplit(human_merged_all_data_final$`Sample Name.x`[i],"-")[[1]]
    double_visit_test[i] = paste0(firstsplit[1],"-",firstsplit[2],"-",firstsplit[3],collapse="")
  } else {
    double_visit_test[i] = human_merged_all_data_final$`Sample Name.x`[i]
  }
}
# look for duplicates in double visit_test
length(unique(double_visit_test)) # 2920 unique/3141 obs
length(which(is.na(double_visit_test) == T)) # 0 missing
count_table = table(double_visit_test, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 221 duplicates
length(dups_table)
dups_table_df = data.frame(dups_table)
# for all these IDs, remove the asymptomatic visit information
human_merged_all_data_final$double_visit_test = double_visit_test
# pull out the duplicate visits
duplicate_visits = human_merged_all_data_final[which(human_merged_all_data_final$double_visit_test %in% dups_table_df$double_visit_test),]
# quickly figure how many of these sick visits on monthly data collection days had a positive RDT
table(duplicate_visits$rdt_rst, useNA = "always") # 71 positive, 150 negative

# check to see if any people who had qpcr data for both monthly follow-up and sick visit
length(which(!(is.na(duplicate_visits$today_hum_monthly_data)) & (!(is.na(duplicate_visits$`Experiment Name.x`)) | !(is.na(duplicate_visits$`Experiment Name.y`)))))
length(which(!(is.na(duplicate_visits$today_hum_sick_data)) & (!(is.na(duplicate_visits$`Experiment Name.x`)) | !(is.na(duplicate_visits$`Experiment Name.y`)))))
length(which(is.na(duplicate_visits$`Experiment Name.x`) & is.na(duplicate_visits$`Experiment Name.y`))) # 219 missing both so theoretically 2 that have two DBS
# look at a dataframe of merge results
small_df_for_merge_results = data.frame(duplicate_visits$double_visit_test, duplicate_visits$`Sample Name.x`,duplicate_visits$`Experiment Name.x`,duplicate_visits$`Experiment Name.y`)
# 7 had asymp and symp DBS collected on same day:
# K01-030817-2, K01-110717-9, K01-120617-4, K02-050817-9, M06-130617-4, M13-130717-1, S01-240817-6
# check qpcr results to see if discrepant
# K01-030817-2 - both positive
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "K01-030817-2"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y
# K01-110717-9 - both positive
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "K01-110717-9"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y
# K01-120617-4 - both positive
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "K01-120617-4"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y
# K02-050817-9 - both positive
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "K02-050817-9"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y
# M06-130617-4 - both positive
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "M06-130617-4"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y
# M13-130717-1 - both negative
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "M13-130717-1"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y
# S01-240817-6 - both negative
test1 = duplicate_visits[which(duplicate_visits$double_visit_test == "S01-240817-6"),]
test1$pf_pcr_infection_status.x
test1$pf_pcr_infection_status.y

# first merge together the qpcr columns
# delete other columns
human_merged_all_data_final$new_sample_id <- NULL
human_merged_all_data_final$experiment_name <- NULL
human_merged_all_data_final$double_visit_test <- NULL
colnames(human_merged_all_data_final)
# figure out some checks ahead of time
length(which(is.na(human_merged_all_data_final$`Experiment Name.x`))) # 1055
length(which(is.na(human_merged_all_data_final$`Experiment Name.y`))) # 2416
# pull out a file where .x is present but not .y
human_merged_all_data_final$`Experiment Name.x`[10]
# pull out a file where .y is present but not .x
human_merged_all_data_final$`Experiment Name.y`[1]
# check for any instances where have two pcr results in same row
length(which(!(is.na(human_merged_all_data_final$`Experiment Name.x`)) & !(is.na(human_merged_all_data_final$`Experiment Name.y`)))) # there are 3 of these
# pull out those 3 that double merged
double_merged_qpcr_data = human_merged_all_data_final[which(!(is.na(human_merged_all_data_final$`Experiment Name.x`)) & !(is.na(human_merged_all_data_final$`Experiment Name.y`))),]
double_merged_qpcr_data$`Sample Name.x`
double_merged_qpcr_data$`Sample Name.y`
double_merged_qpcr_data$pf_pcr_infection_status.x
double_merged_qpcr_data$pf_pcr_infection_status.y
# make the 240617 IDs NA because weren't a perfect merge
colnames(human_merged_all_data_final)
# S11-240617-1
human_merged_all_data_final$`Sample Name.y`[which(human_merged_all_data_final$`Sample Name.y`=="S11-240617-1")] = NA
human_merged_all_data_final$`Experiment Name.y`[which(human_merged_all_data_final$`Sample Name.y`=="S11-240617-1")] = NA
human_merged_all_data_final$`pfr364Q_std_combined.y`[which(human_merged_all_data_final$`Sample Name.y`=="S11-240617-1")] = NA
# S11-240617-5
human_merged_all_data_final$`Sample Name.y`[which(human_merged_all_data_final$`Sample Name.y`=="S11-240617-5")] = NA
human_merged_all_data_final$`Experiment Name.y`[which(human_merged_all_data_final$`Sample Name.y`=="S11-240617-5")] = NA
human_merged_all_data_final$`pfr364Q_std_combined.y`[which(human_merged_all_data_final$`Sample Name.y`=="S11-240617-5")] = NA
# S11-240617-9
human_merged_all_data_final$`Sample Name.y`[which(human_merged_all_data_final$`Sample Name.y`=="S11-240617-9")] = NA
human_merged_all_data_final$`Experiment Name.y`[which(human_merged_all_data_final$`Sample Name.y`=="S11-240617-9")] = NA
human_merged_all_data_final$`pfr364Q_std_combined.y`[which(human_merged_all_data_final$`Sample Name.y`=="S11-240617-9")] = NA
# recheck for any instances where have two pcr results in same row
small_check = human_merged_all_data_final[,120:135]

# check colnames
colnames(human_merged_all_data_final)
# start for loop to combine qpcr results
for (i in 1:nrow(human_merged_all_data_final)){
  if (is.na(human_merged_all_data_final[i,129])){
    for (k in 1:4){   # this is for all data that is present in .y files but not in .x
      startpoint = 127 + k
      human_merged_all_data_final[i,startpoint] = human_merged_all_data_final[i,startpoint+4]
      human_merged_all_data_final[i,startpoint+4] <- NA
    }
  } else if (is.na(human_merged_all_data_final[i,132])){
    for (k in 1:4){ # this is for all data that is present in .x files but not in .y -> just make it NULL
      startpoint = 127 + k
      human_merged_all_data_final[i,startpoint+4] <- NA
    }
  } else {
    for (k in 1:4){ # both data sets are missing -> just make it NULL at .y location
      startpoint = 127 + k
      human_merged_all_data_final[i,startpoint+4] <- NA
    }
  } 
}
# check the output
length(which(is.na(human_merged_all_data_final$`Experiment Name.x`))) # 333
length(which(is.na(human_merged_all_data_final$`Experiment Name.y`))) # 3141
# looks like it is working correctly
small_check = human_merged_all_data_final[,120:135]
# remove the .y columns
human_merged_all_data_final = human_merged_all_data_final[,-c(132:135)]
colnames(human_merged_all_data_final)

# now move up the columns for those ids that had the monthly follow-up and sick visit on the same day
# re-add the double visit test names to the data set to point out people who had the sick and monthly visit on the same day
# create a new sample name variable
# merge together the monthly_unq_memID and sick_unq_memID columns
length(which(is.na(human_merged_all_data_final$monthly_unq_memID))) # 520 - all the sick IDs
length(which(is.na(human_merged_all_data_final$sick_unq_memID))) # 2621 - all the monthly IDs
monthly_or_sick_unq_memID = ifelse(is.na(human_merged_all_data_final$sick_unq_memID),human_merged_all_data_final$monthly_unq_memID,human_merged_all_data_final$sick_unq_memID)
length(which(is.na(monthly_or_sick_unq_memID))) # no missing, which is good
# add to the data set and compare with the monthly and sick ID columns
human_merged_all_data_final$sample_name_final = monthly_or_sick_unq_memID
comparison_df = data.frame(human_merged_all_data_final$sample_name_final,human_merged_all_data_final$monthly_unq_memID, human_merged_all_data_final$sick_unq_memID)

# take off R for sick data and test to see if any IDs match monthly data
double_visit_test = rep(NA,nrow(human_merged_all_data_final))
for (i in 1:nrow(human_merged_all_data_final)){
  if (str_detect(human_merged_all_data_final$sample_name_final[i],"R")){
    firstsplit = strsplit(human_merged_all_data_final$sample_name_final[i],"-")[[1]]
    double_visit_test[i] = paste0(firstsplit[1],"-",firstsplit[2],"-",firstsplit[3],collapse="")
  } else {
    double_visit_test[i] = human_merged_all_data_final$sample_name_final[i]
  }
}
# look for duplicates in double visit_test
length(unique(double_visit_test)) # 2920 unique/3141 obs
length(which(is.na(double_visit_test) == T)) # 0 missing
count_table = table(double_visit_test, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 221 duplicates
length(dups_table)
dups_table_df = data.frame(dups_table)
# for all these IDs, remove the asymptomatic visit information 
human_merged_all_data_final$double_visit_test = double_visit_test
head(human_merged_all_data_final$double_visit_test,20)
small_check = human_merged_all_data_final[,120:133]
# sort the double_visit_test variable
human_merged_all_data_final = human_merged_all_data_final[order(human_merged_all_data_final$double_visit_test),]
small_check = human_merged_all_data_final[,120:133]
# look at the colnames
colnames(human_merged_all_data_final)
length(which(is.na(double_visit_test))) # no missing for double_visit_test
# for each duplicate visit, write some code that pulls the duplicate sick visit information into the corresponding sick columns for the monthly visit
# want to keep all the sick visit and monthly visit entries and the qpcr results (which could be associated with either entry)
# write a for loop that will check for if the participant has a sick visit on the same day and moves information all on one line
# start for loop
for (i in 1:nrow(human_merged_all_data_final)){
  if ((human_merged_all_data_final$double_visit_test[i] == human_merged_all_data_final$double_visit_test[i+1]) & !(is.na(human_merged_all_data_final$double_visit_test[i])) & !(is.na(human_merged_all_data_final$double_visit_test[i+1])) & i != nrow(human_merged_all_data_final)){
       for (k in 1:ncol(human_merged_all_data_final)){
         if (is.na(human_merged_all_data_final[i,k]) & !(is.na(human_merged_all_data_final[i+1,k]))){
           human_merged_all_data_final[i,k] = human_merged_all_data_final[i+1,k]
         }
         if (is.na(human_merged_all_data_final[i+1,k]) & !(is.na(human_merged_all_data_final[i,k]))){
           human_merged_all_data_final[i+1,k] = human_merged_all_data_final[i,k]
         }
       }
  } else {
    human_merged_all_data_final[i,] = human_merged_all_data_final[i,]
  }
}
# check the output
colnames(human_merged_all_data_final)
length(which(is.na(human_merged_all_data_final$monthly_unq_memID))) # 299 - (520-221 = 299)
length(which(is.na(human_merged_all_data_final$sick_unq_memID))) # 2400 - (2621-221 = 2400)
# looks like it is working correctly
# delete the rows that are now duplicates
for (i in 1:nrow(human_merged_all_data_final)){
  if ((human_merged_all_data_final$double_visit_test[i] == human_merged_all_data_final$double_visit_test[i+1]) & !(is.na(human_merged_all_data_final$double_visit_test[i])) & !(is.na(human_merged_all_data_final$double_visit_test[i+1])) & i != nrow(human_merged_all_data_final)){
    human_merged_all_data_final = human_merged_all_data_final[-i,]
  }
}
# check the output
length(which(is.na(human_merged_all_data_final$monthly_unq_memID))) # 299 - (520-221 = 299)
length(which(is.na(human_merged_all_data_final$sick_unq_memID))) # 2400 - (2621-221 = 2400)
# also tested a few ids in dups_table_df to see if occurred in duplicate still
# all looks good

# check colnames
colnames(human_merged_all_data_final)

# rename some column names
human_merged_all_data_final = rename(human_merged_all_data_final, sample_name_dbs = `Sample Name.x`, experiment_name = `Experiment Name.x`, 
                                     pf_pcr_infection_status = pf_pcr_infection_status.x, pfr364Q_std_combined = pfr364Q_std_combined.x)
colnames(human_merged_all_data_final)

# remove participant S11_4 because only in sick visit data set
human_merged_all_data_final = human_merged_all_data_final[-which(human_merged_all_data_final$unq_memID == "S11_4"),]

# create a new column that differentiates whether it was a monthly or sick visit
visit_type = rep("monthly visit",nrow(human_merged_all_data_final))
human_merged_all_data_final$visit_type = visit_type
human_merged_all_data_final$visit_type[which(str_detect(human_merged_all_data_final$sample_name_final,"R"))] = "sick visit"
# check the output
table(human_merged_all_data_final$visit_type, useNA = "always")
# looks good

# add a column for the main exposure
### NOTE: you will have to change # in "no infection" category for each outcome case definition
# right now just changed # in "no infection" category based on primary outcome case defintion
# need to check this again in the future
main_exposure = rep(NA,nrow(human_merged_all_data_final))
human_merged_all_data_final$main_exposure = main_exposure
human_merged_all_data_final$main_exposure[which(human_merged_all_data_final$pf_pcr_infection_status == "negative" & !(is.na(human_merged_all_data_final$monthly_unq_memID)) |
                                                  human_merged_all_data_final$rdt_rst == "negative" & human_merged_all_data_final$pf_pcr_infection_status == "positive" & !(is.na(human_merged_all_data_final$monthly_unq_memID)))] = "no infection"
# the no infection category includes people who didn't have symp infection but had sick visit on same day as monthly visit
human_merged_all_data_final$main_exposure[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & human_merged_all_data_final$visit_type == "monthly visit")] = "asymptomatic infection"
# check the output
table(human_merged_all_data_final$main_exposure, useNA = "always")
length(which(human_merged_all_data_final$pf_pcr_infection_status == "negative" & !(is.na(human_merged_all_data_final$monthly_unq_memID))))
length(which(human_merged_all_data_final$rdt_rst == "negative" & human_merged_all_data_final$pf_pcr_infection_status == "positive" & !(is.na(human_merged_all_data_final$monthly_unq_memID)))) # 51 people had monthly visit on same day, how to handle these
length(which(human_merged_all_data_final$rdt_rst == "positive" & human_merged_all_data_final$pf_pcr_infection_status == "positive" & human_merged_all_data_final$visit_type == "sick visit")) # 153 obs
length(which(human_merged_all_data_final$rdt_rst == "positive" & human_merged_all_data_final$pf_pcr_infection_status == "positive" & !(is.na(human_merged_all_data_final$monthly_unq_memID)))) # 59 obs
# look at what happened on the days where you had double visits (monthly and sick)
double_visit_data = human_merged_all_data_final[which(!(is.na(human_merged_all_data_final$sick_unq_memID)) & !(is.na(human_merged_all_data_final$monthly_unq_memID))),]
table(double_visit_data$pf_pcr_infection_status,double_visit_data$rdt_rst, useNA = "always")
# looks like main exposure coded correctly
# look at the main exposure stratified by sex
table(human_merged_all_data_final$main_exposure,human_merged_all_data_final$gender_hum_monthly_data, useNA = "always")

# add a column for case definition 1
outcome_case_definition_1 = rep(NA,nrow(human_merged_all_data_final))
human_merged_all_data_final$outcome_case_definition_1 = outcome_case_definition_1
human_merged_all_data_final$outcome_case_definition_1[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & human_merged_all_data_final$rdt_rst == "positive" & human_merged_all_data_final$visit_type == "sick visit")] = "symptomatic infection"
# check the case definition coding
table(human_merged_all_data_final$pf_pcr_infection_status,human_merged_all_data_final$rdt_rst, useNA = "always")
table(human_merged_all_data_final$outcome_case_definition_1, useNA = "always")
# look at the primary outcome stratified by sex
table(human_merged_all_data_final$outcome_case_definition_1,human_merged_all_data_final$gender_hum_sick_data, useNA = "always")

# add a column for case definition 2
outcome_case_definition_2 = rep(NA,nrow(human_merged_all_data_final))
human_merged_all_data_final$outcome_case_definition_2 = outcome_case_definition_2
human_merged_all_data_final$outcome_case_definition_2[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & human_merged_all_data_final$rdt_rst == "positive" & human_merged_all_data_final$visit_type == "sick visit" & human_merged_all_data_final$fever == "yes")] = "symptomatic infection"
# check the case definition coding
length(which(human_merged_all_data_final$fever=="yes" & human_merged_all_data_final$pf_pcr_infection_status=="positive" & human_merged_all_data_final$rdt_rst=="positive"))
table(human_merged_all_data_final$outcome_case_definition_2, useNA = "always")
# look at the primary outcome stratified by sex
table(human_merged_all_data_final$outcome_case_definition_2,human_merged_all_data_final$gender_hum_sick_data, useNA = "always")

# add a column for case definition 3
outcome_case_definition_3 = rep(NA,nrow(human_merged_all_data_final))
human_merged_all_data_final$outcome_case_definition_3 = outcome_case_definition_3
human_merged_all_data_final$outcome_case_definition_3[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & human_merged_all_data_final$visit_type == "sick visit")] = "symptomatic infection"
# check the case definition coding
table(human_merged_all_data_final$pf_pcr_infection_status,human_merged_all_data_final$rdt_rst, useNA = "always")
table(human_merged_all_data_final$outcome_case_definition_3, useNA = "always")
# look at the primary outcome stratified by sex
table(human_merged_all_data_final$outcome_case_definition_3,human_merged_all_data_final$gender_hum_sick_data, useNA = "always")

# calculate how many repeated infections occurred
participant_data = human_merged_all_data_final %>%
  filter(outcome_case_definition_1 == "symptomatic infection") %>%
  group_by(unq_memID) %>%
  summarize(n=n())
length(which(participant_data$n>1)) # 33 repeated infections
summary(participant_data$n)
nrow(participant_data)
# calculate repeated infections for case definition 2
participant_data = human_merged_all_data_final %>%
  filter(outcome_case_definition_2 == "symptomatic infection") %>%
  group_by(unq_memID) %>%
  summarize(n=n())
length(which(participant_data$n>1)) # 21 repeated infections
summary(participant_data$n)
nrow(participant_data)
# calculate repeated infections for case definition 3
participant_data = human_merged_all_data_final %>%
  filter(outcome_case_definition_3 == "symptomatic infection") %>%
  group_by(unq_memID) %>%
  summarize(n=n())
length(which(participant_data$n>1)) # 68 repeated infections
summary(participant_data$n)
nrow(participant_data)

# how many people never developed a malaria infection during the study (asymptomatic or symptomatic)
# look at follow-up for everyone
participant_data_all = human_merged_all_data_final %>%
  group_by(unq_memID) %>%
  summarize(n=n())
# now look at follow-up for those that didn't have an infection 
participant_data_no_infection_case_def_1 = human_merged_all_data_final %>%
  filter(main_exposure=="no infection" & is.na(outcome_case_definition_1)) %>%
  group_by(unq_memID) %>%
  summarize(n=n())
# check where the two counts are the same between the two participant data set
length(which(participant_data_all$unq_memID == participant_data_no_infection_case_def_1$unq_memID & participant_data_all$n == participant_data_no_infection_case_def_1$n))
# there were not cases of this


# write out the data set
write_csv(human_merged_all_data_final,"human_merged_all_data_final_6MAR2019.csv")
write_rds(human_merged_all_data_final,"human_merged_all_data_final_6MAR2019.rds")


### old ways to code case definition
# # add a column for case definition 1
# case_definition_1 = rep(NA,nrow(human_merged_all_data_final))
# human_merged_all_data_final$case_definition_1 = case_definition_1
# human_merged_all_data_final$case_definition_1[which(human_merged_all_data_final$pf_pcr_infection_status == "negative")] = "no infection"
# human_merged_all_data_final$case_definition_1[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & human_merged_all_data_final$rdt_rst == "positive")] = "symptomatic infection"
# human_merged_all_data_final$case_definition_1[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & human_merged_all_data_final$rdt_rst == "negative")] = "asymptomatic infection"
# human_merged_all_data_final$case_definition_1[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & is.na(human_merged_all_data_final$rdt_rst))] = "asymptomatic infection"
# # check the case definition coding
# table(human_merged_all_data_final$pf_pcr_infection_status,human_merged_all_data_final$rdt_rst, useNA = "always")
# table(human_merged_all_data_final$pf_pcr_infection_status, useNA = "always")
# table(human_merged_all_data_final$case_definition_1,useNA = "always")
# 
# # add a column for case definition 2
# case_definition_2_stringent = rep(NA,nrow(human_merged_all_data_final))
# human_merged_all_data_final$case_definition_2_stringent = case_definition_2_stringent
# human_merged_all_data_final$case_definition_2_stringent[which(human_merged_all_data_final$pf_pcr_infection_status == "negative")] = "no infection"
# human_merged_all_data_final$case_definition_2_stringent[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & human_merged_all_data_final$rdt_rst == "positive" & human_merged_all_data_final$fever == "yes")] = "symptomatic infection"
# human_merged_all_data_final$case_definition_2_stringent[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & human_merged_all_data_final$rdt_rst == "negative")] = "asymptomatic infection"
# human_merged_all_data_final$case_definition_2_stringent[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & is.na(human_merged_all_data_final$rdt_rst))] = "asymptomatic infection"
# human_merged_all_data_final$case_definition_2_stringent[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & human_merged_all_data_final$rdt_rst == "positive" & human_merged_all_data_final$fever == "no")] = "asymptomatic infection"
# # check the case definition coding
# table(human_merged_all_data_final$case_definition_2_stringent,useNA = "always")
# table(human_merged_all_data_final$fever)
# length(which(human_merged_all_data_final$fever=="yes" & human_merged_all_data_final$pf_pcr_infection_status=="positive" & human_merged_all_data_final$rdt_rst=="positive"))
# 
# # add a column for case definition 2
# case_definition_2_permissive = rep(NA,nrow(human_merged_all_data_final))
# human_merged_all_data_final$case_definition_2_permissive = case_definition_2_permissive
# human_merged_all_data_final$case_definition_2_permissive[which(human_merged_all_data_final$pf_pcr_infection_status == "negative")] = "no infection"
# human_merged_all_data_final$case_definition_2_permissive[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & human_merged_all_data_final$rdt_rst == "positive")] = "symptomatic infection"
# human_merged_all_data_final$case_definition_2_permissive[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & human_merged_all_data_final$rdt_rst == "negative")] = "symptomatic infection"
# human_merged_all_data_final$case_definition_2_permissive[which(human_merged_all_data_final$pf_pcr_infection_status == "positive" & is.na(human_merged_all_data_final$rdt_rst))] = "asymptomatic infection"
# # check the case definition coding
# table(human_merged_all_data_final$case_definition_2_permissive,useNA = "always")
# 


