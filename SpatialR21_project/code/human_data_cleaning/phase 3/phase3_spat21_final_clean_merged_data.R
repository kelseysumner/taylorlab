# --------------------------- #
#         Spat21 Study        #
#    Make changes to final    #
#       merged data set       #
#         Human Data          #
#          Phase 3            #
#         K. Sumner           #
#     August 18, 2020         #
# --------------------------- #

#### ------- load packages ------- ####
library(tidyverse)


#### ------- read in the data sets ------ ####

# read in the spat21 human merged data set with dbs censoring
final_merged_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/De-identified Phase II_v13/final_merged_data/phase3_spat21_human_merged_data_with_dbs_censoring_18AUG2020.rds")
length(unique(final_merged_data$unq_memID))
# note: 3 participants removed after dbs censoring


# read in the spat21 human merged data set without dbs censoring
final_merged_data_no_censoring = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/De-identified Phase II_v13/final_merged_data/phase3_spat21_human_merged_data_no_dbs_censoring_18AUG2020.rds")
length(unique(final_merged_data_no_censoring$unq_memID))




#### ------ now make the suggested changes to the final merged data sets ------- ####


# make visit_type three levels
# final_merged_data
final_merged_data$visit_type = ifelse(is.na(final_merged_data$monthly_unq_memID) & !(is.na(final_merged_data$sick_unq_memID)),"sick visit",ifelse(
  !(is.na(final_merged_data$monthly_unq_memID)) & is.na(final_merged_data$sick_unq_memID),"monthly visit","monthly and sick visit"))
table(final_merged_data$visit_type, useNA = "always")
final_merged_data$visit_type = as.factor(final_merged_data$visit_type)
# final_merged_data_no_censoring
final_merged_data_no_censoring$visit_type = ifelse(is.na(final_merged_data_no_censoring$monthly_unq_memID) & !(is.na(final_merged_data_no_censoring$sick_unq_memID)),"sick visit",ifelse(
  !(is.na(final_merged_data_no_censoring$monthly_unq_memID)) & is.na(final_merged_data_no_censoring$sick_unq_memID),"monthly visit","monthly and sick visit"))
table(final_merged_data_no_censoring$visit_type, useNA = "always")
final_merged_data_no_censoring$visit_type = as.factor(final_merged_data_no_censoring$visit_type)





#### ------- read back in these data sets and check that there aren't any dbs duplicates ------ ####

# read in the data set with dbs censoring applied

# check for duplicate sample ids in the social demographic data sample ids
length(unique(final_merged_data$sample_name_final)) # 6016 unique unique 
length(which(is.na(final_merged_data$sample_name_final) == T)) # 0 missing
count_table = table(final_merged_data$sample_name_final, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table

# check for duplicate sample ids in the dbs sample ids
length(unique(final_merged_data$sample_name_dbs)) # 6016 unique unique 
length(which(is.na(final_merged_data$sample_name_dbs) == T)) # 0 missing
count_table = table(final_merged_data$sample_name_dbs, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table



###

# read in the data set with no dbs censoring applied

# check for duplicate sample ids in the social demographic data sample ids
length(unique(final_merged_data_no_censoring$sample_name_final)) # 6298 unique unique 
length(which(is.na(final_merged_data_no_censoring$sample_name_final) == T)) # 0 missing
count_table = table(final_merged_data_no_censoring$sample_name_final, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table

# check for duplicate sample ids in the dbs sample ids
length(unique(final_merged_data_no_censoring$sample_name_dbs)) # 6017 unique unique 
length(which(is.na(final_merged_data_no_censoring$sample_name_dbs) == T)) # 282 missing
count_table = table(final_merged_data_no_censoring$sample_name_dbs, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 1 duplicates (only NA)
length(dups_table)
dups_table



#### ------- read these data sets back in and fix the gender coding -------- ####

# calculate gender for final_merged_data with dbs censoring
# females
participant_data_female = final_merged_data %>%
  filter(gender == "female") %>%
  group_by(unq_memID) %>%
  summarize(n=n())
# males
participant_data_male = final_merged_data %>%
  filter(gender == "male") %>%
  group_by(unq_memID) %>%
  summarize(n=n())
# look at the intercept
intersect(participant_data_female$unq_memID,participant_data_male$unq_memID)
# M13_1, M16_5, S06_3
m13_test = final_merged_data %>%
  filter(unq_memID=="M13_1") %>%
  group_by(gender) %>%
  summarize(n=n()) # really a female, once miscoded as male
m16_test = final_merged_data %>%
  filter(unq_memID=="M16_5") %>%
  group_by(gender) %>%
  summarize(n=n()) # really a male, once miscoded as female
s06_test = final_merged_data %>%
  filter(unq_memID=="S06_3") %>%
  group_by(gender) %>%
  summarize(n=n()) # really a male, once miscoded as female
# recode both female cases to male for M16_5 and S06_3
final_merged_data$gender[which(final_merged_data$gender=="female" & final_merged_data$unq_memID=="M16_5")]="male"
final_merged_data$gender[which(final_merged_data$gender=="female" & final_merged_data$unq_memID=="S06_3")]="male"
# recode both male case to female for M13_1
final_merged_data$gender[which(final_merged_data$gender=="male" & final_merged_data$unq_memID=="M13_1")]="female"
# check the recode
# M13_1, M16_5, S06_3
m13_test = final_merged_data %>%
  filter(unq_memID=="M13_1") %>%
  group_by(gender) %>%
  summarize(n=n()) # all female now
m16_test = final_merged_data %>%
  filter(unq_memID=="M16_5") %>%
  group_by(gender) %>%
  summarize(n=n()) # all male now
s06_test = final_merged_data %>%
  filter(unq_memID=="S06_3") %>%
  group_by(gender) %>%
  summarize(n=n()) # all male now
# looks good


# calculate gender for final_merged_data_nc
# females
participant_data_female = final_merged_data_no_censoring %>%
  filter(gender == "female") %>%
  group_by(unq_memID) %>%
  summarize(n=n())
# males
participant_data_male = final_merged_data_no_censoring %>%
  filter(gender == "male") %>%
  group_by(unq_memID) %>%
  summarize(n=n())
# look at the intercept
intersect(participant_data_female$unq_memID,participant_data_male$unq_memID)
# M13_1, M16_5, S06_3
m13_test = final_merged_data_no_censoring %>%
  filter(unq_memID=="M13_1") %>%
  group_by(gender) %>%
  summarize(n=n()) # really a female, once miscoded as male
m16_test = final_merged_data_no_censoring %>%
  filter(unq_memID=="M16_5") %>%
  group_by(gender) %>%
  summarize(n=n()) # really a male, once miscoded as female
s06_test = final_merged_data_no_censoring %>%
  filter(unq_memID=="S06_3") %>%
  group_by(gender) %>%
  summarize(n=n()) # really a male, once miscoded as female
# recode both female cases to male for M16_5 and S06_3
final_merged_data_no_censoring$gender[which(final_merged_data_no_censoring$gender=="female" & final_merged_data_no_censoring$unq_memID=="M16_5")]="male"
final_merged_data_no_censoring$gender[which(final_merged_data_no_censoring$gender=="female" & final_merged_data_no_censoring$unq_memID=="S06_3")]="male"
# recode both male case to female for M13_1
final_merged_data_no_censoring$gender[which(final_merged_data_no_censoring$gender=="male" & final_merged_data_no_censoring$unq_memID=="M13_1")]="female"
# check the recode
# M13_1, M16_5, S06_3
m13_test = final_merged_data_no_censoring %>%
  filter(unq_memID=="M13_1") %>%
  group_by(gender) %>%
  summarize(n=n()) # all female now
m16_test = final_merged_data_no_censoring %>%
  filter(unq_memID=="M16_5") %>%
  group_by(gender) %>%
  summarize(n=n()) # all male now
s06_test = final_merged_data_no_censoring %>%
  filter(unq_memID=="S06_3") %>%
  group_by(gender) %>%
  summarize(n=n()) # all male now
# looks good



# write out the final data sets

# final_merged_data
write_csv(final_merged_data,"Desktop/phase3_spat21_human_merged_data_with_dbs_censoring_18AUG2020.csv")
write_rds(final_merged_data,"Desktop/phase3_spat21_human_merged_data_with_dbs_censoring_18AUG2020.rds")

# final_merged_data_no_censoring
write_csv(final_merged_data_no_censoring,"Desktop/phase3_spat21_human_merged_data_no_dbs_censoring_18AUG2020.csv")
write_rds(final_merged_data_no_censoring,"Desktop/phase3_spat21_human_merged_data_no_dbs_censoring_18AUG2020.rds")
