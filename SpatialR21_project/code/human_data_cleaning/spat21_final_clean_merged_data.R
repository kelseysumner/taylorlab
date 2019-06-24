# --------------------------- #
#         Spat21 Study        #
#    Make changes to final    #
#       merged data set       #
#         Human Data          #
#         K. Sumner           #
#       June 24, 2019         #
# --------------------------- #

#### ------- load packages ------- ####
library(tidyverse)


#### ------- read in the data sets ------ ####

# read in the spat21 human merged data set with dbs censoring
final_merged_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_merged_data_with_dbs_censoring_11JUN2019.rds")
length(unique(final_merged_data$unq_memID))
# note: 1 participant removed after dbs censoring


# read in the spat21 human merged data set without dbs censoring
final_merged_data_no_censoring = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_merged_data_no_dbs_censoring_11JUN2019.rds")
length(unique(final_merged_data_no_censoring$unq_memID))




#### ------ now make the suggested changes to the final merged data sets ------- ####

# relabel age_cat as age_cat_baseline
final_merged_data = rename(final_merged_data,"age_cat_baseline"="age_cat")
final_merged_data_no_censoring = rename(final_merged_data_no_censoring,"age_cat_baseline"="age_cat")

# clean remaining age variables
# final_merged_data
# age_y
table(final_merged_data$age_y, useNA = "always")
str(final_merged_data$age_y)
length(which(is.na(final_merged_data$age_y)))
# do a different filter
new_df = final_merged_data %>%
  filter(!(is.na(final_merged_data$age_y))) %>%
  select(unq_memID,age_y) %>%
  distinct
length(intersect(new_df$unq_memID,unique(final_merged_data$unq_memID))) # 236, correct
# join in the new age categories
human_merged_data_test = left_join(final_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
final_merged_data = human_merged_data_test
# join the age cat new columns
table(final_merged_data$age_y.x, useNA = "always")
table(final_merged_data$age_y.y, useNA = "always")
length(unique(final_merged_data$unq_memID))
final_merged_data$age_y.x <- NULL
final_merged_data = rename(final_merged_data,"age_y"="age_y.y")
# tabulate how many participants in new categories
participant_data = final_merged_data %>%
  group_by(unq_memID,age_y) %>%
  summarize(n=n()) %>%
  group_by(age_y) %>%
  summarize(totaln = n())
# looks good, clean
# age_m
table(final_merged_data$age_m, useNA = "always")
str(final_merged_data$age_m)
length(which(is.na(final_merged_data$age_m)))
# do a different filter
new_df = final_merged_data %>%
  filter(!(is.na(final_merged_data$age_m))) %>%
  select(unq_memID,age_m) %>%
  distinct
length(intersect(new_df$unq_memID,unique(final_merged_data$unq_memID))) # 6, correct
# join in the new age categories
human_merged_data_test = left_join(final_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
final_merged_data = human_merged_data_test
# join the age cat new columns
table(final_merged_data$age_m.x, useNA = "always")
table(final_merged_data$age_m.y, useNA = "always")
length(unique(final_merged_data$unq_memID))
final_merged_data$age_m.x <- NULL
final_merged_data = rename(final_merged_data,"age_m"="age_m.y")
# tabulate how many participants in new categories
participant_data = final_merged_data %>%
  group_by(unq_memID,age_m) %>%
  summarize(n=n()) %>%
  group_by(age_m) %>%
  summarize(totaln = n())
# looks good, clean
# age_type
table(final_merged_data$age_type, useNA = "always")
str(final_merged_data$age_type)
length(which(is.na(final_merged_data$age_type)))
# do a different filter
new_df = final_merged_data %>%
  filter(!(is.na(final_merged_data$age_type))) %>%
  select(unq_memID,age_type) %>%
  distinct
length(intersect(new_df$unq_memID,unique(final_merged_data$unq_memID))) # 6, correct
# join in the new age categories
human_merged_data_test = left_join(final_merged_data,new_df,by="unq_memID")
colnames(human_merged_data_test)
final_merged_data = human_merged_data_test
# join the age cat new columns
table(final_merged_data$age_type.x, useNA = "always")
table(final_merged_data$age_type.y, useNA = "always")
length(unique(final_merged_data$unq_memID))
final_merged_data$age_type.x <- NULL
final_merged_data = rename(final_merged_data,"age_type"="age_type.y")
# tabulate how many participants in new categories
participant_data = final_merged_data %>%
  group_by(unq_memID,age_type) %>%
  summarize(n=n()) %>%
  group_by(age_type) %>%
  summarize(totaln = n())
# looks good, clean

# final_merged_data_no_censoring
# age_y
table(final_merged_data_no_censoring$age_y, useNA = "always")
str(final_merged_data_no_censoring$age_y)
length(which(is.na(final_merged_data_no_censoring$age_y)))
# do a different filter
new_df = final_merged_data_no_censoring %>%
  filter(!(is.na(final_merged_data_no_censoring$age_y))) %>%
  select(unq_memID,age_y) %>%
  distinct
length(intersect(new_df$unq_memID,unique(final_merged_data_no_censoring$unq_memID))) # 237, correct
# join in the new age categories
human_merged_data_test = left_join(final_merged_data_no_censoring,new_df,by="unq_memID")
colnames(human_merged_data_test)
final_merged_data_no_censoring = human_merged_data_test
# join the age cat new columns
table(final_merged_data_no_censoring$age_y.x, useNA = "always")
table(final_merged_data_no_censoring$age_y.y, useNA = "always")
length(unique(final_merged_data_no_censoring$unq_memID))
final_merged_data_no_censoring$age_y.x <- NULL
final_merged_data_no_censoring = rename(final_merged_data_no_censoring,"age_y"="age_y.y")
# tabulate how many participants in new categories
participant_data = final_merged_data_no_censoring %>%
  group_by(unq_memID,age_y) %>%
  summarize(n=n()) %>%
  group_by(age_y) %>%
  summarize(totaln = n())
# looks good, clean
# age_m
table(final_merged_data_no_censoring$age_m, useNA = "always")
str(final_merged_data_no_censoring$age_m)
length(which(is.na(final_merged_data_no_censoring$age_m)))
# do a different filter
new_df = final_merged_data_no_censoring %>%
  filter(!(is.na(final_merged_data_no_censoring$age_m))) %>%
  select(unq_memID,age_m) %>%
  distinct
length(intersect(new_df$unq_memID,unique(final_merged_data_no_censoring$unq_memID))) # 6, correct
# join in the new age categories
human_merged_data_test = left_join(final_merged_data_no_censoring,new_df,by="unq_memID")
colnames(human_merged_data_test)
final_merged_data_no_censoring = human_merged_data_test
# join the age cat new columns
table(final_merged_data_no_censoring$age_m.x, useNA = "always")
table(final_merged_data_no_censoring$age_m.y, useNA = "always")
length(unique(final_merged_data_no_censoring$unq_memID))
final_merged_data_no_censoring$age_m.x <- NULL
final_merged_data_no_censoring = rename(final_merged_data_no_censoring,"age_m"="age_m.y")
# tabulate how many participants in new categories
participant_data = final_merged_data_no_censoring %>%
  group_by(unq_memID,age_m) %>%
  summarize(n=n()) %>%
  group_by(age_m) %>%
  summarize(totaln = n())
# looks good, clean
# age_type
table(final_merged_data_no_censoring$age_type, useNA = "always")
str(final_merged_data_no_censoring$age_type)
length(which(is.na(final_merged_data_no_censoring$age_type)))
# do a different filter
new_df = final_merged_data_no_censoring %>%
  filter(!(is.na(final_merged_data_no_censoring$age_type))) %>%
  select(unq_memID,age_type) %>%
  distinct
length(intersect(new_df$unq_memID,unique(final_merged_data_no_censoring$unq_memID))) # 243, correct
# join in the new age categories
human_merged_data_test = left_join(final_merged_data_no_censoring,new_df,by="unq_memID")
colnames(human_merged_data_test)
final_merged_data_no_censoring = human_merged_data_test
# join the age cat new columns
table(final_merged_data_no_censoring$age_type.x, useNA = "always")
table(final_merged_data_no_censoring$age_type.y, useNA = "always")
length(unique(final_merged_data_no_censoring$unq_memID))
final_merged_data_no_censoring$age_type.x <- NULL
final_merged_data_no_censoring = rename(final_merged_data_no_censoring,"age_type"="age_type.y")
# tabulate how many participants in new categories
participant_data = final_merged_data_no_censoring %>%
  group_by(unq_memID,age_type) %>%
  summarize(n=n()) %>%
  group_by(age_type) %>%
  summarize(totaln = n())
# looks good, clean

# now create one age variable that combines age_m and and age_y
# final_merged_data
final_merged_data$age_m_chr = ifelse(is.na(final_merged_data$age_m),NA,paste0(final_merged_data$age_m,"mos"))
table(final_merged_data$age_m_chr,final_merged_data$age_m,useNA = "always")
final_merged_data$age_all_baseline = ifelse(is.na(final_merged_data$age_m_chr),final_merged_data$age_y,final_merged_data$age_m_chr)
table(final_merged_data$age_all_baseline, final_merged_data$age_m_chr,useNA = "always")
table(final_merged_data$age_all_baseline, final_merged_data$age_y,useNA = "always")
table(final_merged_data$age_all_baseline, useNA = "always")
table(final_merged_data$age_y, useNA = "always")
# final_merged_data_no_censoring
final_merged_data_no_censoring$age_m_chr = ifelse(is.na(final_merged_data_no_censoring$age_m),NA,paste0(final_merged_data_no_censoring$age_m,"mos"))
table(final_merged_data_no_censoring$age_m_chr,final_merged_data_no_censoring$age_m,useNA = "always")
final_merged_data_no_censoring$age_all_baseline = ifelse(is.na(final_merged_data_no_censoring$age_m_chr),final_merged_data_no_censoring$age_y,final_merged_data_no_censoring$age_m_chr)
table(final_merged_data_no_censoring$age_all_baseline, final_merged_data_no_censoring$age_m_chr,useNA = "always")
table(final_merged_data_no_censoring$age_all_baseline, final_merged_data_no_censoring$age_y,useNA = "always")
table(final_merged_data_no_censoring$age_all_baseline, useNA = "always")
table(final_merged_data_no_censoring$age_y, useNA = "always")

# make visit_type three levels














