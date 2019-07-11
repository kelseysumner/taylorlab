# ------------------------------------- #
#           Spat21/Mozzie Study         #
#    Make table consecutive follow-up   #
#               Human Data              #
#                K. Sumner              #
#               June 25, 2019           #
# ------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)



#### -------- read in the data sets --------- ####

# read in the spat21 human merged data set with dbs censoring
final_merged_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_merged_data_with_dbs_censoring_25JUN2019.rds")
length(unique(final_merged_data$unq_memID))
# note: 1 participant removed after dbs censoring



#### ------- make a table of consecutive follow-up ---------- ####

# table total monthly follow-up per participant
total_follow_up_df = final_merged_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  group_by(unq_memID) %>%
  summarise(total_follow_up = n())
# 242 ppts, that's correct

# check for those that had a total monthly follow-up <1 month
length(which(total_follow_up_df$total_follow_up < 2))
# 2 participants had <1 month follow-up after dbs merging didn't pass
# remove these two participants from the data set
ppts_to_remove = total_follow_up_df$unq_memID[which(total_follow_up_df$total_follow_up < 2)]
final_merged_data = final_merged_data[-which(final_merged_data$unq_memID %in% ppts_to_remove),]

# now look at new table of total monthly follow-up per participant
total_follow_up_df = final_merged_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  group_by(unq_memID) %>%
  summarise(total_follow_up = n())
# now 240 ppts, that's correct

# now tabulate the consecutive follow-up
consecutive_follow_up_df = final_merged_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  select(unq_memID,sample_id_date) %>%
  group_by(unq_memID) %>%
  mutate(id = 1:n()) %>%
  spread(key=id,value=sample_id_date)

# now order and tabulate the consecutive follow-up
consecutive_follow_up_ordered_df = final_merged_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  select(unq_memID,sample_id_date) %>%
  group_by(unq_memID) %>%
  mutate(id = paste0(as.character(lubridate::month(sample_id_date)),"-",as.character(lubridate::year(sample_id_date)))) %>%
  spread(key=id,value=sample_id_date)

# write out the data frame
write_csv(consecutive_follow_up_ordered_df,"spat21_human_consecutive_follow_up_df.csv")
# reorder the columns in Excel to be in chronological order





