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
  select(visit_type == "monthly visit") %>%
  group_by(unq_memID) %>%
  summarise(total_follow_up = n())










