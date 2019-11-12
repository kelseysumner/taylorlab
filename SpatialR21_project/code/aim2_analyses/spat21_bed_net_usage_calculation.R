# ----------------------------------------- #
#  Aim2 bed net usage across symp status    #
#             Mozzie Phase 1                #
#                CSP data                   #
#            November 12, 2019              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(tidyverse)
library(devtools)
library(streamgraph)
library(lubridate)
library(ggalluvial)
library(gridExtra)


#### ---------- read in the data sets ---------- ####

# read in the merged csp abdomen edgelist ready for the multilevel models
csp_abdomens = read_rds("Desktop/clean_ids_haplotype_results/CSP/model data set/spat21_csp_abdomen_sharing_final_model_data_31OCT2019.rds")

# read in the merged anopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the csp haplotype data
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
csp_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")


#### ---- calculate regular bed net usage across symptomatic status ------ ####


# calculate how many people sleep under net regularly
participant_data = final_data %>%
  group_by(village_name,unq_memID) %>%
  summarize(slept_avg=mean(slept_times, na.rm =T))
# make a variable that indicates some slept under a net more than usual
slept_under_net_regularly = ifelse(is.na(participant_data$slept_avg),NA,ifelse(participant_data$slept_avg>5,"yes","no"))
table(slept_under_net_regularly,participant_data$slept_avg, useNA = "always")
participant_data$slept_under_net_regularly = as.factor(slept_under_net_regularly)
participant_data_v2 = participant_data %>%
  group_by(village_name, slept_under_net_regularly) %>%
  summarize(totaln = n())
participant_data_v2$slept_under_net_regularly=as.factor(participant_data_v2$slept_under_net_regularly)
# add the variables to the final data set with all observations
participant_data$village_name <- NULL
new_data = left_join(final_data,participant_data,by="unq_memID")
length(intersect(final_data$unq_memID,participant_data$unq_memID))
setdiff(participant_data$unq_memID,final_data$unq_memID)
final_data = new_data

# get variables of interest from final_data
final_data = final_data %>%
  filter(main_outcome_primary_case_def=="symptomatic infection" | main_exposure_primary_case_def=="asymptomatic infection") %>%
  select(c(unq_memID,sample_id_date,sample_name_dbs,sample_name_final,HH_ID,village_name,main_outcome_primary_case_def,slept_under_net_regularly)) %>%
  mutate(infection_status = ifelse(!(is.na(main_outcome_primary_case_def)),"symptomatic infection","asymptomatic infection")) %>%
  rename(sample_id_human = sample_name_dbs)
table(final_data$infection_status,final_data$slept_under_net_regularly, useNA = "always")

# merge the data sets
test = left_join(csp_abdomens,final_data,by="sample_id_human")

# look at bed net usage across symptomatic status
table(test$infection_status,test$slept_under_net_regularly, useNA = "always")
