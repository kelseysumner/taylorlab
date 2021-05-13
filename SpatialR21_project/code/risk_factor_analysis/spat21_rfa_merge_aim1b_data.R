# -------------------------------------- #
#           Spat21/Mozzie Study          #
#          Risk factor analysis          #
#         Merging in aim 1B data         #
#            Mozzie Phase 1              #
#               K. Sumner                #
#              May 11, 2021              #
# -------------------------------------- #

# good resource for trouble shooting convergence problems
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)
library(lme4)
library(ggplot2)
library(sjstats)
library(lmerTest)
library(glmmTMB)
library(tibble)
library(stringr)


#### ------ load in data sets ---------- ####

# read in the clean rfa data set
model_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Risk factor analysis/data/clean model data/mozzie_rfa_data_1APR2021.rds")

# now read in the aim 1B data set before initial infections are removed
# read in the ama data set with the first infection
aim1b_ama_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/with first infection/ama_data_aim1b_11JUN2020.rds")
# read in the csp data set with the first infection
aim1b_csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/with first infection/csp_data_aim1b_11JUN2020.rds")

# read in the human demographic data
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")



#### ------- now merge the ama and csp aim1b data to the model data set --------- ####

# remove columns you don't need 
# ama
aim1b_ama_data$unq_memID <- NULL
aim1b_ama_data$village_name <- NULL
aim1b_ama_data$age_all_baseline <- NULL
aim1b_ama_data$age_cat_baseline <- NULL
aim1b_ama_data$sample_id_date <- NULL
# csp
aim1b_csp_data$unq_memID <- NULL
aim1b_csp_data$village_name <- NULL
aim1b_csp_data$age_all_baseline <- NULL
aim1b_csp_data$age_cat_baseline <- NULL
aim1b_csp_data$sample_id_date <- NULL

# first rename sample_name_dbs to sample_id_human
aim1b_ama_data = rename(aim1b_ama_data,"sample_id_human"="sample_name_dbs")
aim1b_csp_data = rename(aim1b_csp_data,"sample_id_human"="sample_name_dbs")

# now merge the data
ama_model_data = left_join(model_data,aim1b_ama_data,by=c("sample_id_human"))
csp_model_data = left_join(model_data,aim1b_csp_data,by=c("sample_id_human"))

# check the merge
# ama
length(unique(model_data$sample_id_human))
length(unique(aim1b_ama_data$sample_id_human))
length(intersect(model_data$sample_id_human,aim1b_ama_data$sample_id_human))
ama_model_data %>%
  filter(is.na(haplotype_category)) %>%
  View()
unmerged_data_ama = ama_model_data %>% filter(is.na(haplotype_category))
# csp
length(unique(model_data$sample_id_human))
length(unique(aim1b_csp_data$sample_id_human))
length(intersect(model_data$sample_id_human,aim1b_csp_data$sample_id_human))
csp_model_data %>%
  filter(is.na(haplotype_category)) %>%
  View()
unmerged_data_csp = csp_model_data %>% filter(is.na(haplotype_category))

# now cut down the data sets to just those with haplotypes for the specific target (ama or csp)
ama_model_data = ama_model_data %>% filter(!(is.na(ama_haps_shared)))
csp_model_data = csp_model_data %>% filter(!(is.na(csp_haps_shared)))

# now check the merge again
unmerged_data_ama = ama_model_data %>% filter(is.na(haplotype_category))
unmerged_data_csp = csp_model_data %>% filter(is.na(haplotype_category))
# looks good


#### ------ add in information for the days since first infection ------- ####


####
# LOOK AT CSP DATA 
####

# first order the data set by date
csp_model_data = dplyr::arrange(csp_model_data,unq_memID,human_date)

# add a variable for month
csp_model_data$month = paste0(as.character(lubridate::month(csp_model_data$human_date)),"-",as.character(lubridate::year(csp_model_data$human_date)))
csp_model_data$month = as.factor(csp_model_data$month)
summary(csp_model_data$month)

# first pull out when each participant entered the study
unq_memID_start_date = csp_model_data[match(unique(csp_model_data$unq_memID), csp_model_data$unq_memID),]

# now calculate the time since the participant first entered the study
days_in_study = rep(NA,nrow(csp_model_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(csp_model_data)){
    if (unq_memID_start_date$unq_memID[i] == csp_model_data$unq_memID[j]){
      days_in_study[j] = csp_model_data$human_date[j]-unq_memID_start_date$human_date[i]
    }
  }
}
summary(days_in_study)  
csp_model_data$days_in_study = days_in_study


## ------ add in information for the number of prior infections

# first order the data set by date
csp_model_data = dplyr::arrange(csp_model_data,unq_memID,human_date)

# first pull out each participant's first infection
unq_memID_first_infection = csp_model_data[match(unique(csp_model_data$unq_memID), csp_model_data$unq_memID),]

# now calculate the time since the participant first entered the study
number_prior_infections = rep(NA,nrow(csp_model_data))
for (i in 1:nrow(unq_memID_first_infection)){
  count = 0
  for (j in 1:nrow(csp_model_data)){
    if (unq_memID_first_infection$unq_memID[i] == csp_model_data$unq_memID[j]){
      count = count + 1
      number_prior_infections[j] = count - 1
    }
  }
}
summary(number_prior_infections)  
csp_model_data$number_prior_infections = number_prior_infections



####
# LOOK AT AMA DATA 
####

# first order the data set by date
ama_model_data = dplyr::arrange(ama_model_data,unq_memID,human_date)

# add a variable for month
ama_model_data$month = paste0(as.character(lubridate::month(ama_model_data$human_date)),"-",as.character(lubridate::year(ama_model_data$human_date)))
ama_model_data$month = as.factor(ama_model_data$month)
summary(ama_model_data$month)

# first pull out when each participant entered the study
unq_memID_start_date = ama_model_data[match(unique(ama_model_data$unq_memID), ama_model_data$unq_memID),]

# now calculate the time since the participant first entered the study
days_in_study = rep(NA,nrow(ama_model_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(ama_model_data)){
    if (unq_memID_start_date$unq_memID[i] == ama_model_data$unq_memID[j]){
      days_in_study[j] = ama_model_data$human_date[j]-unq_memID_start_date$human_date[i]
    }
  }
}
summary(days_in_study)  
ama_model_data$days_in_study = days_in_study


## ------ add in information for the number of prior infections

# first order the data set by date
ama_model_data = dplyr::arrange(ama_model_data,unq_memID,human_date)

# first pull out each participant's first infection
unq_memID_first_infection = ama_model_data[match(unique(ama_model_data$unq_memID), ama_model_data$unq_memID),]

# now calculate the time since the participant first entered the study
number_prior_infections = rep(NA,nrow(ama_model_data))
for (i in 1:nrow(unq_memID_first_infection)){
  count = 0
  for (j in 1:nrow(ama_model_data)){
    if (unq_memID_first_infection$unq_memID[i] == ama_model_data$unq_memID[j]){
      count = count + 1
      number_prior_infections[j] = count - 1
    }
  }
}
summary(number_prior_infections)  
ama_model_data$number_prior_infections = number_prior_infections


#### ------- now remove infections that are the first time we saw someone in the study ---------- ####

# now use the full demographic data to determine when we first looked at a person's DBS
# first order the data set by date
final_data = dplyr::arrange(final_data,unq_memID,sample_id_date)
# add a variable for month
final_data$month = paste0(as.character(lubridate::month(final_data$sample_id_date)),"-",as.character(lubridate::year(final_data$sample_id_date)))
final_data$month = as.factor(final_data$month)
summary(final_data$month)
# first pull out when each participant entered the study
unq_memID_start_date = final_data[match(unique(final_data$unq_memID), final_data$unq_memID),]

# now check if any of the csp data infections are in the unq_memID start date and remove
to_remove = intersect(csp_model_data$sample_id_human,unq_memID_start_date$sample_name_dbs)
length(to_remove)
csp_model_data = csp_model_data[-which(csp_model_data$sample_id_human %in% to_remove),]

# now check if any of the ama data infections are in the unq_memID start date and remove
to_remove = intersect(ama_model_data$sample_id_human,unq_memID_start_date$sample_name_dbs)
length(to_remove)
ama_model_data = ama_model_data[-which(ama_model_data$sample_id_human %in% to_remove),]

# check how many unique people we have
length(unique(csp_model_data$unq_memID)) # 180
length(unique(ama_model_data$unq_memID)) # 159



#### -------- now remove possibly recrudescent infections -------- ####

# take out possible recrudescent infections
# 3 infections for csp
csp_model_data = csp_model_data[-which(csp_model_data$unq_memID == "K01_5" & csp_model_data$human_date == "2017-07-06"),]
csp_model_data = csp_model_data[-which(csp_model_data$unq_memID == "K01_7" & csp_model_data$human_date == "2017-08-03"),]
csp_model_data = csp_model_data[-which(csp_model_data$unq_memID == "M14_2" & csp_model_data$human_date == "2017-08-17"),]
# 1 infection for ama
ama_model_data = ama_model_data[-which(ama_model_data$unq_memID == "K01_7" & ama_model_data$human_date == "2017-08-03"),]



#### ------ figure out time between persistent infections ------- ####

# calculate number of days between persistent infections
ama_model_data = arrange(ama_model_data,unq_memID,human_date)
csp_model_data = arrange(csp_model_data,unq_memID,human_date)

# calculate the time between each infection for each person
# for ama
unq_memID_start_date = ama_model_data[match(unique(ama_model_data$unq_memID), ama_model_data$unq_memID),]
days_btwn_infxns = rep(NA,nrow(ama_model_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(ama_model_data)){
    if (unq_memID_start_date$unq_memID[i] == ama_model_data$unq_memID[j]){
      if (ama_model_data$human_date[j] == unq_memID_start_date$human_date[i]){
        days_btwn_infxns[j] = 0
      } else {
        days_btwn_infxns[j] = ama_model_data$human_date[j] - ama_model_data$human_date[j-1]
      }
    }
  }
}
summary(days_btwn_infxns)  
ama_model_data$days_btwn_infxns = days_btwn_infxns
# for csp
unq_memID_start_date = csp_model_data[match(unique(csp_model_data$unq_memID), csp_model_data$unq_memID),]
days_btwn_infxns = rep(NA,nrow(csp_model_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(csp_model_data)){
    if (unq_memID_start_date$unq_memID[i] == csp_model_data$unq_memID[j]){
      if (csp_model_data$human_date[j] == unq_memID_start_date$human_date[i]){
        days_btwn_infxns[j] = 0
      } else {
        days_btwn_infxns[j] = csp_model_data$human_date[j] - csp_model_data$human_date[j-1]
      }
    }
  }
}
summary(days_btwn_infxns)  
csp_model_data$days_btwn_infxns = days_btwn_infxns

# first subset to infections within 30 days
csp_30days = csp_model_data %>% filter(days_btwn_infxns <= 30 & days_btwn_infxns > 0 & str_detect(haplotype_category,"persistent"))
ama_30days = ama_model_data %>% filter(days_btwn_infxns <= 30 & days_btwn_infxns > 0 & str_detect(haplotype_category,"persistent"))
summary(csp_30days$days_btwn_infxns)
summary(ama_30days$days_btwn_infxns)
table(csp_30days$haplotype_category,useNA="always")
table(ama_30days$haplotype_category,useNA="always")



#### ------- determine chronic/non-chronic categories ---------- ####

# define a chronic infection as one with persistent haplotypes occurring within 30 days

# now set the chronic infection categories
csp_model_data$chronic_infection_cat = ifelse(csp_model_data$sample_id_human %in% csp_30days$sample_id_human,"chronic infection","non-chronic infection")
ama_model_data$chronic_infection_cat = ifelse(ama_model_data$sample_id_human %in% ama_30days$sample_id_human,"chronic infection","non-chronic infection")
table(csp_model_data$chronic_infection_cat,useNA="always")
table(ama_model_data$chronic_infection_cat,useNA="always")

# make a factor
csp_model_data$chronic_infection_cat = factor(csp_model_data$chronic_infection_cat,levels=c("non-chronic infection","chronic infection"))
ama_model_data$chronic_infection_cat = factor(ama_model_data$chronic_infection_cat,levels=c("non-chronic infection","chronic infection"))


#### ------- create a new median probability of transmission variable -------- ####

# we are doing this because the median likely changed since the sample size changed with the new exclusion criteria

# create a variable for transmission that just uses the mean P_te_all value
# for csp
median_csp = median(csp_model_data$p_te_all_csp)
csp_model_data$csp_transmission_median = ifelse(csp_model_data$p_te_all_csp >= median_csp,"yes",ifelse(csp_model_data$p_te_all_csp < median_csp,"no",NA))
csp_model_data$csp_transmission_median = factor(csp_model_data$csp_transmission_median,levels=c("no","yes"))
# for ama
median_ama = mean(ama_model_data$p_te_all_ama)
ama_model_data$ama_transmission_median = ifelse(ama_model_data$p_te_all_ama >= median_ama,"yes",ifelse(ama_model_data$p_te_all_ama < median_ama,"no",NA))
ama_model_data$ama_transmission_median = factor(ama_model_data$ama_transmission_median,levels=c("no","yes"))


#### ------- write out the new data sets --------- ####

# export the data sets
write_csv(csp_model_data,"Desktop/mozzie_rfa_data_with_chronic_categories_CSP_11MAY2021.csv")
write_rds(csp_model_data,"Desktop/mozzie_rfa_data_with_chronic_categories_CSP_11MAY2021.rds")
write_csv(ama_model_data,"Desktop/mozzie_rfa_data_with_chronic_categories_AMA_11MAY2021.csv")
write_rds(ama_model_data,"Desktop/mozzie_rfa_data_with_chronic_categories_AMA_11MAY2021.rds")








