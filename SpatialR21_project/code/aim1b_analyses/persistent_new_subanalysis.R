# ---------------------------- #
#   Look at the new compared   #
#   to persistent infections   #
#        Mozzie study          #
#           Aim 1B             #
#      February 25, 2021       #
#          K. Sumner           #
# ---------------------------- #

#### ------- load libraries -------- ####
library(tidyverse)
library(car)
library(ggbeeswarm)
library(lme4)
library(glmmTMB)



#### ------ read in the data sets ------- ####

# read in the ama data set with the first infection
ama_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/with first infection/ama_data_aim1b_11JUN2020.rds")

# read in the csp data set with the first infection
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/with first infection/csp_data_aim1b_11JUN2020.rds")

# make a new age category variable
# for csp
table(csp_data$age_cat_baseline)
csp_data$age_cat_baseline = ifelse(csp_data$age_cat_baseline== ">15 years",">15 years","15 years or less")
table(csp_data$age_cat_baseline)
csp_data$age_cat_baseline = as.factor(csp_data$age_cat_baseline)
csp_data$age_cat_baseline = relevel(csp_data$age_cat_baseline,ref="15 years or less")
# for ama
table(ama_data$age_cat_baseline)
ama_data$age_cat_baseline = ifelse(ama_data$age_cat_baseline== ">15 years",">15 years","15 years or less")
table(ama_data$age_cat_baseline)
ama_data$age_cat_baseline = as.factor(ama_data$age_cat_baseline)
ama_data$age_cat_baseline = relevel(ama_data$age_cat_baseline,ref="15 years or less")


#### ------ figure out time between persistent infections ------- ####

# calculate number of days between persistent infections
ama_data = arrange(ama_data,unq_memID,sample_id_date)
csp_data = arrange(csp_data,unq_memID,sample_id_date)


# calculate the time between each infection for each person
# for ama
unq_memID_start_date = ama_data[match(unique(ama_data$unq_memID), ama_data$unq_memID),]
days_btwn_infxns = rep(NA,nrow(ama_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(ama_data)){
    if (unq_memID_start_date$unq_memID[i] == ama_data$unq_memID[j]){
      if (ama_data$sample_id_date[j] == unq_memID_start_date$sample_id_date[i]){
        days_btwn_infxns[j] = 0
      } else {
        days_btwn_infxns[j] = ama_data$sample_id_date[j] - ama_data$sample_id_date[j-1]
      }
    }
  }
}
summary(days_btwn_infxns)  
ama_data$days_btwn_infxns = days_btwn_infxns
# for csp
unq_memID_start_date = csp_data[match(unique(csp_data$unq_memID), csp_data$unq_memID),]
days_btwn_infxns = rep(NA,nrow(csp_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(csp_data)){
    if (unq_memID_start_date$unq_memID[i] == csp_data$unq_memID[j]){
      if (csp_data$sample_id_date[j] == unq_memID_start_date$sample_id_date[i]){
        days_btwn_infxns[j] = 0
      } else {
        days_btwn_infxns[j] = csp_data$sample_id_date[j] - csp_data$sample_id_date[j-1]
      }
    }
  }
}
summary(days_btwn_infxns)  
csp_data$days_btwn_infxns = days_btwn_infxns


#### ------ look at the days before infections for symptomatic infections ------- ####

# this is a way to look at pre-symptomatic infections

# first order the data set by date
symptomatic_csp_data = dplyr::arrange(csp_data,unq_memID,sample_id_date)
symptomatic_ama_data = dplyr::arrange(ama_data,unq_memID,sample_id_date)

# look at how many infections each participant had
num_infections_before = symptomatic_csp_data %>%
  group_by(unq_memID) %>%
  summarize (n= n())
num_infections_before = symptomatic_ama_data %>%
  group_by(unq_memID) %>%
  summarize (n= n())

# looks like this worked correctly so apply to everything
symptomatic_csp_data = slice(group_by(symptomatic_csp_data, unq_memID), -1)
symptomatic_ama_data = slice(group_by(symptomatic_ama_data, unq_memID), -1)




#### ------ subset infections to those with consecutive infections within 30 days and with only persistent haplotypes --------- ####

# first subset to infections within 30 days
csp_30days = symptomatic_csp_data %>% filter(days_btwn_infxns <= 30)
ama_30days = symptomatic_ama_data %>% filter(days_btwn_infxns <= 30)
summary(csp_30days$days_btwn_infxns)
summary(ama_30days$days_btwn_infxns)

# take out possible recrudescent infections
# 3 infections for csp
csp_30days = csp_30days[-which(csp_30days$unq_memID == "K01_5" & csp_30days$sample_id_date == "2017-07-06"),]
csp_30days = csp_30days[-which(csp_30days$unq_memID == "K01_7" & csp_30days$sample_id_date == "2017-08-03"),]
csp_30days = csp_30days[-which(csp_30days$unq_memID == "M14_2" & csp_30days$sample_id_date == "2017-08-17"),]
# 1 infection for ama
ama_30days = ama_30days[-which(ama_30days$unq_memID == "K01_7" & ama_30days$sample_id_date == "2017-08-03"),]



## for csp

# take out the infections with recurrent haplotypes
all_persistent_data_csp = csp_30days[which((str_detect(csp_30days$haplotype_category,"persistent"))),]
table(all_persistent_data_csp$haplotype_category, useNA = "always")
all_persistent_data_csp$haplotype_category = as.character(all_persistent_data_csp$haplotype_category)
all_persistent_data_csp$haplotype_category = as.factor(all_persistent_data_csp$haplotype_category)
levels(all_persistent_data_csp$haplotype_category)
all_persistent_data_csp$haplotype_category = relevel(all_persistent_data_csp$haplotype_category,ref="all persistent")

# merge in covariates
csp_cov_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_csp_final_model_data_21JUL2020.rds")
csp_cov_data = csp_cov_data %>% select(sample_name_dbs,add_cat_number_prior_infections,mosquito_week_count_cat_add,moi_cat)
all_persistent_data_csp = left_join(all_persistent_data_csp,csp_cov_data,by="sample_name_dbs")
all_persistent_data_csp = rename(all_persistent_data_csp,unq_memID = unq_memID.x)
all_persistent_data_csp$unq_memID.y <- NULL
length(which(is.na(all_persistent_data_csp$moi_cat)))
all_persistent_data_csp$symptomatic_status = as.factor(all_persistent_data_csp$symptomatic_status)
levels(all_persistent_data_csp$symptomatic_status)

# subset the data sets
test_data = all_persistent_data_csp %>%
  filter(haplotype_category == "all persistent" | haplotype_category == "new and persistent")
table(test_data$haplotype_category)
test_data$new_persistent_cats = ifelse(test_data$haplotype_category == "all persistent","all persistent","new and persistent")
table(test_data$ new_persistent_cats)
test_data$new_persistent_cats = as.factor(test_data$new_persistent_cats)
levels(test_data$new_persistent_cats)
# now rerun the model
csp_model_1 <- glmmTMB(symptomatic_status ~ new_persistent_cats + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = test_data)
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))
# OR comparing (new and persistent) to (all persistent): 1.70 (0.29 to 2.05)
table(test_data$new_persistent_cats,test_data$symptomatic_status)
# run a crude model with moi only added
csp_model_1_crude <- glmmTMB(symptomatic_status ~ new_persistent_cats + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
  data = test_data)
summary(csp_model_1_crude)



## for ama

# take out the infections with recurrent haplotypes
all_persistent_data_ama = ama_30days[which((str_detect(ama_30days$haplotype_category,"persistent"))),]
table(all_persistent_data_ama$haplotype_category, useNA = "always")
all_persistent_data_ama$haplotype_category = as.character(all_persistent_data_ama$haplotype_category)
all_persistent_data_ama$haplotype_category = as.factor(all_persistent_data_ama$haplotype_category)
levels(all_persistent_data_ama$haplotype_category)
all_persistent_data_ama$haplotype_category = relevel(all_persistent_data_ama$haplotype_category,ref="all persistent")

# merge in covariates
ama_cov_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_ama_final_model_data_21JUL2020.rds")
ama_cov_data = ama_cov_data %>% select(sample_name_dbs,add_cat_number_prior_infections,mosquito_week_count_cat_add,moi_cat)
all_persistent_data_ama = left_join(all_persistent_data_ama,ama_cov_data,by="sample_name_dbs")
all_persistent_data_ama = rename(all_persistent_data_ama,unq_memID = unq_memID.x)
all_persistent_data_ama$unq_memID.y <- NULL
length(which(is.na(all_persistent_data_ama$moi_cat)))
all_persistent_data_ama$symptomatic_status = as.factor(all_persistent_data_ama$symptomatic_status)
levels(all_persistent_data_ama$symptomatic_status)

# subset the data sets
test_data_ama = all_persistent_data_ama %>%
  filter(haplotype_category == "all persistent" | haplotype_category == "new and persistent")
table(test_data_ama$haplotype_category)
test_data_ama$new_persistent_cats = ifelse(test_data_ama$haplotype_category == "all persistent","all persistent","new and persistent")
table(test_data_ama$new_persistent_cats)
test_data_ama$new_persistent_cats = as.factor(test_data_ama$new_persistent_cats)
levels(test_data_ama$new_persistent_cats)
# now rerun the model
ama_model_1 <- glmmTMB(symptomatic_status ~ new_persistent_cats + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = test_data_ama)
summary(ama_model_1)
performance::icc(ama_model_1)
exp(confint(ama_model_1,method="Wald"))
# OR comparing (new and persistent) to (all persistent): 0.33 (0.06 to 1.88)
table(test_data_ama$new_persistent_cats,test_data_ama$symptomatic_status)
# run a crude model with moi only added
ama_model_1_crude <- glmmTMB(symptomatic_status ~ new_persistent_cats + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                             data = test_data_ama)
summary(ama_model_1_crude)

