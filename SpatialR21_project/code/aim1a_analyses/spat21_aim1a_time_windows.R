# -------------------------------------- #
#           Spat21/Mozzie Study          #
#           Look at time windows         #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#             February 9, 2021           #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)
library(survminer)
library(survival)
library(ggalluvial)
library(gridExtra)
library(coxme)


#### ------- read in the data sets -------- ####

# read in the primary data set
survival_data_primary = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_19NOV2020.rds")


# change the age category coding
# for primary data
table(survival_data_primary$age_cat_baseline)
survival_data_primary$new_age_cat_baseline = ifelse(survival_data_primary$age_cat_baseline== ">15 years",">15 years","15 years or less")
table(survival_data_primary$new_age_cat_baseline,useNA = "always")
survival_data_primary$new_age_cat_baseline = as.factor(survival_data_primary$new_age_cat_baseline)
survival_data_primary$new_age_cat_baseline = relevel(survival_data_primary$new_age_cat_baseline,ref="15 years or less")


#### ------- recode the survival data primary to just look at when people first leave the study --------- ####

##  primary data set

# look when each person first lost to follow-up


## first look at follow-up through August 1, 2017
# first order the data set by date
survival_data_primary = dplyr::arrange(survival_data_primary,unq_memID,sample_id_date)

# first pull out when each participant entered the study
unq_memID_start_date = survival_data_primary[match(unique(survival_data_primary$unq_memID), survival_data_primary$unq_memID),]

# now subset the data set to only those events that have the same original follow-up date
keep = rep(NA,nrow(survival_data_primary))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(survival_data_primary)){
    if (survival_data_primary$unq_memID[j] == unq_memID_start_date$unq_memID[i] & 
        survival_data_primary$fu_end_date[j] == unq_memID_start_date$fu_end_date[i]){
      keep[j] = "yes"
    } 
  }
}
survival_data_primary$keep = keep
survival_data_primary = survival_data_primary %>% filter(keep == "yes")

# now calculate the time to symptomatic malaria but only include people with followup past the first month of the study (August 1 to capture variation in sampling)
aug_1_data = survival_data_primary %>% filter(sample_id_date < "2017-08-01")
length(unique(aug_1_data$unq_memID)) # 177 participants
# recalculate follow-up time
days_until_event_aug = rep(NA,nrow(aug_1_data))
status_aug = rep(NA,nrow(aug_1_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(aug_1_data)){
    if (aug_1_data$unq_memID[j] == unq_memID_start_date$unq_memID[i]){
        if (aug_1_data$fu_end_date[j] < "2017-08-01" & aug_1_data$status[j] == "symptomatic infection"){
          status_aug[j] = "symptomatic infection"
          days_until_event_aug[j] = aug_1_data$days_until_event[j]
        } else {
          status_aug[j] = "censored"
          days_until_event_aug[j] = as.Date("2017-08-01") - aug_1_data$sample_id_date[j]
      }
    } 
  }
}
aug_1_data$days_until_event_aug = days_until_event_aug
aug_1_data$status_aug = status_aug
table(aug_1_data$status_aug, useNA = "always")
summary(aug_1_data$days_until_event_aug)
aug_1_data %>%
  filter(status_aug == "symptomatic infection") %>%
  View()

# test the output
symptomatic_data = aug_1_data %>% filter(status_aug == "symptomatic infection")
summary(symptomatic_data$days_until_event_aug)
test = symptomatic_data %>% select(unq_memID,fu_end_date)
length(unique(test$unq_memID,test$fu_end_date))
summary(aug_1_data$days_until_event_aug)
table(aug_1_data$status_aug,useNA="always")

# update the event indicator variable
aug_1_data$event_indicator_aug = ifelse(aug_1_data$status_aug == "symptomatic infection",1,0)
table(aug_1_data$event_indicator_aug,aug_1_data$status_aug,useNA = "always")

# run a crude multi-level coxph model with random intercepts for the participant level
# primary data set
fit.coxph.aug.crude <- coxme(Surv(days_until_event_aug, event_indicator_aug) ~ main_exposure_primary_case_def + (1 | unq_memID), 
                               data = aug_1_data)
fit.coxph.aug.crude
exp(confint(fit.coxph.aug.crude))

# run a multi-level coxph model with random intercepts for the participant level (not doing hh level because not using hh level covariates and didn't explain much variance, also makes interpretation better)
# primary data set
fit.coxph.aug <- coxme(Surv(days_until_event_aug, event_indicator_aug) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = aug_1_data)
fit.coxph.aug
exp(confint(fit.coxph.aug))




# look at follow-up through January 1, 2018

# first order the data set by date
survival_data_primary = dplyr::arrange(survival_data_primary,unq_memID,sample_id_date)

# first pull out when each participant entered the study
unq_memID_start_date = survival_data_primary[match(unique(survival_data_primary$unq_memID), survival_data_primary$unq_memID),]

# now subset the data set to only those events that have the same original follow-up date
keep = rep(NA,nrow(survival_data_primary))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(survival_data_primary)){
    if (survival_data_primary$unq_memID[j] == unq_memID_start_date$unq_memID[i] & 
        survival_data_primary$fu_end_date[j] == unq_memID_start_date$fu_end_date[i]){
      keep[j] = "yes"
    } 
  }
}
survival_data_primary$keep = keep
survival_data_primary = survival_data_primary %>% filter(keep == "yes")

# now calculate the time to symptomatic malaria but only include people with followup past the first month of the study (Jan 1 to capture variation in sampling)
jan_1_data = survival_data_primary %>% filter(sample_id_date < "2018-01-01")
length(unique(jan_1_data$unq_memID)) # 220 participants
# recalculate follow-up time
days_until_event_jan = rep(NA,nrow(jan_1_data))
status_jan = rep(NA,nrow(jan_1_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(jan_1_data)){
    if (jan_1_data$unq_memID[j] == unq_memID_start_date$unq_memID[i]){
      if (jan_1_data$fu_end_date[j] < "2018-01-01" & jan_1_data$status[j] == "symptomatic infection"){
        status_jan[j] = "symptomatic infection"
        days_until_event_jan[j] = jan_1_data$days_until_event[j]
      } else {
        status_jan[j] = "censored"
        days_until_event_jan[j] = as.Date("2018-01-01") - jan_1_data$sample_id_date[j]
      }
    } 
  }
}
jan_1_data$days_until_event_jan = days_until_event_jan
jan_1_data$status_jan = status_jan
table(jan_1_data$status_jan, useNA = "always")
summary(jan_1_data$days_until_event_jan)
jan_1_data %>%
  filter(status_jan == "symptomatic infection") %>%
  View()

# test the output
symptomatic_data = jan_1_data %>% filter(status_jan == "symptomatic infection")
summary(symptomatic_data$days_until_event_jan)
test = symptomatic_data %>% select(unq_memID,fu_end_date)
length(unique(test$unq_memID,test$fu_end_date))
summary(jan_1_data$days_until_event_jan)
table(jan_1_data$status_jan,useNA="always")

# update the event indicator variable
jan_1_data$event_indicator_jan = ifelse(jan_1_data$status_jan == "symptomatic infection",1,0)
table(jan_1_data$event_indicator_jan,jan_1_data$status_jan,useNA = "always")

# run a crude multi-level coxph model with random intercepts for the participant level
# primary data set
fit.coxph.jan.crude <- coxme(Surv(days_until_event_jan, event_indicator_jan) ~ main_exposure_primary_case_def + (1 | unq_memID), 
                             data = jan_1_data)
fit.coxph.jan.crude
exp(confint(fit.coxph.jan.crude))

# run a multi-level coxph model with random intercepts for the participant level (not doing hh level because not using hh level covariates and didn't explain much variance, also makes interpretation better)
# primary data set
fit.coxph.jan <- coxme(Surv(days_until_event_jan, event_indicator_jan) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                       data = jan_1_data)
fit.coxph.jan
exp(confint(fit.coxph.jan))


# look at follow-up through January 1, 2019

# first order the data set by date
survival_data_primary = dplyr::arrange(survival_data_primary,unq_memID,sample_id_date)

# first pull out when each participant entered the study
unq_memID_start_date = survival_data_primary[match(unique(survival_data_primary$unq_memID), survival_data_primary$unq_memID),]

# now subset the data set to only those events that have the same original follow-up date
keep = rep(NA,nrow(survival_data_primary))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(survival_data_primary)){
    if (survival_data_primary$unq_memID[j] == unq_memID_start_date$unq_memID[i] & 
        survival_data_primary$fu_end_date[j] == unq_memID_start_date$fu_end_date[i]){
      keep[j] = "yes"
    } 
  }
}
survival_data_primary$keep = keep
survival_data_primary = survival_data_primary %>% filter(keep == "yes")

# now calculate the time to symptomatic malaria but only include people with followup past the first month of the study (Jan 1 2019 to capture variation in sampling)
jan_1_data = survival_data_primary %>% filter(sample_id_date < "2019-01-01")
length(unique(jan_1_data$unq_memID)) # 249 participants
# recalculate follow-up time
days_until_event_jan = rep(NA,nrow(jan_1_data))
status_jan = rep(NA,nrow(jan_1_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(jan_1_data)){
    if (jan_1_data$unq_memID[j] == unq_memID_start_date$unq_memID[i]){
      if (jan_1_data$fu_end_date[j] < "2019-01-01" & jan_1_data$status[j] == "symptomatic infection"){
        status_jan[j] = "symptomatic infection"
        days_until_event_jan[j] = jan_1_data$days_until_event[j]
      } else {
        status_jan[j] = "censored"
        days_until_event_jan[j] = as.Date("2019-01-01") - jan_1_data$sample_id_date[j]
      }
    } 
  }
}
jan_1_data$days_until_event_jan = days_until_event_jan
jan_1_data$status_jan = status_jan
table(jan_1_data$status_jan, useNA = "always")
summary(jan_1_data$days_until_event_jan)
jan_1_data %>%
  filter(status_jan == "symptomatic infection") %>%
  View()

# test the output
symptomatic_data = jan_1_data %>% filter(status_jan == "symptomatic infection")
summary(symptomatic_data$days_until_event_jan)
test = symptomatic_data %>% select(unq_memID,fu_end_date)
length(unique(test$unq_memID,test$fu_end_date))
summary(jan_1_data$days_until_event_jan)
table(jan_1_data$status_jan,useNA="always")

# update the event indicator variable
jan_1_data$event_indicator_jan = ifelse(jan_1_data$status_jan == "symptomatic infection",1,0)
table(jan_1_data$event_indicator_jan,jan_1_data$status_jan,useNA = "always")

# run a crude multi-level coxph model with random intercepts for the participant level
# primary data set
fit.coxph.jan.crude <- coxme(Surv(days_until_event_jan, event_indicator_jan) ~ main_exposure_primary_case_def + (1 | unq_memID), 
                             data = jan_1_data)
fit.coxph.jan.crude
exp(confint(fit.coxph.jan.crude))

# run a multi-level coxph model with random intercepts for the participant level (not doing hh level because not using hh level covariates and didn't explain much variance, also makes interpretation better)
# primary data set
fit.coxph.jan <- coxme(Surv(days_until_event_jan, event_indicator_jan) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                       data = jan_1_data)
fit.coxph.jan
exp(confint(fit.coxph.jan))





#### ------ make plot of symptomatic infections over time for aim 1A ------- ####

symptomatic_months = final_data %>%
  filter(main_outcome_primary_case_def == "symptomatic infection")
symptomatic_months$month_year = lubridate::floor_date(ymd(symptomatic_months$sample_id_date),"month")
table(symptomatic_months$month_year,useNA="always")
symptomatic_df = symptomatic_months %>%
  select(month_year) %>%
  group_by(month_year) %>%
  summarise(n=n())
test_plot = ggplot(data=symptomatic_df,aes(x=month_year,y=n)) +
  geom_bar(stat="identity") +
  theme_bw() +
  ylab("Number of symptomatic infections") +
  xlab("Month") +
  scale_x_date(date_breaks="1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
test_plot
ggsave(test_plot, filename="/Users/kelseysumner/Desktop/aim1a_symp_infections_over_time.png", device="png",
       height=6, width=11, units="in", dpi=500)



