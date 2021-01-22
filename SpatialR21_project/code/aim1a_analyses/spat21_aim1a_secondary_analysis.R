# -------------------------------------- #
#           Spat21/Mozzie Study          #
#           Third survival analysis      #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#            December 1, 2020            #
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
survival_data_primary = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival_data_primary_final_data_19NOV2020.rds")

# read in the secondary stringent data set
survival_data_secondary_stringent = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival_data_secondary_stringent_final_data_19NOV2020.rds")

# read in the secondary permissive data set
survival_data_secondary_permissive = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival_data_secondary_permissive_final_data_19NOV2020.rds")



#### ------ now subset to remove follow-up not after a symptomatic infection ------ ####

# first subset the data to only participants with 1 or more symptomatic infections
multiple_symp_infxns = survival_data_primary %>%
  group_by(unq_memID) %>%
  summarize(n=sum(event_indicator,na.rm=T))
# only 70 participants with at least 1 symptomatic infection
multiple_symp_infxns = multiple_symp_infxns %>% filter(n>0)
secondary_analysis_data = survival_data_primary %>% filter(unq_memID %in% multiple_symp_infxns$unq_memID)

# now make a data set of symptomatic infections
symp_data = secondary_analysis_data %>% filter(event_indicator == 1)
min_dates = symp_data %>%
  group_by(unq_memID) %>%
  summarize(first_infxn = min(sample_id_date))

# now remove those first symptomatic infections
remove = rep(NA,nrow(secondary_analysis_data))
for (i in 1:nrow(secondary_analysis_data)){
  for (j in 1:nrow(min_dates)){
    if (secondary_analysis_data$unq_memID[i] == min_dates$unq_memID[j] &
        secondary_analysis_data$fu_end_date[i] == min_dates$first_infxn[j] &
        secondary_analysis_data$status[i] == "symptomatic infection"){
      remove[i] = "yes"
    }
  }
}
secondary_analysis_data$remove = remove
table(secondary_analysis_data$remove,useNA = "always")
secondary_analysis_data = secondary_analysis_data %>% filter(is.na(remove))
length(unique(secondary_analysis_data$unq_memID))

# check the lost to follow up entries and remove the ones that occur before a symptomatic visit
test = secondary_analysis_data %>% filter(status == "lost to follow up")
unique(test$unq_memID)
# K02_4
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="K02_4" & secondary_analysis_data$status == "lost to follow up"),]
# K02_5
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="K02_5" & secondary_analysis_data$status == "lost to follow up"),]
# K03_5
# lost to follow-up after symptomatic infection and nothing after it so fine
# K04_4
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="K04_4" & secondary_analysis_data$status == "study ended"),]
# K14_1
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="K14_1" & secondary_analysis_data$status == "study ended"),]
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="K14_1" & secondary_analysis_data$status == "lost to follow up" & secondary_analysis_data$fu_end_date == "2019-07-01"),]
# M05_4
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="M05_4" & secondary_analysis_data$status == "lost to follow up"),]
# M06_2
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="M06_2" & secondary_analysis_data$status == "study ended"),]
# M06_4
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="M06_4" & secondary_analysis_data$status == "study ended"),]
# M06_5
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="M06_5" & secondary_analysis_data$status == "lost to follow up"),]
# M09_1
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="M09_1" & secondary_analysis_data$status == "lost to follow up"),]
# M09_4
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="M09_4" & secondary_analysis_data$status == "lost to follow up"),]
# M13_11
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="M13_11" & secondary_analysis_data$status == "lost to follow up"),]
# M13_12
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="M13_12" & secondary_analysis_data$status == "lost to follow up"),]
# M14_6
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="M14_6" & secondary_analysis_data$status == "lost to follow up"),]
# M16_2
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="M16_2" & secondary_analysis_data$status == "symptomatic infection" & secondary_analysis_data$fu_end_date == "2019-10-09"),]
# M16_3
# fine
# S01_4
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S01_4" & secondary_analysis_data$status == "lost to follow up"),]
# S02_7
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S02_7" & secondary_analysis_data$status == "lost to follow up"),]
# S02_8
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S02_8" & secondary_analysis_data$status == "lost to follow up" & secondary_analysis_data$fu_end_date == "2017-10-01"),]
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S02_8" & secondary_analysis_data$status == "lost to follow up" & secondary_analysis_data$fu_end_date == "2018-03-01"),]
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S02_8" & secondary_analysis_data$status == "lost to follow up" & secondary_analysis_data$fu_end_date == "2018-09-01"),]
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S02_8" & secondary_analysis_data$status == "lost to follow up" & secondary_analysis_data$fu_end_date == "2019-01-01"),]
# S03_2
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S03_2" & secondary_analysis_data$status == "lost to follow up"),]
# S04_6
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S04_6" & secondary_analysis_data$status == "study ended"),]
# S05_3
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S05_3" & secondary_analysis_data$status == "lost to follow up"),]
# S05_5
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S05_5" & secondary_analysis_data$status == "symptomatic infection" & secondary_analysis_data$fu_end_date == "2018-04-19"),]
# S05_8
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S05_8" & secondary_analysis_data$status == "lost to follow up"),]
# S08_7
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S08_7" & secondary_analysis_data$status == "study ended"),]
# S09_1
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S09_1" & secondary_analysis_data$status == "lost to follow up"),]
# S10_6
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S10_6" & secondary_analysis_data$status == "lost to follow up"),]
# S10_7
secondary_analysis_data = secondary_analysis_data[-which(secondary_analysis_data$unq_memID=="S10_7" & secondary_analysis_data$status == "lost to follow up"),]

# now remove all entries with 0 days until event
secondary_analysis_data$days_until_event = as.numeric(secondary_analysis_data$days_until_event)
secondary_analysis_data = secondary_analysis_data %>% filter(days_until_event > 0)
summary(secondary_analysis_data$days_until_event)

# update the event indicator variable
secondary_analysis_data$event_indicator = ifelse(secondary_analysis_data$status=="symptomatic infection",1,0)
table(secondary_analysis_data$event_indicator,secondary_analysis_data$status,useNA="always")

# look at some general descriptives
length(unique(secondary_analysis_data$unq_memID))
summary(secondary_analysis_data$days_until_event)
table(secondary_analysis_data$main_exposure_primary_case_def)

# make the age cat variable
secondary_analysis_data$age_cat_baseline = ifelse(secondary_analysis_data$age_all_baseline < 5,"<5 years",
                                                ifelse(secondary_analysis_data$age_all_baseline >= 5 & secondary_analysis_data$age_all_baseline <= 15,"5-15 years",">15 years"))
table(secondary_analysis_data$age_all_baseline,secondary_analysis_data$age_cat_baseline,useNA="always")
secondary_analysis_data$age_cat_baseline = as.factor(secondary_analysis_data$age_cat_baseline)
levels(secondary_analysis_data$age_cat_baseline)

# make village name a factor
secondary_analysis_data$village_name = as.factor(secondary_analysis_data$village_name)

# make slept under a net regularly a factor
secondary_analysis_data$slept_under_net_regularly = as.factor(secondary_analysis_data$slept_under_net_regularly)

# make status a factor
secondary_analysis_data$status = as.factor(secondary_analysis_data$status)

# make the main exposure a factor
secondary_analysis_data$main_exposure_primary_case_def = as.factor(secondary_analysis_data$main_exposure_primary_case_def)
secondary_analysis_data$main_exposure_primary_case_def = relevel(secondary_analysis_data$main_exposure_primary_case_def,ref="no infection")

# make gender a factor
secondary_analysis_data$gender = as.factor(secondary_analysis_data$gender)

# now do the cox proportional hazards model
fit.coxph <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                   data = secondary_analysis_data)
fit.coxph
exp(confint(fit.coxph))



#### -------- explore EMM by age using primary case definition ------- ####

# under 5
data_under5 = secondary_analysis_data %>% filter(age_cat_baseline == "<5 years")
fit.coxph.under5 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_under5)
fit.coxph.under5
# 5 to 15
data_5to15 = secondary_analysis_data %>% filter(age_cat_baseline == "5-15 years")
fit.coxph.5to15 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_5to15)
fit.coxph.5to15
# over 15
data_over15 = secondary_analysis_data %>% filter(age_cat_baseline == ">15 years")
fit.coxph.over15 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_over15)
fit.coxph.over15

# model with an interaction term for age
fit.coxph.interaction <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*age_cat_baseline + (1 | unq_memID), 
                               data = secondary_analysis_data)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph) # does not appear to be significant interaction


#### ----- look at EMM by sex using primary case definition ------- ####

# primary data set
# model with an interaction term for gender
fit.coxph.interaction <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*gender + (1 | unq_memID), 
                               data = secondary_analysis_data)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph) # does not appear to be significant interaction

# now run stratified models
# females
data_female = secondary_analysis_data %>% filter(gender=="female")
fit.coxph.female <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_female)
fit.coxph.female
# males
data_male = secondary_analysis_data %>% filter(gender == "male")
fit.coxph.male <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = data_male)
fit.coxph.male




#### ---------- now set up for the 30-day analysis ---------- ####


# first pull out when each participant entered the study
unq_memID_start_date = secondary_analysis_data[match(unique(secondary_analysis_data$unq_memID), secondary_analysis_data$unq_memID),]

# only follow-up participants for 30 days
days_until_event_30day = rep(NA,nrow(secondary_analysis_data))
status_30day = rep(NA,nrow(secondary_analysis_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(secondary_analysis_data)){
    if (secondary_analysis_data$unq_memID[j] == unq_memID_start_date$unq_memID[i]){
      if (secondary_analysis_data$days_until_event[j] <= 30){
        days_until_event_30day[j] = secondary_analysis_data$days_until_event[j]
        if (secondary_analysis_data$status[j] == "symptomatic infection"){
          status_30day[j] = "symptomatic infection"
        } else {
          status_30day[j] = "censored"
        }
      } else {
        days_until_event_30day[j] = 30
        status_30day[j] = "censored"
      }
    }
  }
}
secondary_analysis_data$days_until_event_30day = days_until_event_30day
secondary_analysis_data$status_30day = status_30day

# test the output
symptomatic_data = secondary_analysis_data %>% filter(status_30day == "symptomatic infection")
summary(symptomatic_data$days_until_event_30day)
test = symptomatic_data %>% select(unq_memID,fu_end_date)
length(unique(test$unq_memID,test$fu_end_date))
summary(secondary_analysis_data$days_until_event_30day)
table(secondary_analysis_data$status_30day,useNA="always")

# update the event indicator variable
secondary_analysis_data$event_indicator_30day = ifelse(secondary_analysis_data$status_30day == "symptomatic infection",1,0)
table(secondary_analysis_data$event_indicator_30day,secondary_analysis_data$status_30day,useNA = "always")

# primary data set
fit.coxph.30day <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = secondary_analysis_data)
fit.coxph.30day


# now test for EMM by age
# model with an interaction term for age
fit.coxph.interaction <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*age_cat_baseline + (1 | unq_memID), 
                               data = secondary_analysis_data)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.30day) # does not appear to be significant interaction

# now run stratified models
# under 5
data_under5 = secondary_analysis_data %>% filter(age_cat_baseline == "<5 years")
fit.coxph.under5 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_under5)
fit.coxph.under5
# 5 to 15
data_5to15 = secondary_analysis_data %>% filter(age_cat_baseline == "5-15 years")
fit.coxph.5to15 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_5to15)
fit.coxph.5to15
# over 15
data_over15 = secondary_analysis_data %>% filter(age_cat_baseline == ">15 years")
fit.coxph.over15 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_over15)
fit.coxph.over15


# now test for EMM by gender
# model with an interaction term for gender
fit.coxph.interaction <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*gender + (1 | unq_memID), 
                               data = secondary_analysis_data)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.30day) # does appear to be significant interaction

# now run stratified models
# females
data_female = secondary_analysis_data %>% filter(gender=="female")
fit.coxph.female <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_female)
fit.coxph.female
# males
data_male = secondary_analysis_data %>% filter(gender == "male")
fit.coxph.male <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = data_male)
fit.coxph.male


##  comparison of data set in 30 post-treatment analysis to full analysis
##  first look at the number of symptomatic infections across covariates for post-treatment analysis
symp_infections = secondary_analysis_data %>% filter(status=="symptomatic infection")
table(symp_infections$main_exposure_primary_case_def)
table(symp_infections$age_cat_baseline)
table(symp_infections$gender)
table(symp_infections$slept_under_net_regularly)
table(symp_infections$village_name)
##  now compare the proportions between data sets
##  main exposure
# first make a contingency table
df = data.frame(full = c(1580,826),post=c(570,264))
# then do a chi-squared test
chisq.test(df)
# correct for repeated measures
0.1716*29
## age
# first make a contingency table
df = data.frame(full = c(329,1319,758),post=c(63,617,154))
# then do a chi-squared test
chisq.test(df)
# correct for repeated measures
2.2e-16*29
## sex
# first make a contingency table
df = data.frame(full = c(1190,1216),post=c(348,486))
# then do a chi-squared test
chisq.test(df)
# correct for repeated measures
0.0001369*29
## regular bed net usage
# first make a contingency table
df = data.frame(full = c(730,1676),post=c(257,577))
# then do a chi-squared test
chisq.test(df)
# correct for repeated measures
0.8314*29
## village
# first make a contingency table
df = data.frame(full = c(876,745,785),post=c(259,294,281))
# then do a chi-squared test
chisq.test(df)
# correct for repeated measures
0.01208*29







