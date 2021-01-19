# -------------------------------------- #
#           Spat21/Mozzie Study          #
#          30-day hazard analysis        #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#            December 8, 2020            #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)
library(survminer)
library(survival)
library(ggalluvial)
library(gridExtra)
library(coxme)
library(lme4)


#### ------- read in the data sets -------- ####

# read in the primary data set
survival_data_primary = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_19NOV2020.rds")

# read in the secondary stringent data set
survival_data_secondary_stringent = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_stringent_survival_format_19NOV2020.rds")

# read in the secondary permissive data set
survival_data_secondary_permissive = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_permissive_survival_format_19NOV2020.rds")



#### ------ now recode follow-up to be within 30 days ------- ####

## ------- for primary data

# first pull out when each participant entered the study
unq_memID_start_date = survival_data_primary[match(unique(survival_data_primary$unq_memID), survival_data_primary$unq_memID),]

# only follow-up participants for 30 days
days_until_event_30day = rep(NA,nrow(survival_data_primary))
status_30day = rep(NA,nrow(survival_data_primary))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(survival_data_primary)){
    if (survival_data_primary$unq_memID[j] == unq_memID_start_date$unq_memID[i]){
      if (survival_data_primary$days_until_event[j] <= 30){
        days_until_event_30day[j] = survival_data_primary$days_until_event[j]
        if (survival_data_primary$status[j] == "symptomatic infection"){
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
survival_data_primary$days_until_event_30day = days_until_event_30day
survival_data_primary$status_30day = status_30day

# test the output
symptomatic_data = survival_data_primary %>% filter(status_30day == "symptomatic infection")
summary(symptomatic_data$days_until_event_30day)
test = symptomatic_data %>% select(unq_memID,fu_end_date)
length(unique(test$unq_memID,test$fu_end_date))
summary(survival_data_primary$days_until_event_30day)
table(survival_data_primary$status_30day,useNA="always")

# update the event indicator variable
survival_data_primary$event_indicator_30day = ifelse(survival_data_primary$status_30day == "symptomatic infection",1,0)
table(survival_data_primary$event_indicator_30day,survival_data_primary$status_30day,useNA = "always")

# export the data set
# write_csv(survival_data_primary,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_30day_10DEC2020.csv")
# write_rds(survival_data_primary,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_30day_10DEC2020.rds")


## ------- for secondary stringent data

# first pull out when each participant entered the study
unq_memID_start_date = survival_data_secondary_stringent[match(unique(survival_data_secondary_stringent$unq_memID), survival_data_secondary_stringent$unq_memID),]

# only follow-up participants for 30 days
days_until_event_30day = rep(NA,nrow(survival_data_secondary_stringent))
status_30day = rep(NA,nrow(survival_data_secondary_stringent))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(survival_data_secondary_stringent)){
    if (survival_data_secondary_stringent$unq_memID[j] == unq_memID_start_date$unq_memID[i]){
      if (survival_data_secondary_stringent$days_until_event[j] <= 30){
        days_until_event_30day[j] = survival_data_secondary_stringent$days_until_event[j]
        if (survival_data_secondary_stringent$status[j] == "symptomatic infection"){
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
survival_data_secondary_stringent$days_until_event_30day = days_until_event_30day
survival_data_secondary_stringent$status_30day = status_30day

# test the output
symptomatic_data = survival_data_secondary_stringent %>% filter(status_30day == "symptomatic infection")
summary(symptomatic_data$days_until_event_30day)
test = symptomatic_data %>% select(unq_memID,fu_end_date)
length(unique(test$unq_memID,test$fu_end_date))
summary(survival_data_secondary_stringent$days_until_event_30day)
table(survival_data_secondary_stringent$status_30day,useNA="always")

# update the event indicator variable
survival_data_secondary_stringent$event_indicator_30day = ifelse(survival_data_secondary_stringent$status_30day == "symptomatic infection",1,0)
table(survival_data_secondary_stringent$event_indicator_30day,survival_data_secondary_stringent$status_30day,useNA = "always")

# export the data set
# write_csv(survival_data_secondary_stringent,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_stringent_survival_format_30day_10DEC2020.csv")
# write_rds(survival_data_secondary_stringent,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_stringent_survival_format_30day_10DEC2020.rds")


## ------- for secondary permissive data

# first pull out when each participant entered the study
unq_memID_start_date = survival_data_secondary_permissive[match(unique(survival_data_secondary_permissive$unq_memID), survival_data_secondary_permissive$unq_memID),]

# only follow-up participants for 30 days
days_until_event_30day = rep(NA,nrow(survival_data_secondary_permissive))
status_30day = rep(NA,nrow(survival_data_secondary_permissive))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(survival_data_secondary_permissive)){
    if (survival_data_secondary_permissive$unq_memID[j] == unq_memID_start_date$unq_memID[i]){
      if (survival_data_secondary_permissive$days_until_event[j] <= 30){
        days_until_event_30day[j] = survival_data_secondary_permissive$days_until_event[j]
        if (survival_data_secondary_permissive$status[j] == "symptomatic infection"){
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
survival_data_secondary_permissive$days_until_event_30day = days_until_event_30day
survival_data_secondary_permissive$status_30day = status_30day

# test the output
symptomatic_data = survival_data_secondary_permissive %>% filter(status_30day == "symptomatic infection")
summary(symptomatic_data$days_until_event_30day)
test = symptomatic_data %>% select(unq_memID,fu_end_date)
length(unique(test$unq_memID,test$fu_end_date))
summary(survival_data_secondary_permissive$days_until_event_30day)
table(survival_data_secondary_permissive$status_30day,useNA="always")

# update the event indicator variable
survival_data_secondary_permissive$event_indicator_30day = ifelse(survival_data_secondary_permissive$status_30day == "symptomatic infection",1,0)
table(survival_data_secondary_permissive$event_indicator_30day,survival_data_secondary_permissive$status_30day,useNA = "always")

# export the data set
# write_csv(survival_data_secondary_permissive,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_permissive_survival_format_30day_10DEC2020.csv")
# write_rds(survival_data_secondary_permissive,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_permissive_survival_format_30day_10DEC2020.rds")


#### ----- run a multi-level logistic regression model --------- ####

## ---- for the primary case definition
# this is logistic regression model
survival_data_primary$status_30day = factor(survival_data_primary$status_30day,levels=c("censored","symptomatic infection"))
logit.model.30day <- glmer(status_30day ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = survival_data_primary, family = binomial(link="logit"),control = glmerControl(optimizer="bobyqa"))
summary(logit.model.30day)
exp(confint(logit.model.30day,method="Wald"))

# also calculate crude RR of symptomatic malaria within 30 days
table(survival_data_primary$main_exposure_primary_case_def,survival_data_primary$status_30day,useNA = "always")
RR = (161/(1681+161))/(123/(123+3414))
RR


#### ----------- now run a multi-level cox regression model ----------- ####

## ------ for the primary case definition

# check model assumptions
check <- coxph(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name, 
                         data = survival_data_primary)
check
cox.zph(check)
ggcoxzph(cox.zph(check))

# make a KM plot
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def, data = survival_data_primary), 
                      xlab = "Time in days", 
                      ylab = "Survival probability",
                      surv.median.line = "hv",
                      tables.height = 0.2,
                      tables.theme = theme_cleantable(),
                      conf.int = T,
                      legend = "none",
                     legend.labs = c("no infection","asymptomatic infection"),
                      pval = F,
                      ggtheme = theme_bw(),
                      risk.table = T,
                      ncensor.plot = F,
                      palette = c("#cccccc","#000000"),
                      conf.int.style = "step",
                      risk.table.y.text = FALSE,
                      risk.table.y.text.col = T,
                      font.title = c(11, "bold"),
                     title = "1-month follow-up")
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier_30day.png", device="png",
       height=5, width=4, units="in", dpi=300)
# log rank test for difference in two KM survival curves
survdiff(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def, data = survival_data_primary)
sd <- survdiff(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def, data = survival_data_primary)
1 - pchisq(sd$chisq, length(sd$n) - 1)


# run a multi-level coxph model with random intercepts for the participant level (not doing hh level because not using hh level covariates and didn't explain much variance, also makes interpretation better)
# primary data set
fit.coxph.30day <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                   data = survival_data_primary)
fit.coxph.30day


# rerun the model but with a robust variance estimator (GEE)
gee.coxph.30day <- coxph(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + cluster(unq_memID),robust=TRUE, 
                         data = survival_data_primary)
gee.coxph.30day


# make a forest plot of the model results
table1 = exp(confint(fit.coxph.30day,method="Wald"))
estimates = c(2.6126717,NA,1.3682320,0.5601212,NA,0.9328874,NA,1.0038190,NA,1.0845478,0.7170426)
lower_ci = c(table1[1,1],NA,table1[3,1],table1[2,1],NA,table1[4,1],NA,table1[5,1],NA,table1[6,1],table1[7,1])
upper_ci = c(table1[1,2],NA,table1[3,2],table1[2,2],NA,table1[4,2],NA,table1[5,2],NA,table1[6,2],table1[7,2])
names = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village"))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1,1,1,1,1,1,1,1,1,1),colour=c("#000000","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0,0.2,0.3,0.4,0.6,0.8,1.0,2.0,3.0,4.0,5.0)) +
  theme_bw() +
  theme(text = element_text(size=11)) 
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/primary_forest_plot_coxph_1levels_30day.png", device="png",
       height=3, width=6, units="in", dpi=400)



# now test for EMM by age
# model with an interaction term for age
fit.coxph.interaction <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*age_cat_baseline + (1 | unq_memID), 
                               data = survival_data_primary)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.30day) # does not appear to be significant interaction

# now run stratified models
# under 5
data_under5 = survival_data_primary %>% filter(age_cat_baseline == "<5 years")
fit.coxph.under5 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_under5)
fit.coxph.under5
# 5 to 15
data_5to15 = survival_data_primary %>% filter(age_cat_baseline == "5-15 years")
fit.coxph.5to15 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_5to15)
fit.coxph.5to15
# over 15
data_over15 = survival_data_primary %>% filter(age_cat_baseline == ">15 years")
fit.coxph.over15 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_over15)
fit.coxph.over15


# now test for EMM by gender
# model with an interaction term for gender
fit.coxph.interaction <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*gender + (1 | unq_memID), 
                               data = survival_data_primary)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.30day) # does appear to be significant interaction

# now run stratified models
# females
data_female = survival_data_primary %>% filter(gender=="female")
fit.coxph.female <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_female)
fit.coxph.female
# males
data_male = survival_data_primary %>% filter(gender == "male")
fit.coxph.male <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = data_male)
fit.coxph.male



## ----- for the secondary permissive case definition

# run a multi-level coxph model with random intercepts for the participant level (not doing hh level because not using hh level covariates and didn't explain much variance, also makes interpretation better)
# secondary permissive data set
fit.coxph.30day.perm <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = survival_data_secondary_permissive)
fit.coxph.30day.perm


# make a forest plot of the model results
table1 = exp(confint(fit.coxph.30day.perm,method="Wald"))
estimates = c(1.9743178,NA,1.0742610,0.8151275,NA,1.0008147,NA,1.0475783,NA,1.0569645,0.8562032)
lower_ci = c(table1[1,1],NA,table1[3,1],table1[2,1],NA,table1[4,1],NA,table1[5,1],NA,table1[6,1],table1[7,1])
upper_ci = c(table1[1,2],NA,table1[3,2],table1[2,2],NA,table1[4,2],NA,table1[5,2],NA,table1[6,2],table1[7,2])
names = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village"))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1,1,1,1,1,1,1,1,1,1),colour=c("#000000","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0,0.2,0.3,0.4,0.6,0.8,1.0,2.0,3.0,4.0,5.0)) +
  theme_bw() +
  theme(text = element_text(size=11)) 
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/secondary_permissive_forest_plot_coxph_1levels_30day.png", device="png",
       height=3, width=6, units="in", dpi=400)



# now test for EMM by age
# model with an interaction term for age
fit.coxph.interaction <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_secondary_permissive_case_def*age_cat_baseline + (1 | unq_memID), 
                               data = survival_data_secondary_permissive)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.30day.perm) # does not appear to be significant interaction

# now run stratified models
# under 5
data_under5 = survival_data_secondary_permissive %>% filter(age_cat_baseline == "<5 years")
fit.coxph.under5 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_permissive_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_under5)
fit.coxph.under5
# 5 to 15
data_5to15 = survival_data_secondary_permissive %>% filter(age_cat_baseline == "5-15 years")
fit.coxph.5to15 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_permissive_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_5to15)
fit.coxph.5to15
# over 15
data_over15 = survival_data_secondary_permissive %>% filter(age_cat_baseline == ">15 years")
fit.coxph.over15 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_permissive_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_over15)
fit.coxph.over15


# now test for EMM by gender
# model with an interaction term for gender
fit.coxph.interaction <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_secondary_permissive_case_def*gender + (1 | unq_memID), 
                               data = survival_data_secondary_permissive)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.30day.perm) # does appear to be significant interaction

# now run stratified models
# females
data_female = survival_data_secondary_permissive %>% filter(gender=="female")
fit.coxph.female <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_female)
fit.coxph.female
# males
data_male = survival_data_secondary_permissive %>% filter(gender == "male")
fit.coxph.male <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = data_male)
fit.coxph.male



## ----- for the secondary stringent case definition

# run a multi-level coxph model with random intercepts for the participant level (not doing hh level because not using hh level covariates and didn't explain much variance, also makes interpretation better)
# secondary stringent data set
fit.coxph.30day.string <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_stringent_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                              data = survival_data_secondary_stringent)
fit.coxph.30day.string


# make a forest plot of the model results
table1 = exp(confint(fit.coxph.30day.string,method="Wald"))
estimates = c(2.7639582,NA,1.1081548,0.3821611,NA,0.9310280,NA,0.8087201,NA,1.2337654,0.7710805)
lower_ci = c(table1[1,1],NA,table1[3,1],table1[2,1],NA,table1[4,1],NA,table1[5,1],NA,table1[6,1],table1[7,1])
upper_ci = c(table1[1,2],NA,table1[3,2],table1[2,2],NA,table1[4,2],NA,table1[5,2],NA,table1[6,2],table1[7,2])
names = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village"))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1,1,1,1,1,1,1,1,1,1),colour=c("#000000","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0,0.2,0.3,0.4,0.6,0.8,1.0,2.0,3.0,4.0,5.0)) +
  theme_bw() +
  theme(text = element_text(size=11)) 
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/secondary_stringent_forest_plot_coxph_1levels_30day.png", device="png",
       height=3, width=6, units="in", dpi=400)



# now test for EMM by age
# model with an interaction term for age
fit.coxph.interaction <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_stringent_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_secondary_stringent_case_def*age_cat_baseline + (1 | unq_memID), 
                               data = survival_data_secondary_stringent)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.30day.string) # does not appear to be significant interaction

# now run stratified models
# under 5
data_under5 = survival_data_secondary_stringent %>% filter(age_cat_baseline == "<5 years")
fit.coxph.under5 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_stringent_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_under5)
fit.coxph.under5
# 5 to 15
data_5to15 = survival_data_secondary_stringent %>% filter(age_cat_baseline == "5-15 years")
fit.coxph.5to15 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_stringent_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_5to15)
fit.coxph.5to15
# over 15
data_over15 = survival_data_secondary_stringent %>% filter(age_cat_baseline == ">15 years")
fit.coxph.over15 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_stringent_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_over15)
fit.coxph.over15


# now test for EMM by gender
# model with an interaction term for gender
fit.coxph.interaction <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_stringent_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_secondary_stringent_case_def*gender + (1 | unq_memID), 
                               data = survival_data_secondary_stringent)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.30day.string) # does appear to be significant interaction

# now run stratified models
# females
data_female = survival_data_secondary_stringent %>% filter(gender=="female")
fit.coxph.female <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_stringent_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_female)
fit.coxph.female
# males
data_male = survival_data_secondary_stringent %>% filter(gender == "male")
fit.coxph.male <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_stringent_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = data_male)
fit.coxph.male



#### ---------- now make a figure compiling all results ---------- ####


# make a forest plot of the model results 
estimates = c(2.61,NA,3.77,2.45,2.55,NA,1.76,3.71,1.97,NA,2.27,2.09,1.77,NA,1.73,2.18,2.76,NA,3.94,2.64,2.47,NA,2.05,3.60)
lower_ci = c(2.05,NA,2.02,1.79,1.57,NA,1.24,2.62,1.63,NA,1.37,1.58,1.29,NA,1.30,1.68,2.11,NA,2.09,1.88,1.36,NA,1.39,2.46)
upper_ci = c(3.33,NA,7.04,3.35,4.15,NA,2.50,5.24,2.40,NA,3.74,2.77,2.44,NA,2.32,2.83,3.62,NA,7.43,3.72,4.49,NA,3.02,5.28)
type = c("Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)")
names = c("Main model","Age-stratified model","<5 years","5-15 years",">15 years","Sex-stratified model","Male","Female","Main model","Age-stratified model","<5 years","5-15 years",">15 years","Sex-stratified model","Male","Female","Main model","Age-stratified model","<5 years","5-15 years",">15 years","Sex-stratified model","Male","Female")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci,type)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Main model","Age-stratified model","<5 years","5-15 years",">15 years","Sex-stratified model","Male","Female"))
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(1,2,3,4,5,6,7,8)) +
  theme_bw() +
  facet_wrap(~type,ncol=1,strip.position = "top",scales = "free_y") + 
  theme(text = element_text(size=12),axis.text.y = element_text(face=c("plain","plain","bold","plain","plain","plain","bold","bold","bold","plain","plain","plain","bold","bold","bold","plain","plain","bold","plain","plain","plain","bold","bold","bold")))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/figure3_combo_forest_plot_coxph_1levels_30daymodel.png", device="png",
       height=7.5, width=7, units="in", dpi=400)


#### ------- now make a plot comparing this model to the original model ------- ####


# make a forest plot of the model results
estimates = c(2.61,1.97,2.76,1.11,1.20,1.02)
lower_ci = c(2.05,1.63,2.11,1.01,1.11,0.92)
upper_ci = c(3.33,2.40,3.62,1.22,1.31,1.13)
type = c("Short-term asymptomatic malaria exposure","Short-term asymptomatic malaria exposure","Short-term asymptomatic malaria exposure","Long-term asymptomatic malaria exposure","Long-term asymptomatic malaria exposure","Long-term asymptomatic malaria exposure")
names = c("Symptomatic malaria \n (primary)","Symptomatic malaria \n (secondary permissive)","Symptomatic malaria \n (secondary stringent)","Symptomatic malaria \n (primary)","Symptomatic malaria \n (secondary permissive)","Symptomatic malaria \n (secondary stringent)")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci,type)
forest_plot_df$type = factor(forest_plot_df$type,levels = c("Short-term asymptomatic malaria exposure","Long-term asymptomatic malaria exposure"))
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=1.25) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Adjusted hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0.8,0.9,1.0,1.5,2.0,2.5,3.0,3.5)) +
  theme_bw() +
  facet_wrap(~type,ncol=1,strip.position = "top",scales = "free_y") +
  theme(text = element_text(size=12.5))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/figure4_hazardsympmalaria_across_casedefs_shortandlong.png", device="png",
       height=4, width=7, units="in", dpi=400)





#### ------- now do the short-term analysis stratified by parasite density ------- #####

# read in the full data set
final_merged_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/De-identified Phase II_v13/final_merged_data/phase3_spat21_human_merged_data_with_dbs_censoring_18AUG2020.rds")
final_merged_data = final_merged_data %>% select(sample_name_final,pfr364Q_std_combined)

## ----- primary case definition

# merge in parasite density
survival_data_primary = left_join(survival_data_primary,final_merged_data,by="sample_name_final")
length(which(is.na(survival_data_primary$pfr364Q_std_combined)))
asymp_test = survival_data_primary %>% filter(main_exposure_primary_case_def == "asymptomatic infection")
length(which(is.na(asymp_test$pfr364Q_std_combined)))
asymp_test %>%
  filter(is.na(pfr364Q_std_combined)) %>%
  View()
# missing observations are imputed

# classify each asymptomatic infection by its parasite density
survival_data_primary$micro_detectable = ifelse(survival_data_primary$pfr364Q_std_combined >= 500,"Microscopy",NA)
survival_data_primary$rdt_detectable = ifelse(survival_data_primary$pfr364Q_std_combined >= 100,"RDT",NA)
survival_data_primary$hsrdt_detectable = ifelse(survival_data_primary$pfr364Q_std_combined >= 1,"HS-RDT",NA)
survival_data_primary$pcr_detectable = ifelse(!(is.na(survival_data_primary$pfr364Q_std_combined)),"qPCR",NA)
survival_data_primary %>%
  filter(micro_detectable == "Microscopy") %>%
  View()
summary(survival_data_primary$pfr364Q_std_combined)
length(which(!(is.na(survival_data_primary$pfr364Q_std_combined))))
table(survival_data_primary$micro_detectable)
table(survival_data_primary$rdt_detectable)
table(survival_data_primary$hsrdt_detectable)
table(survival_data_primary$pcr_detectable)
summary(survival_data_primary %>% filter(micro_detectable == "Microscopy") %>% select(pfr364Q_std_combined))
summary(survival_data_primary %>% filter(rdt_detectable == "RDT") %>% select(pfr364Q_std_combined))
summary(survival_data_primary %>% filter(hsrdt_detectable == "HS-RDT") %>% select(pfr364Q_std_combined))
summary(survival_data_primary %>% filter(pcr_detectable == "qPCR") %>% select(pfr364Q_std_combined))


# now run separate models based on the parasite densities
survival_data_primary$p_any = ifelse(!(is.na(survival_data_primary$pfr364Q_std_combined)),"yes",NA)
survival_data_primary$p_1 = ifelse(survival_data_primary$pfr364Q_std_combined > 1,"yes",NA)
survival_data_primary$p_10 = ifelse(survival_data_primary$pfr364Q_std_combined > 10,"yes",NA)
survival_data_primary$p_100 = ifelse(survival_data_primary$pfr364Q_std_combined > 100,"yes",NA)
survival_data_primary$p_500 = ifelse(survival_data_primary$pfr364Q_std_combined > 500,"yes",NA)
survival_data_primary$p_1000 = ifelse(survival_data_primary$pfr364Q_std_combined > 1000,"yes",NA)
survival_data_primary$p_5000 = ifelse(survival_data_primary$pfr364Q_std_combined > 5000,"yes",NA)
summary(survival_data_primary %>% filter(p_any == "yes") %>% select(pfr364Q_std_combined))
summary(survival_data_primary %>% filter(p_1 == "yes") %>% select(pfr364Q_std_combined))
summary(survival_data_primary %>% filter(p_10 == "yes") %>% select(pfr364Q_std_combined))
summary(survival_data_primary %>% filter(p_100 == "yes") %>% select(pfr364Q_std_combined))
summary(survival_data_primary %>% filter(p_500 == "yes") %>% select(pfr364Q_std_combined))
summary(survival_data_primary %>% filter(p_1000 == "yes") %>% select(pfr364Q_std_combined))
summary(survival_data_primary %>% filter(p_5000 == "yes") %>% select(pfr364Q_std_combined))


# now run separate models for each parasite density stratification
## p_any
pany_data = survival_data_primary %>% filter(p_any == "yes" | main_exposure_primary_case_def == "no infection")
fit.coxph.pany <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                           data = pany_data)
fit.coxph.pany
## 1
p1_data = survival_data_primary %>% filter(p_1 == "yes" | main_exposure_primary_case_def == "no infection")
fit.coxph.p1 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = p1_data)
fit.coxph.p1
## 10
p10_data = survival_data_primary %>% filter(p_10 == "yes" | main_exposure_primary_case_def == "no infection")
fit.coxph.p10 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                      data = p10_data)
fit.coxph.p10
## 100
p100_data = survival_data_primary %>% filter(p_100 == "yes" | main_exposure_primary_case_def == "no infection")
fit.coxph.p100 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                       data = p100_data)
fit.coxph.p100
## 500
p500_data = survival_data_primary %>% filter(p_500 == "yes" | main_exposure_primary_case_def == "no infection")
fit.coxph.p500 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = p500_data)
fit.coxph.p500
## 1000
p1000_data = survival_data_primary %>% filter(p_1000 == "yes" | main_exposure_primary_case_def == "no infection")
fit.coxph.p1000 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = p1000_data)
fit.coxph.p1000


# now reclassify asymptomatic infections based on parasite densities and detectability
survival_data_primary$micro_detectable = ifelse(!(is.na(survival_data_primary$micro_detectable)),survival_data_primary$micro_detectable,"no infection")
survival_data_primary$rdt_detectable = ifelse(!(is.na(survival_data_primary$rdt_detectable)),survival_data_primary$rdt_detectable,"no infection")
survival_data_primary$hsrdt_detectable = ifelse(!(is.na(survival_data_primary$hsrdt_detectable)),survival_data_primary$hsrdt_detectable,"no infection")
survival_data_primary$pcr_detectable = ifelse(!(is.na(survival_data_primary$pcr_detectable)),survival_data_primary$pcr_detectable,"no infection")
table(survival_data_primary$micro_detectable,useNA = "always")
table(survival_data_primary$rdt_detectable,useNA = "always")
table(survival_data_primary$hsrdt_detectable,useNA = "always")
table(survival_data_primary$pcr_detectable,useNA = "always")
survival_data_primary$micro_detectable = factor(survival_data_primary$micro_detectable,levels=c("no infection","Microscopy"))
survival_data_primary$rdt_detectable = factor(survival_data_primary$rdt_detectable,levels=c("no infection","RDT"))
survival_data_primary$hsrdt_detectable = factor(survival_data_primary$hsrdt_detectable,levels=c("no infection","HS-RDT"))
survival_data_primary$pcr_detectable = factor(survival_data_primary$pcr_detectable,levels=c("no infection","qPCR"))

# now run separate models for each parasite density stratification but with exposure re-classified
## microscopy
fit.coxph.micro.2 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ micro_detectable + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = survival_data_primary)
fit.coxph.micro.2
## rdt
fit.coxph.rdt.2 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ rdt_detectable + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                       data = survival_data_primary)
fit.coxph.rdt.2
## hs-rdt
fit.coxph.hsrdt.2 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ hsrdt_detectable + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = survival_data_primary)
fit.coxph.hsrdt.2
## pcr
fit.coxph.pcr.2 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ pcr_detectable + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                       data = survival_data_primary)
fit.coxph.pcr.2


# make parasite density figure
# figure 1: detectability restricted to asymptomatic infections
estimates = c(2.21,2.78,2.89,2.93,3.41,3.99)
lower_ci = c(1.70,2.10,2.10,2.00,2.17,2.41)
upper_ci = c(2.87,3.66,3.98,4.29,5.37,6.62)
names = c("Any density \n N=1602/5139",">1 p/uL \n N=1098/4635",">10 p/uL \n N=691/4228",">100 p/uL \n N=398/3935",">500 p/uL \n N=170/3707",">1000 p/uL \n N=110/3647")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$name, levels=c("Any density \n N=1602/5139",">1 p/uL \n N=1098/4635",">10 p/uL \n N=691/4228",">100 p/uL \n N=398/3935",">500 p/uL \n N=170/3707",">1000 p/uL \n N=110/3647"))
fp <- ggplot(data=forest_plot_df, aes(x=names, y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=1.25) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  xlab("Parasite density threshold") + ylab("Adjusted hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks = c(1,2,3,4,5,6,7)) +
  theme_bw() +
  theme(text = element_text(size=11))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/detectability_analysis_restricted.png", device="png",
       height=4, width=6, units="in", dpi=400)
# figure 2: detectability recoding asymptomatic infections
estimates = c(2.41,1.93,2.11,1.70)
lower_ci = c(1.56,1.36,1.63,1.33)
upper_ci = c(3.71,2.74,2.72,2.17)
names = c("Microscopy \n N=170/5379","RDT \n N=398/5379","HS-RDT \n N=1098/5379","qPCR \n N=1602/5379")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$name, levels=c("qPCR \n N=1602/5379","HS-RDT \n N=1098/5379","RDT \n N=398/5379","Microscopy \n N=170/5379"))
fp <- ggplot(data=forest_plot_df, aes(x=names, y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=1.25) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  xlab("Malaria diagnostic") + ylab("Adjusted hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks = c(1,2,3,4,5,6,7)) +
  theme_bw() +
  theme(text = element_text(size=11))
n = fp + expand_limits(y=c(0,7))
ggsave(n, filename="/Users/kelseysumner/Desktop/detectability_analysis_recoded.png", device="png",
       height=4, width=6, units="in", dpi=400)


# also make a histogram of parasite densities across asymptomatic infections
survival_data_primary_pf = survival_data_primary %>% filter(!(is.na(pfr364Q_std_combined)))
hist_p = ggplot(data = survival_data_primary_pf,aes(x=pfr364Q_std_combined)) +
  geom_density(fill="#A9A9A9")+
  theme_bw() +
  ylab("Parasite density (p/uL)")
hist_p
# now restrict to just 1000 p/uL
survival_data_primary_pf = survival_data_primary %>% filter(pfr364Q_std_combined <= 1000)
hist_p = ggplot(data = survival_data_primary_pf,aes(x=pfr364Q_std_combined)) +
  geom_density(fill="#A9A9A9")+
  theme_bw() +
  xlab("Parasite density (p/uL)")
hist_p
# this is still super right skewed





#### ------ now do a misclassification analysis --------- ####

# remove monthly visits up to 14 days before the symptomatic infection
# primary data set
remove = rep(NA,nrow(survival_data_primary))
for (i in 1:nrow(survival_data_primary)){
  if (survival_data_primary$days_until_event_30day[i] < 15 & survival_data_primary$event_indicator_30day[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_primary$days_until_event_30day < 15 & survival_data_primary$status == "symptomatic infection"))
survival_data_primary$remove = remove
survival_data_primary = survival_data_primary %>% filter(is.na(remove))
survival_data_primary$remove <- NULL


# remove monthly visits up to 14 days before the symptomatic infection
# secondary stringent data set
remove = rep(NA,nrow(survival_data_secondary_stringent))
for (i in 1:nrow(survival_data_secondary_stringent)){
  if (survival_data_secondary_stringent$days_until_event_30day[i] < 15 & survival_data_secondary_stringent$event_indicator_30day[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_secondary_stringent$days_until_event_30day < 15 & survival_data_secondary_stringent$status == "symptomatic infection"))
survival_data_secondary_stringent$remove = remove
survival_data_secondary_stringent = survival_data_secondary_stringent %>% filter(is.na(remove))
survival_data_secondary_stringent$remove <- NULL


# remove monthly visits up to 14 days before the symptomatic infection
# secondary permissive data set
remove = rep(NA,nrow(survival_data_secondary_permissive))
for (i in 1:nrow(survival_data_secondary_permissive)){
  if (survival_data_secondary_permissive$days_until_event_30day[i] < 15 & survival_data_secondary_permissive$event_indicator_30day[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_secondary_permissive$days_until_event_30day < 15 & survival_data_secondary_permissive$status == "symptomatic infection"))
survival_data_secondary_permissive$remove = remove
survival_data_secondary_permissive = survival_data_secondary_permissive %>% filter(is.na(remove))
survival_data_secondary_permissive$remove <- NULL


# run a model using the primary case definition
summary(survival_data_primary$days_until_event_30day)
fit.coxph.primary <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                           data = survival_data_primary)
fit.coxph.primary
exp(confint(fit.coxph.primary))


# run a model using the secondary permissive case definition
fit.coxph.secondarypermissive <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                                       data = survival_data_secondary_permissive)
fit.coxph.secondarypermissive
exp(confint(fit.coxph.secondarypermissive))


# run a model using the secondary stringent case definition
fit.coxph.secondarystringent <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_secondary_stringent_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                                      data = survival_data_secondary_stringent)
fit.coxph.secondarystringent
exp(confint(fit.coxph.secondarystringent))


## ------ now explore EMM by age for the primary case definition

# compare model with interaction term
fit.coxph.interaction <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*age_cat_baseline + (1 | unq_memID), 
                          data = survival_data_primary)
fit.coxph.interaction
anova(fit.coxph.interaction,fit.coxph.primary)

# under 5
data_under5 = survival_data_primary %>% filter(age_cat_baseline == "<5 years")
fit.coxph.under5 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_under5)
fit.coxph.under5
# 5 to 15
data_5to15 = survival_data_primary %>% filter(age_cat_baseline == "5-15 years")
fit.coxph.5to15 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_5to15)
fit.coxph.5to15
# over 15
data_over15 = survival_data_primary %>% filter(age_cat_baseline == ">15 years")
fit.coxph.over15 <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_over15)
fit.coxph.over15


## ----- no explore EMM by sex

# primary data set
# model with an interaction term for gender
fit.coxph.interaction <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*gender + (1 | unq_memID), 
                               data = survival_data_primary)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.primary) # does not appear to be significant interaction

# now run stratified models
# females
data_female = survival_data_primary %>% filter(gender=="female")
fit.coxph.female <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_female)
fit.coxph.female
# males
data_male = survival_data_primary %>% filter(gender == "male")
fit.coxph.male <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = data_male)
fit.coxph.male



#### -------- make a figure of hazard of symptomatic malaria across sensitivity analyses ---------- ####

# make a forest plot of the model results
estimates = c(2.61,2.54,1.77,1.97,2.76)
lower_ci = c(2.05,1.76,1.26,1.63,2.11)
upper_ci = c(3.33,3.67,2.47,2.40,3.62)
type = c("Main model","Exposure sensitivity analyses","Exposure sensitivity analyses","Outcome sensitivity analyses","Outcome sensitivity analyses")
names = c("Symptomatic malaria \n (primary)","Post-treatment analysis","Misclassification analysis","Symptomatic malaria \n (secondary permissive)","Symptomatic malaria \n (secondary stringent)")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci,type)
forest_plot_df$type = factor(forest_plot_df$type,levels = c("Main model","Exposure sensitivity analyses","Outcome sensitivity analyses"))
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=1.25) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks = c(1,1.5,2,2.5,3,3.5,4)) +
  theme_bw() +
  facet_wrap(~type,ncol=1,strip.position = "top",scales = "free_y") +
  theme(text = element_text(size=12.5))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/figure4b_hazardsympmalaria_across_shortterm_casedefs.png", device="png",
       height=4, width=7, units="in", dpi=400)




