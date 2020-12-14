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
write_csv(survival_data_primary,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_30day_10DEC2020.csv")
write_rds(survival_data_primary,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_30day_10DEC2020.rds")


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
write_csv(survival_data_secondary_stringent,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_stringent_survival_format_30day_10DEC2020.csv")
write_rds(survival_data_secondary_stringent,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_stringent_survival_format_30day_10DEC2020.rds")


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
write_csv(survival_data_secondary_permissive,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_permissive_survival_format_30day_10DEC2020.csv")
write_rds(survival_data_secondary_permissive,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_permissive_survival_format_30day_10DEC2020.rds")



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
                      legend = "bottom",
                     legend.labs = c("no infection","asymptomatic infection"),
                      pval = T,
                      ggtheme = theme_bw(),
                      risk.table = T,
                      ncensor.plot = F,
                      palette = c("#cccccc","#000000"),
                      conf.int.style = "step",
                      risk.table.y.text = FALSE,
                      risk.table.y.text.col = T,
                      font.title = c(11, "bold"))
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier_30day.png", device="png",
       height=4, width=4, units="in", dpi=300)
# log rank test for difference in two KM survival curves
survdiff(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def, data = survival_data_primary)
sd <- survdiff(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def, data = survival_data_primary)
1 - pchisq(sd$chisq, length(sd$n) - 1)


# run a multi-level coxph model with random intercepts for the participant level (not doing hh level because not using hh level covariates and didn't explain much variance, also makes interpretation better)
# primary data set
fit.coxph.30day <- coxme(Surv(days_until_event_30day, event_indicator_30day) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                   data = survival_data_primary)
fit.coxph.30day


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
       height=4, width=7, units="in", dpi=400)



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


# make a forest plot of the model results - additional way
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
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0.8,0.9,1.0,1.5,2.0,2.5,3.0,3.5)) +
  theme_bw() +
  facet_wrap(~type,ncol=1,strip.position = "top",scales = "free_y") +
  theme(text = element_text(size=12.5))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/figure4_hazardsympmalaria_across_casedefs_shortandlong.png", device="png",
       height=4, width=7, units="in", dpi=400)



#### ------- now do the short-term analysis stratified by parasite density ------- #####




