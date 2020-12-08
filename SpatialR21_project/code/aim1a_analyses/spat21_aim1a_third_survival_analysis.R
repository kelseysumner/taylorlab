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
survival_data_primary = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_19NOV2020.rds")

# read in the secondary stringent data set
survival_data_secondary_stringent = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_stringent_survival_format_19NOV2020.rds")

# read in the secondary permissive data set
survival_data_secondary_permissive = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_permissive_survival_format_19NOV2020.rds")





#### ------- now subset the data set to remove all monthly visits up to 14 days before the symptomatic infection --------- ####

# remove monthly visits up to 14 days before the symptomatic infection
# primary data set
remove = rep(NA,nrow(survival_data_primary))
for (i in 1:nrow(survival_data_primary)){
  if (survival_data_primary$days_until_event[i] < 15 & survival_data_primary$event_indicator[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_primary$days_until_event < 15 & survival_data_primary$status == "symptomatic infection"))
survival_data_primary$remove = remove
survival_data_primary = survival_data_primary %>% filter(is.na(remove))
survival_data_primary$remove <- NULL


# remove monthly visits up to 14 days before the symptomatic infection
# secondary stringent data set
remove = rep(NA,nrow(survival_data_secondary_stringent))
for (i in 1:nrow(survival_data_secondary_stringent)){
  if (survival_data_secondary_stringent$days_until_event[i] < 15 & survival_data_secondary_stringent$event_indicator[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_secondary_stringent$days_until_event < 15 & survival_data_secondary_stringent$status == "symptomatic infection"))
survival_data_secondary_stringent$remove = remove
survival_data_secondary_stringent = survival_data_secondary_stringent %>% filter(is.na(remove))
survival_data_secondary_stringent$remove <- NULL


# remove monthly visits up to 14 days before the symptomatic infection
# secondary permissive data set
remove = rep(NA,nrow(survival_data_secondary_permissive))
for (i in 1:nrow(survival_data_secondary_permissive)){
  if (survival_data_secondary_permissive$days_until_event[i] < 15 & survival_data_secondary_permissive$event_indicator[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_secondary_permissive$days_until_event < 15 & survival_data_secondary_permissive$status == "symptomatic infection"))
survival_data_secondary_permissive$remove = remove
survival_data_secondary_permissive = survival_data_secondary_permissive %>% filter(is.na(remove))
survival_data_secondary_permissive$remove <- NULL





#### --------- now rerun the cox porportional hazards model across outcome definitions ---------- ####

# run a model using the primary case definition
fit.coxph.primary <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                   data = survival_data_primary)
fit.coxph.primary
exp(confint(fit.coxph.primary))


# run a model using the secondary stringent case definition
fit.coxph.secondarystringent <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_stringent_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                   data = survival_data_secondary_stringent)
fit.coxph.secondarystringent
exp(confint(fit.coxph.secondarystringent))


# run a model using the secondary permissive case definition
fit.coxph.secondarypermissive <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                                      data = survival_data_secondary_permissive)
fit.coxph.secondarypermissive
exp(confint(fit.coxph.secondarypermissive))




#### -------- explore EMM by age using primary case definition ------- ####

# under 5
data_under5 = survival_data_primary %>% filter(age_cat_baseline == "<5 years")
fit.coxph.under5 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_under5)
fit.coxph.under5
# 5 to 15
data_5to15 = survival_data_primary %>% filter(age_cat_baseline == "5-15 years")
fit.coxph.5to15 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_5to15)
fit.coxph.5to15
# over 15
data_over15 = survival_data_primary %>% filter(age_cat_baseline == ">15 years")
fit.coxph.over15 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_over15)
fit.coxph.over15



# now create a forest plot comparing these results to the main model
# make a forest plot of the model results
estimates = c(1.38,1.16,0.96,1.23,1.06,0.88)
lower_ci = c(1.05,1.02,0.81,0.92,0.93,0.74)
upper_ci = c(1.81,1.32,1.13,1.64,1.21,1.05)
type = c("Main model","Main model","Main model","Misclassification analysis","Misclassification analysis","Misclassification analysis")
names = c("<5 years","5-15 years",">15 years","<5 years","5-15 years",">15 years")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci,type)
forest_plot_df$names = factor(forest_plot_df$names, levels=c("<5 years","5-15 years",">15 years"))
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=1.25) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0.5,0.6,0.7,0.75,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)) +
  theme_bw() +
  facet_wrap(~type,ncol=1,strip.position = "top",scales = "free_y") +
  theme(text = element_text(size=10.5))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/figure4_maintomisclassification_fp.png", device="png",
       height=4, width=5.5, units="in", dpi=400)



# model with an interaction term for age
fit.coxph.interaction <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*age_cat_baseline + (1 | unq_memID), 
                               data = survival_data_primary)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.primary) # does appear to be significant interaction



#### ----- look at EMM by sex using primary case definition ------- ####

# primary data set
# model with an interaction term for gender
fit.coxph.interaction <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*gender + (1 | unq_memID), 
                               data = survival_data_primary)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.primary) # does not appear to be significant interaction

# now run stratified models
# females
data_female = survival_data_primary %>% filter(gender=="female")
fit.coxph.female <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_female)
fit.coxph.female
# males
data_male = survival_data_primary %>% filter(gender == "male")
fit.coxph.male <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = data_male)
fit.coxph.male







