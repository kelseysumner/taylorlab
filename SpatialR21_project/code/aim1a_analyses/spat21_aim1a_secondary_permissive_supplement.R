# -------------------------------------- #
#           Spat21/Mozzie Study          #
#           Manuscript figures           #
#          Secondary permissive           #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#           November 30, 2020            #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)
library(survminer)
library(survival)
library(ggalluvial)
library(gridExtra)
library(coxme)



#### ---------- load the data set ------- ####

# read in the secondary permissive data set
survival_data_secondary_permissive = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_permissive_survival_format_19NOV2020.rds")



#### ------ make table 1: comparison of covariates and symptomatic infections ------ ####

## first look at total number of observations across covariates
table(survival_data_secondary_permissive$main_exposure_secondary_permissive_case_def)
table(survival_data_secondary_permissive$age_cat_baseline)
table(survival_data_secondary_permissive$gender)
table(survival_data_secondary_permissive$slept_under_net_regularly)
table(survival_data_secondary_permissive$village_name)

# now look at the number of symptomatic infections across covariates
symp_infections = survival_data_secondary_permissive %>% filter(status=="symptomatic infection")
table(symp_infections$main_exposure_secondary_permissive_case_def)
table(symp_infections$age_cat_baseline)
table(symp_infections$gender)
table(symp_infections$slept_under_net_regularly)
table(symp_infections$village_name)

# now look at the median time to symptoms
symp_infections %>% group_by(main_exposure_secondary_permissive_case_def) %>% summarize(median = median(days_until_event),lower = quantile(days_until_event, 0.25),upper = quantile(days_until_event, 0.75))
symp_infections %>% group_by(age_cat_baseline) %>% summarize(median = median(days_until_event),lower = quantile(days_until_event, 0.25),upper = quantile(days_until_event, 0.75))
symp_infections %>% group_by(gender) %>% summarize(median = median(days_until_event),lower = quantile(days_until_event, 0.25),upper = quantile(days_until_event, 0.75))
symp_infections %>% group_by(slept_under_net_regularly) %>% summarize(median = median(days_until_event),lower = quantile(days_until_event, 0.25),upper = quantile(days_until_event, 0.75))
symp_infections %>% group_by(village_name) %>% summarize(median = median(days_until_event),lower = quantile(days_until_event, 0.25),upper = quantile(days_until_event, 0.75))

# do the wilcoxon rank sum test to compare median time to symptoms across each covariates
# multiply each p-value by 29 which is the maximum number of time a person could have been in the data set
# main exposure
wilcox.test(days_until_event ~ main_exposure_secondary_permissive_case_def,data = symp_infections)
4.772e-10*29
# age
kruskal.test(days_until_event ~ age_cat_baseline,data = symp_infections)
0.5297*29
# sex
wilcox.test(days_until_event ~ gender, data=symp_infections)
0.004592*29
# bed net usage
wilcox.test(days_until_event ~ slept_under_net_regularly, data=symp_infections)
0.1798*29
# village
kruskal.test(days_until_event ~ village_name,data = symp_infections)
0.06139*29


#### -------- now make figure 3: model output and EMM by age and gender -------- ####

## ------ first run the full model

# run a multi-level coxph model with random intercepts for the participant level (not doing hh level because not using hh level covariates and didn't explain much variance, also makes interpretation better)
fit.coxph <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                   data = survival_data_secondary_permissive)
fit.coxph
# make a forest plot of the model results
table1 = exp(confint(fit.coxph,method="Wald"))
estimates = c(1.2042924,NA,1.9640060,1.2055278,NA,0.8528926,NA,0.9610377,NA,1.2527728,0.9067240)
lower_ci = c(table1[1,1],NA,table1[3,1],table1[2,1],NA,table1[4,1],NA,table1[5,1],NA,table1[6,1],table1[7,1])
upper_ci = c(table1[1,2],NA,table1[3,2],table1[2,2],NA,table1[4,2],NA,table1[5,2],NA,table1[6,2],table1[7,2])
names = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age 5-15 years","Participant age >15 years","  ","Female","   ","Regular bed net usage","    ","Maruti village","Sitabicha village"))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(2,1,1,1,1,1,1,1,1,1,1),colour=c("#000000","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9","#A9A9A9")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0,0.2,0.4,0.6,0.8,1.0,2.0,3.0,4.0,5.0)) +
  theme_bw() +
  theme(text = element_text(size=12)) 
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/secondary_permissive_forest_plot_coxph_1levels.png", device="png",
       height=4, width=9, units="in", dpi=400)


## ---- check effect measure modification by age

# model with an interaction term for age
fit.coxph.interaction <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_secondary_permissive_case_def*age_cat_baseline + (1 | unq_memID), 
                               data = survival_data_secondary_permissive)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph) # does not appear to be significant interaction

# now run stratified models
# under 5
data_under5 = survival_data_secondary_permissive %>% filter(age_cat_baseline == "<5 years")
fit.coxph.under5 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_under5)
fit.coxph.under5
# 5 to 15
data_5to15 = survival_data_secondary_permissive %>% filter(age_cat_baseline == "5-15 years")
fit.coxph.5to15 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_5to15)
fit.coxph.5to15
# over 15
data_over15 = survival_data_secondary_permissive %>% filter(age_cat_baseline == ">15 years")
fit.coxph.over15 <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_over15)
fit.coxph.over15

# make a forest plot of the results
names = c("Participant age <5 years","Asymptomatic infection","","Participant age 5-15 years"," Asymptomatic infection","   ","Participant age >15 years","  Asymptomatic infection")
estimate = c(NA,1.1359990,NA,NA,1.2258923,NA,NA,1.1913778)
lower_ci = c(NA,0.8977458,NA,NA,1.0918706,NA,NA,1.0411187)
upper_ci = c(NA,1.437482,NA,NA,1.376365,NA,NA,1.363323)
forest_plot_df = data.frame(names,estimate,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Participant age <5 years","Asymptomatic infection","","Participant age 5-15 years"," Asymptomatic infection","   ","Participant age >15 years","  Asymptomatic infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Participant age <5 years","Asymptomatic infection","","Participant age 5-15 years"," Asymptomatic infection","   ","Participant age >15 years","  Asymptomatic infection"))
# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimate, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(colour=c(NA,"#000000",NA,NA,"#000000",NA,NA,"#000000"),size=c(2,2,2,2,2,2,2,2)) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.y = element_text(face=c("plain","bold","plain","plain","bold","plain","plain","bold"))) +
  scale_y_continuous(trans="log10",breaks=c(0,0.8,1.0,1.2,1.4,1.6,1.8,2))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/secondary_permissive_age_stratified_forest_plot_coxph_1levels.png", device="png",
       height=4.5, width=6.5, units="in", dpi=400)


## ---- check effect measure modification by gender

# model with an interaction term for gender
fit.coxph.interaction <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_secondary_permissive_case_def*gender + (1 | unq_memID), 
                               data = survival_data_secondary_permissive)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph) # appears to be significant interaction

# now run stratified models
# females
data_female = survival_data_secondary_permissive %>% filter(gender=="female")
fit.coxph.female <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_female)
fit.coxph.female
# males
data_male = survival_data_secondary_permissive %>% filter(gender == "male")
fit.coxph.male <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = data_male)
fit.coxph.male

# make a forest plot of the results
names = c("Male","Asymptomatic infection","","Female"," Asymptomatic infection")
estimate = c(NA,1.116285,NA,NA,1.2609724)
lower_ci = c(NA,0.9854205,NA,NA,1.1305188)
upper_ci = c(NA,1.264528,NA,NA,1.406479)
forest_plot_df = data.frame(names,estimate,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Male","Asymptomatic infection","","Female"," Asymptomatic infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Male","Asymptomatic infection","","Female"," Asymptomatic infection"))
# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimate, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(colour=c(NA,"#000000",NA,NA,"#000000"),size=c(2,2,2,2,2)) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.y = element_text(face=c("plain","bold","plain","plain","bold","plain","plain","bold"))) +
  scale_y_continuous(trans="log10",breaks=c(0,0.9,1.0,1.1,1.2,1.3,1.4,2))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/secondary_permissive_gender_stratified_forest_plot_coxph_1levels.png", device="png",
       height=4.5, width=6.5, units="in", dpi=400)



