# -------------------------------------- #
#           Spat21/Mozzie Study          #
#          12-month hazard analysis       #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#            January 12, 2020            #
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



#### ------ now recode follow-up to be within 12 months (365 days) ------- ####

## ------- for primary data

# first pull out when each participant entered the study
unq_memID_start_date = survival_data_primary[match(unique(survival_data_primary$unq_memID), survival_data_primary$unq_memID),]

# only follow-up participants for 12 months
days_until_event_12month = rep(NA,nrow(survival_data_primary))
status_12month = rep(NA,nrow(survival_data_primary))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(survival_data_primary)){
    if (survival_data_primary$unq_memID[j] == unq_memID_start_date$unq_memID[i]){
      if (survival_data_primary$days_until_event[j] <= 365){
        days_until_event_12month[j] = survival_data_primary$days_until_event[j]
        if (survival_data_primary$status[j] == "symptomatic infection"){
          status_12month[j] = "symptomatic infection"
        } else {
          status_12month[j] = "censored"
        }
      } else {
        days_until_event_12month[j] = 365
        status_12month[j] = "censored"
      }
    }
  }
}
survival_data_primary$days_until_event_12month = days_until_event_12month
survival_data_primary$status_12month = status_12month

# test the output
symptomatic_data = survival_data_primary %>% filter(status_12month == "symptomatic infection")
summary(symptomatic_data$days_until_event_12month)
test = symptomatic_data %>% select(unq_memID,fu_end_date)
length(unique(test$unq_memID,test$fu_end_date))
summary(survival_data_primary$days_until_event_12month)
table(survival_data_primary$status_12month,useNA="always")

# update the event indicator variable
survival_data_primary$event_indicator_12month = ifelse(survival_data_primary$status_12month == "symptomatic infection",1,0)
table(survival_data_primary$event_indicator_12month,survival_data_primary$status_12month,useNA = "always")

# export the data set
# write_csv(survival_data_primary,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_12month_12JAN2020.csv")
# write_rds(survival_data_primary,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_12month_12JAN2020.rds")




#### ----------- now run a multi-level cox regression model ----------- ####

## ------ for the primary case definition

# check model assumptions
check <- coxph(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name, 
                         data = survival_data_primary)
check
cox.zph(check)
ggcoxzph(cox.zph(check))

# make a KM plot
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def, data = survival_data_primary), 
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
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier_12month.png", device="png",
       height=4, width=4, units="in", dpi=300)
# log rank test for difference in two KM survival curves
survdiff(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def, data = survival_data_primary)
sd <- survdiff(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def, data = survival_data_primary)
1 - pchisq(sd$chisq, length(sd$n) - 1)


# run a multi-level coxph model with random intercepts for the participant level (not doing hh level because not using hh level covariates and didn't explain much variance, also makes interpretation better)
# primary data set
fit.coxph.12month <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                   data = survival_data_primary)
fit.coxph.12month


# make a forest plot of the model results
table1 = exp(confint(fit.coxph.12month,method="Wald"))
estimates = c(1.1161694,NA,2.3709758,0.8786827,NA,0.6799884,NA,0.5945794,NA,1.1322884,0.7277195)
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
ggsave(fp, filename="/Users/kelseysumner/Desktop/primary_forest_plot_coxph_1levels_12month.png", device="png",
       height=3, width=6, units="in", dpi=400)



# now test for EMM by age
# model with an interaction term for age
fit.coxph.interaction <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*age_cat_baseline + (1 | unq_memID), 
                               data = survival_data_primary)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.12month) # does not appear to be significant interaction

# now run stratified models
# under 5
data_under5 = survival_data_primary %>% filter(age_cat_baseline == "<5 years")
fit.coxph.under5 <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_under5)
fit.coxph.under5
# 5 to 15
data_5to15 = survival_data_primary %>% filter(age_cat_baseline == "5-15 years")
fit.coxph.5to15 <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_5to15)
fit.coxph.5to15
# over 15
data_over15 = survival_data_primary %>% filter(age_cat_baseline == ">15 years")
fit.coxph.over15 <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_over15)
fit.coxph.over15
# model wouldn't converge


# now test for EMM by gender
# model with an interaction term for gender
fit.coxph.interaction <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*gender + (1 | unq_memID), 
                               data = survival_data_primary)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.12month) # does appear to be significant interaction

# now run stratified models
# females
data_female = survival_data_primary %>% filter(gender=="female")
fit.coxph.female <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_female)
fit.coxph.female
# males
data_male = survival_data_primary %>% filter(gender == "male")
fit.coxph.male <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                        data = data_male)
fit.coxph.male




#### ------ now do a misclassification analysis --------- ####

# remove monthly visits up to 14 days before the symptomatic infection
# primary data set
remove = rep(NA,nrow(survival_data_primary))
for (i in 1:nrow(survival_data_primary)){
  if (survival_data_primary$days_until_event_12month[i] < 15 & survival_data_primary$event_indicator_12month[i] == 1){
    remove[i] = "yes"
  }
}
table(remove)
length(which(survival_data_primary$days_until_event_12month < 15 & survival_data_primary$status == "symptomatic infection"))
survival_data_primary$remove = remove
survival_data_primary = survival_data_primary %>% filter(is.na(remove))
survival_data_primary$remove <- NULL



# run a model using the primary case definition
summary(survival_data_primary$days_until_event_12month)
fit.coxph.primary <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                           data = survival_data_primary)
fit.coxph.primary
exp(confint(fit.coxph.primary))



## ------ now explore EMM by age for the primary case definition

# compare model with interaction term
fit.coxph.interaction <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*age_cat_baseline + (1 | unq_memID), 
                          data = survival_data_primary)
fit.coxph.interaction
anova(fit.coxph.interaction,fit.coxph.primary)

# under 5
data_under5 = survival_data_primary %>% filter(age_cat_baseline == "<5 years")
fit.coxph.under5 <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_under5)
fit.coxph.under5
# 5 to 15
data_5to15 = survival_data_primary %>% filter(age_cat_baseline == "5-15 years")
fit.coxph.5to15 <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                         data = data_5to15)
fit.coxph.5to15
# over 15
data_over15 = survival_data_primary %>% filter(age_cat_baseline == ">15 years")
fit.coxph.over15 <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_over15)
fit.coxph.over15


## ----- no explore EMM by sex

# primary data set
# model with an interaction term for gender
fit.coxph.interaction <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + main_exposure_primary_case_def*gender + (1 | unq_memID), 
                               data = survival_data_primary)
fit.coxph.interaction
# now run an anova to compare models
anova(fit.coxph.interaction,fit.coxph.primary) # does not appear to be significant interaction

# now run stratified models
# females
data_female = survival_data_primary %>% filter(gender=="female")
fit.coxph.female <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
                          data = data_female)
fit.coxph.female
# males
data_male = survival_data_primary %>% filter(gender == "male")
fit.coxph.male <- coxme(Surv(days_until_event_12month, event_indicator_12month) ~ main_exposure_primary_case_def + age_cat_baseline + slept_under_net_regularly + village_name + (1 | unq_memID), 
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




