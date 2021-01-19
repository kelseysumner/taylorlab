# -------------------------------------- #
#           Spat21/Mozzie Study          #
#    Combined figure 3 forest plot       #
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



#### --------- make one big forest plot of model results across case definitions -------- ####

# run a multi-level coxph model with random intercepts for the participant level (not doing hh level because not using hh level covariates and didn't explain much variance, also makes interpretation better)
# primary data set
fit.coxph <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                   data = survival_data_primary)
fit.coxph
exp(confint(fit.coxph))

# make a forest plot of the model results
estimates = c(1.11,NA,1.38,1.16,0.96,NA,1.08,1.14,1.02,NA,1.38,1.10,0.73,1.20,NA,1.14,1.23,1.19,NA,1.12,1.26)
lower_ci = c(1.01,NA,1.05,1.02,0.81,NA,0.94,1.01,0.92,NA,1.05,0.96,0.59,1.11,NA,0.90,1.09,1.04,NA,0.99,1.13)
upper_ci = c(1.22,NA,1.81,1.32,1.13,NA,1.24,1.30,1.13,NA,1.81,1.25,0.90,1.31,NA,1.44,1.38,1.36,NA,1.26,1.41)
type = c("Primary case definition","Primary case definition","Primary case definition","Primary case definition","Primary case definition","Primary case definition","Primary case definition","Primary case definition","Secondary stringent case definition","Secondary stringent case definition","Secondary stringent case definition","Secondary stringent case definition","Secondary stringent case definition","Secondary permissive case definition","Secondary permissive case definition","Secondary permissive case definition","Secondary permissive case definition","Secondary permissive case definition","Secondary permissive case definition","Secondary permissive case definition","Secondary permissive case definition")
names = c("Main model","Age-stratified model","<5 years","5-15 years",">15 years","Sex-stratified model","Male","Female","Main model ","Age-stratified model ","<5 years ","5-15 years ",">15 years ","Main model  ","Age-stratified model  ","<5 years  ","5-15 years  ",">15 years  ","Sex-stratified model  ","Male  ","Female  ")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci,type)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Main model","Age-stratified model","<5 years","5-15 years",">15 years","Sex-stratified model","Male","Female","Main model ","Age-stratified model ","<5 years ","5-15 years ",">15 years ","Main model  ","Age-stratified model  ","<5 years  ","5-15 years  ",">15 years  ","Sex-stratified model  ","Male  ","Female  "))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Main model","Age-stratified model","<5 years","5-15 years",">15 years","Sex-stratified model","Male","Female","Main model ","Age-stratified model ","<5 years ","5-15 years ",">15 years ","Main model  ","Age-stratified model  ","<5 years  ","5-15 years  ",">15 years  ","Sex-stratified model  ","Male  ","Female  "))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)) +
  theme_bw() +
  facet_wrap(~type,ncol=1,strip.position = "right") +
  theme(text = element_text(size=10),axis.text.y = element_text(face=c("plain","plain","bold","plain","plain","plain","bold","bold","bold","plain","plain","plain","bold","bold","bold","plain","plain","bold","plain","plain","plain","bold","bold","bold"))) 
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/figure3_combo_forest_plot_coxph_1levels.png", device="png",
       height=6, width=7, units="in", dpi=400)




# make a forest plot of the model results - additional way
estimates = c(1.11,NA,1.38,1.16,0.96,NA,1.08,1.14,1.02,NA,1.38,1.10,0.73,1.20,NA,1.14,1.23,1.19,NA,1.12,1.26)
lower_ci = c(1.01,NA,1.05,1.02,0.81,NA,0.94,1.01,0.92,NA,1.05,0.96,0.59,1.11,NA,0.90,1.09,1.04,NA,0.99,1.13)
upper_ci = c(1.22,NA,1.81,1.32,1.13,NA,1.24,1.30,1.13,NA,1.81,1.25,0.90,1.31,NA,1.44,1.38,1.36,NA,1.26,1.41)
type = c("Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (primary)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary stringent)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)","Symptomatic malaria (secondary permissive)")
names = c("Main model","Age-stratified model","<5 years","5-15 years",">15 years","Sex-stratified model","Male","Female","Main model","Age-stratified model","<5 years","5-15 years",">15 years","Main model","Age-stratified model","<5 years","5-15 years",">15 years","Sex-stratified model","Male","Female")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci,type)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Main model","Age-stratified model","<5 years","5-15 years",">15 years","Sex-stratified model","Male","Female"))
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)) +
  theme_bw() +
  facet_wrap(~type,ncol=1,strip.position = "top",scales = "free_y")
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/figure3_combo_forest_plot_coxph_1levels.png", device="png",
       height=7.5, width=7, units="in", dpi=400)





#### ------- now make a figure that is all follow-up time analyses together -------- ####

# make a forest plot of the model results 
# make a forest plot of the model results
estimates = c(2.61,1.77,2.54)
lower_ci = c(2.05,1.26,1.76)
upper_ci = c(3.33,2.47,3.67)
names = c("Main model","Pre-symptomatic analysis","Post-treatment analysis")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names,levels = c("Main model","Pre-symptomatic analysis","Post-treatment analysis"))
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=1.25) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Adusted hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0,1.0,1.5,2.0,2.5,3.0,3.5,4.0)) +
  theme_bw() +
  theme(text = element_text(size=10.5))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/figure3a_hazardsympmalaria_1monthanalysis.png", device="png",
       height=2.5, width=6, units="in", dpi=400)


# make a forest plot of the model results across all months
estimates = c(2.61,1.64,1.38,1.12,1.11)
lower_ci = c(2.05,1.40,1.20,1.00,1.01)
upper_ci = c(3.33,1.94,1.58,1.25,1.22)
names = c("1-month","3-month","6-month","12-month","29-month")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names,levels = c("1-month","3-month","6-month","12-month","29-month"))
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=1.25) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Adjusted hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0,2.2,2.4,2.6,2.8,3.0,3.2,3.4)) +
  theme_bw() +
  theme(text = element_text(size=12.5))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/figure3b_hazardsympmalaria_across_monthfollowup.png", device="png",
       height=4, width=7, units="in", dpi=400)





