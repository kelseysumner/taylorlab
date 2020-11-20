# -------------------------------------- #
#           Spat21/Mozzie Study          #
#       General follow-up descriptives   #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#           November 19, 2020            #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)
library(survminer)
library(survival)


#### ------- read in the data sets -------- ####

# read in the primary data set
survival_data_primary = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_primary_survival_format_19NOV2020.rds")

# read in the secondary stringent data set
survival_data_secondary_stringent = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_stringent_survival_format_19NOV2020.rds")

# read in the secondary permissive data set
survival_data_secondary_permissive = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/survival_data_sets/Final data sets/survival format/survival_data_secondary_permissive_survival_format_19NOV2020.rds")



#### ------ look at basic descriptives of follow-up time ---------- ####

## ------ for survival data primary

# first just look at general summary of days until event
survival_data_primary$days_until_event = as.numeric(survival_data_primary$days_until_event)
summary(survival_data_primary$days_until_event)

# look at the distribution of follow-up times across main exposure
follow_up_plot = ggplot(data=survival_data_primary,aes(x=days_until_event,fill=main_exposure_primary_case_def)) +
  geom_density(alpha=0.5) +
  theme_bw() +
  xlab("Follow up in days") +
  ylab("Density") +
  labs(fill="Main exposure") +
  theme(legend.position = c(0.7, 0.8),legend.box.background = element_rect(colour = "black"))
follow_up_plot
ggsave(follow_up_plot, filename="/Users/kelseysumner/Desktop/primary_exposure_distribution.png", device="png",
 height=4, width=5, units="in", dpi=500)

# look at the distribution of follow-up times across censored and events
follow_up_plot = ggplot(data=survival_data_primary,aes(x=days_until_event,fill=factor(event_indicator))) +
  geom_density(alpha=0.5) +
  theme_bw() +
  xlab("Follow up in days") +
  ylab("Density") +
  labs(fill="Event indicator") +
  scale_fill_manual(values = c("#4daf4a","#ff7f00")) +
  theme(legend.position = c(0.7, 0.8),legend.box.background = element_rect(colour = "black"))
follow_up_plot
ggsave(follow_up_plot, filename="/Users/kelseysumner/Desktop/primary_event_distribution.png", device="png",
       height=4, width=5, units="in", dpi=500)


## ------ for survival secondary stringent

# first just look at general summary of days until event
survival_data_secondary_stringent$days_until_event = as.numeric(survival_data_secondary_stringent$days_until_event)
summary(survival_data_secondary_stringent$days_until_event)

# look at the distribution of follow-up times
follow_up_plot = ggplot(data=survival_data_secondary_stringent,aes(x=days_until_event,fill=main_exposure_secondary_stringent_case_def)) +
  geom_density(alpha=0.5) +
  theme_bw() +
  xlab("Follow up in days") +
  ylab("Density") +
  labs(fill="Main exposure") +
  theme(legend.position = c(0.7, 0.8),legend.box.background = element_rect(colour = "black"))
follow_up_plot
ggsave(follow_up_plot, filename="/Users/kelseysumner/Desktop/secondary_stringent_exposure_distribution.png", device="png",
       height=4, width=5, units="in", dpi=500)

# look at the distribution of follow-up times across censored and events
follow_up_plot = ggplot(data=survival_data_secondary_stringent,aes(x=days_until_event,fill=factor(event_indicator))) +
  geom_density(alpha=0.5) +
  theme_bw() +
  xlab("Follow up in days") +
  ylab("Density") +
  labs(fill="Event indicator") +
  scale_fill_manual(values = c("#4daf4a","#ff7f00")) +
  theme(legend.position = c(0.7, 0.8),legend.box.background = element_rect(colour = "black"))
follow_up_plot
ggsave(follow_up_plot, filename="/Users/kelseysumner/Desktop/secondary_stringent_event_distribution.png", device="png",
       height=4, width=5, units="in", dpi=500)


## ------ for survival secondary permissive

# first just look at general summary of days until event
survival_data_secondary_permissive$days_until_event = as.numeric(survival_data_secondary_permissive$days_until_event)
summary(survival_data_secondary_permissive$days_until_event)

# look at the distribution of follow-up times
follow_up_plot = ggplot(data=survival_data_secondary_permissive,aes(x=days_until_event,fill=main_exposure_secondary_permissive_case_def)) +
  geom_density(alpha=0.5) +
  theme_bw() +
  xlab("Follow up in days") +
  ylab("Density") +
  labs(fill="Main exposure") +
  theme(legend.position = c(0.7, 0.8),legend.box.background = element_rect(colour = "black"))
follow_up_plot
ggsave(follow_up_plot, filename="/Users/kelseysumner/Desktop/secondary_permissive_exposure_distribution.png", device="png",
       height=4, width=5, units="in", dpi=500)

# look at the distribution of follow-up times across censored and events
follow_up_plot = ggplot(data=survival_data_secondary_permissive,aes(x=days_until_event,fill=factor(event_indicator))) +
  geom_density(alpha=0.5) +
  theme_bw() +
  xlab("Follow up in days") +
  ylab("Density") +
  labs(fill="Event indicator") +
  scale_fill_manual(values = c("#4daf4a","#ff7f00")) +
  theme(legend.position = c(0.7, 0.8),legend.box.background = element_rect(colour = "black"))
follow_up_plot
ggsave(follow_up_plot, filename="/Users/kelseysumner/Desktop/secondary_permissive_event_distribution.png", device="png",
       height=4, width=5, units="in", dpi=500)


#### ----- plot survival lines for each entry and participant ------ ####

# also try to make a heat map of follow-up for people









#### ---- look at some general KM survival curves ----- ####

## --- for primary data
# KM curve not stratified
ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ 1, data = survival_data_primary), 
           xlab = "Days", 
           ylab = "Overall survival probability",
           surv.median.line = "hv")
# KM curve not stratified
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary), 
           xlab = "Days", 
           ylab = "Overall survival probability",
           surv.median.line = "hv",
           risk.table = T,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           conf.int = T,
           legend.title = "Infection status",
           legend.labs = c("No infection", "Asymptomatic infection"),
           pval = T,
           ggtheme = theme_bw())
km_plot
png("Desktop/primary_kaplan_meier.png",dpi=500)
print(km_plot, newpage = FALSE)
dev.off()
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier.png", device="png",
       height=4, width=6, units="in", dpi=500)
# log rank test for difference in two KM survival curves
survdiff(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary)
sd <- survdiff(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary)
1 - pchisq(sd$chisq, length(sd$n) - 1)
# also look at the cumulative number of events
km_plot$cumevents


## --- for secondary stringent data
# KM curve not stratified
ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ 1, data = survival_data_secondary_stringent), 
           xlab = "Days", 
           ylab = "Overall survival probability",
           surv.median.line = "hv")
# KM curve not stratified
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_stringent_case_def, data = survival_data_secondary_stringent), 
                     xlab = "Days", 
                     ylab = "Overall survival probability",
                     surv.median.line = "hv",
                     risk.table = T,
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend.title = "Infection status",
                     legend.labs = c("No infection", "Asymptomatic infection"),
                     pval = T,
                     ggtheme = theme_bw())
km_plot
png("Desktop/secondary_stringent_kaplan_meier.png")
print(km_plot, newpage = FALSE)
dev.off()


## --- for secondary permissive data
# KM curve not stratified
ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ 1, data = survival_data_secondary_permissive), 
           xlab = "Days", 
           ylab = "Overall survival probability",
           surv.median.line = "hv")
# KM curve not stratified
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def, data = survival_data_secondary_permissive), 
                     xlab = "Days", 
                     ylab = "Overall survival probability",
                     surv.median.line = "hv",
                     risk.table = T,
                     tables.height = 0.2,
                     tables.theme = theme_cleantable(),
                     conf.int = T,
                     legend.title = "Infection status",
                     legend.labs = c("No infection", "Asymptomatic infection"),
                     pval = T,
                     ggtheme = theme_bw())
km_plot
png("Desktop/secondary_permissive_kaplan_meier.png")
print(km_plot, newpage = FALSE)
dev.off()
