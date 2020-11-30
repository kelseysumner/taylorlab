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


#### ------ make a plot of censoring by type across follow-up time ------ ####

## ----- primary data
follow_up_plot = ggplot(data=survival_data_primary,aes(x=days_until_event,fill=factor(status))) +
  geom_density(alpha=0.5) +
  theme_bw() +
  xlab("Follow up in days") +
  ylab("Density") +
  labs(fill="End follow up status") +
  theme(legend.position = c(0.7, 0.8),legend.box.background = element_rect(colour = "black"))
follow_up_plot
ggsave(follow_up_plot, filename="/Users/kelseysumner/Desktop/primary_end_followup_status_distribution.png", device="png",
       height=4, width=5, units="in", dpi=500)


## ----- secondary stringent data
follow_up_plot = ggplot(data=survival_data_secondary_stringent,aes(x=days_until_event,fill=factor(status))) +
  geom_density(alpha=0.5) +
  theme_bw() +
  xlab("Follow up in days") +
  ylab("Density") +
  labs(fill="End follow up status") +
  theme(legend.position = c(0.7, 0.8),legend.box.background = element_rect(colour = "black"))
follow_up_plot
ggsave(follow_up_plot, filename="/Users/kelseysumner/Desktop/secondary_stringent_end_followup_status_distribution.png", device="png",
       height=4, width=5, units="in", dpi=500)


## ----- secondary permissive data
follow_up_plot = ggplot(data=survival_data_secondary_permissive,aes(x=days_until_event,fill=factor(status))) +
  geom_density(alpha=0.5) +
  theme_bw() +
  xlab("Follow up in days") +
  ylab("Density") +
  labs(fill="End follow up status") +
  theme(legend.position = c(0.7, 0.8),legend.box.background = element_rect(colour = "black"))
follow_up_plot
ggsave(follow_up_plot, filename="/Users/kelseysumner/Desktop/secondary_permissive_end_followup_status_distribution.png", device="png",
       height=4, width=5, units="in", dpi=500)


#### ------ make some plots of general follow up curves ------- ####

## ----- primary data
# Use ggplot to plot data from dat object
ggplot(survival_data_primary, aes(x = unq_memID)) + 
  # Plot solid line representing non-interval censored time from 0 to t1
  geom_linerange(aes(ymin = 0, ymax = t1)) + 
  # Plot line (dotted for censored time) representing time from t1 to t2
  geom_linerange(aes(ymin = t1, ymax = t2, linetype = as.factor(censored))) +  
  # Plot points representing event
  # The ifelse() function moves censored marker to middle of interval
  geom_point(aes(y = ifelse(censored, t1 + (t2 - t1) / 2, t2), shape = event), 
             size = 4) +
  # Flip coordinates
  coord_flip() + 
  # Add custom name to linetype scale, 
  # otherwise it will default to "as.factor(censored))"
  scale_linetype_manual(name = "Censoring", values = c(1, 2), 
                        labels = c("Not censored", "Interval censored")) +
  # Add custom shape scale.  Change the values to get different shapes.
  scale_shape_manual(name = "Event", values = c(19, 15)) +
  # Add main title and axis labels
  opts(title = "Patient follow-up") + xlab("Patient ID") +  ylab("Days") + 
  # I think the bw theme looks better for this graph, 
  # but leave it out if you prefer the default theme
  theme_bw()





#### ----- make alluvial plots ------- ####

## ------ primary data

##  make a plot of how malaria exposure infection status changes over time (from having an asymptomatic infection to having no infection during that month)

# cut down the data set to just the variables of interest
plot_human_data = survival_data_primary %>%
  select(main_exposure_primary_case_def,month_year,unq_memID) %>%
  group_by(month_year,main_exposure_primary_case_def,unq_memID) %>%
  summarize(n=n())
plot_human_data_withperc = plot_human_data %>%
  group_by(month_year) %>%
  mutate(perc_n=n/sum(n))

# reorder so asymptomatic infections are on the bottom
plot_human_data_withperc$main_exposure_primary_case_def = relevel(as.factor(plot_human_data_withperc$main_exposure_primary_case_def),"no infection")

# now make an alluvial plot of how infection status changes over time
alluvial_plot = ggplot(plot_human_data_withperc,
                      aes(x = month_year, stratum = main_exposure_primary_case_def, alluvium = unq_memID,
                          y = perc_n,fill = main_exposure_primary_case_def, label = main_exposure_primary_case_def)) +
  geom_flow(na.rm=T,alpha=0.25) +
  geom_stratum(width = 5) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants") +
  labs(fill="Main exposure") +
  scale_x_date(limits = as.Date(c("2017-05-01","2019-12-01")),breaks = "3 months")
alluvial_plot
ggsave(alluvial_plot, filename="/Users/kelseysumner/Desktop/primary_alluvial_exposure.png", device="png",
       height=5.25, width=11, units="in", dpi=500)


##  make a plot of how event status changes over time (from having a symptomatic infection to being censored during that month)

# cut down the data set to just the variables of interest
plot_human_data = survival_data_primary %>%
  select(event_indicator,month_year,unq_memID) %>%
  group_by(month_year,event_indicator,unq_memID) %>%
  summarize(n=n())
plot_human_data_withperc = plot_human_data %>%
  group_by(month_year) %>%
  mutate(perc_n=n/sum(n))

# now make an alluvial plot of how infection status changes over time
alluvial_plot = ggplot(plot_human_data_withperc,
                       aes(x = month_year, stratum = factor(event_indicator), alluvium = unq_memID,
                           y = perc_n,fill = factor(event_indicator), label = factor(event_indicator))) +
  geom_flow(na.rm=T,alpha=0.25) +
  geom_stratum(width = 5) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants") +
  labs(fill="Event indicator") +
  scale_fill_manual(values = c("#4daf4a","#ff7f00")) +
  scale_x_date(limits = as.Date(c("2017-05-01","2019-12-01")),breaks = "3 months")
alluvial_plot
ggsave(alluvial_plot, filename="/Users/kelseysumner/Desktop/primary_alluvial_event.png", device="png",
       height=5.25, width=11, units="in", dpi=500)


## ------ secondary_stringent data

##  make a plot of how malaria exposure infection status changes over time (from having an asymptomatic infection to having no infection during that month)

# cut down the data set to just the variables of interest
plot_human_data = survival_data_secondary_stringent %>%
  select(main_exposure_secondary_stringent_case_def,month_year,unq_memID) %>%
  group_by(month_year,main_exposure_secondary_stringent_case_def,unq_memID) %>%
  summarize(n=n())
plot_human_data_withperc = plot_human_data %>%
  group_by(month_year) %>%
  mutate(perc_n=n/sum(n))

# reorder so asymptomatic infections are on the bottom
plot_human_data_withperc$main_exposure_secondary_stringent_case_def = relevel(as.factor(plot_human_data_withperc$main_exposure_secondary_stringent_case_def),"no infection")

# now make an alluvial plot of how infection status changes over time
alluvial_plot = ggplot(plot_human_data_withperc,
                       aes(x = month_year, stratum = main_exposure_secondary_stringent_case_def, alluvium = unq_memID,
                           y = perc_n,fill = main_exposure_secondary_stringent_case_def, label = main_exposure_secondary_stringent_case_def)) +
  geom_flow(na.rm=T,alpha=0.25) +
  geom_stratum(width = 5) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants") +
  labs(fill="Main exposure") +
  scale_x_date(limits = as.Date(c("2017-05-01","2019-12-01")),breaks = "3 months")
alluvial_plot
ggsave(alluvial_plot, filename="/Users/kelseysumner/Desktop/secondary_stringent_alluvial_exposure.png", device="png",
       height=5.25, width=11, units="in", dpi=500)


##  make a plot of how event status changes over time (from having a symptomatic infection to being censored during that month)

# cut down the data set to just the variables of interest
plot_human_data = survival_data_secondary_stringent %>%
  select(event_indicator,month_year,unq_memID) %>%
  group_by(month_year,event_indicator,unq_memID) %>%
  summarize(n=n())
plot_human_data_withperc = plot_human_data %>%
  group_by(month_year) %>%
  mutate(perc_n=n/sum(n))

# now make an alluvial plot of how infection status changes over time
alluvial_plot = ggplot(plot_human_data_withperc,
                       aes(x = month_year, stratum = factor(event_indicator), alluvium = unq_memID,
                           y = perc_n,fill = factor(event_indicator), label = factor(event_indicator))) +
  geom_flow(na.rm=T,alpha=0.25) +
  geom_stratum(width = 5) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants") +
  labs(fill="Event indicator") +
  scale_fill_manual(values = c("#4daf4a","#ff7f00")) +
  scale_x_date(limits = as.Date(c("2017-05-01","2019-12-01")),breaks = "3 months")
alluvial_plot
ggsave(alluvial_plot, filename="/Users/kelseysumner/Desktop/secondary_stringent_alluvial_event.png", device="png",
       height=5.25, width=11, units="in", dpi=500)


## ------ secondary_permissive data

##  make a plot of how malaria exposure infection status changes over time (from having an asymptomatic infection to having no infection during that month)

# cut down the data set to just the variables of interest
plot_human_data = survival_data_secondary_permissive %>%
  select(main_exposure_secondary_permissive_case_def,month_year,unq_memID) %>%
  group_by(month_year,main_exposure_secondary_permissive_case_def,unq_memID) %>%
  summarize(n=n())
plot_human_data_withperc = plot_human_data %>%
  group_by(month_year) %>%
  mutate(perc_n=n/sum(n))

# reorder so asymptomatic infections are on the bottom
plot_human_data_withperc$main_exposure_secondary_permissive_case_def = relevel(as.factor(plot_human_data_withperc$main_exposure_secondary_permissive_case_def),"no infection")

# now make an alluvial plot of how infection status changes over time
alluvial_plot = ggplot(plot_human_data_withperc,
                       aes(x = month_year, stratum = main_exposure_secondary_permissive_case_def, alluvium = unq_memID,
                           y = perc_n,fill = main_exposure_secondary_permissive_case_def, label = main_exposure_secondary_permissive_case_def)) +
  geom_flow(na.rm=T,alpha=0.25) +
  geom_stratum(width = 5) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants") +
  labs(fill="Main exposure") +
  scale_x_date(limits = as.Date(c("2017-05-01","2019-12-01")),breaks = "3 months")
alluvial_plot
ggsave(alluvial_plot, filename="/Users/kelseysumner/Desktop/secondary_permissive_alluvial_exposure.png", device="png",
       height=5.25, width=11, units="in", dpi=500)


##  make a plot of how event status changes over time (from having a symptomatic infection to being censored during that month)

# cut down the data set to just the variables of interest
plot_human_data = survival_data_secondary_permissive %>%
  select(event_indicator,month_year,unq_memID) %>%
  group_by(month_year,event_indicator,unq_memID) %>%
  summarize(n=n())
plot_human_data_withperc = plot_human_data %>%
  group_by(month_year) %>%
  mutate(perc_n=n/sum(n))

# now make an alluvial plot of how infection status changes over time
alluvial_plot = ggplot(plot_human_data_withperc,
                       aes(x = month_year, stratum = factor(event_indicator), alluvium = unq_memID,
                           y = perc_n,fill = factor(event_indicator), label = factor(event_indicator))) +
  geom_flow(na.rm=T,alpha=0.25) +
  geom_stratum(width = 5) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants") +
  labs(fill="Event indicator") +
  scale_fill_manual(values = c("#4daf4a","#ff7f00")) +
  scale_x_date(limits = as.Date(c("2017-05-01","2019-12-01")),breaks = "3 months")
alluvial_plot
ggsave(alluvial_plot, filename="/Users/kelseysumner/Desktop/secondary_permissive_alluvial_event.png", device="png",
       height=5.25, width=11, units="in", dpi=500)



##### -------- make some at risk and survival curves ------- ####

## ------ for primary data
#Fit a survival model by exposure
s2 <- survival::survfit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary, type="kaplan-meier")
s_by_exp <- summary(s2)
s_by_exp

# create a data frame of the survival times
#Create a data frame
s_df <- data.frame(
  time = s_by_exp$time,
  exposure = factor(as.numeric(s_by_exp$strata), 
                    levels = c(1,2), 
                    labels=c("No infection", "Asymptomatic infection")),
  n_at_risk = s_by_exp$n.risk,
  n_symptomatic = s_by_exp$n.event,
  survival = s_by_exp$surv,
  risk = 1 - s_by_exp$surv,
  lower_ci_risk = 1 - s_by_exp$upper,
  upper_ci_risk = 1 - s_by_exp$lower)
s_df

#Plot cumulative risk by exposure group
risk_plot = ggplot(data=s_df, aes(x=time, y=risk, group=exposure, fill=exposure)) +
  geom_ribbon(aes(ymin=lower_ci_risk, ymax=upper_ci_risk), alpha=0.5) +
  geom_point(alpha=0.5,size=0.5) +
  geom_line() + 
  theme_bw() + 
  xlab("Risk period in days") + 
  ylab("Cumulative risk of symptomatic infection (%)") +
  theme(legend.title = element_blank())
risk_plot
ggsave(risk_plot, filename="/Users/kelseysumner/Desktop/primary_risk_plot_event.png", device="png",
       height=4, width=7, units="in", dpi=500)



## ------ for secondary stringent data
#Fit a survival model by exposure
s2 <- survival::survfit(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_stringent_case_def, data = survival_data_secondary_stringent, type="kaplan-meier")
s_by_exp <- summary(s2)
s_by_exp

# create a data frame of the survival times
#Create a data frame
s_df <- data.frame(
  time = s_by_exp$time,
  exposure = factor(as.numeric(s_by_exp$strata), 
                    levels = c(1,2), 
                    labels=c("No infection", "Asymptomatic infection")),
  n_at_risk = s_by_exp$n.risk,
  n_symptomatic = s_by_exp$n.event,
  survival = s_by_exp$surv,
  risk = 1 - s_by_exp$surv,
  lower_ci_risk = 1 - s_by_exp$upper,
  upper_ci_risk = 1 - s_by_exp$lower)
s_df

#Plot cumulative risk by exposure group
risk_plot = ggplot(data=s_df, aes(x=time, y=risk, group=exposure, fill=exposure)) +
  geom_ribbon(aes(ymin=lower_ci_risk, ymax=upper_ci_risk), alpha=0.5) +
  geom_point(alpha=0.5,size=0.5) +
  geom_line() + 
  theme_bw() + 
  xlab("Risk period in days") + 
  ylab("Cumulative risk of symptomatic infection (%)") +
  theme(legend.title = element_blank())
risk_plot
ggsave(risk_plot, filename="/Users/kelseysumner/Desktop/secondary_stringent_risk_plot_event.png", device="png",
       height=4, width=7, units="in", dpi=500)


## ------ for secondary_permissive data
#Fit a survival model by exposure
s2 <- survival::survfit(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def, data = survival_data_secondary_permissive, type="kaplan-meier")
s_by_exp <- summary(s2)
s_by_exp

# create a data frame of the survival times
#Create a data frame
s_df <- data.frame(
  time = s_by_exp$time,
  exposure = factor(as.numeric(s_by_exp$strata), 
                    levels = c(1,2), 
                    labels=c("No infection", "Asymptomatic infection")),
  n_at_risk = s_by_exp$n.risk,
  n_symptomatic = s_by_exp$n.event,
  survival = s_by_exp$surv,
  risk = 1 - s_by_exp$surv,
  lower_ci_risk = 1 - s_by_exp$upper,
  upper_ci_risk = 1 - s_by_exp$lower)
s_df

#Plot cumulative risk by exposure group
risk_plot = ggplot(data=s_df, aes(x=time, y=risk, group=exposure, fill=exposure)) +
  geom_ribbon(aes(ymin=lower_ci_risk, ymax=upper_ci_risk), alpha=0.5) +
  geom_point(alpha=0.5,size=0.5) +
  geom_line() + 
  theme_bw() + 
  xlab("Risk period in days") + 
  ylab("Cumulative risk of symptomatic infection (%)") +
  theme(legend.title = element_blank())
risk_plot
ggsave(risk_plot, filename="/Users/kelseysumner/Desktop/secondary_permissive_risk_plot_event.png", device="png",
       height=4, width=7, units="in", dpi=500)


#### ------ make a dot plot by main exposure ------- ####

## ---- for the primary exposure
# first a dot plot for the primary exposure
dot_plot = ggplot(data=survival_data_primary, aes(x=main_exposure_primary_case_def, y=days_until_event, group=main_exposure_primary_case_def, color=factor(event_indicator))) +
  geom_beeswarm(aes(y=days_until_event),pch=21) +
  geom_line() + 
  theme_bw() + 
  ylab("Days until event") + 
  xlab("Main exposure") +
  scale_color_manual(values=c("#7fbf7b","#D3D3D3")) +
  coord_flip() 
dot_plot
ggsave(dot_plot, filename="/Users/kelseysumner/Desktop/primary_dot_plot_exposure.png", device="png",
       height=10, width=15, units="in", dpi=500)


# violin plot for the village covariate by exposure
violin_plot = ggplot(data=survival_data_primary, aes(x=village_name, y=days_until_event, group=village_name, color=main_exposure_primary_case_def)) +
  geom_violin() +
  geom_beeswarm(aes(color=main_exposure_primary_case_def),alpha = 0.5) + 
  geom_line() + 
  theme_bw() + 
  ylab("Days until event") + 
  xlab("Village") +
  coord_flip() 
violin_plot
ggsave(violin_plot, filename="/Users/kelseysumner/Desktop/primary_violin_plot_village_exposure.png", device="png",
       height=10, width=15, units="in", dpi=500)


# violin plot for gender by exposure
violin_plot = ggplot(data=survival_data_primary, aes(x=gender, y=days_until_event, group=gender, color=main_exposure_primary_case_def)) +
  geom_violin() +
  geom_beeswarm(aes(color=main_exposure_primary_case_def),alpha = 0.5) + 
  geom_line() + 
  theme_bw() + 
  ylab("Days until event") + 
  xlab("Gender") +
  coord_flip() 
violin_plot
ggsave(violin_plot, filename="/Users/kelseysumner/Desktop/primary_violin_plot_gender_exposure.png", device="png",
       height=10, width=15, units="in", dpi=500)


# violin plot for age by exposure
violin_plot = ggplot(data=survival_data_primary, aes(x=age_cat_baseline, y=days_until_event, group=age_cat_baseline, color=main_exposure_primary_case_def)) +
  geom_violin() +
  geom_beeswarm(aes(color=main_exposure_primary_case_def),alpha = 0.5) + 
  geom_line() + 
  theme_bw() + 
  ylab("Days until event") + 
  xlab("Age category") +
  coord_flip() 
violin_plot
ggsave(violin_plot, filename="/Users/kelseysumner/Desktop/primary_violin_plot_exposure_age.png", device="png",
       height=10, width=15, units="in", dpi=500)


# violin plot for slept under bed net regularly by exposure
violin_plot = ggplot(data=survival_data_primary, aes(x=slept_under_net_regularly, y=days_until_event, group=slept_under_net_regularly, color=main_exposure_primary_case_def)) +
  geom_violin() +
  geom_beeswarm(aes(color=main_exposure_primary_case_def),alpha = 0.5) + 
  geom_line() + 
  theme_bw() + 
  ylab("Days until event") + 
  xlab("Slept under net regularly") +
  coord_flip() 
violin_plot
ggsave(violin_plot, filename="/Users/kelseysumner/Desktop/primary_violin_plot_slept_under_net_regularly_exposure.png", device="png",
       height=10, width=15, units="in", dpi=500)


# violin plot for the village covariate
violin_plot = ggplot(data=survival_data_primary, aes(x=village_name, y=days_until_event, group=village_name, color=factor(event_indicator))) +
  geom_violin() +
  geom_beeswarm(aes(color=factor(event_indicator)),alpha = 0.5) + 
  geom_line() + 
  theme_bw() + 
  ylab("Days until event") + 
  xlab("Village") +
  coord_flip() 
violin_plot
ggsave(violin_plot, filename="/Users/kelseysumner/Desktop/primary_violin_plot_village.png", device="png",
       height=10, width=15, units="in", dpi=500)


# violin plot for gender
violin_plot = ggplot(data=survival_data_primary, aes(x=gender, y=days_until_event, group=gender, color=factor(event_indicator))) +
  geom_violin() +
  geom_beeswarm(aes(color=factor(event_indicator)),alpha = 0.5) + 
  geom_line() + 
  theme_bw() + 
  ylab("Days until event") + 
  xlab("Gender") +
  coord_flip() 
violin_plot
ggsave(violin_plot, filename="/Users/kelseysumner/Desktop/primary_violin_plot_gender.png", device="png",
       height=10, width=15, units="in", dpi=500)


# violin plot for age
violin_plot = ggplot(data=survival_data_primary, aes(x=age_cat_baseline, y=days_until_event, group=age_cat_baseline, color=factor(event_indicator))) +
  geom_violin() +
  geom_beeswarm(aes(color=factor(event_indicator)),alpha = 0.5) + 
  geom_line() + 
  theme_bw() + 
  ylab("Days until event") + 
  xlab("Age category") +
  coord_flip() 
violin_plot
ggsave(violin_plot, filename="/Users/kelseysumner/Desktop/primary_violin_plot_age.png", device="png",
       height=10, width=15, units="in", dpi=500)


# violin plot for slept under bed net regularly
violin_plot = ggplot(data=survival_data_primary, aes(x=slept_under_net_regularly, y=days_until_event, group=slept_under_net_regularly, color=factor(event_indicator))) +
  geom_violin() +
  geom_beeswarm(aes(color=factor(event_indicator)),alpha = 0.5) + 
  geom_line() + 
  theme_bw() + 
  ylab("Days until event") + 
  xlab("Slept under net regularly") +
  coord_flip() 
violin_plot
ggsave(violin_plot, filename="/Users/kelseysumner/Desktop/primary_violin_plot_slept_under_net_regularly.png", device="png",
       height=10, width=15, units="in", dpi=500)


#### ---- make a stacked bar chart of each person's exposure time ----- ####

## ------ for the primary case definition

# first make a summary of exposure by person
exposure_summary = survival_data_primary %>% 
  group_by(unq_memID,main_exposure_primary_case_def) %>%
  summarize(n=n(),)
exposure_summary_pct = exposure_summary %>%
  group_by(unq_memID) %>%
  mutate(perc_n = n/sum(n))
# note: 16 people never switched their exposure status

# make a plot of the percent of exposure status
exposure_plot = ggplot(data=exposure_summary_pct,aes(x=factor(unq_memID),y=perc_n,group=main_exposure_primary_case_def,fill=main_exposure_primary_case_def)) +
  geom_bar(stat = "identity",color="black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1),legend.position = "bottom")
exposure_plot
# need to figure out how to sort 
ggsave(exposure_plot, filename="/Users/kelseysumner/Desktop/primary_prop_exposure_plot.png", device="png",
       height=6, width=24, units="in", dpi=500)


#### ---- look at some general KM survival curves ----- ####

## --- for primary data
# KM curve not stratified
ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ 1, data = survival_data_primary), 
           xlab = "Days", 
           ylab = "Overall survival probability",
           surv.median.line = "hv")
# KM curve stratified
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary), 
           xlab = "Days", 
           ylab = "Overall survival probability",
           surv.median.line = "hv",
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           conf.int = T,
           legend.title = "Infection status",
           legend.labs = c("No infection", "Asymptomatic infection"),
           pval = T,
           ggtheme = theme_bw(),
           risk.table = T,
           ncensor.plot = F)
km_plot
png("Desktop/primary_kaplan_meier.png")
print(km_plot, newpage = FALSE)
dev.off()
ggsave(km_plot$plot, filename="/Users/kelseysumner/Desktop/primary_kaplan_meier.png", device="png",
       height=6, width=6, units="in", dpi=500)
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
# KM curve stratified
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



#### -------- fit a cox proportional hazards model ------- ####

## ------ for primary data

#Fit a survival model by exposure
s2 <- survival::survfit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = survival_data_primary, type="kaplan-meier")
s_by_exp <- summary(s2)


# run a coxph model
fit.coxph <- coxph(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name, 
                   data = survival_data_primary)
cox_plot = ggforest(fit.coxph, data = survival_data_primary)
ggsave(cox_plot, filename="/Users/kelseysumner/Desktop/primary_cox_model_results.png", device="png",
       height=4, width=10, units="in", dpi=500)


# test proportional hazards assumption
# first, do hypothesis test of whether an interaction effect of each covariate differs according to time, and a global test of all covariates at once
cz <- cox.zph(fit.coxph)
print(cz)
# this tests an interaction effect between the covariate and log(time)
# a significant p-value indicates that the proportional hazards assumption is violated
# second, plot schoenfeld residuals 
ggcoxzph(cz)
# deviation from a zero-slope line is evidence that the proportional hazards assumption is violated


# now run a multi-level coxph model with a random intercept for the participant level
fit.coxph <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | unq_memID), 
                   data = survival_data_primary)
fit.coxph


# now try to run a multi-level coxph model with random intercepts for the participant and household levels
# primary data set
fit.coxph <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | HH_ID/unq_memID), 
                   data = survival_data_primary)
fit.coxph
# make a forest plot of the model results
table1 = exp(confint(fit.coxph,method="Wald"))
estimates = c(1.1132191,NA,0.9614935,2.4604534,NA,0.6278866,NA,0.5139503,NA,1.0871182,0.6940651)
lower_ci = c(table1[1,1],NA,table1[2,1],table1[3,1],NA,table1[4,1],NA,table1[5,1],NA,table1[6,1],table1[7,1])
upper_ci = c(table1[1,2],NA,table1[2,2],table1[3,2],NA,table1[4,2],NA,table1[5,2],NA,table1[6,2],table1[7,2])
names = c("Asymptomatic infection","","Participant age >15 years","Participant age 5-15 years","  ","Gender","   ","Regular bed net usage","    ","Kinesamo village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age >15 years","Participant age 5-15 years","  ","Gender","   ","Regular bed net usage","    ","Kinesamo village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age >15 years","Participant age 5-15 years","  ","Gender","   ","Regular bed net usage","    ","Kinesamo village","Sitabicha village"))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1,1,1,1),colour=c("#E1AF00","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/primary_forest_plot_coxph_2levels.png", device="png",
       height=9, width=12.5, units="in", dpi=400)



# now try to run a multi-level coxph model with random intercepts for the participant and household levels
# primary data set
# with interaction term with time
survival_data_primary$days_until_event_scaled = scale(survival_data_primary$days_until_event)
summary(survival_data_primary$days_until_event_scaled)
fit.coxph <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + days_until_event_scaled*main_exposure_primary_case_def + (1 | HH_ID/unq_memID), 
                   data = survival_data_primary)
fit.coxph
# having trouble converging - try rescaled and centered time
summary(survival_data_primary$days_until_event_scaled)
fit.coxph <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + days_until_event_scaled*main_exposure_primary_case_def + (1 | unq_memID), 
                   data = survival_data_primary)
fit.coxph
# another try with log time and not multilevel
fit.coxph <- coxph(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + log(days_until_event)*main_exposure_primary_case_def, 
                   data = survival_data_primary)
fit.coxph
# now categorize time by 300 day increments
survival_data_primary$time_category = ifelse(survival_data_primary$days_until_event < 300,"<300 days","300+ days")
table(survival_data_primary$time_category,survival_data_primary$days_until_event, useNA = 'always')
table(survival_data_primary$time_category,useNA = "always")
# run a regular model
fit.coxph <- coxph(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + time_category*main_exposure_primary_case_def, 
                   data = survival_data_primary)
fit.coxph
# run a multilevel model - still not working great
fit.coxph <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + time_category*main_exposure_primary_case_def + (1 | unq_memID), 
                   data = survival_data_primary)
fit.coxph
# now try putting it in the model as a strata - maybe a little wonky of a way to do it
fit.coxph <- coxph(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + strata(time_category), 
                   data = survival_data_primary)
fit.coxph
ggsurvplot(survfit(fit.coxph), data = survival_data_primary, conf.int = TRUE)
# now try a model with just the first 365 days of follow-up
test_365days_follow_up = survival_data_primary %>% filter(days_until_event <= 365)
fit.coxph <- coxph(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name, 
                   data = test_365days_follow_up)
cox_plot = ggforest(fit.coxph, data = survival_data_primary)
cz <- cox.zph(fit.coxph)
print(cz)
ggsurvplot(survfit(fit.coxph), data = test_365days_follow_up, conf.int = TRUE)
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = test_365days_follow_up), 
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
# now try a model with just the first 300 days of follow-up
test_300days_follow_up = survival_data_primary %>% filter(days_until_event <= 300)
fit.coxph <- coxph(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name, 
                   data = test_300days_follow_up)
cox_plot = ggforest(fit.coxph, data = survival_data_primary)
cz <- cox.zph(fit.coxph)
print(cz)
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = test_300days_follow_up), 
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
# now try a model with just the first 30 days of follow-up
test_30days_follow_up = survival_data_primary %>% filter(days_until_event <= 30)
fit.coxph <- coxph(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name, 
                   data = test_30days_follow_up)
cox_plot = ggforest(fit.coxph, data = survival_data_primary)
cz <- cox.zph(fit.coxph)
print(cz)
km_plot = ggsurvplot(fit = surv_fit(Surv(days_until_event, event_indicator) ~ main_exposure_primary_case_def, data = test_30days_follow_up), 
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
# synopsis: stratifying time interval doesn't solve the issue



## ------ secondary stringent

# now run a multi-level coxph model with random intercepts at participant and household level for secondary stringent
fit.coxph.ss <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_stringent_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | HH_ID/unq_memID), 
                      data = survival_data_secondary_stringent)
fit.coxph.ss
# make a forest plot of the model results
table1 = exp(confint(fit.coxph.ss,method="Wald"))
estimates = c(1.0332143,NA,1.1918822,2.0218979,NA,1.5585409,NA,0.6375286,NA,3.6823999,0.6318716)
lower_ci = c(table1[1,1],NA,table1[2,1],table1[3,1],NA,table1[4,1],NA,table1[5,1],NA,table1[6,1],table1[7,1])
upper_ci = c(table1[1,2],NA,table1[2,2],table1[3,2],NA,table1[4,2],NA,table1[5,2],NA,table1[6,2],table1[7,2])
names = c("Asymptomatic infection","","Participant age >15 years","Participant age 5-15 years","  ","Gender","   ","Regular bed net usage","    ","Kinesamo village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age >15 years","Participant age 5-15 years","  ","Gender","   ","Regular bed net usage","    ","Kinesamo village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age >15 years","Participant age 5-15 years","  ","Gender","   ","Regular bed net usage","    ","Kinesamo village","Sitabicha village"))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1,1,1,1),colour=c("#E1AF00","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/secondary_stringent_forest_plot_coxph_2levels.png", device="png",
       height=9, width=12.5, units="in", dpi=400)




## -------- secondary permissive

# now run a multi-level coxph model with random intercepts at participant and household level for secondary permissive
fit.coxph.sp <- coxme(Surv(days_until_event, event_indicator) ~ main_exposure_secondary_permissive_case_def + age_cat_baseline + gender + slept_under_net_regularly + village_name + (1 | HH_ID/unq_memID), 
                      data = survival_data_secondary_permissive)
fit.coxph.sp
# make a forest plot of the model results
table1 = exp(confint(fit.coxph.sp,method="Wald"))
estimates = c(1.2068357,NA,1.2279561,1.8447753,NA,0.8630035,NA,0.8996846,NA,1.2212655,0.8723563)
lower_ci = c(table1[1,1],NA,table1[2,1],table1[3,1],NA,table1[4,1],NA,table1[5,1],NA,table1[6,1],table1[7,1])
upper_ci = c(table1[1,2],NA,table1[2,2],table1[3,2],NA,table1[4,2],NA,table1[5,2],NA,table1[6,2],table1[7,2])
names = c("Asymptomatic infection","","Participant age >15 years","Participant age 5-15 years","  ","Gender","   ","Regular bed net usage","    ","Kinesamo village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age >15 years","Participant age 5-15 years","  ","Gender","   ","Regular bed net usage","    ","Kinesamo village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","","Participant age >15 years","Participant age 5-15 years","  ","Gender","   ","Regular bed net usage","    ","Kinesamo village","Sitabicha village"))
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1,1,1,1),colour=c("#E1AF00","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/secondary_permissive_forest_plot_coxph_2levels.png", device="png",
       height=9, width=12.5, units="in", dpi=400)

