# -------------------------------------- #
#           Spat21/Mozzie Study          #
#  Combined short-term/long-term plot    #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#           December 15, 2020            #
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


#### --------- now make a new figure that compares short-term and long-term effect across all exposures and outcomes ------- ####


# make a forest plot of the model results
estimates = c(1.11,1.20,1.02,1.20,1.02,2.61,2.54,1.77,1.97,2.76)
lower_ci = c(1.01,1.03,0.92,1.11,0.92,2.05,1.76,1.26,1.63,2.11)
upper_ci = c(1.22,1.41,1.12,1.31,1.13,3.33,3.67,2.47,2.40,3.62)
type = c("Long-term effect of asymptomatic malaria exposure","Long-term effect of asymptomatic malaria exposure","Long-term effect of asymptomatic malaria exposure","Long-term effect of asymptomatic malaria exposure","Long-term effect of asymptomatic malaria exposure","Short-term effect of asymptomatic malaria exposure","Short-term effect of asymptomatic malaria exposure","Short-term effect of asymptomatic malaria exposure","Short-term effect of asymptomatic malaria exposure","Short-term effect of asymptomatic malaria exposure")
names = c("Symptomatic malaria \n (Main model: primary)","Post-treatment analysis","Pre-symptomatic analysis","Symptomatic malaria \n (secondary permissive)","Symptomatic malaria \n (secondary stringent)","Symptomatic malaria \n (Main model: primary)","Post-treatment analysis","Pre-symptomatic analysis","Symptomatic malaria \n (secondary permissive)","Symptomatic malaria \n (secondary stringent)")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci,type)
forest_plot_df$names = factor(forest_plot_df$names,levels=c("Symptomatic malaria \n (Main model: primary)","Symptomatic malaria \n (secondary permissive)","Symptomatic malaria \n (secondary stringent)","Post-treatment analysis","Pre-symptomatic analysis"))
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=1.25) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Adjusted hazard of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0.5,0.9,1.0,1.2,1.5,2.0,2.5,3.0,3.5,4.0)) +
  theme_bw() +
  facet_wrap(~type,ncol=1,strip.position = "top",scales = "free_y") +
  theme(text = element_text(size=12.5),axis.text.y = element_text(face=c("plain","plain","plain","plain","bold","plain","plain","bold")))
fp
ggsave(fp, filename="/Users/kelseysumner/Desktop/shortterm_longterm_hazardsympmalaria_across_casedefs.png", device="png",
       height=5, width=7, units="in", dpi=400)






