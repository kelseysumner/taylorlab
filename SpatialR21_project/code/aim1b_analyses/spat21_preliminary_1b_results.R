# -------------------------------- #
#       Preliminary results        #
#         Mozzie phase 1           #
#             Aim 1B               #
#        March 20, 2020            #
#           K. Sumner              #
# -------------------------------- #

# will call repeated infections: "reinfection"

#### ------- load libraries -------- ####
library(tidyverse)


#### ----- read in the data sets ------- ####

# read in the ama data set
ama_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/ama_data_aim1b_20MAR2020.rds")

# read in the csp data set
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/csp_data_aim1b_20MAR2020.rds")


#### ------ for each particpant, remove the first infection ------- ####

# first order the data set by date
csp_data = dplyr::arrange(csp_data,unq_memID,sample_id_date)

# look at how many infections each participant had
num_infections_before = csp_data %>%
  group_by(unq_memID) %>%
  summarize (n= n())

# remove the first instance of infection for each person
test = slice(group_by(csp_data, unq_memID), -1)

# now look at the new amount of infections every had (for those that had 2+ infections)
num_infections_after = test %>%
  group_by(unq_memID) %>%
  summarize (n= n())

# looks like this worked correctly so apply to everything
csp_data = slice(group_by(csp_data, unq_memID), -1)


#### ------- now look at summaries of the data set -------- ####

# look at a table of the exposure and outcome
table(csp_data$haplotype_category,csp_data$symptomatic_status, useNA="always")

# summary of number of infections per participant
num_infections_after = csp_data %>%
  group_by(unq_memID) %>%
  summarize (n= n())
summary(num_infections_after$n)

# make a density plot of the number of infections within individuals stratified by symptomatic status
# first set up the data
num_infections_stratified = csp_data %>%
  group_by(unq_memID,symptomatic_status) %>%
  summarize (n=n())
# then make the plot
plot1 = ggplot(data=num_infections_stratified,aes(x=n,fill=symptomatic_status)) + 
  geom_density() + facet_wrap(~symptomatic_status) + theme_bw() + theme(legend.position = "none") +
  xlab("Number of infections within each individual") + scale_x_continuous(limits=c(0,15))
# export the plot
ggsave(plot1, filename="/Users/kelseysumner/Desktop/plot1.png", device="png",
       height=3, width=5, units="in", dpi=500)

# make a density plot of the different haplotype categories within each participant
# first set up the data
num_hap_categories_stratified = csp_data %>%
  group_by(unq_memID,haplotype_category) %>%
  summarize (n=n())
# then make the plot
plot1 = ggplot(data=num_hap_categories_stratified,aes(x=n,fill=haplotype_category)) + 
  geom_density() + facet_wrap(~haplotype_category) + theme_bw() + theme(legend.position = "none") +
  xlab("Number of haplotype categories within each individual") + scale_x_continuous(limits=c(0,15))
# export the plot
ggsave(plot1, filename="/Users/kelseysumner/Desktop/plot1.png", device="png",
       height=3, width=5, units="in", dpi=500)

# now see how many symptomatic infections were within 30 days after the asymptomatic infections for each participant
# csp data is still sorted by participant and date


# what did these infections look like?

