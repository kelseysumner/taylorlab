# ---------------------------- #
#     Look specifically at     #
#   pre-symptomatic infections #
#            Aim 1B            #
#           K. Sumner          #
#         April 2, 2020        #
# ---------------------------- #


#### ------- load libraries ------- ####

# load tidyverse
library(tidyverse)


#### ------- read in the data sets --------- ####

# read in the csp pre-symptomatic subset
csp_pre_symptomatic = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/CSP_subsets/pre_symptomatic/spat_1b_pre_symptomatic_data_csp_26MAR2020.csv")

# read in the csp pre-symptomatic subset
ama_pre_symptomatic = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/AMA_subsets/pre_symptomatic/spat_1b_pre_symptomatic_data_ama_26MAR2020.csv")



#### ---- split up the data sets to just those that are asymptomatic ----- ####

# now subset the csp data set to be asymptomatic
symptomatic_status = rep(c("Asymptomatic","Symptomatic"),27)
csp_pre_symptomatic$symptomatic_status = symptomatic_status
csp_pre_symptomatic = csp_pre_symptomatic %>%
  filter(symptomatic_status == "Asymptomatic")

# now subset the ama data set to be asymptomatic
symptomatic_status = rep(c("Asymptomatic","Symptomatic"),21)
ama_pre_symptomatic$symptomatic_status = symptomatic_status
ama_pre_symptomatic = ama_pre_symptomatic %>%
  filter(symptomatic_status == "Asymptomatic")


#### ----- calculate the proportion of unique haplotypes in asymptomatic infections that became symptomatic ----- ####

# this is the number haplotypes shared between asymptomatic and symptomatic infections 
# divided by the total number of haplotypes in asymptomatic infections

# calculate the average proportion haplotypes in asymptomatic infections that became symptomatic for each row
# for csp
summary(csp_pre_symptomatic$proportion_haps_shared)
length(which(csp_pre_symptomatic$proportion_haps_shared == 1))
length(which(csp_pre_symptomatic$proportion_haps_shared == 0))
length(which(csp_pre_symptomatic$proportion_haps_shared > 0)) # 20/27
# for ama
summary(ama_pre_symptomatic$proportion_haps_shared)
length(which(ama_pre_symptomatic$proportion_haps_shared == 1))
length(which(ama_pre_symptomatic$proportion_haps_shared == 0))
length(which(ama_pre_symptomatic$proportion_haps_shared > 0)) # 17/21




