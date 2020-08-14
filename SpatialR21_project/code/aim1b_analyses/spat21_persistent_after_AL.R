# -------------------------------- #
#   Look at persistent infections  # 
#            after AL              #
#         Mozzie phase 1           #
#             Aim 1B               #
#        August 13, 2020           #
#           K. Sumner              #
# -------------------------------- #



#### ------- load libraries -------- ####
library(tidyverse)
library(schoolmath)
library(lme4)
library(glmmTMB)
library(scales)



#### -------- read in the data sets --------- ####

ama_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_ama_final_model_data_21JUL2020.rds")
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_csp_final_model_data_21JUL2020.rds")


#### ------- look at how infections that occurred after being symptomatic and taking AL --------- ####

### ---- do this first for csp

# first order the data set by date
csp_data = dplyr::arrange(csp_data,unq_memID,sample_id_date)

# now see how many infections (asymptomatic or symptomatic) were within 30 days after the symptomatic infections for each participant
# csp data is still sorted by participant and date
# also check to make sure person received study-prescribed antimalarials
persistent_after_al = rep(NA,nrow(csp_data))
for (i in 1:nrow(csp_data)){
  if (csp_data$unq_memID[i] == csp_data$unq_memID[i+1] & i != nrow(csp_data) &
      csp_data$symptomatic_status[i] == "symptomatic infection" & 
      csp_data$prescription[i] == "prescribed" & !(is.na(csp_data$unq_memID[i])) & 
      !(is.na(csp_data$unq_memID[i+1])) & !(is.na(csp_data$prescription[i])) &
      !is.na(csp_data$symptomatic_status[i])){
    if (str_detect(csp_data$haplotype_category[i+1],"persistent")){
      persistent_after_al[i] = "yes"
      persistent_after_al[i+1] = "yes"
    }
  } 
}
csp_data$persistent_after_al = persistent_after_al
# check the output
check_data = csp_data %>%
  select(unq_memID, symptomatic_status, sample_id_date,prescription,haplotype_category,persistent_after_al)
# look at how many infections had persistent haplotypes after a prescription
check_data = check_data %>% filter(!(is.na(persistent_after_al)))
# there were 3 infections that had persistent haplotypes after being treated with AL



