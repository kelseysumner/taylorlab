# -------------------------------------- #
#           Spat21/Mozzie Study          #
#     Run final computational model      #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#           January 14, 2020             #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)
library(betareg)
library(lme4)
library(ggplot2)



#### ----- read in the data sets ----- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/model data/spat21_aim2_computational_model_subset_data_14JAN2020.rds")


#### -------- create two copies for everyone for logistic regression ------- ####

# first make two copies of everyone
copy_data = model_data

# now make a new variable in the original data set that is having a transmission event or not
model_data$transmission_event = rep("yes",nrow(model_data)) # here, everyone has transmission event
model_data$transmission_weight = model_data$p_te_all

# now make a new variable in the copied data set that is not having a transmission event
copy_data$transmission_event = rep("no",nrow(copy_data)) # here, no one has transmission event
copy_data$transmission_weight = 1-model_data$p_te_all # inverse weight for those that didn't have a transmission event

# now combine the original and copy data sets
combined_data = rbind(model_data,copy_data)

# make the transmission event variable a factor
combined_data$transmission_event = as.factor(combined_data$transmission_event)
combined_data$transmission_event = relevel(combined_data$transmission_event,ref="no")


#### -------- now run a crude logistic regression model ------- ####

# this is a regular logistic regression model and not multi-level model with the original model data set
regular_logistic = glm(p_te_all ~ aim2_exposure, family=binomial(link="logit"), data=model_data)
summary(regular_logistic)

# this is a regular logistic regression model and not multi-level model with the combined data set jess suggested
# that has two observations for each person
combo_logistic = glm(transmission_event ~ aim2_exposure, family=binomial(link="logit"), data=combined_data, weights=transmission_weight)
summary(combo_logistic)
table(combined_data$transmission_event)
table(combined_data$transmission_weight)
# both these models produce the exact same answer

# now try running a multi-level logistic regression model on the original model data set
multi_model <- glmer(p_te_all~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(multi_model)
# this one did not have trouble converging

# now run a multi-level logistic regression model on the combined data set jess suggested
# that has two boservations for each person
combo_multi_model <- glmer(transmission_event~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = combined_data, weights=transmission_weight)
summary(combo_multi_model)
# this one had trouble converging








