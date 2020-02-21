# -------------------------------------- #
#           Spat21/Mozzie Study          #
#     Run final computational model      #
#               CSP Target               # 
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#           February 18, 2020            #
# -------------------------------------- #

# good resource for trouble shooting convergence problems
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)
library(betareg)
library(lme4)
library(ggplot2)
library(sjstats)
library(lmerTest)


#### ----- read in the data sets ----- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/model data/final_model_data/spat21_aim2_merged_data_with_weights_18FEB2020.rds")



#### ------ check covariate coding ------- ####

# rescale csp and ama moi
model_data$csp_moi_rescaled = scale(model_data$csp_moi)
model_data$ama_moi_rescaled = scale(model_data$ama_moi)

# subset the data set to samples that passed pfcsp sequencing only
model_data = model_data %>%
  filter(!(is.na(csp_haps_shared)))



#### -------- make a table of the model covariates stratified by symptomatic status --------- ####

# make separate data sets for asymptomatic and symptomatic infections
asymp_data = model_data %>%
  filter(aim2_exposure == "asymptomatic infection")
symp_data = model_data %>%
  filter(aim2_exposure == "symptomatic infection")

# parasite density
median(asymp_data$pfr364Q_std_combined)
sd(asymp_data$pfr364Q_std_combined)
median(symp_data$pfr364Q_std_combined)
sd(symp_data$pfr364Q_std_combined)

# age
table(model_data$age_cat_baseline,model_data$aim2_exposure, useNA = "always")

# mosquito week count
table(model_data$mosquito_week_count_cat,model_data$aim2_exposure, useNA = "always")

# moi
median(asymp_data$csp_moi)
sd(asymp_data$csp_moi)
median(symp_data$csp_moi)
sd(symp_data$csp_moi)

# village name
table(model_data$village_name,model_data$aim2_exposure, useNA = "always")

# p_te_all_csp
median(asymp_data$p_te_all_csp)
sd(asymp_data$p_te_all_csp)
median(symp_data$p_te_all_csp)
sd(symp_data$p_te_all_csp)

# p_te_t
median(asymp_data$p_te_t)
sd(asymp_data$p_te_t)
median(symp_data$p_te_t)
sd(symp_data$p_te_t)

# p_te_d
median(asymp_data$p_te_d)
sd(asymp_data$p_te_d)
median(symp_data$p_te_d)
sd(symp_data$p_te_d)

# p_te_c_alt
median(asymp_data$p_te_c_alt)
sd(asymp_data$p_te_c_alt)
median(symp_data$p_te_c_alt)
sd(symp_data$p_te_c_alt)

# csp haps shared
median(asymp_data$csp_haps_shared)
sd(asymp_data$csp_haps_shared)
median(symp_data$csp_haps_shared)
sd(symp_data$csp_haps_shared)






