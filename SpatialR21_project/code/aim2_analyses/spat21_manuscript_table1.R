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
model_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/model data/final_model_data/spat21_aim2_merged_data_with_weights_5MAR2020.rds")



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
IQR(asymp_data$pfr364Q_std_combined)
median(symp_data$pfr364Q_std_combined)
IQR(symp_data$pfr364Q_std_combined)

# age
table(model_data$age_cat_baseline,model_data$aim2_exposure, useNA = "always")

# mosquito week count
table(model_data$mosquito_week_count_cat,model_data$aim2_exposure, useNA = "always")

# moi
median(asymp_data$csp_moi)
IQR(asymp_data$csp_moi)
median(symp_data$csp_moi)
IQR(symp_data$csp_moi)

# village name
table(model_data$village_name,model_data$aim2_exposure, useNA = "always")

# p_te_all_csp
median(asymp_data$p_te_all_csp)
IQR(asymp_data$p_te_all_csp)
median(symp_data$p_te_all_csp)
IQR(symp_data$p_te_all_csp)

# p_te_t
median(asymp_data$p_te_t)
IQR(asymp_data$p_te_t)
median(symp_data$p_te_t)
IQR(symp_data$p_te_t)

# p_te_d
median(asymp_data$p_te_d)
IQR(asymp_data$p_te_d)
median(symp_data$p_te_d)
IQR(symp_data$p_te_d)

# p_te_c_alt
median(asymp_data$p_te_c_alt)
IQR(asymp_data$p_te_c_alt)
median(symp_data$p_te_c_alt)
IQR(symp_data$p_te_c_alt)

# p_te_c_alt for those that shared pfcsp haplotypes
asymp_cut = asymp_data %>%
  filter(csp_haps_shared > 0)
symp_cut = symp_data %>%
  filter(csp_haps_shared > 0)
median(asymp_cut$p_te_c_alt)
IQR(asymp_cut$p_te_c_alt)
median(symp_cut$p_te_c_alt)
IQR(symp_cut$p_te_c_alt)

# csp haps shared
median(asymp_data$csp_haps_shared)
IQR(asymp_data$csp_haps_shared)
median(symp_data$csp_haps_shared)
IQR(symp_data$csp_haps_shared)

# csp haps shared for those that shared haplotypes
median(asymp_cut$csp_haps_shared)
IQR(asymp_cut$csp_haps_shared)
median(symp_cut$csp_haps_shared)
IQR(symp_cut$csp_haps_shared)


#### -------- now recreate this table but for the participant-mosquito pairs that were excluded from the analysis ----- ####

# read in the full merged data data set
full_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/full data/spat21_aim2_merged_data_with_weights_full_data_5MAR2020.rds")

# subset the data set to samples that passed pfcsp sequencing only
full_data = full_data %>%
  filter(!(is.na(csp_haps_shared)))

# subset the full data set to not include any of the pairs in model_data
model_exclude = model_data %>%
  select(sample_id_human,sample_id_abdomen) %>%
  mutate(exclude = rep("exclude",nrow(model_data)))
full_data = left_join(full_data,model_exclude,by=c("sample_id_human","sample_id_abdomen"))
full_data = full_data %>%
  filter(is.na(exclude))
# check it: 159285-3727 = 155558, looks good

# read in the data set with the mosquito week count cat information
mosquito_extra_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/time sensitivity analysis data sets/spat21_aim2_sensitivity_analysis_data_set_2FEB2020.rds")
colnames(mosquito_extra_data)

# make a categorical variable for mosquito_week_count
mosquito_extra_data$mosquito_week_count_cat = ifelse(mosquito_extra_data$mosquito_week_count < 75, "<75 mosquitoes","75-147 mosquitoes")
table(mosquito_extra_data$mosquito_week_count_cat,mosquito_extra_data$mosquito_week_count,useNA = "always")

# merge in the mosquito week information
mosquito_extra_data = mosquito_extra_data %>%
  select(sample_id_human,sample_id_abdomen,mosquito_week_count_cat)
full_data = left_join(full_data,mosquito_extra_data,by=c("sample_id_human","sample_id_abdomen"))

# now create the supplemental table 1

# make separate data sets for asymptomatic and symptomatic infections
asymp_data = full_data %>%
  filter(aim2_exposure == "asymptomatic infection")
symp_data = full_data %>%
  filter(aim2_exposure == "symptomatic infection")

# parasite density
median(asymp_data$pfr364Q_std_combined)
IQR(asymp_data$pfr364Q_std_combined)
median(symp_data$pfr364Q_std_combined)
IQR(symp_data$pfr364Q_std_combined)

# age
table(full_data$age_cat_baseline,full_data$aim2_exposure, useNA = "always")

# mosquito week count
table(full_data$mosquito_week_count_cat,full_data$aim2_exposure, useNA = "always")

# moi
median(asymp_data$csp_moi)
IQR(asymp_data$csp_moi)
median(symp_data$csp_moi)
IQR(symp_data$csp_moi)

# village name
table(full_data$village_name,full_data$aim2_exposure, useNA = "always")

# p_te_all_csp
median(asymp_data$p_te_all_csp)
IQR(asymp_data$p_te_all_csp)
median(symp_data$p_te_all_csp)
IQR(symp_data$p_te_all_csp)

# p_te_t
median(asymp_data$p_te_t)
IQR(asymp_data$p_te_t)
median(symp_data$p_te_t)
IQR(symp_data$p_te_t)

# p_te_d
median(asymp_data$p_te_d)
IQR(asymp_data$p_te_d)
median(symp_data$p_te_d)
IQR(symp_data$p_te_d)

# p_te_c_alt
median(asymp_data$p_te_c_alt)
IQR(asymp_data$p_te_c_alt)
median(symp_data$p_te_c_alt)
IQR(symp_data$p_te_c_alt)

# p_te_c_alt for those that shared pfcsp haplotypes
asymp_cut = asymp_data %>%
  filter(csp_haps_shared > 0)
symp_cut = symp_data %>%
  filter(csp_haps_shared > 0)
median(asymp_cut$p_te_c_alt)
IQR(asymp_cut$p_te_c_alt)
median(symp_cut$p_te_c_alt)
IQR(symp_cut$p_te_c_alt)

# csp haps shared
median(asymp_data$csp_haps_shared)
IQR(asymp_data$csp_haps_shared)
median(symp_data$csp_haps_shared)
IQR(symp_data$csp_haps_shared)

# csp haps shared for those that shared haplotypes
median(asymp_cut$csp_haps_shared)
IQR(asymp_cut$csp_haps_shared)
median(symp_cut$csp_haps_shared)
IQR(symp_cut$csp_haps_shared)







