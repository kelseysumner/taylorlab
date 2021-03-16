# -------------------------------------- #
#           Spat21/Mozzie Study          #
#          Risk factor analysis          #
#            Mozzie Phase 1              #
#               K. Sumner                #
#             March 4, 2021              #
# -------------------------------------- #

# good resource for trouble shooting convergence problems
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)
library(lme4)
library(ggplot2)
library(sjstats)
library(lmerTest)
library(glmmTMB)


#### ----- read in the data sets ----- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA_and_CSP/final/model data/final_model_data/spat21_aim2_merged_data_with_weights_5MAR2020.rds")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")


#### ------ cut down the model data set to just the variables of interest --------- ####




#### ----- now merge in the variables you need from the demographic data set --------- ####




#### ------ check covariate coding ------- ####




#### ------ compare PTEall across covariates for csp ------- ####







