# ----------------------------------------- #
#  Create aim 2 visualizations for astmh    #
#             Mozzie Phase 1                #
#                CSP data                   #
#            October 31, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(tidyverse)
library(devtools)
library(streamgraph)
library(lubridate)


#### ---------- read in the data sets ---------- ####

# read in the merged csp abdomen edgelist ready for the multilevel models
csp_abdomens = read_rds("Desktop/clean_ids_haplotype_results/CSP/model data set/spat21_csp_abdomen_sharing_final_model_data_31OCT2019.rds")

# read in the merged anpopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")


#### -------- make visualization 1 --------- ####

##  make a plot of the anopheles mosquitoes using the stream graph plot

# set up the data set
mosquito_data = anoph_merged_data %>%
  select(collection_date,abdominal_status) %>%
  mutate(value=rep(1,nrow(anoph_merged_data)), month_date = floor_date(collection_date, "month"), 
         new_abdominal_status = ifelse(abdominal_status=="Gravid" | abdominal_status == "Half Gravid", "Gravid", ifelse(
           abdominal_status == "Blood Fed","Blood Fed",ifelse(
             abdominal_status == "Un-identified" | abdominal_status == "Undetermined", "Undetermined", "Unfed")))) %>%
  group_by(month_date,new_abdominal_status) %>%
  tally(wt=value)

# make the plot
mosquito_plot = mosquito_data %>%
  streamgraph("new_abdominal_status","n","month_date", offset="zero", interactive = TRUE) %>%
  sg_fill_brewer("RdBu") %>%
  sg_legend(TRUE,"Abdominal Status: ")
mosquito_plot  



