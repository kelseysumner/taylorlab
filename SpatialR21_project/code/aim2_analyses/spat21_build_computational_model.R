# -------------------------------------- #
#           Spat21/Mozzie Study          #
#       Build computational model        #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#          December 3, 2019              #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)



#### ----- read in the data sets ----- ####

# read in the csp data set for mosquito abdomens
csp_data = read_rds("Desktop/clean_ids_haplotype_results/CSP/csp_edgelists_no_restrictions/csp_abdomens_with_ama/csp_edgelist_abdomens_with_ama.rds")


#### ----- create variable distributions ------ ####

# note: Focus of analysis will be on csp

## create a distribution based on the distance between each human-mosquito pair

# create dummy datasets
pairs = c("pair1", "pair2", "pair3")
hap = c("H1","H2","H3")
prev = c(0.5,0.2,0.15)
haps_shared = c(1,2,3)
order_haps = c("H1","H1,H2","H1,H2,H3")
pair_dummy_df = data.frame(pairs,haps_shared,order_haps)
hap_dummy_df = data.frame(hap,prev)
pair_dummy_df
hap_dummy_df

1-(1-.5)
1-(1-.5)*(1-.2)
1-(1-.5)*(1-.2)*(1-.15)
1-(1-.5)*(1-.6)*(1-.15)


