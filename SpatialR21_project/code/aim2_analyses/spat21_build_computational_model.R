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
csp_data = read_rds("Desktop/clean_ids_haplotype_results/CSP/csp_edgelists_no_restrictions/spat21_csp_edgelist_abdomen_3DEC2019.rds")

# read in the ama data set for mosquito heads
ama_data = read_rds("Desktop/clean_ids_haplotype_results/AMA/ama_edgelists_no_restrictions/spat21_ama_edgelist_abdomen_no_restrictions_3DEC2019.rds")
ama_data = ama_data %>%
  select(-X1)


#### ----- create variable distributions ------ ####

# note: Focus of analysis will be on csp

## create a distribution based on if >= 1 haplotype is shared between that pair in ama as well as csp

# first create a variable that indicates whether there's at least 1 haplotype shared in ama as well (is number of haplotypes shared in ama)
csp_data$haps_shared_in_ama = rep(NA,nrow(csp_data))
for (i in 1:nrow(csp_data)){
  if (csp_data$haps_shared[i] > 0) {
    for (j in 1:nrow(ama_data)){
      if (csp_data$sample_id_human[i] == ama_data$sample_id_human[j] & csp_data$sample_id_abdomen[i] == ama_data$sample_id_abdomen[j] & ama_data$haps_shared[j] > 0){
        csp_data$haps_shared_in_ama[i] = ama_data$haps_shared[j]
      }
    }
  }
}







