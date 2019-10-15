# ----------------------------------------- #
#        Create aim 2 descriptive info      #
#             Mozzie Phase 1                #
#            AMA and CSP data               #
#            October 15, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(tidyverse)



#### ---------- read in the data sets ---------- ####

# read in the merged ama abdomen edgelist
ama_abdomens = read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_ama_edgelist_abdomen_08OCT2019.rds")

# read in the merged ama head edgelist
ama_heads = read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_ama_edgelist_head_08OCT2019.rds")

# read in the merged csp abdomen edgelist
csp_abdomens = read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_edgelist_abdomen_08OCT2019.rds")

# read in the merged csp head edgelist
csp_heads = read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_edgelist_head_08OCT2019.rds")



#### -------- look at csp descriptives ---------- ####

# look at the number of asymptomatic vs. symptomatic infections
table(csp_abdomens$aim2_exposure, useNA = "always")
# assess median and mean number shared haplotypes in asymptomatic infections
csp_abdomens %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  summarize(mean_val = mean(haps_shared), median_val = median(haps_shared)) %>%
  View()
# assess median and mean number of shared haplotypes in symptomatic infections
csp_abdomens %>%
  filter(aim2_exposure=="symptomatic infection") %>%
  summarize(mean_val = mean(haps_shared), median_val = median(haps_shared)) %>%
  View()







