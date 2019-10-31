# ---------------------------- #
#    Look at overall csp       #
#    haplotype sharing with    #
#    mosquito abdomens         #
#    regardless of time and    #
#    place                     #
#       October 28, 2019       #
#          K. Sumner           #
# ---------------------------- #

#### --------- load libraries ------ ####
library(tidyverse)



#### ----- read in the data set ------ ####

# read in the full csp abdomen sharing data set (not restricted to 0-14 days and same household)
csp_abdomens_unrestricted = read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_edgelist_abdomen_unrestricted_by_hh_29OCT2019.rds")



#### ----- now calculate the haplotype sharing in the entire population regardless of time and space restrictions ------- ####

# look at the number of asymptomatic vs. symptomatic infections for mosquito abdomens
table(csp_abdomens_unrestricted$aim2_exposure, useNA = "always")
# assess median and mean number shared haplotypes in asymptomatic infections
csp_abdomens_unrestricted %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  summarize(mean_val = mean(haps_shared), median_val = median(haps_shared)) %>%
  View()
# assess median and mean number of shared haplotypes in symptomatic infections
csp_abdomens_unrestricted %>%
  filter(aim2_exposure=="symptomatic infection") %>%
  summarize(mean_val = mean(haps_shared), median_val = median(haps_shared)) %>%
  View()
# assess number of human-mosquito pairs with >= 1 haplotype shared
table(csp_abdomens_unrestricted$aim2_exposure,csp_abdomens_unrestricted$haps_shared, useNA = "always")


# look at the overall haplotype sharing
length(which(csp_abdomens_unrestricted$haps_shared>=1))/nrow(csp_abdomens_unrestricted)
length(which(csp_abdomens_unrestricted$haps_shared>=1))
nrow(csp_abdomens_unrestricted)

# look at sharing stratified by village
table(csp_abdomens_unrestricted$village_name, useNA = "always")
table(csp_abdomens_unrestricted$village_name,csp_abdomens_unrestricted$haps_shared, useNA = "always")
