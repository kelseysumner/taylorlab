# ----------------------------------------- #
#     Make venn diagram of CSP variants     #
#      we found compared to literature      #
#             Mozzie Phase 1                #
#            October 21, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #


#### ------- load libraries ---------- ####
library(VennDiagram)
library(tidyverse)


#### -------- read in the full csp literature data set --------- ####

# read in the merged variant table
variant_table = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/literature_csp_variants/final_merged_output_with_our_data/final_literature_and_our_csp_variants_merged.csv")


#### ------- make a venn diagram ------- ####

# figure out the counts of each category
colnames(variant_table)
# present_in_neafsey
length(which(variant_table$present_in_neafsey==1)) # 39
# present_in_plasmodb
length(which(variant_table$present_in_plasmodb==1)) # 30
# present_in_pf3k
length(which(variant_table$present_in_pf3k==1)) # 44
# present_in_our_csp
length(which(variant_table$present_in_our_csp==1)) # 74

# neafsey only
length(which(variant_table$present_in_neafsey==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_pf3k) & is.na(variant_table$present_in_our_csp))) # 1

# neafsey and plasmodb
length(which(variant_table$present_in_neafsey==1 & variant_table$present_in_plasmodb==1 & is.na(variant_table$present_in_pf3k) & is.na(variant_table$present_in_our_csp))) # 0

# neafsey and pf3k
length(which(variant_table$present_in_neafsey==1 & variant_table$present_in_pf3k==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_our_csp))) # 3

# neafsey and our csp
length(which(variant_table$present_in_neafsey==1 & variant_table$present_in_our_csp==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_pf3k))) # 4

# plasmodb only
length(which(variant_table$present_in_plasmodb==1 & is.na(variant_table$present_in_pf3k) & is.na(variant_table$present_in_neafsey) & is.na(variant_table$present_in_our_csp))) # 1

# plasmodb and pf3k
length(which(variant_table$present_in_plasmodb==1 & variant_table$present_in_pf3k==1 & is.na(variant_table$present_in_neafsey) & is.na(variant_table$present_in_our_csp))) # 0

# plasmodb and our csp
length(which(variant_table$present_in_plasmodb==1 & is.na(variant_table$present_in_pf3k) & is.na(variant_table$present_in_neafsey) & variant_table$present_in_our_csp==1)) # 0

# pf3k only
length(which(variant_table$present_in_pf3k==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_neafsey) & is.na(variant_table$present_in_our_csp))) # 15

# pf3k and our csp
length(which(variant_table$present_in_pf3k==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_neafsey) & variant_table$present_in_our_csp==1)) # 1

# our csp only
length(which(variant_table$present_in_our_csp==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_neafsey) & is.na(variant_table$present_in_pf3k))) # 37

# neafsey, plasmodb, and pf3k
length(which(is.na(variant_table$present_in_our_csp) & variant_table$present_in_plasmodb==1 & variant_table$present_in_neafsey==1 & variant_table$present_in_pf3k==1)) # 0

# neafsey, plasmodb, and our csp data
length(which(variant_table$present_in_our_csp==1 & variant_table$present_in_plasmodb==1 & variant_table$present_in_neafsey==1 & is.na(variant_table$present_in_pf3k))) # 7

# neafsey, pf3k, and our csp data
length(which(variant_table$present_in_our_csp==1 & is.na(variant_table$present_in_plasmodb) & variant_table$present_in_neafsey==1 & variant_table$present_in_pf3k==1)) # 3

# plasmodb, pf3k, and our csp data
length(which(variant_table$present_in_our_csp==1 & variant_table$present_in_plasmodb==1 & is.na(variant_table$present_in_neafsey) & variant_table$present_in_pf3k==1)) # 1

# everything shared
length(which(variant_table$present_in_our_csp==1 & variant_table$present_in_plasmodb==1 & variant_table$present_in_neafsey==1 & variant_table$present_in_pf3k==1)) # 21




