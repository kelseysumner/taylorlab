# ------------------------ #
#    Assess Variants in    #
#      Sequencing Runs     #
#     Literature Data      #
#         K. Sumner        #
#    September 12, 2019    #
# ------------------------ #

#### ------- read in the libraries ------- ####
library(tidyverse)



#### ------ read in the variant tables ------- ####

# read in the variants from the Neafsey data
neafsey_variants = read_tsv("/Users/kelseysumner/Desktop/literature_csp_variants/neafsey_haplotype_output/final_censored_output/forward_csp_final_results/neafsey_forward_snp_report")

# read in the variants from the plasmodb data
plasmodb_variants = read_csv("/Users/kelseysumner/Desktop/literature_csp_variants/plasmodb_variant_output/plasmo_db_variants_10SEPT2019.csv")

# read in the variants from the pf3k data
pf3k_variants = read_csv("/Users/kelseysumner/Desktop/literature_csp_variants/pf3k_variant_output/Pf3K csp variant table 30JUL2019.csv")



#### ------- create a merged literature file for the variants -------- ####

# set up the plasmodb and pf3k data sets for merging
plasmodb_variants = plasmodb_variants %>%
  dplyr::rename("Ref Pos"="final ref position") %>%
  mutate("present_in_plasmodb" = rep(1,nrow(plasmodb_variants))) %>%
  select("Ref Pos","present_in_plasmodb")
pf3k_variants = pf3k_variants %>%
  dplyr::rename("Ref Pos"="finalRefPosition") %>%
  mutate("present_in_pf3k" = rep(1,nrow(pf3k_variants))) %>%
  select("Ref Pos","present_in_pf3k")
neafsey_variants = neafsey_variants %>%
  mutate("present_in_neafsey" = rep(1,nrow(neafsey_variants))) %>%
  select("Ref Pos","present_in_neafsey")

# now merge the three files together
merge1 = full_join(neafsey_variants,plasmodb_variants,by="Ref Pos")
final_merge_variants = full_join(merge1,pf3k_variants,by="Ref Pos")

# reorder the file to be in numeric order
final_merge_variants = final_merge_variants[order(final_merge_variants$`Ref Pos`),]

# calculate how much overlap was found across literature values
length(which(final_merge_variants$present_in_neafsey == 1 & final_merge_variants$present_in_pf3k == 1 & final_merge_variants$present_in_plasmodb == 1))
# 21/57 (36.8%) variants found in all literature sources 

# write out as a final merged file
write_csv(final_merge_variants,"Desktop/literature_csp_variants_merged.csv")


