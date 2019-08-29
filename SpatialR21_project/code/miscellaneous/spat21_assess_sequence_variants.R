# ------------------------ #
#    Assess Variants in    #
#      Sequencing Runs     #
#        Mozzie Data       #
#         K. Sumner        #
#       August 29, 2019    #
# ------------------------ #


#### ------ read in the variant tables ------- ####

# mosquito samples
ama_mosquitoes = read_tsv("/Users/kelseysumner/Desktop/mosquito_snp_report/snp_report_ama_mosquitoes")
csp_mosquitoes = read_tsv("/Users/kelseysumner/Desktop/mosquito_snp_report/snp_report_csp_mosquitoes")

# human run 1: high parasite density
ama_humans_high = read_tsv("/Users/kelseysumner/Desktop/human_snp_report_high/ama_snp_report_humans_high")
csp_humans_high = read_tsv("/Users/kelseysumner/Desktop/human_snp_report_high/csp_snp_report_humans_high")

# human run 2: low parasite density
ama_humans_low = read_tsv("/Users/kelseysumner/Desktop/human_snp_report_low/ama_snp_report_human_low")
csp_humans_low = read_tsv("/Users/kelseysumner/Desktop/human_snp_report_low/csp_snp_report_human_low")


#### ------ assess that those variants were like that ------- ####

# create a huge merged file that tallies up how many snps you have across all data sets

## first do this for ama 

# first cut down data sets
ama_mosquitoes_cut = ama_mosquitoes %>%
  select("Ref Pos","SNP %") %>%
  mutate(ama_mosquitoes = "yes") %>%
  rename("SNP %"="mosquitoes_snp_pct")
ama_humans_low_cut = ama_humans_low %>%
  select("Ref Pos","SNP %") %>%
  mutate(ama_humans_low = "yes") %>%
  rename("SNP %"="humans_low_snp_pct")
ama_humans_high_cut = ama_humans_high %>%
  select("Ref Pos","SNP %") %>%
  mutate(ama_humans_high = "yes") %>%
  rename("SNP %"="humans_high_snp_pct")
# merge the data sets together based on ref position
ama_merged = full_join(ama_mosquitoes_cut,ama_humans_low_cut,by="Ref Pos")
ama_merged = full_join(ama_merged,ama_humans_high_cut,by="Ref Pos")

# tally up how often a snp is found across all three data sets


## now do the same for csp

# cut down the csp data sets
csp_mosquitoes_cut = csp_mosquitoes %>%
  select("Ref Pos","SNP %") %>%
  mutate(csp_mosquitoes = "yes") %>%
  rename("SNP %"="mosquitoes_snp_pct")
csp_humans_low_cut = csp_humans_low %>%
  select("Ref Pos","SNP %") %>%
  mutate(csp_humans_low = "yes") %>%
  rename("SNP %"="humans_low_snp_pct")
csp_humans_high_cut = csp_humans_high %>%
  select("Ref Pos","SNP %") %>%
  mutate(csp_humans_high = "yes") %>%
  rename("SNP %"="humans_high_snp_pct")
# merge the data sets together based on ref position
csp_merged = full_join(csp_mosquitoes_cut,csp_humans_low_cut,by="Ref Pos")
csp_merged = full_join(csp_merged,csp_humans_high_cut,by="Ref Pos")
# merge the data sets together based on ref positioncs
csp_merged = full_join(csp_mosquitoes_cut,csp_humans_low_cut,by="Ref Pos")
csp_merged = full_join(csp_merged,csp_humans_high_cut,by="Ref Pos")


# make data sets of just the snps found in all three data sets
# for ama
ama_found_in_all = ama_merged %>%
  filter(ama_mosquitoes=="yes" & ama_humans_low=="yes" & ama_humans_high=="yes")
# 42 variants found in all across ama
# for csp
csp_found_in_all = csp_merged %>%
  filter(csp_mosquitoes=="yes" & csp_humans_low=="yes" & csp_humans_high=="yes")
# 34 variants found in all across csp








