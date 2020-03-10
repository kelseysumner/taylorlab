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
library(schoolmath)


#### -------- read in the full csp literature data set --------- ####

# read in the merged variant table
variant_table = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/literature_csp_variants/final_merged_output_with_our_data/final_literature_and_our_csp_variants_merged.csv")

# read in the variants from our data
our_variants = read_tsv("Desktop/clean_ids_haplotype_results/CSP/Variant Table/forward_csp_variant_table_report")


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



#### ----- look at the variants that were only found in our data ------ ####

# our csp only
length(which(variant_table$present_in_our_csp==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_neafsey) & is.na(variant_table$present_in_pf3k))) # 37
our_csp_only = variant_table %>%
  filter(present_in_our_csp==1 & is.na(present_in_plasmodb) & is.na(present_in_neafsey) & is.na(present_in_pf3k))

# figure out which of those are our variants
combo_our_variants = left_join(our_csp_only,our_variants,by="Ref Pos")

# see how rare these SNPs were 
table(combo_our_variants$`SNP %`)


#### ------- now look at the variants only found in the aim 2 analysis (asymptomatic participants, symptomatic participants, and mosquito abdomens) ----- ####

# read in the csp haplotype data
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
csp_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")

# read in the fasta file of csp haplotype sequences
haplotype_fasta = read_tsv("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_uniqueSeqs_final_censored.fasta")

# make separate data sets for humans and mosquitoes
human_haps = csp_haplotypes %>%
  filter(sample_type=="Human")
abdomen_haps = csp_haplotypes %>%
  filter(sample_type=="Abdomen")
abdomen_haps = abdomen_haps[,c(4:301)]

# merge the final_data info for symptomatic status with the human haps
cut_data = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  select(sample_name_dbs,main_exposure_primary_case_def,main_outcome_primary_case_def) %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def)))
table(cut_data$aim2_exposure, useNA = "always")
human_haps = left_join(human_haps,cut_data,by="sample_name_dbs")
table(human_haps$aim2_exposure, useNA = "always")
colnames(human_haps)
asymp_human_haps = human_haps %>% filter(aim2_exposure == "asymptomatic infection")
symp_human_haps = human_haps %>% filter(aim2_exposure == "symptomatic infection")
asymp_human_haps = asymp_human_haps[,c(4:301)]
symp_human_haps = symp_human_haps[,c(4:301)]

# summarize the number of samples within each haplotype for the asymp human samples
haplotype.names = rep(1:ncol(asymp_human_haps))
haplotypes_in_samples = rep(NA,ncol(asymp_human_haps))
total_reads_in_samples = rep(NA,ncol(asymp_human_haps))
for (k in 1:ncol(asymp_human_haps)){
  haplotypes_in_samples[k] = length(which(asymp_human_haps[,k] > 0))
  total_reads_in_samples[k] = sum(asymp_human_haps[,k],na.rm=T)
}
asymp_human_hap_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)

# summarize the number of samples within each haplotype for the symp human samples
haplotype.names = rep(1:ncol(symp_human_haps))
haplotypes_in_samples = rep(NA,ncol(symp_human_haps))
total_reads_in_samples = rep(NA,ncol(symp_human_haps))
for (k in 1:ncol(symp_human_haps)){
  haplotypes_in_samples[k] = length(which(symp_human_haps[,k] > 0))
  total_reads_in_samples[k] = sum(symp_human_haps[,k],na.rm=T)
}
symp_human_hap_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)

# summarize the number of samples within each haplotype for the mosquito abdomen samples
haplotype.names = rep(1:ncol(abdomen_haps))
haplotypes_in_samples = rep(NA,ncol(abdomen_haps))
total_reads_in_samples = rep(NA,ncol(abdomen_haps))
for (k in 1:ncol(abdomen_haps)){
  haplotypes_in_samples[k] = length(which(abdomen_haps[,k] > 0))
  total_reads_in_samples[k] = sum(abdomen_haps[,k],na.rm=T)
}
abdomen_hap_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)

# change the hap summary names
asymp_human_hap_summary$haplotype_ids = paste0(">Seq",asymp_human_hap_summary$haplotype_ids)
symp_human_hap_summary$haplotype_ids = paste0(">Seq",symp_human_hap_summary$haplotype_ids)
abdomen_hap_summary$haplotype_ids = paste0(">Seq",abdomen_hap_summary$haplotype_ids)

# subset to just have haplotype found in > 0 samples
asymp_human_hap_summary = asymp_human_hap_summary %>%
  filter(haplotypes_across_samples > 0)
symp_human_hap_summary = symp_human_hap_summary %>%
  filter(haplotypes_across_samples > 0)
abdomen_hap_summary = abdomen_hap_summary %>%
  filter(haplotypes_across_samples > 0)

# now create a vector of unique haplotype ids found in aim 2 manuscript
unique_haplotype_ids = unique(c(asymp_human_hap_summary$haplotype_ids,symp_human_hap_summary$haplotype_ids,abdomen_hap_summary$haplotype_ids))

# reorganize the haplotype sequence fasta file to be in a better format
sequence_names = rep(NA,298) # number is number of haplotypes in fasta file of unique haplotype sequences
sequences = rep(NA,298)
sequence_names[1] = ">Seq1"
for (i in 1:nrow(haplotype_fasta)) {
  if (is.even(i)) {
    sequence_names[i+1] = haplotype_fasta$`>Seq1`[i]
  }
  if (is.odd(i)){
    sequences[i] = haplotype_fasta$`>Seq1`[i]
  }
}
new_haplotype_sequences = data.frame(sequence_names,sequences)
new_haplotype_sequences = na.omit(new_haplotype_sequences)
new_haplotype_sequences$sequences = as.character(new_haplotype_sequences$sequences)
new_haplotype_sequences$sequence_names = as.character(new_haplotype_sequences$sequence_names)

# now create a new sequence that is the reverse complement of the old one (only do for pfcsp - not pfama1!)
# code from: https://www.r-bloggers.com/r-function-to-reverse-and-complement-a-dna-sequence/
rev.comp<-function(x,rev=TRUE)
{
  x<-toupper(x)
  y<-rep("N",nchar(x))
  xx<-unlist(strsplit(x,NULL))
  for (bbb in 1:nchar(x))
  {
    if(xx[bbb]=="A") y[bbb]<-"T"    
    if(xx[bbb]=="C") y[bbb]<-"G"    
    if(xx[bbb]=="G") y[bbb]<-"C"    
    if(xx[bbb]=="T") y[bbb]<-"A"
  }
  if(rev==FALSE) 
  {
    for(ccc in (1:nchar(x)))
    {
      if(ccc==1) yy<-y[ccc] else yy<-paste(yy,y[ccc],sep="")
    }
  }
  if(rev==T)
  {
    zz<-rep(NA,nchar(x))
    for(ccc in (1:nchar(x)))
    {
      zz[ccc]<-y[nchar(x)+1-ccc]
      if(ccc==1) yy<-zz[ccc] else yy<-paste(yy,zz[ccc],sep="")
    }
  }
  return(yy)  
}
# now reverse complement each string
reverse_complement_sequence = rep(NA,nrow(new_haplotype_sequences))
for (i in 1:nrow(new_haplotype_sequences)){
  reverse_complement_sequence[i] = rev.comp(new_haplotype_sequences$sequences[i])
}
new_haplotype_sequences$reverse_complement_sequence = reverse_complement_sequence

# now subset the new haplotype sequence data set to just the haplotypes in the aim 2 manuscript
new_haplotype_sequences_subset = new_haplotype_sequences %>%
  filter(sequence_names %in% unique_haplotype_ids)

# subset the data set to the forward sequences
new_haplotype_sequences_subset = new_haplotype_sequences_subset %>%
  select(sequence_names,reverse_complement_sequence)

# export this data set and create a new fasta file
write_tsv(new_haplotype_sequences_subset,"Desktop/aim2_unique_csp_sequences")

# now read in the new variant sequence file for aim 2 from our data
our_variants = read_tsv("Desktop/clean_ids_haplotype_results/CSP/aim 2 manuscript variant table/aim2_manuscript_snp_report")

## make an update venn diagram

# merge in the aim 2 variants
colnames(our_variants)
colnames(variant_table)
our_variants = our_variants %>%
  select(`Ref Pos`) %>%
  mutate(present_in_our_csp_aim2 = rep(1,nrow(our_variants)))
variant_table = left_join(variant_table,our_variants,by="Ref Pos")

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
# present_in_our_csp_aim2
length(which(variant_table$present_in_our_csp_aim2==1)) # 72

# neafsey only
length(which(variant_table$present_in_neafsey==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_pf3k) & is.na(variant_table$present_in_our_csp_aim2))) # 1

# neafsey and plasmodb
length(which(variant_table$present_in_neafsey==1 & variant_table$present_in_plasmodb==1 & is.na(variant_table$present_in_pf3k) & is.na(variant_table$present_in_our_csp_aim2))) # 0

# neafsey and pf3k
length(which(variant_table$present_in_neafsey==1 & variant_table$present_in_pf3k==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_our_csp_aim2))) # 3

# neafsey and our csp
length(which(variant_table$present_in_neafsey==1 & variant_table$present_in_our_csp_aim2==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_pf3k))) # 4

# plasmodb only
length(which(variant_table$present_in_plasmodb==1 & is.na(variant_table$present_in_pf3k) & is.na(variant_table$present_in_neafsey) & is.na(variant_table$present_in_our_csp_aim2))) # 1

# plasmodb and pf3k
length(which(variant_table$present_in_plasmodb==1 & variant_table$present_in_pf3k==1 & is.na(variant_table$present_in_neafsey) & is.na(variant_table$present_in_our_csp_aim2))) # 0

# plasmodb and our csp
length(which(variant_table$present_in_plasmodb==1 & is.na(variant_table$present_in_pf3k) & is.na(variant_table$present_in_neafsey) & variant_table$present_in_our_csp_aim2==1)) # 0

# pf3k only
length(which(variant_table$present_in_pf3k==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_neafsey) & is.na(variant_table$present_in_our_csp_aim2))) # 15

# pf3k and our csp
length(which(variant_table$present_in_pf3k==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_neafsey) & variant_table$present_in_our_csp_aim2==1)) # 1

# our csp only
length(which(variant_table$present_in_our_csp_aim2==1 & is.na(variant_table$present_in_plasmodb) & is.na(variant_table$present_in_neafsey) & is.na(variant_table$present_in_pf3k))) # 37

# neafsey, plasmodb, and pf3k
length(which(is.na(variant_table$present_in_our_csp_aim2) & variant_table$present_in_plasmodb==1 & variant_table$present_in_neafsey==1 & variant_table$present_in_pf3k==1)) # 0

# neafsey, plasmodb, and our csp data
length(which(variant_table$present_in_our_csp_aim2==1 & variant_table$present_in_plasmodb==1 & variant_table$present_in_neafsey==1 & is.na(variant_table$present_in_pf3k))) # 7

# neafsey, pf3k, and our csp data
length(which(variant_table$present_in_our_csp_aim2==1 & is.na(variant_table$present_in_plasmodb) & variant_table$present_in_neafsey==1 & variant_table$present_in_pf3k==1)) # 3

# plasmodb, pf3k, and our csp data
length(which(variant_table$present_in_our_csp_aim2==1 & variant_table$present_in_plasmodb==1 & is.na(variant_table$present_in_neafsey) & variant_table$present_in_pf3k==1)) # 1

# everything shared
length(which(variant_table$present_in_our_csp_aim2==1 & variant_table$present_in_plasmodb==1 & variant_table$present_in_neafsey==1 & variant_table$present_in_pf3k==1)) # 21






