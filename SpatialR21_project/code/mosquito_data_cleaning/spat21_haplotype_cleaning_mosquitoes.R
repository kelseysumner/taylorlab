# ----------------------------------------- #
#       Spat21 Haplotype Output Cleaning    #
#               Mosquito Data               #
#              April 17, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #


#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(tableone)
library(MHCtools)
library(dada2)
library(ggplot2)


#### ------- read in the AMA haplotype output -------------- ####

# read in the haplotype data set
foo = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output_old/AMA/AMA_spat21_mosquito_haplotypes.rds")

# figure out how many rows and columns
nrow(foo)
ncol(foo)
table(nchar(getSequences(foo)))

### --- look at the raw haplotype output 

# remove the controls (BF289 and BF294)
foo = foo[-which(rownames(foo) == "BF289" | rownames(foo) == "BF294"),]

# summarize the samples for each haplotype
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
total_reads_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
  total_reads_in_samples[k] = sum(foo[,k],na.rm=T)
}
haplotype_num_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)
# censor haplotypes found with <500 reads 
haplotype_num_summary = haplotype_num_summary[-which(haplotype_num_summary$total_reads_across_samples < 500),]
# output the haplotype summary
write_csv(haplotype_num_summary,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/AMA/AMA_haplotype_num_summary.csv")

# enforce censoring to rds data set
foo = foo[,c(haplotype_num_summary$haplotype_ids)]

# write some code that summarizes the haplotypes across each sample for censored haplotypes
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary = data.frame("sample_names" = sample.names, "haplotype_number" = haplotype_num, "haplotype_reads" = haplotype_reads)
# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary$haplotype_reads == 0) # 2 samples removed
haplotype_summary = haplotype_summary[-needtoremove,]
# export as a csv
write_csv(haplotype_summary,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/AMA/AMA_sample_summary.csv")

# write out the haplotypes results as a fasta
uniquesToFasta(getUniques(foo), fout="Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/AMA/AMA_uniqueSeqs.fasta", ids=paste0("Seq", seq(length(getUniques(foo)))))

# rename the columns to be a unique haplotype column number
newcolnames = c(1:ncol(foo))
pastedcolnames = rep(NA,length(newcolnames))
for (i in 1:length(newcolnames)){
  pastedcolnames[i] = paste0("H",newcolnames[i])
}
colnames(foo) <- pastedcolnames

# make the matrix a dataframe
foo = as.data.frame(foo)

# create a new column of foo that is the sample names (rownames)
foo$`MiSeq.ID` = rownames(foo)
colnames(foo)

# censor the rds file
foo = foo[which(foo$MiSeq.ID %in% haplotype_summary$sample_names),]

# write out the new rds file with censoring enforced
write_csv(foo,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/AMA/AMA_spat21_mosquito_haplotypes_censored.csv")


# calculate number of SNPs in haplotypes from MESA CSP haplotypes
library(Biostrings)
dna = readDNAStringSet("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/AMA/AMA_uniqueSeqs.fasta")
snp_output = stringDist(dna, method="hamming")
snp_output = as.matrix(snp_output)
max(snp_output)
summary(snp_output)


# calculate number of SNPs in haplotypes from this run
library(Biostrings)
dna = readDNAStringSet("Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/CSP/CSP_uniqueSeqs.fasta")
snp_output = stringDist(dna, method="hamming")
snp_output = as.matrix(snp_output)
max(snp_output)
summary(snp_output)



#### ------- read in the CSP haplotype output -------------- ####

# read in the haplotype data set
foo = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/CSP/spat21_mosquitoes_CSP_haplotypes.rds")

# figure out how many rows and columns
nrow(foo)
ncol(foo)

### --- look at the raw haplotype output 

# remove the controls (BF289 and BF294)
foo = foo[-which(rownames(foo) == "BF289" | rownames(foo) == "BF294"),]

# summarize the samples for each haplotype
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
total_reads_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
  total_reads_in_samples[k] = sum(foo[,k],na.rm=T)
}
haplotype_num_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)

# calculate how many haplotypes remain when remove <50 reads
length(which(haplotype_num_summary$total_reads_across_samples>=50)) # 505
# calculate how many haplotypes remain when remove <100 reads
length(which(haplotype_num_summary$total_reads_across_samples>=100)) # 485
# calculate how many haplotypes remain when remove <150 reads
length(which(haplotype_num_summary$total_reads_across_samples>=150)) # 447
# calculate how many haplotypes remain when remove <200 reads
length(which(haplotype_num_summary$total_reads_across_samples>=200)) # 414
# calculate how many haplotypes remain when remove <250 reads
length(which(haplotype_num_summary$total_reads_across_samples>=250)) # 378
# calculate how many haplotypes remain when remove <300 reads
length(which(haplotype_num_summary$total_reads_across_samples>=300)) # 357
# calculate how many haplotypes remain when remove <350 reads
length(which(haplotype_num_summary$total_reads_across_samples>=350)) # 331
# calculate how many haplotypes remain when remove <400 reads
length(which(haplotype_num_summary$total_reads_across_samples>=400)) # 312
# calculate how many haplotypes remain when remove <450 reads
length(which(haplotype_num_summary$total_reads_across_samples>=450)) # 291
# calculate how many haplotypes remain when remove <500 reads
length(which(haplotype_num_summary$total_reads_across_samples>=500)) # 271
# calculate how many haplotypes remain when remove <550 reads
length(which(haplotype_num_summary$total_reads_across_samples>=550)) # 252
# calculate how many haplotypes remain when remove <600 reads
length(which(haplotype_num_summary$total_reads_across_samples>=600)) # 244

# create a histogram of the read depths across haplotypes
hap_plot = ggplot(data=haplotype_num_summary,aes(x=haplotype_ids,y=total_reads_across_samples)) +
  geom_histogram(stat="identity") +
  geom_vline(yintercept = 50,xintercept=505,color="pink",lwd=1.5) +
  geom_vline(yintercept = 500,xintercept = 271, color = "orange",lwd=1.5) +
  theme_bw()
hap_plot

# censor haplotypes found with <500 reads 
haplotype_num_summary = haplotype_num_summary[-which(haplotype_num_summary$total_reads_across_samples < 500),]
# output the haplotype summary
write_csv(haplotype_num_summary,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/CSP/CSP_haplotype_num_summary.csv")

# enforce censoring to rds data set
foo = foo[,c(haplotype_num_summary$haplotype_ids)]

# write some code that summarizes the haplotypes across each sample for censored haplotypes
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary = data.frame("sample_names" = sample.names, "haplotype_number" = haplotype_num, "haplotype_reads" = haplotype_reads)
# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary$haplotype_reads == 0) # 3 samples removed
haplotype_summary = haplotype_summary[-needtoremove,]
# export as a csv
write_csv(haplotype_summary,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/CSP/CSP_sample_summary.csv")

# write out the haplotypes results as a fasta
uniquesToFasta(getUniques(foo), fout="Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/CSP/CSP_uniqueSeqs.fasta", ids=paste0("Seq", seq(length(getUniques(foo)))))

# rename the columns to be a unique haplotype column number
newcolnames = c(1:ncol(foo))
pastedcolnames = rep(NA,length(newcolnames))
for (i in 1:length(newcolnames)){
  pastedcolnames[i] = paste0("H",newcolnames[i])
}
colnames(foo) <- pastedcolnames

# make the matrix a dataframe
foo = as.data.frame(foo)

# create a new column of foo that is the sample names (rownames)
foo$`MiSeq.ID` = rownames(foo)
colnames(foo)

# censor the rds file
foo = foo[which(foo$MiSeq.ID %in% haplotype_summary$sample_names),]

# write out the new rds file with censoring enforced
write_csv(foo,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/CSP/CSP_spat21_mosquito_haplotypes_censored.csv")



#### --------- read in the anopheles demographic data set -------- ####

# read in the mosquito demographic data (merged anpopheles mosquito data set)
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the cleaned ama haplotype data
ama_haps = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/AMA/AMA_spat21_mosquito_haplotypes_censored.csv")

# read in the cleaned csp haplotype data
csp_haps = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/CSP/CSP_spat21_mosquito_haplotypes_censored.csv")

# read in the mosquito miseq inventory
miseq_inventory = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/mosquito_sequencing_inventory/spat21_mosquito_sequencing_inventory_with_CT_values.csv")

# cut down the miseq_inventory to just the columns of interest
miseq_inventory = miseq_inventory[,c(1,2,9,10)]
miseq_inventory = rename(miseq_inventory, "MiSeq.ID" = "MiSeq ID", "sample_id" = "Sample ID")

# merge in the mosquito miseq inventory with the ama haplotype data
ama_merge_data = left_join(miseq_inventory,ama_haps,by="MiSeq.ID")

# remove the controls (BF289 and BF294)
ama_merge_data = ama_merge_data[-which(ama_merge_data$MiSeq.ID == "BF289" | ama_merge_data$MiSeq.ID == "BF294"),]

# export the ama_merge_data
write_csv(ama_merge_data,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/AMA/AMA_spat21_mosquito_haplotypes_censored_final.csv")

# merge in the mosquito miseq inventory with the csp haplotype data
csp_merge_data = left_join(miseq_inventory,csp_haps,by="MiSeq.ID")

# remove the controls (BF289 and BF294)
csp_merge_data = csp_merge_data[-which(csp_merge_data$MiSeq.ID == "BF289" | csp_merge_data$MiSeq.ID == "BF294"),]

# export the csp_merge_data
write_csv(csp_merge_data,"Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/CSP/CSP_spat21_mosquito_haplotypes_censored_final.csv")

# read in the ama sample summary
ama_sample_summary = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/AMA/AMA_sample_summary.csv")

# read in the csp sample summary
csp_sample_summary = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/CSP/CSP_sample_summary.csv")


#### ------------- look at mosquito data summaries ----------- ####

# look at the summaries
summary(anoph_merged_data)
nrow(ama_haps)
ncol(ama_haps)
summary(ama_sample_summary$haplotype_reads)
nrow(csp_haps)
ncol(csp_haps)
summary(csp_sample_summary$haplotype_reads)
