# ----------------------------------------- #
#       Spat21 Haplotype Output Cleaning    #
#       All Samples (Human and Mosquito)    #
#         FINAL CENSORING VERSION           #
#           September 24, 2020              #
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
library(stringr)
library(Biostrings)
library(schoolmath)


#### ------- read in the AMA haplotype output -------------- ####

# read in the haplotype data set
foo = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Haplotype Results/AMA/raw haplotype output/AMA_spat21_allsamples_haplotypes.rds")

# figure out how many rows and columns
nrow(foo)
ncol(foo)
table(nchar(getSequences(foo)))

### --- look at the raw haplotype output and censor it

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
needtoremove = which(haplotype_summary$haplotype_reads == 0) # 30 samples to remove
haplotype_summary = haplotype_summary[-needtoremove,] 

# write some code that removes haplotypes that occur in <250 of the sample reads
for (i in 1:nrow(foo)){
  for (h in 1:ncol(foo)){
    if (foo[i,h] < 250 & !(is.na(foo[i,h])) & sum(foo[i,]) != 0){
      foo[i,h] = 0
    } else {
      foo[i,h] = foo[i,h]
    }
  }
}


# write some code that calculates what percentage each haplotype occurs in and removes haplotypes that occur in <3% of the sample reads
for (i in 1:nrow(foo)){
  for (h in 1:ncol(foo)){
    if ((foo[i,h]/sum(foo[i,])) < 0.03 & !(is.na(foo[i,h])) & sum(foo[i,]) != 0){
      foo[i,h] = 0
    } else {
      foo[i,h] = foo[i,h]
    }
  }
}

# for each haplotype that is a different length than the majority of haplotypes, throw it out
table(nchar(getSequences(foo))) # most haplotypes 300 bp, but 10 are not 300 bp long
nchar(getSequences(foo))
haps_to_remove = rep(NA,ncol(foo))
for (i in 1:ncol(foo)) {
  if (nchar(getSequences(foo))[i] != 300) {
    haps_to_remove[i] = i
  }
}
# check the output
haps_to_remove
length(which(!(is.na(haps_to_remove))))
# looks like coded correctly
# now remove those columns from the data set
haps_to_remove = na.omit(haps_to_remove)
foo = foo[,-haps_to_remove]
ncol(foo) # only 1493 columns left which is correct

# look at an updated haplotype summary
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary_censored = data.frame("sample_names" = sample.names, "haplotype_number" = haplotype_num, "haplotype_reads" = haplotype_reads)
# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary_censored$haplotype_reads == 0) # 256 sample removed
haplotype_summary_censored = haplotype_summary_censored[-needtoremove,]

# remove any samples that have no haplotypes anymore
foo = foo[(rownames(foo) %in% haplotype_summary_censored$sample_names),]
ncol(foo)
nrow(foo)

# tally up the number of SNPs between all haplotype pairings
uniquesToFasta(getUniques(foo), fout="Desktop/ama_snps_between_haps_within_samples.fasta", ids=paste0("Seq", seq(length(getUniques(foo)))))
dna = readDNAStringSet("Desktop/ama_snps_between_haps_within_samples.fasta")
snp_output = stringDist(dna, method="hamming")
snp_output = as.matrix(snp_output)
max(snp_output)
summary(snp_output)

# rename the columns to be a unique haplotype column number but create test data set for this
foo_test = foo
newcolnames = c(1:ncol(foo_test))
pastedcolnames = rep(NA,length(newcolnames))
for (i in 1:length(newcolnames)){
  pastedcolnames[i] = paste0("Seq",newcolnames[i])
}
colnames(foo_test) <- pastedcolnames

# figure out number of SNPS between haplotypes within each sample
all_cols = colnames(foo_test)
for (i in 1:nrow(foo_test)){
  hap_list = c()
  for (j in 1:ncol(foo_test)){
    if (foo_test[i,j] > 0) {
      hap_list = append(hap_list,all_cols[j])
    }
  }
  hap_list = unique(hap_list)
  for (k in 1:(length(hap_list))){
    if (length(hap_list) > 1){
      for (l in 1:(length(hap_list)-1)){
        if (!(is.null(hap_list)) & snp_output[hap_list[k],hap_list[l+1]] == 1 & foo_test[i,hap_list[k]]*8 < foo_test[i,hap_list[l+1]]) { 
          print(paste(rownames(foo_test)[i],hap_list[k]))
          foo_test[i,hap_list[k]] = 0
        } 
        if (!(is.null(hap_list)) & snp_output[hap_list[k],hap_list[l+1]] == 1 & foo_test[i,hap_list[l+1]]*8 < foo_test[i,hap_list[k]]) {
          print(paste(rownames(foo_test)[i],hap_list[l+1]))
          foo_test[i,hap_list[l+1]] = 0
        }
      }
    }
  }
}


# look at an updated haplotype summary
sample.names = row.names(foo_test)
haplotype_num = rep(NA,nrow(foo_test))
haplotype_reads = rep(NA,nrow(foo_test))
for (i in 1:nrow(foo_test)){
  haplotype_num[i] = length(which(foo_test[i,] > 0))
  haplotype_reads[i] = sum(foo_test[i,])
}
haplotype_summary_censored_final = data.frame("sample_names" = sample.names, "haplotype_number" = haplotype_num, "haplotype_reads" = haplotype_reads)
# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary_censored_final$haplotype_reads == 0) # 0 samples removed

# make foo, foo_test
orig_foo = foo
foo = foo_test

# make sure foo retains its column names
colnames(foo) = colnames(orig_foo)

# remove the controls and samples with empty entries
foo = foo[-which(rownames(foo) == "USID289" | rownames(foo) == "USID294" | rownames(foo) == "USID303" | rownames(foo) == "USID304" | rownames(foo) == "USID305" | rownames(foo) == "USID779" | rownames(foo) == "USID780" | rownames(foo) == "USID781" | rownames(foo) == "USID782" | rownames(foo) == "USID783" | rownames(foo) == "USID784" | rownames(foo) == "USID350" | rownames(foo) == "USID353" | rownames(foo) == "USID936"),]

# look at an updated haplotype summary
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary_censored_final = data.frame("sample_names" = sample.names, "haplotype_number" = haplotype_num, "haplotype_reads" = haplotype_reads)
# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary_censored_final$haplotype_reads == 0) # 0 samples removed

# summarize the samples for each haplotype
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
total_reads_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
  total_reads_in_samples[k] = sum(foo[,k],na.rm=T)
}
haplotype_num_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)
# remove haplotypes with 0 reads after censoring
haplotype_num_summary = haplotype_num_summary[which(haplotype_num_summary$total_reads_across_samples>0),]

# enforce censoring to rds data set
foo = foo[,c(haplotype_num_summary$haplotype_ids)]

# write out the haplotypes results as a fasta
uniquesToFasta(getUniques(foo), fout="Desktop/AMA_uniqueSeqs.fasta", ids=paste0("Seq", seq(length(getUniques(foo)))))
# created a sequence variant table with the haplotype sequences

### ---- read back in that haplotype sequence file

# read in the haplotype sequence fasta file
haplotype_sequences = read_tsv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Haplotype Results/AMA/raw variant table/AMA_uniqueSeqs.fasta")

# read in the haplotype sequence variant table
variant_table = read_tsv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Haplotype Results/AMA/raw variant table/spat21_all_samples_variant_table")

# filter variant table to see the variants that only occur in 1 haplotype
filtered_variant_table = variant_table %>%
  filter(`SNP %` == "0.20 %")

# reorganize the haplotype sequence fasta file to be in a better format
sequence_names = rep(NA,485) # number is number of haplotypes in fasta file of unique haplotype sequences
sequences = rep(NA,485)
sequence_names[1] = ">Seq1"
for (i in 1:nrow(haplotype_sequences)) {
  if (is.even(i)) {
    sequence_names[i+1] = haplotype_sequences$`>Seq1`[i]
  }
  if (is.odd(i)){
    sequences[i] = haplotype_sequences$`>Seq1`[i]
  }
}
new_haplotype_sequences = data.frame(sequence_names,sequences)
new_haplotype_sequences = na.omit(new_haplotype_sequences)
new_haplotype_sequences$sequences = as.character(new_haplotype_sequences$sequences)
new_haplotype_sequences$sequence_names = as.character(new_haplotype_sequences$sequence_names)

# now loop through each of the haplotype sequences and see if they have the variant for those variants that were only found in 1 haplotype
haplotypes_to_remove_from_final = rep(NA,nrow(variant_table))
for (i in 1:nrow(new_haplotype_sequences)){
  hap_chars = stringr::str_split(new_haplotype_sequences$sequences[i],"")
  for (k in 1:nrow(variant_table)){
    contig_position = filtered_variant_table$`Contig Pos`[k]
    called_base = filtered_variant_table$`Called Base`[k]
    if (hap_chars[[1]][as.numeric(contig_position)] == as.character(called_base) & !(is.na(hap_chars[[1]][as.numeric(contig_position)])) & !(is.na(as.character(called_base)))){
      haplotypes_to_remove_from_final[i] = new_haplotype_sequences$sequence_names[i]
    }
  }
}
# remove the missing values
haplotypes_to_remove_from_final = na.omit(haplotypes_to_remove_from_final)
length(haplotypes_to_remove_from_final) # 28
nrow(filtered_variant_table) # 28
haplotypes_to_remove_from_final = c(haplotypes_to_remove_from_final)
colnames(foo)
# pull out the haplotype sequences associated with each haplotype name
haplotype_sequences_to_remove = c(new_haplotype_sequences$sequences[which(new_haplotype_sequences$sequence_names %in% haplotypes_to_remove_from_final)])

# enforce censoring to rds data set
ncol(foo) # 485
foo = foo[,-(which(colnames(foo) %in% haplotype_sequences_to_remove))]
ncol(foo) # 457 (which is 485-28 and correct)

# look at an updated haplotype summary
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary_censored_final = data.frame("sample_names" = sample.names, "haplotype_number" = haplotype_num, "haplotype_reads" = haplotype_reads)
# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary_censored_final$haplotype_reads == 0) # 1 sample removed
haplotype_summary_censored_final = haplotype_summary_censored_final[-needtoremove,]

# remove any samples that have no haplotypes anymore
ncol(foo)
nrow(foo)
foo = foo[(rownames(foo) %in% haplotype_summary_censored_final$sample_names),]
ncol(foo)
nrow(foo)

# summarize the samples for each haplotype
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
total_reads_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
  total_reads_in_samples[k] = sum(foo[,k],na.rm=T)
}
haplotype_num_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)
# remove haplotypes with 0 reads after censoring
haplotype_num_summary = haplotype_num_summary[which(haplotype_num_summary$total_reads_across_samples>0),]

# enforce censoring to rds data set
foo = foo[,c(haplotype_num_summary$haplotype_ids)]
ncol(foo)
nrow(foo)

# write out the haplotypes results as a fasta
uniquesToFasta(getUniques(foo), fout="Desktop/spat21_AMA_uniqueSeqs_final_censored.fasta", ids=paste0("Seq", seq(length(getUniques(foo)))))
# created a sequence variant table with the haplotype sequences

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

# output the censored rds file
write_rds(foo,"Desktop/spat21_AMA_haplotype_table_censored_final_verison_24SEPT2019.rds")

### ------- now combine with the miseq inventory

# read in the miseq inventory
miseq_inventory = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Sequencing Inventories/All Samples/spat21_sequencing_all_samples_inventory_24SEP2019.csv")

# cut down the miseq_inventory to just the columns of interest
miseq_inventory = miseq_inventory[,c(2,3,4)]
miseq_inventory = dplyr::rename(miseq_inventory, "MiSeq.ID" = "New MiSeq ID","sample_id"="Sample ID")

# merge in the mosquito miseq inventory with the ama haplotype data
ama_merge_data = left_join(miseq_inventory,foo,by="MiSeq.ID")

# remove the controls 
ama_merge_data = ama_merge_data[-which(ama_merge_data$MiSeq.ID == "USID289" | ama_merge_data$MiSeq.ID == "USID294" | ama_merge_data$MiSeq.ID == "USID303" | ama_merge_data$MiSeq.ID == "USID304" | ama_merge_data$MiSeq.ID == "USID305" | ama_merge_data$MiSeq.ID == "USID779" | ama_merge_data$MiSeq.ID == "USID780" | ama_merge_data$MiSeq.ID == "USID781" | ama_merge_data$MiSeq.ID == "USID782" | ama_merge_data$MiSeq.ID == "USID783" | ama_merge_data$MiSeq.ID == "USID784" | ama_merge_data$MiSeq.ID == "USID350" | ama_merge_data$MiSeq.ID == "USID353" | ama_merge_data$MiSeq.ID == "USID936"),]

# loop through the merged data for pfama1 and indicate whether mosquito part is an abdomen or head
mosquito_part_type = rep(NA,nrow(ama_merge_data))
for (i in 1:nrow(ama_merge_data)){
  if (str_detect(ama_merge_data$sample_id[i],"A")) {
    mosquito_part_type[i] = "Abdomen"
  } else if (str_detect(ama_merge_data$sample_id[i],"H")) {
    mosquito_part_type[i] = "Head"
  } else {
    mosquito_part_type[i] = "Human"
  }
}
ama_merge_data$sample_type = mosquito_part_type
table(ama_merge_data$sample_type, useNA = "always")

# merge in the sample summaries with the ama data
haplotype_summary_censored_final = dplyr::rename(haplotype_summary_censored_final,"MiSeq.ID"="sample_names")
ama_merge_data = dplyr::left_join(ama_merge_data,haplotype_summary_censored_final,by="MiSeq.ID")

# remove samples without MOI
length(which(is.na(ama_merge_data$haplotype_number))) # 449
ama_merge_data = ama_merge_data %>%
  filter(!(is.na(haplotype_number)))

# write out as an RDS and CSV files
write_rds(ama_merge_data,"Desktop/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_24SEPT2019.rds")
write_csv(ama_merge_data,"Desktop/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_24SEPT2019.csv")




#### ------- read in the CSP haplotype output -------------- ####

# read in the haplotype data set
foo = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Haplotype Results/CSP/raw haplotype output/CSP_spat21_allsamples_haplotypes.rds")

# figure out how many rows and columns
nrow(foo)
ncol(foo)
table(nchar(getSequences(foo)))

### --- look at the raw haplotype output 

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
needtoremove = which(haplotype_summary$haplotype_reads == 0) # 1 sample removed
haplotype_summary = haplotype_summary[-needtoremove,]

# write some code that removes haplotypes that occur in <250 of the sample reads
for (i in 1:nrow(foo)){
  for (h in 1:ncol(foo)){
    if (foo[i,h] < 250 & !(is.na(foo[i,h])) & sum(foo[i,]) != 0){
      foo[i,h] = 0
    } else {
      foo[i,h] = foo[i,h]
    }
  }
}

# write some code that calculates what percentage each haplotype occurs in and removes haplotypes that occur in <3% of the sample reads
for (i in 1:nrow(foo)){
  for (h in 1:ncol(foo)){
    if ((foo[i,h]/sum(foo[i,])) < 0.03 & !(is.na(foo[i,h])) & sum(foo[i,]) != 0){
      foo[i,h] = 0
    } else {
      foo[i,h] = foo[i,h]
    }
  }
}

# for each haplotype that is a different length than the majority of haplotypes, throw it out
table(nchar(getSequences(foo))) # most haplotypes 288 bp, but 36 are not 288 bp long
nchar(getSequences(foo))
haps_to_remove = rep(NA,ncol(foo))
for (i in 1:ncol(foo)) {
  if (nchar(getSequences(foo))[i] != 288) {
    haps_to_remove[i] = i
  }
}
# check the output
haps_to_remove
length(which(!(is.na(haps_to_remove))))
# looks like coded correctly
# now remove those columns from the data set
haps_to_remove = na.omit(haps_to_remove)
foo = foo[,-haps_to_remove]
ncol(foo) # only 477 columns left which is correct

# look at an updated haplotype summary
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary_censored = data.frame("sample_names" = sample.names, "haplotype_number" = haplotype_num, "haplotype_reads" = haplotype_reads)
# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary_censored$haplotype_reads == 0) # 3 samples removed
haplotype_summary_censored = haplotype_summary_censored[-needtoremove,]

# remove any samples that have no haplotypes anymore
foo = foo[(rownames(foo) %in% haplotype_summary_censored$sample_names),]
ncol(foo)
nrow(foo)

# tally up the number of SNPs between all haplotype pairings
uniquesToFasta(getUniques(foo), fout="Desktop/csp_snps_between_haps_within_samples.fasta", ids=paste0("Seq", seq(length(getUniques(foo)))))
dna = readDNAStringSet("Desktop/csp_snps_between_haps_within_samples.fasta")
snp_output = stringDist(dna, method="hamming")
snp_output = as.matrix(snp_output)
max(snp_output)
summary(snp_output)

# rename the columns to be a unique haplotype column number but create test data set for this
foo_test = foo
newcolnames = c(1:ncol(foo_test))
pastedcolnames = rep(NA,length(newcolnames))
for (i in 1:length(newcolnames)){
  pastedcolnames[i] = paste0("Seq",newcolnames[i])
}
colnames(foo_test) <- pastedcolnames

# figure out number of SNPS between haplotypes within each sample
all_cols = colnames(foo_test)
for (i in 1:nrow(foo_test)){
  hap_list = c()
  for (j in 1:ncol(foo_test)){
    if (foo_test[i,j] > 0) {
      hap_list = append(hap_list,all_cols[j])
    }
  }
  hap_list = unique(hap_list)
  for (k in 1:(length(hap_list))){
    if (length(hap_list) > 1){
      for (l in 1:(length(hap_list)-1)){
        if (!(is.null(hap_list)) & snp_output[hap_list[k],hap_list[l+1]] == 1 & foo_test[i,hap_list[k]]*8 < foo_test[i,hap_list[l+1]]) { 
          print(paste(rownames(foo_test)[i],hap_list[k]))
          foo_test[i,hap_list[k]] = 0
        } 
        if (!(is.null(hap_list)) & snp_output[hap_list[k],hap_list[l+1]] == 1 & foo_test[i,hap_list[l+1]]*8 < foo_test[i,hap_list[k]]) {
          print(paste(rownames(foo_test)[i],hap_list[l+1]))
          foo_test[i,hap_list[l+1]] = 0
        }
      }
    }
  }
}


# look at an updated haplotype summary
sample.names = row.names(foo_test)
haplotype_num = rep(NA,nrow(foo_test))
haplotype_reads = rep(NA,nrow(foo_test))
for (i in 1:nrow(foo_test)){
  haplotype_num[i] = length(which(foo_test[i,] > 0))
  haplotype_reads[i] = sum(foo_test[i,])
}
haplotype_summary_censored_final = data.frame("sample_names" = sample.names, "haplotype_number" = haplotype_num, "haplotype_reads" = haplotype_reads)
# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary_censored_final$haplotype_reads == 0) # 0 samples removed

# look at the controls
control_check = haplotype_summary_censored_final[which(haplotype_summary_censored_final$sample_names %in% c("BF289","BF294","BF303","BF304","BF305")),]

# make foo, foo_test
orig_foo = foo
foo = foo_test

# make sure foo retains its column names
colnames(foo) = colnames(orig_foo)

# remove the controls and empty samples
foo = foo[-which(rownames(foo) == "USID289" | rownames(foo) == "USID294" | rownames(foo) == "USID303" | rownames(foo) == "USID304" | rownames(foo) == "USID305" | rownames(foo) == "USID779" | rownames(foo) == "USID780" | rownames(foo) == "USID781" | rownames(foo) == "USID782" | rownames(foo) == "USID783" | rownames(foo) == "USID784" | rownames(foo) == "USID350" | rownames(foo) == "USID353" | rownames(foo) == "USID936"),]

# look at an updated haplotype summary
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary_censored_final = data.frame("sample_names" = sample.names, "haplotype_number" = haplotype_num, "haplotype_reads" = haplotype_reads)
# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary_censored_final$haplotype_reads == 0) # 0 samples removed

# summarize the samples for each haplotype
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
total_reads_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
  total_reads_in_samples[k] = sum(foo[,k],na.rm=T)
}
haplotype_num_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)
# remove haplotypes with 0 reads after censoring
haplotype_num_summary = haplotype_num_summary[which(haplotype_num_summary$total_reads_across_samples>0),]

# enforce censoring to rds data set
foo = foo[,c(haplotype_num_summary$haplotype_ids)]

# write out the haplotypes results as a fasta
uniquesToFasta(getUniques(foo), fout="Desktop/CSP_uniqueSeqs.fasta", ids=paste0("Seq", seq(length(getUniques(foo)))))

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

# read in the mosquito miseq inventory
miseq_inventory = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Sequencing Information/Sequencing Inventories/All Samples/spat21_sequencing_all_samples_inventory_24SEP2019.csv")

# cut down the miseq_inventory to just the columns of interest
miseq_inventory = miseq_inventory[,c(2,3,4)]
miseq_inventory = dplyr::rename(miseq_inventory, "MiSeq.ID" = "New MiSeq ID","sample_id"="Sample ID")

# merge in the mosquito miseq inventory with the ama haplotype data
csp_merge_data = dplyr::left_join(miseq_inventory,foo,by="MiSeq.ID")

# remove the controls
csp_merge_data = csp_merge_data[-which(csp_merge_data$MiSeq.ID == "USID289" | csp_merge_data$MiSeq.ID == "USID294" | csp_merge_data$MiSeq.ID == "USID303" | csp_merge_data$MiSeq.ID == "USID304" | csp_merge_data$MiSeq.ID == "USID305" | csp_merge_data$MiSeq.ID == "USID779" | csp_merge_data$MiSeq.ID == "USID780" | csp_merge_data$MiSeq.ID == "USID781" | csp_merge_data$MiSeq.ID == "USID782" | csp_merge_data$MiSeq.ID == "USID783" | csp_merge_data$MiSeq.ID == "USID784" | ama_merge_data$MiSeq.ID == "USID350" | ama_merge_data$MiSeq.ID == "USID353" | ama_merge_data$MiSeq.ID == "USID936"),]

# loop through the merged data for pfcsp1 and indicate whether mosquito part is an abdomen or head
mosquito_part_type = rep(NA,nrow(csp_merge_data))
for (i in 1:nrow(csp_merge_data)){
  if (str_detect(csp_merge_data$sample_id[i],"A")) {
    mosquito_part_type[i] = "Abdomen"
  } else if (str_detect(csp_merge_data$sample_id[i],"H")) {
    mosquito_part_type[i] = "Head"
  } else {
    mosquito_part_type[i] = "Human"
  }
}
csp_merge_data$sample_type = mosquito_part_type
table(csp_merge_data$sample_type, useNA = "always")

# merge in the sample summaries with the csp data
haplotype_summary_censored = dplyr::rename(haplotype_summary_censored,"MiSeq.ID"="sample_names")
csp_merge_data = dplyr::left_join(csp_merge_data,haplotype_summary_censored,by="MiSeq.ID")





