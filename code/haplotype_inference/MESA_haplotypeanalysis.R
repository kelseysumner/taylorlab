# ------------------------------------ #
#       MESA Haplotype Analysis        #
#           July 31, 2018              #
#             K. Sumner                #
# ------------------------------------ #


### ----------------- AMA ------------------------ ###


# load in the data set (the haplotypes after chimeras have been removed - seqtab.nochim from dada2 r script)
foo <- readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/MESA_AMA_haplotypes.rds")


### --- look at the raw haplotype output --- #

# look at the data
head(foo)

# load in the MHCtools library
library(MHCtools)

# write some code that summarizes the haplotypes across each sample
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary_original = data.frame("Samples" = sample.names, "Haplotype.Number" = haplotype_num, "Haplotype.Reads" = haplotype_reads)

# write some code that summarizes the number of samples that have each haplotype
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
}
haplotype_num_summary_orginal = data.frame("Haplotypes" = haplotype.names, "Haplotypes.Across.Samples" = haplotypes_in_samples)


### --- enforce haplotype censoring --- #

# loop through each row in the foo data frame to look at number of reads in each haplotype for each sample
# do a nested for loop so loop through each sample then through each possible haplotype in that sample
# haplotype has to have >50 reads and be present in greater than 1% of the data
keep_hap_foo = foo
for (s in 1:nrow(foo)){
  hap_sum = sum(foo[s,])
  for (h in 1:ncol(foo)){
    if (foo[s,h] < 50 | foo[s,h]/hap_sum < 0.01) {
      keep_hap_foo[s,h] = 0
    }
  }
}

# write some code that summarizes the haplotypes across each sample for censored haplotypes
sample.names = row.names(keep_hap_foo)
haplotype_num = rep(NA,nrow(keep_hap_foo))
haplotype_reads = rep(NA,nrow(keep_hap_foo))
for (i in 1:nrow(keep_hap_foo)){
  haplotype_num[i] = length(which(keep_hap_foo[i,] > 0))
  haplotype_reads[i] = sum(keep_hap_foo[i,])
}
haplotype_summary = data.frame("Samples" = sample.names, "Haplotype.Number" = haplotype_num, "Haplotype.Reads" = haplotype_reads)


# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary$Haplotype.Reads == 0) # 4 reads were removed at indices: 10,99,198,332
keep_hap_foo = keep_hap_foo[-needtoremove,]
haplotype_summary = haplotype_summary[-needtoremove,]
write.csv(haplotype_summary,"haplotype_summary_censored.csv")


# write some code that summarizes the number of samples that have each haplotype for censored haplotypes
haplotype.names = rep(1:ncol(keep_hap_foo))
haplotypes_in_samples = rep(NA,ncol(keep_hap_foo))
for (k in 1:ncol(keep_hap_foo)){
  haplotypes_in_samples[k] = length(which(keep_hap_foo[,k] > 0))
}
haplotype_num_summary = data.frame("Haplotypes" = haplotype.names, "Haplotypes.Across.Samples" = haplotypes_in_samples)
# remove the haplotypes observed in 0 samples
indices = which(with(haplotype_num_summary, Haplotypes.Across.Samples > 0))
cut_haplotype_num_summary = haplotype_num_summary[indices,]
cut_haplotype_num_summary$New_Haplotypes_ID = rep(1:nrow(cut_haplotype_num_summary))
write.csv(cut_haplotype_num_summary, "cut_zeros_haplotype_num_summary_censored.csv")

# remove the columns in the data frame keep_hap_foo that have all 0s in them (these haplotypes were not found across any samples once censoring criteria was applied)
haplotype_indices = rep(NA,ncol(keep_hap_foo))
for (p in 1:ncol(keep_hap_foo)){
  if (sum(keep_hap_foo[,p] > 0)){
    haplotype_indices[p] = p
  }
}
# remove the NAs in the haplotype_indices vector
haplotype_indices <- haplotype_indices[!is.na(haplotype_indices)]
# remove those columns with no haplotypes observed anymore to get final data frame for haplotypes across samples
final_foo = keep_hap_foo[,haplotype_indices]

### --- write out the final haplotype data information ---#
# write out this dataframe as an R object
saveRDS(final_foo, "/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/MESA_AMA_haplotypes_final.rds")
# write out the haplotypes results as a fasta
uniquesToFasta(getUniques(final_foo), fout="/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/uniqueSeqs.fasta", ids=paste0("Seq", seq(length(getUniques(final_foo)))))



### ----------------- CSP ------------------------ ###


# load in the data set (the haplotypes after chimeras have been removed - seqtab.nochim from dada2 r script)
foo <- readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/MESA_CSP_haplotypes.rds")


### --- look at the raw haplotype output --- #

# look at the data
head(foo)

# load in the MHCtools library
library(MHCtools)

# write some code that summarizes the haplotypes across each sample
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary_original = data.frame("Samples" = sample.names, "Haplotype.Number" = haplotype_num, "Haplotype.Reads" = haplotype_reads)

# write some code that summarizes the number of samples that have each haplotype
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
}
haplotype_num_summary_orginal = data.frame("Haplotypes" = haplotype.names, "Haplotypes.Across.Samples" = haplotypes_in_samples)


### --- enforce haplotype censoring --- #

# loop through each row in the foo data frame to look at number of reads in each haplotype for each sample
# do a nested for loop so loop through each sample then through each possible haplotype in that sample
# haplotype has to have >50 reads and be present in greater than 1% of the data
keep_hap_foo = foo
for (s in 1:nrow(foo)){
  hap_sum = sum(foo[s,])
  for (h in 1:ncol(foo)){
    if (foo[s,h] < 50 | foo[s,h]/hap_sum < 0.01) {
      keep_hap_foo[s,h] = 0
    }
  }
}

# write some code that summarizes the haplotypes across each sample for censored haplotypes
sample.names = row.names(keep_hap_foo)
haplotype_num = rep(NA,nrow(keep_hap_foo))
haplotype_reads = rep(NA,nrow(keep_hap_foo))
for (i in 1:nrow(keep_hap_foo)){
  haplotype_num[i] = length(which(keep_hap_foo[i,] > 0))
  haplotype_reads[i] = sum(keep_hap_foo[i,])
}
haplotype_summary = data.frame("Samples" = sample.names, "Haplotype.Number" = haplotype_num, "Haplotype.Reads" = haplotype_reads)


# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary$Haplotype.Reads == 0) # 1 read was removed at index: 3 
keep_hap_foo = keep_hap_foo[-needtoremove,]
haplotype_summary = haplotype_summary[-needtoremove,]
write.csv(haplotype_summary,"haplotype_summary_censored.csv")


# write some code that summarizes the number of samples that have each haplotype for censored haplotypes
haplotype.names = rep(1:ncol(keep_hap_foo))
haplotypes_in_samples = rep(NA,ncol(keep_hap_foo))
for (k in 1:ncol(keep_hap_foo)){
  haplotypes_in_samples[k] = length(which(keep_hap_foo[,k] > 0))
}
haplotype_num_summary = data.frame("Haplotypes" = haplotype.names, "Haplotypes.Across.Samples" = haplotypes_in_samples)
# remove the haplotypes observed in 0 samples
indices = which(with(haplotype_num_summary, Haplotypes.Across.Samples > 0))
cut_haplotype_num_summary = haplotype_num_summary[indices,]
cut_haplotype_num_summary$New_Haplotypes_ID = rep(1:nrow(cut_haplotype_num_summary))
write.csv(cut_haplotype_num_summary, "cut_zeros_haplotype_num_summary_censored.csv")

# remove the columns in the data frame keep_hap_foo that have all 0s in them (these haplotypes were not found across any samples once censoring criteria was applied)
haplotype_indices = rep(NA,ncol(keep_hap_foo))
for (p in 1:ncol(keep_hap_foo)){
  if (sum(keep_hap_foo[,p] > 0)){
    haplotype_indices[p] = p
  }
}
# remove the NAs in the haplotype_indices vector
haplotype_indices <- haplotype_indices[!is.na(haplotype_indices)]
# remove those columns with no haplotypes observed anymore to get final data frame for haplotypes across samples
final_foo = keep_hap_foo[,haplotype_indices]

### --- write out the final haplotype data information ---#
# write out this dataframe as an R object
saveRDS(final_foo, "/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/MESA_CSP_haplotypes_final.rds")
# write out the haplotypes results as a fasta
uniquesToFasta(getUniques(final_foo), fout="/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/uniqueSeqs.fasta", ids=paste0("Seq", seq(length(getUniques(final_foo)))))



### ----------------- HISTB ------------------------ ###


# load in the data set (the haplotypes after chimeras have been removed - seqtab.nochim from dada2 r script)
foo <- readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/MESA_HistB_haplotypes.rds")


### --- look at the raw haplotype output --- #

# look at the data
head(foo)

# load in the MHCtools library
library(MHCtools)

# write some code that summarizes the haplotypes across each sample
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary_original = data.frame("Samples" = sample.names, "Haplotype.Number" = haplotype_num, "Haplotype.Reads" = haplotype_reads)

# write some code that summarizes the number of samples that have each haplotype
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
}
haplotype_num_summary_orginal = data.frame("Haplotypes" = haplotype.names, "Haplotypes.Across.Samples" = haplotypes_in_samples)


### --- enforce haplotype censoring --- #

# loop through each row in the foo data frame to look at number of reads in each haplotype for each sample
# do a nested for loop so loop through each sample then through each possible haplotype in that sample
# haplotype has to have >50 reads and be present in greater than 1% of the data
keep_hap_foo = foo
for (s in 1:nrow(foo)){
  hap_sum = sum(foo[s,])
  for (h in 1:ncol(foo)){
    if (foo[s,h] < 50 | foo[s,h]/hap_sum < 0.01) {
      keep_hap_foo[s,h] = 0
    }
  }
}

# write some code that summarizes the haplotypes across each sample for censored haplotypes
sample.names = row.names(keep_hap_foo)
haplotype_num = rep(NA,nrow(keep_hap_foo))
haplotype_reads = rep(NA,nrow(keep_hap_foo))
for (i in 1:nrow(keep_hap_foo)){
  haplotype_num[i] = length(which(keep_hap_foo[i,] > 0))
  haplotype_reads[i] = sum(keep_hap_foo[i,])
}
haplotype_summary = data.frame("Samples" = sample.names, "Haplotype.Number" = haplotype_num, "Haplotype.Reads" = haplotype_reads)


# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary$Haplotype.Reads == 0) # no reads needed to be removed
write.csv(haplotype_summary,"haplotype_summary_censored.csv")


# write some code that summarizes the number of samples that have each haplotype for censored haplotypes
haplotype.names = rep(1:ncol(keep_hap_foo))
haplotypes_in_samples = rep(NA,ncol(keep_hap_foo))
for (k in 1:ncol(keep_hap_foo)){
  haplotypes_in_samples[k] = length(which(keep_hap_foo[,k] > 0))
}
haplotype_num_summary = data.frame("Haplotypes" = haplotype.names, "Haplotypes.Across.Samples" = haplotypes_in_samples)
# remove the haplotypes observed in 0 samples
indices = which(with(haplotype_num_summary, Haplotypes.Across.Samples > 0))
cut_haplotype_num_summary = haplotype_num_summary[indices,]
cut_haplotype_num_summary$New_Haplotypes_ID = rep(1:nrow(cut_haplotype_num_summary))
write.csv(cut_haplotype_num_summary, "cut_zeros_haplotype_num_summary_censored.csv")

# remove the columns in the data frame keep_hap_foo that have all 0s in them (these haplotypes were not found across any samples once censoring criteria was applied)
haplotype_indices = rep(NA,ncol(keep_hap_foo))
for (p in 1:ncol(keep_hap_foo)){
  if (sum(keep_hap_foo[,p] > 0)){
    haplotype_indices[p] = p
  }
}
# remove the NAs in the haplotype_indices vector
haplotype_indices <- haplotype_indices[!is.na(haplotype_indices)]
# remove those columns with no haplotypes observed anymore to get final data frame for haplotypes across samples
final_foo = keep_hap_foo[,haplotype_indices]

### --- write out the final haplotype data information ---#
# write out this dataframe as an R object
saveRDS(final_foo, "/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/MESA_HistB_haplotypes_final.rds")
# write out the haplotypes results as a fasta
uniquesToFasta(getUniques(final_foo), fout="/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/uniqueSeqs.fasta", ids=paste0("Seq", seq(length(getUniques(final_foo)))))



