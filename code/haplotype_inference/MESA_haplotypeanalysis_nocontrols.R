# ------------------------------------ #
#       MESA Haplotype Analysis        #
#           August 28, 2018            #
#             K. Sumner                #
# ------------------------------------ #


### ----------------- AMA ------------------------ ###


# load in the data set (the haplotypes after chimeras have been removed - seqtab.nochim from dada2 r script)
foo <- readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/MESA_AMA_haplotypes_final_clean_column_names.rds")


### --- look at the raw haplotype output --- #

# look at the data
head(foo)

# load in the MHCtools and dada2 library
library(MHCtools)
library(dada2)

# write some code that summarizes the haplotypes across each sample for censored haplotypes
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary = data.frame("Samples" = sample.names, "Haplotype.Number" = haplotype_num, "Haplotype.Reads" = haplotype_reads)


# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary$Haplotype.Reads == 0) # no additional reads were removed
write.csv(haplotype_summary,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_haplotype_summary_censored.csv")


# write some code that summarizes the number of samples that have each haplotype for censored haplotypes
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
}
haplotype_num_summary = data.frame("Haplotypes" = haplotype.names, "Haplotypes.Across.Samples" = haplotypes_in_samples)
# remove the haplotypes observed in 0 samples
indices = which(with(haplotype_num_summary, Haplotypes.Across.Samples > 0)) # 1 haplotype found in 0 samples now that controls are removed
cut_haplotype_num_summary = haplotype_num_summary[indices,]
cut_haplotype_num_summary$New_Haplotypes_ID = rep(1:nrow(cut_haplotype_num_summary))
write.csv(cut_haplotype_num_summary, "/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/cut_zeros_haplotype_num_summary_censored.csv")

# remove the columns in the data frame keep_hap_foo that have all 0s in them (these haplotypes were not found across any samples once censoring criteria was applied)
haplotype_indices = rep(NA,ncol(foo))
for (p in 1:ncol(foo)){
  if (sum(foo[,p] > 0)){
    haplotype_indices[p] = p
  }
}
# remove the NAs in the haplotype_indices vector
haplotype_indices <- haplotype_indices[!is.na(haplotype_indices)]
# remove those columns with no haplotypes observed anymore to get final data frame for haplotypes across samples
final_foo = foo[,haplotype_indices]

### --- write out the final haplotype data information ---#
# write out this dataframe as an R object
saveRDS(final_foo, "/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/MESA_AMA_haplotypes_final.rds")


### ----------------- CSP ------------------------ ###


# load in the data set (the haplotypes after chimeras have been removed - seqtab.nochim from dada2 r script)
foo <- readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/MESA_CSP_haplotypes_final_clean_column_names.rds")


### --- look at the raw haplotype output --- #

# look at the data
head(foo)

# load in the MHCtools and dada2 library
library(MHCtools)
library(dada2)

# write some code that summarizes the haplotypes across each sample for censored haplotypes
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary = data.frame("Samples" = sample.names, "Haplotype.Number" = haplotype_num, "Haplotype.Reads" = haplotype_reads)


# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary$Haplotype.Reads == 0) # no additional reads were removed
write.csv(haplotype_summary,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/CSP_haplotype_summary_censored.csv")


# write some code that summarizes the number of samples that have each haplotype for censored haplotypes
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
}
haplotype_num_summary = data.frame("Haplotypes" = haplotype.names, "Haplotypes.Across.Samples" = haplotypes_in_samples)
# remove the haplotypes observed in 0 samples
indices = which(with(haplotype_num_summary, Haplotypes.Across.Samples > 0)) # 1 haplotype found in 0 samples now that controls are removed
cut_haplotype_num_summary = haplotype_num_summary[indices,]
cut_haplotype_num_summary$New_Haplotypes_ID = rep(1:nrow(cut_haplotype_num_summary))
write.csv(cut_haplotype_num_summary, "/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/cut_zeros_haplotype_num_summary_censored.csv")

# remove the columns in the data frame keep_hap_foo that have all 0s in them (these haplotypes were not found across any samples once censoring criteria was applied)
haplotype_indices = rep(NA,ncol(foo))
for (p in 1:ncol(foo)){
  if (sum(foo[,p] > 0)){
    haplotype_indices[p] = p
  }
}
# remove the NAs in the haplotype_indices vector
haplotype_indices <- haplotype_indices[!is.na(haplotype_indices)]
# remove those columns with no haplotypes observed anymore to get final data frame for haplotypes across samples
final_foo = foo[,haplotype_indices]

### --- write out the final haplotype data information ---#
# write out this dataframe as an R object
saveRDS(final_foo, "/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/MESA_CSP_haplotypes_final.rds")



### ----------------- HISTB ------------------------ ###


# load in the data set (the haplotypes after chimeras have been removed - seqtab.nochim from dada2 r script)
foo <- readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/MESA_HistB_haplotypes_final_clean_column_names.rds")

### --- look at the raw haplotype output --- #

# look at the data
head(foo)

# load in the MHCtools and dada2 library
library(MHCtools)
library(dada2)

# write some code that summarizes the haplotypes across each sample for censored haplotypes
sample.names = row.names(foo)
haplotype_num = rep(NA,nrow(foo))
haplotype_reads = rep(NA,nrow(foo))
for (i in 1:nrow(foo)){
  haplotype_num[i] = length(which(foo[i,] > 0))
  haplotype_reads[i] = sum(foo[i,])
}
haplotype_summary = data.frame("Samples" = sample.names, "Haplotype.Number" = haplotype_num, "Haplotype.Reads" = haplotype_reads)


# remove samples that ended up with no reads at the end
needtoremove = which(haplotype_summary$Haplotype.Reads == 0) # no additional reads were removed
write.csv(haplotype_summary,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/HistB_haplotype_summary_censored.csv")


# write some code that summarizes the number of samples that have each haplotype for censored haplotypes
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
}
haplotype_num_summary = data.frame("Haplotypes" = haplotype.names, "Haplotypes.Across.Samples" = haplotypes_in_samples)
# remove the haplotypes observed in 0 samples
indices = which(with(haplotype_num_summary, Haplotypes.Across.Samples > 0)) # 0 haplotypes found in 0 samples now that controls are removed
cut_haplotype_num_summary = haplotype_num_summary[indices,]
cut_haplotype_num_summary$New_Haplotypes_ID = rep(1:nrow(cut_haplotype_num_summary))
write.csv(cut_haplotype_num_summary, "/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/cut_zeros_haplotype_num_summary_censored.csv")

# remove the columns in the data frame keep_hap_foo that have all 0s in them (these haplotypes were not found across any samples once censoring criteria was applied)
haplotype_indices = rep(NA,ncol(foo))
for (p in 1:ncol(foo)){
  if (sum(foo[,p] > 0)){
    haplotype_indices[p] = p
  }
}
# remove the NAs in the haplotype_indices vector
haplotype_indices <- haplotype_indices[!is.na(haplotype_indices)]
# remove those columns with no haplotypes observed anymore to get final data frame for haplotypes across samples
final_foo = foo[,haplotype_indices]

### --- write out the final haplotype data information ---#
# write out this dataframe as an R object
saveRDS(final_foo, "/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/MESA_HistB_haplotypes_final.rds")

