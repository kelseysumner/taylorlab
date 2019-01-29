# ----------------------------------------- #
#       MESA Haplotype Output Cleaning      #
#             January 27, 2019              #
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

#### ------- clean up the sample inventory for miseq and MESA IDs ---------- ####

# read in the sample id inventory
sample_inventory = read_csv("Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/inventory/MESA_sequencing_ids_ALLSAMPLES.csv")

# clean up the sample inventory ids
new_bf_id = rep(NA,nrow(sample_inventory))
for (i in 1:nrow(sample_inventory)){
  if (i < 515){
    newid = strsplit(sample_inventory$`MiSeq ID`[i],"")[[1]]
    numberid = as.character(paste(newid[-c(1:2)],collapse = ""))
    new_bf_id[i] = paste0("S",numberid)
  } else {
    newid = strsplit(sample_inventory$`MiSeq ID`[i],"")[[1]]
    numberid = as.character(as.numeric(paste(newid[-c(1:2)],collapse = ""))+514)
    new_bf_id[i] = paste0("S",numberid)
  }
}
# replace the first column with the new column
sample_inventory$`MiSeq ID` = new_bf_id



#### ------- read in the AMA haplotype output -------------- ####

# read in the haplotype data set
foo = read_rds("Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/AMA/MESA_AMA_haplotypes.rds")

# figure out how many rows and columns
nrow(foo)
ncol(foo)

### --- look at the raw haplotype output 

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
needtoremove = which(haplotype_summary$Haplotype.Reads == 0) # 5 additional reads were removed
haplotype_summary = haplotype_summary[-needtoremove,]
write.csv(haplotype_summary,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/AMA/AMA_haplotype_summary_censored.csv")

# write out the haplotypes results as a fasta
uniquesToFasta(getUniques(foo), fout="/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/AMA/AMA_uniqueSeqs.fasta", ids=paste0("Seq", seq(length(getUniques(foo)))))

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
foo$`MiSeq ID` = rownames(foo)
colnames(foo)

# now merge in the sample names with the rownames
merged_data = left_join(foo,sample_inventory, by = "MiSeq ID")
head(merged_data$`MiSeq ID`)
head(merged_data$`MESA ID`)

# export the merged data as an RDS file
write_rds(merged_data,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/AMA/AMA_haplotype_output_with_sample_names.rds")


#### ------- read in the CSP haplotype output -------------- ####

# read in the haplotype data set
foo = read_rds("Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/CSP/MESA_CSP_haplotypes.rds")

# figure out how many rows and columns
nrow(foo)
ncol(foo)

### --- look at the raw haplotype output 

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
needtoremove = which(haplotype_summary$Haplotype.Reads == 0) # 5 additional reads were removed
haplotype_summary = haplotype_summary[-needtoremove,]
write.csv(haplotype_summary,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/CSP/CSP_haplotype_summary_censored.csv")

# write out the haplotypes results as a fasta
uniquesToFasta(getUniques(foo), fout="/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/CSP/CSP_uniqueSeqs.fasta", ids=paste0("Seq", seq(length(getUniques(foo)))))

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
foo$`MiSeq ID` = rownames(foo)
colnames(foo)

# now merge in the sample names with the rownames
merged_data = left_join(foo,sample_inventory, by = "MiSeq ID")
head(merged_data$`MiSeq ID`)
head(merged_data$`MESA ID`)

# export the merged data as an RDS file
write_rds(merged_data,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/CSP/CSP_haplotype_output_with_sample_names.rds")


#### ------- read in the HISTB haplotype output -------------- ####

# read in the haplotype data set
foo = read_rds("Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/HISTB/MESA_HISTB_haplotypes.rds")

# figure out how many rows and columns
nrow(foo)
ncol(foo)

### --- look at the raw haplotype output 

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
needtoremove = which(haplotype_summary$Haplotype.Reads == 0) # 5 additional reads were removed
haplotype_summary = haplotype_summary[-needtoremove,]
write.csv(haplotype_summary,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/HISTB/HISTB_haplotype_summary_censored.csv")

# write out the haplotypes results as a fasta
uniquesToFasta(getUniques(foo), fout="/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/HISTB/HISTB_uniqueSeqs.fasta", ids=paste0("Seq", seq(length(getUniques(foo)))))

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
foo$`MiSeq ID` = rownames(foo)
colnames(foo)

# now merge in the sample names with the rownames
merged_data = left_join(foo,sample_inventory, by = "MiSeq ID")
head(merged_data$`MiSeq ID`)
head(merged_data$`MESA ID`)

# export the merged data as an RDS file
write_rds(merged_data,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Haplotype analysis ALL data/haplotype_output/HISTB/HISTB_haplotype_output_with_sample_names.rds")




