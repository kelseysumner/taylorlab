# -------------------------------------- #
#           Spat21/Mozzie Study          #
#  Pull out specific haplotypes for moi  #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#          December 31, 2019             #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)



#### ------- load in the data sets ------- ####

# read in the clean ama haplotype data
ama_haplotypes <- read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")

# read in the clean csp haplotype data
csp_haplotypes <- read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")



#### ------ loop through the ama haplotypes and output the specific haplotypes in each sample ------- ####

# edit the data set to be in the correct format
ama_haplotypes = as.matrix(ama_haplotypes)
rownames(ama_haplotypes) = ama_haplotypes[,2] # make sure is column of sample names
colnames(ama_haplotypes)
ama_haplotypes=ama_haplotypes[,-c(1,2,3,460,461,462)] # update to columns that are non-H#
colnames(ama_haplotypes)

# make ama_haplotypes values numeric
finalfoo_test = apply(ama_haplotypes, 2, as.numeric)
rownames(finalfoo_test) = rownames(ama_haplotypes)
ama_haplotypes = finalfoo_test

# summarize the haplotypes across each sample for censored haplotypes
sample.names = row.names(ama_haplotypes)
haplotype_num = rep(NA,nrow(ama_haplotypes))
haplotype_reads = rep(NA,nrow(ama_haplotypes))
moi_hap_vecs_output_final = rep(NA,nrow(ama_haplotypes))
for (i in 1:nrow(ama_haplotypes)){
  haplotype_num[i] = length(which(ama_haplotypes[i,] > 0))
  haplotype_reads[i] = sum(ama_haplotypes[i,])
  moi_hap_vecs_output = c()
  for (j in 1:ncol(ama_haplotypes)){
    if (ama_haplotypes[i,j]>0){
      moi_hap_vecs_output = c(moi_hap_vecs_output,names(ama_haplotypes[i,])[j])
    }
  }
  moi_hap_vecs_output_final[i] <- paste0(unique(moi_hap_vecs_output),collapse = ",")
}
haplotype_summary = data.frame("sample_name_dbs" = sample.names, "haplotype_number" = haplotype_num, "haplotype_reads" = haplotype_reads,"haplotype_list" = moi_hap_vecs_output_final)

# export the data set
write_csv(haplotype_summary,"Desktop/spat21_ama_summarized_haplotype_list_31DEC2019.csv")
write_rds(haplotype_summary,"Desktop/spat21_ama_summarized_haplotype_list_31DEC2019.rds")



#### ------ loop through the csp haplotypes and output the specific haplotypes in each sample ------- ####

# edit the data set to be in the correct format
csp_haplotypes = as.matrix(csp_haplotypes)
rownames(csp_haplotypes) = csp_haplotypes[,2] # make sure is column of sample names
colnames(csp_haplotypes)
csp_haplotypes=csp_haplotypes[,-c(1,2,3,302,303,304)] # update to non H# columns
colnames(csp_haplotypes)

# make csp_haplotypes values numeric
finalfoo_test = apply(csp_haplotypes, 2, as.numeric)
rownames(finalfoo_test) = rownames(csp_haplotypes)
csp_haplotypes = finalfoo_test

# summarize the haplotypes across each sample for censored haplotypes
sample.names = row.names(csp_haplotypes)
haplotype_num = rep(NA,nrow(csp_haplotypes))
haplotype_reads = rep(NA,nrow(csp_haplotypes))
moi_hap_vecs_output_final = rep(NA,nrow(csp_haplotypes))
for (i in 1:nrow(csp_haplotypes)){
  haplotype_num[i] = length(which(csp_haplotypes[i,] > 0))
  haplotype_reads[i] = sum(csp_haplotypes[i,])
  moi_hap_vecs_output = c()
  for (j in 1:ncol(csp_haplotypes)){
    if (csp_haplotypes[i,j]>0){
      moi_hap_vecs_output = c(moi_hap_vecs_output,names(csp_haplotypes[i,])[j])
    }
  }
  moi_hap_vecs_output_final[i] <- paste0(unique(moi_hap_vecs_output),collapse = ",")
}
haplotype_summary = data.frame("sample_name_dbs" = sample.names, "haplotype_number" = haplotype_num, "haplotype_reads" = haplotype_reads,"haplotype_list" = moi_hap_vecs_output_final)

# export the data set
write_csv(haplotype_summary,"Desktop/spat21_csp_summarized_haplotype_list_31DEC2019.csv")
write_rds(haplotype_summary,"Desktop/spat21_csp_summarized_haplotype_list_31DEC2019.rds")






