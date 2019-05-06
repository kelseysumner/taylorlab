# ----------------------------------------- #
#       Spat21 Haplotype Output Cleaning    #
#               Mosquito Data               #
#         FINAL CENSORING VERSION           #
#               May 5, 2019                 #
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


#### ------- read in the AMA haplotype output -------------- ####

# read in the haplotype data set
foo = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output_new/AMA/AMA_spat21_mosquito_haplotypes.rds")

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
needtoremove = which(haplotype_summary$haplotype_reads == 0) # 2 samples removed
haplotype_summary = haplotype_summary[-needtoremove,]

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
needtoremove = which(haplotype_summary_censored$haplotype_reads == 0) # 2 samples removed
haplotype_summary_censored = haplotype_summary_censored[-needtoremove,]

# look at the controls


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

# enforce censoring to rds data set
foo = foo[,c(haplotype_num_summary$haplotype_ids)]

# write out the haplotypes results as a fasta
# uniquesToFasta(getUniques(foo), fout="Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/AMA/AMA_uniqueSeqs.fasta", ids=paste0("Seq", seq(length(getUniques(foo)))))

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
miseq_inventory = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/mosquito_sequencing_inventory/spat21_mosquito_sequencing_inventory_with_CT_values.csv")

# cut down the miseq_inventory to just the columns of interest
miseq_inventory = miseq_inventory[,c(1,2,9,10)]
miseq_inventory = rename(miseq_inventory, "MiSeq.ID" = "MiSeq ID", "sample_id" = "Sample ID")

# merge in the mosquito miseq inventory with the ama haplotype data
ama_merge_data = left_join(miseq_inventory,foo,by="MiSeq.ID")

# remove the controls (BF289 and BF294)
ama_merge_data = ama_merge_data[-which(ama_merge_data$MiSeq.ID == "BF289" | ama_merge_data$MiSeq.ID == "BF294"),]

# loop through the merged data for pfama1 and indicate whether mosquito part is an abdomen or head
mosquito_part_type = rep(NA,nrow(ama_merge_data))
for (i in 1:nrow(ama_merge_data)){
  if (str_detect(ama_merge_data$sample_id[i],"A")) {
    mosquito_part_type[i] = "Abdomen"
  }
  if (str_detect(ama_merge_data$sample_id[i],"H")) {
    mosquito_part_type[i] = "Head"
  }
}
ama_merge_data$mosquito_part_type = mosquito_part_type
table(ama_merge_data$mosquito_part_type, useNA = "always")
check_data = ama_merge_data[,c(1,2,478)]

# merge in the sample summaries with the ama data
haplotype_summary_censored = rename(haplotype_summary_censored,"MiSeq.ID"="sample_names")
ama_merge_data = left_join(ama_merge_data,haplotype_summary_censored,by="MiSeq.ID")

# create separate data sets for the mosquito heads and abdomens for ama
ama_merge_data_heads = ama_merge_data[which(ama_merge_data$mosquito_part_type == "Head"),]
ama_merge_data_abdomens = ama_merge_data[which(ama_merge_data$mosquito_part_type == "Abdomen"),]

# look at MOI summaries across mosquito parts
summary(ama_merge_data_heads$haplotype_number)
summary(ama_merge_data_abdomens$haplotype_number)

# create a summarized data frame of the number of abdomens with each MOI
ama_abdomen_moi_df <- ama_merge_data_abdomens %>% 
  filter(!(is.na(haplotype_number))) %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
ama_abdomen_moi_df$haplotype_number = as.numeric(ama_abdomen_moi_df$haplotype_number)
sum(ama_abdomen_moi_df$n) # 172+9 = 181

# create a summarized data frame of the number of heads with each MOI
ama_head_moi_df <- ama_merge_data_heads %>% 
  filter(!(is.na(haplotype_number))) %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
ama_head_moi_df$haplotype_number = as.numeric(ama_head_moi_df$haplotype_number)
sum(ama_head_moi_df$n) # 114+5 = 119

# make ama abdomen figure
ama_title <- expression(paste(italic("pfama1"), ": Mosquito abdomens"))
ama_abdomen_moi_plot = ggplot() +
  geom_bar(data=ama_abdomen_moi_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#F1BB7B") +
  labs(x="Multiplicity of infection", y="Number of mosquito parts", title= ama_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
ama_abdomen_moi_plot

# make ama head figure
ama_title <- expression(paste(italic("pfama1"), ": Mosquito heads"))
ama_head_moi_plot = ggplot() +
  geom_bar(data=ama_head_moi_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#D67236") +
  labs(x="Multiplicity of infection", y="Number of mosquito parts", title= ama_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
ama_head_moi_plot

# put both ama moi plots on same grid
figure1_ama = gridExtra::grid.arrange(ama_abdomen_moi_plot, ama_head_moi_plot, ncol=2)

# export ama moi plots
ggsave(figure1_ama, filename="/Users/kelseysumner/Desktop/figure1_ama.png", device="png",
height=10.5, width=11.2, units="in", dpi=400)




#### ------- read in the CSP haplotype output -------------- ####

# read in the haplotype data set
foo = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output_new/CSP/spat21_mosquitoes_CSP_haplotypes.rds")

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
needtoremove = which(haplotype_summary$haplotype_reads == 0) # 2 samples removed
haplotype_summary = haplotype_summary[-needtoremove,]

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
needtoremove = which(haplotype_summary_censored$haplotype_reads == 0) # 2 samples removed
haplotype_summary_censored = haplotype_summary_censored[-needtoremove,]

# look at the controls


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

# enforce censoring to rds data set
foo = foo[,c(haplotype_num_summary$haplotype_ids)]

# write out the haplotypes results as a fasta
uniquesToFasta(getUniques(foo), fout="Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output_new/CSP/CSP_uniqueSeqs.fasta", ids=paste0("Seq", seq(length(getUniques(foo)))))

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
miseq_inventory = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/mosquito_sequencing_inventory/spat21_mosquito_sequencing_inventory_with_CT_values_with_pilot.csv")

# cut down the miseq_inventory to just the columns of interest
miseq_inventory = miseq_inventory[,c(1,2,9,10)]
miseq_inventory = rename(miseq_inventory, "MiSeq.ID" = "MiSeq ID", "sample_id" = "Sample ID")

# merge in the mosquito miseq inventory with the ama haplotype data
csp_merge_data = left_join(miseq_inventory,foo,by="MiSeq.ID")

# remove the controls (BF289 and BF294 and BF303, BF304, BF305)
csp_merge_data = csp_merge_data[-which(csp_merge_data$MiSeq.ID == "BF289" | csp_merge_data$MiSeq.ID == "BF294" | csp_merge_data$MiSeq.ID == "BF303" | csp_merge_data$MiSeq.ID == "BF304" | csp_merge_data$MiSeq.ID == "BF305"),]

# loop through the merged data for pfcsp1 and indicate whether mosquito part is an abdomen or head
mosquito_part_type = rep(NA,nrow(csp_merge_data))
for (i in 1:nrow(csp_merge_data)){
  if (str_detect(csp_merge_data$sample_id[i],"A")) {
    mosquito_part_type[i] = "Abdomen"
  }
  if (str_detect(csp_merge_data$sample_id[i],"H")) {
    mosquito_part_type[i] = "Head"
  }
}
csp_merge_data$mosquito_part_type = mosquito_part_type
table(csp_merge_data$mosquito_part_type, useNA = "always")
check_data = csp_merge_data[,c(1,2,361)]

# merge in the sample summaries with the csp data
haplotype_summary_censored = rename(haplotype_summary_censored,"MiSeq.ID"="sample_names")
csp_merge_data = left_join(csp_merge_data,haplotype_summary_censored,by="MiSeq.ID")

# create separate data sets for the mosquito heads and abdomens for csp
csp_merge_data_heads = csp_merge_data[which(csp_merge_data$mosquito_part_type == "Head"),]
csp_merge_data_abdomens = csp_merge_data[which(csp_merge_data$mosquito_part_type == "Abdomen"),]

# look at MOI summaries across mosquito parts
summary(csp_merge_data_heads$haplotype_number)
summary(csp_merge_data_abdomens$haplotype_number)

# create a summarized data frame of the number of abdomens with each MOI
csp_abdomen_moi_df <- csp_merge_data_abdomens %>% 
  filter(!(is.na(haplotype_number))) %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
csp_abdomen_moi_df$haplotype_number = as.numeric(csp_abdomen_moi_df$haplotype_number)
sum(csp_abdomen_moi_df$n) # 172+9 = 181

# create a summarized data frame of the number of heads with each MOI
csp_head_moi_df <- csp_merge_data_heads %>% 
  filter(!(is.na(haplotype_number))) %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
csp_head_moi_df$haplotype_number = as.numeric(csp_head_moi_df$haplotype_number)
sum(csp_head_moi_df$n) # 114+5 = 119

# make csp abdomen figure
csp_title <- expression(paste(italic("pfcsp1"), ": Mosquito abdomens"))
csp_abdomen_moi_plot = ggplot() +
  geom_bar(data=csp_abdomen_moi_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#FD6467") +
  labs(x="Multiplicity of infection", y="Number of mosquito parts", title= csp_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_abdomen_moi_plot

# make csp head figure
csp_title <- expression(paste(italic("pfcsp1"), ": Mosquito heads"))
csp_head_moi_plot = ggplot() +
  geom_bar(data=csp_head_moi_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#5B1A18") +
  labs(x="Multiplicity of infection", y="Number of mosquito parts", title= csp_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_head_moi_plot

# put both csp moi plots on same grid
figure1_csp = gridExtra::grid.arrange(csp_abdomen_moi_plot, csp_head_moi_plot, ncol=2)

# export csp moi plots
ggsave(figure1_csp, filename="/Users/kelseysumner/Desktop/figure1_csp.png", device="png",
       height=10.5, width=11.2, units="in", dpi=400)











