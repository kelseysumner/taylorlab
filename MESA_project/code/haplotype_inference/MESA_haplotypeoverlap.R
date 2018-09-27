# ----------------------------------------- #
#      MESA Batch 1 Haplotype Sharing       #
#                21AUG2018                  #
#                K. Sumner                  #
# ----------------------------------------- #

# load in packages
library(readr) # for reading in csvs using read_csv (more efficient and better at keeping data format)
library(dplyr) # for left_join function


#### ------------------ AMA -------------------- ####

# read in the track reads through pipeline document to reorganize for matching it up with samples
AMA_track = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/MESA_AMA_trackReadsThroughPipeline.csv")

# add a column with the sample numbers pulled out of the first column
split_sample_name = rep(NA,nrow(AMA_track))
for (i in 1:nrow(AMA_track)){
  firstsplit = strsplit(AMA_track$Sample[i],"_")[[1]][1]
  split_sample_name[i] = substring(firstsplit, 2)
}
AMA_track$Sample_order = as.integer(split_sample_name)

# reorder the data set 
neworder_sample_names = order(split_sample_name)
ordered_data = left_join(data.frame(Sample_order=neworder_sample_names),AMA_track,by="Sample_order")

# export the new ordered data set
write_csv(ordered_data,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/MESA_AMA_trackReadsThroughPipeline_Ordered.csv")
# paste in Betsy's sequence ID and MESA ID inventory in the new ordered data set

## ----- ##
# read back the data set that Betsy's sequence ID and MESA ID inventory have been merged with
AMA_ID = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/Prelim Materials/MESA_AMA_trackReadsThroughPipeline_Ordered.csv")

# split the MESA ID column
mesa_id = rep(NA,nrow(AMA_ID))
person = rep(NA,nrow(AMA_ID))
for (k in 1:nrow(AMA_ID)){
  part_mesa_id = strsplit(AMA_ID$`MESA ID`[k],"_")[[1]][1]
  if (nchar(part_mesa_id) == 4){
    mesa_id[k] = paste0("MPS",part_mesa_id)
  }
  if (nchar(part_mesa_id) == 3){
    mesa_id[k] = paste0("MPS","0",part_mesa_id)
  }
  if (nchar(part_mesa_id) == 2){
    mesa_id[k] = paste0("MPS","00",part_mesa_id)
  }
  if (nchar(part_mesa_id) == 1){
    mesa_id[k] = paste0("MPS","000",part_mesa_id)
  }
  person[k] = strsplit(AMA_ID$`MESA ID`[k],"_")[[1]][2]
}
AMA_ID$mesa_id_meta_data <- mesa_id
AMA_ID$person_meta_data <- person

# change the name of some of the columns
colnames(AMA_ID)[colnames(AMA_ID) == 'MESA ID'] <- 'lab_mesa_id'
colnames(AMA_ID)[colnames(AMA_ID) == 'MiSeq ID'] <- 'lab_miseq_id'
colnames(AMA_ID)[colnames(AMA_ID) == 'Sample'] <- 'lab_miseq_sample'
colnames(AMA_ID)[colnames(AMA_ID) == 'reads.in'] <- 'raw_reads'
colnames(AMA_ID)[colnames(AMA_ID) == 'reads.out'] <- 'filtered_reads'
colnames(AMA_ID)[colnames(AMA_ID) == 'merged'] <- 'merged_reads'
colnames(AMA_ID)[colnames(AMA_ID) == 'tabled'] <- 'total_tabled_haplotype_reads'
colnames(AMA_ID)[colnames(AMA_ID) == 'nonchim'] <- 'no_chimeras_haplotype_reads'

# make the control columns be indicated
AMA_ID$mesa_id_meta_data[512] = "Control"
AMA_ID$person_meta_data[512] = "Control"
AMA_ID$mesa_id_meta_data[513] = "Control"
AMA_ID$person_meta_data[513] = "Control"
AMA_ID$mesa_id_meta_data[514] = "Control"
AMA_ID$person_meta_data[514] = "Control"

# reorder the columns in the data set
AMA_ID = AMA_ID %>% select(lab_miseq_sample,lab_miseq_id,lab_mesa_id,mesa_id_meta_data,person_meta_data,raw_reads,filtered_reads,merged_reads,total_tabled_haplotype_reads,no_chimeras_haplotype_reads)

# export the data set
write_csv(AMA_ID,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/Prelim Materials/MESA_AMA_trackReadsThroughPipeline_Inventory.csv")

## ----- ##
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - MESA_AMA_haplotypes_final.rds)
AMA_data = readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/MESA_AMA_haplotypes_final.rds")

# rename the columns to be a unique haplotype column number
newcolnames = c(1:ncol(AMA_data))
pastedcolnames = rep(NA,length(newcolnames))
for (i in 1:length(newcolnames)){
  pastedcolnames[i] = paste0("H",newcolnames[i])
}
colnames(AMA_data) <- pastedcolnames

# remove the rows with the controls
control_list <- c("S512", "S513", "S514")
"%ni%" <- Negate("%in%")
AMA_data = subset(AMA_data, rownames(AMA_data) %ni% control_list)

# export the data set as something easier for others to analyze
saveRDS(AMA_data,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/MESA_AMA_haplotypes_final_clean_column_names.rds")


#### ------------------ CSP -------------------- ####

# read in the track reads through pipeline document to reorganize for matching it up with samples
CSP_track = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/MESA_CSP_trackReadsThroughPipeline.csv")

# add a column with the sample numbers pulled out of the first column
split_sample_name = rep(NA,nrow(CSP_track))
for (i in 1:nrow(CSP_track)){
  firstsplit = strsplit(CSP_track$Sample[i],"_")[[1]][1]
  split_sample_name[i] = substring(firstsplit, 2)
}
CSP_track$Sample_order = as.integer(split_sample_name)

# reorder the data set 
neworder_sample_names = order(split_sample_name)
ordered_data = left_join(data.frame(Sample_order=neworder_sample_names),CSP_track,by="Sample_order")

# export the new ordered data set
write_csv(ordered_data,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/MESA_CSP_trackReadsThroughPipeline_Ordered.csv")
# add Betsy's sequence ID and MESA ID inventory have been merged with

## ----- ##
# read back the data set that Betsy's sequence ID and MESA ID inventory have been merged with
CSP_ID = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/MESA_CSP_trackReadsThroughPipeline_Ordered.csv")

# split the MESA ID column
mesa_id = rep(NA,nrow(CSP_ID))
person = rep(NA,nrow(CSP_ID))
for (k in 1:nrow(CSP_ID)){
  part_mesa_id = strsplit(CSP_ID$`MESA ID`[k],"_")[[1]][1]
  if (nchar(part_mesa_id) == 4){
    mesa_id[k] = paste0("MPS",part_mesa_id)
  }
  if (nchar(part_mesa_id) == 3){
    mesa_id[k] = paste0("MPS","0",part_mesa_id)
  }
  if (nchar(part_mesa_id) == 2){
    mesa_id[k] = paste0("MPS","00",part_mesa_id)
  }
  if (nchar(part_mesa_id) == 1){
    mesa_id[k] = paste0("MPS","000",part_mesa_id)
  }
  person[k] = strsplit(CSP_ID$`MESA ID`[k],"_")[[1]][2]
}
CSP_ID$mesa_id_meta_data <- mesa_id
CSP_ID$person_meta_data <- person

# change the name of some of the columns
colnames(CSP_ID)[colnames(CSP_ID) == 'MESA ID'] <- 'lab_mesa_id'
colnames(CSP_ID)[colnames(CSP_ID) == 'MiSeq ID'] <- 'lab_miseq_id'
colnames(CSP_ID)[colnames(CSP_ID) == 'Sample'] <- 'lab_miseq_sample'
colnames(CSP_ID)[colnames(CSP_ID) == 'reads.in'] <- 'raw_reads'
colnames(CSP_ID)[colnames(CSP_ID) == 'reads.out'] <- 'filtered_reads'
colnames(CSP_ID)[colnames(CSP_ID) == 'merged'] <- 'merged_reads'
colnames(CSP_ID)[colnames(CSP_ID) == 'tabled'] <- 'total_tabled_haplotype_reads'
colnames(CSP_ID)[colnames(CSP_ID) == 'nonchim'] <- 'no_chimeras_haplotype_reads'

# make the control columns be indicated
CSP_ID$mesa_id_meta_data[512] = "Control"
CSP_ID$person_meta_data[512] = "Control"
CSP_ID$mesa_id_meta_data[513] = "Control"
CSP_ID$person_meta_data[513] = "Control"
CSP_ID$mesa_id_meta_data[514] = "Control"
CSP_ID$person_meta_data[514] = "Control"

# reorder the columns in the data set
CSP_ID = CSP_ID %>% select(lab_miseq_sample,lab_miseq_id,lab_mesa_id,mesa_id_meta_data,person_meta_data,raw_reads,filtered_reads,merged_reads,total_tabled_haplotype_reads,no_chimeras_haplotype_reads)

# export the data set
write_csv(CSP_ID,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/MESA_CSP_trackReadsThroughPipeline_Inventory.csv")


## ----- ##
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - MESA_CSP_haplotypes_final.rds)
CSP_data = readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/MESA_CSP_haplotypes_final.rds")

# rename the columns to be a unique haplotype column number
newcolnames = c(1:ncol(CSP_data))
pastedcolnames = rep(NA,length(newcolnames))
for (i in 1:length(newcolnames)){
  pastedcolnames[i] = paste0("H",newcolnames[i])
}
colnames(CSP_data) <- pastedcolnames

# remove the rows with the controls
control_list <- c("S512", "S513", "S514")
"%ni%" <- Negate("%in%")
CSP_data = subset(CSP_data, rownames(CSP_data) %ni% control_list)

# export the data set as something easier for others to analyze
saveRDS(CSP_data,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/MESA_CSP_haplotypes_final_clean_column_names.rds")


#### ------------------ HIST B -------------------- ####

# read in the track reads through pipeline document to reorganize for matching it up with samples
HistB_track = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/MESA_HistB_trackReadsThroughPipeline.csv")

# add a column with the sample numbers pulled out of the first column
split_sample_name = rep(NA,nrow(HistB_track))
for (i in 1:nrow(HistB_track)){
  firstsplit = strsplit(HistB_track$Sample[i],"_")[[1]][1]
  split_sample_name[i] = substring(firstsplit, 2)
}
HistB_track$Sample_order = as.integer(split_sample_name)

# reorder the data set 
neworder_sample_names = order(split_sample_name)
ordered_data = left_join(data.frame(Sample_order=neworder_sample_names),HistB_track,by="Sample_order")

# export the new ordered data set
write_csv(ordered_data,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/MESA_HistB_trackReadsThroughPipeline_Ordered.csv")
# add Betsy's sequence ID and MESA ID inventory have been merged with

## ----- ##
# read back the data set that Betsy's sequence ID and MESA ID inventory have been merged with
HistB_ID = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/MESA_HistB_trackReadsThroughPipeline_Ordered.csv")

# split the MESA ID column
mesa_id = rep(NA,nrow(HistB_ID))
person = rep(NA,nrow(HistB_ID))
for (k in 1:nrow(HistB_ID)){
  part_mesa_id = strsplit(HistB_ID$`MESA ID`[k],"_")[[1]][1]
  if (nchar(part_mesa_id) == 4){
    mesa_id[k] = paste0("MPS",part_mesa_id)
  }
  if (nchar(part_mesa_id) == 3){
    mesa_id[k] = paste0("MPS","0",part_mesa_id)
  }
  if (nchar(part_mesa_id) == 2){
    mesa_id[k] = paste0("MPS","00",part_mesa_id)
  }
  if (nchar(part_mesa_id) == 1){
    mesa_id[k] = paste0("MPS","000",part_mesa_id)
  }
  person[k] = strsplit(HistB_ID$`MESA ID`[k],"_")[[1]][2]
}
HistB_ID$mesa_id_meta_data <- mesa_id
HistB_ID$person_meta_data <- person

# change the name of some of the columns
colnames(HistB_ID)[colnames(HistB_ID) == 'MESA ID'] <- 'lab_mesa_id'
colnames(HistB_ID)[colnames(HistB_ID) == 'MiSeq ID'] <- 'lab_miseq_id'
colnames(HistB_ID)[colnames(HistB_ID) == 'Sample'] <- 'lab_miseq_sample'
colnames(HistB_ID)[colnames(HistB_ID) == 'reads.in'] <- 'raw_reads'
colnames(HistB_ID)[colnames(HistB_ID) == 'reads.out'] <- 'filtered_reads'
colnames(HistB_ID)[colnames(HistB_ID) == 'merged'] <- 'merged_reads'
colnames(HistB_ID)[colnames(HistB_ID) == 'tabled'] <- 'total_tabled_haplotype_reads'
colnames(HistB_ID)[colnames(HistB_ID) == 'nonchim'] <- 'no_chimeras_haplotype_reads'

# make the control columns be indicated
HistB_ID$mesa_id_meta_data[512] = "Control"
HistB_ID$person_meta_data[512] = "Control"
HistB_ID$mesa_id_meta_data[513] = "Control"
HistB_ID$person_meta_data[513] = "Control"
HistB_ID$mesa_id_meta_data[514] = "Control"
HistB_ID$person_meta_data[514] = "Control"

# reorder the columns in the data set
HistB_ID = HistB_ID %>% select(lab_miseq_sample,lab_miseq_id,lab_mesa_id,mesa_id_meta_data,person_meta_data,raw_reads,filtered_reads,merged_reads,total_tabled_haplotype_reads,no_chimeras_haplotype_reads)

# export the data set
write_csv(HistB_ID,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/MESA_HistB_trackReadsThroughPipeline_Inventory.csv")


## ------ ##
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - MESA_HistB_haplotypes_final.rds)
HistB_data = readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/MESA_HistB_haplotypes_final.rds")

# rename the columns to be a unique haplotype column number
newcolnames = c(1:ncol(HistB_data))
pastedcolnames = rep(NA,length(newcolnames))
for (i in 1:length(newcolnames)){
  pastedcolnames[i] = paste0("H",newcolnames[i])
}
colnames(HistB_data) <- pastedcolnames

# remove the rows with the controls
control_list <- c("S512", "S513", "S514")
"%ni%" <- Negate("%in%")
HistB_data = subset(HistB_data, rownames(HistB_data) %ni% control_list)

# export the data set as something easier for others to analyze
saveRDS(HistB_data,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/MESA_HistB_haplotypes_final_clean_column_names.rds")




