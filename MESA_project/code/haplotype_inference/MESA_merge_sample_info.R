# ------------------------------------ #
#       MESA Haplotype Analysis        #
#       Merge All Sample Info          #
#           August 28, 2018            #
#             K. Sumner                #
# ------------------------------------ #


# load in packages
library(readr) # for reading in csvs using read_csv (more efficient and better at keeping data format)
library(dplyr) # for left_join function


#### ----------------- AMA ------------------------ ####

# read in the two data sets with sample info
# the data set with each sample and its haplotypes
sample_hap_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/Prelim Materials/AMA_haplotype_summary_censored.csv")
# the data set tracking reads throughout the pipeline
sample_track_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/Prelim Materials/MESA_AMA_trackReadsThroughPipeline_Inventory.csv")

# create a new column for the samples for merging in the sample_track_data
merged_id = rep(NA,nrow(sample_track_data))
for (j in 1:nrow(sample_track_data)){
  merged_id[j] = strsplit(sample_track_data$lab_miseq_sample[j],"_")[[1]][1]
}
sample_track_data$Samples = merged_id

# merge the two data sets
combined_data = left_join(sample_track_data,sample_hap_data,by="Samples")

# remove the Samples column
combined_data$Samples <- NULL

# rename the haplotype.number and haplotype.reads columns
colnames(combined_data)[colnames(combined_data) == 'Haplotype.Number'] <- 'number_of_haplotypes'
colnames(combined_data)[colnames(combined_data) == 'Haplotype.Reads'] <- 'total_reads_all_noncontrol_haplotypes'

# recode all the missing values in the two new columns to "Removed"
combined_data$number_of_haplotypes[is.na(combined_data$number_of_haplotypes)] = "Removed"
combined_data$total_reads_all_noncontrol_haplotypes[is.na(combined_data$total_reads_all_noncontrol_haplotypes)] = "Removed"

# reorder the columns in the data set
combined_data = combined_data %>% select(lab_miseq_sample,lab_miseq_id,lab_mesa_id,mesa_id_meta_data,person_meta_data,raw_reads,filtered_reads,merged_reads,total_tabled_haplotype_reads,no_chimeras_haplotype_reads,total_reads_all_noncontrol_haplotypes,number_of_haplotypes)

# export the final combined data set as a CSV
write_csv(combined_data,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_sample_summary.csv")



#### ----------------- CSP ------------------------ ####

# read in the two data sets with sample info
# the data set with each sample and its haplotypes
sample_hap_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/Prelim Materials/CSP_haplotype_summary_censored.csv")
# the data set tracking reads throughout the pipeline
sample_track_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/Prelim Materials/MESA_CSP_trackReadsThroughPipeline_Inventory.csv")

# create a new column for the samples for merging in the sample_track_data
merged_id = rep(NA,nrow(sample_track_data))
for (j in 1:nrow(sample_track_data)){
  merged_id[j] = strsplit(sample_track_data$lab_miseq_sample[j],"_")[[1]][1]
}
sample_track_data$Samples = merged_id

# merge the two data sets
combined_data = left_join(sample_track_data,sample_hap_data,by="Samples")

# remove the Samples column
combined_data$Samples <- NULL

# rename the haplotype.number and haplotype.reads columns
colnames(combined_data)[colnames(combined_data) == 'Haplotype.Number'] <- 'number_of_haplotypes'
colnames(combined_data)[colnames(combined_data) == 'Haplotype.Reads'] <- 'total_reads_all_noncontrol_haplotypes'

# recode all the missing values in the two new columns to "Removed"
combined_data$number_of_haplotypes[is.na(combined_data$number_of_haplotypes)] = "Removed"
combined_data$total_reads_all_noncontrol_haplotypes[is.na(combined_data$total_reads_all_noncontrol_haplotypes)] = "Removed"

# reorder the columns in the data set
combined_data = combined_data %>% select(lab_miseq_sample,lab_miseq_id,lab_mesa_id,mesa_id_meta_data,person_meta_data,raw_reads,filtered_reads,merged_reads,total_tabled_haplotype_reads,no_chimeras_haplotype_reads,total_reads_all_noncontrol_haplotypes,number_of_haplotypes)

# export the final combined data set as a CSV
write_csv(combined_data,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/CSP_sample_summary.csv")



#### ----------------- HistB ------------------------ ####

# read in the two data sets with sample info
# the data set with each sample and its haplotypes
sample_hap_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/Prelim Materials/HistB_haplotype_summary_censored.csv")
# the data set tracking reads throughout the pipeline
sample_track_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/Prelim Materials/MESA_HistB_trackReadsThroughPipeline_Inventory.csv")

# create a new column for the samples for merging in the sample_track_data
merged_id = rep(NA,nrow(sample_track_data))
for (j in 1:nrow(sample_track_data)){
  merged_id[j] = strsplit(sample_track_data$lab_miseq_sample[j],"_")[[1]][1]
}
sample_track_data$Samples = merged_id

# merge the two data sets
combined_data = left_join(sample_track_data,sample_hap_data,by="Samples")

# remove the Samples column
combined_data$Samples <- NULL

# rename the haplotype.number and haplotype.reads columns
colnames(combined_data)[colnames(combined_data) == 'Haplotype.Number'] <- 'number_of_haplotypes'
colnames(combined_data)[colnames(combined_data) == 'Haplotype.Reads'] <- 'total_reads_all_noncontrol_haplotypes'

# recode all the missing values in the two new columns to "Removed"
combined_data$number_of_haplotypes[is.na(combined_data$number_of_haplotypes)] = "Removed"
combined_data$total_reads_all_noncontrol_haplotypes[is.na(combined_data$total_reads_all_noncontrol_haplotypes)] = "Removed"

# reorder the columns in the data set
combined_data = combined_data %>% select(lab_miseq_sample,lab_miseq_id,lab_mesa_id,mesa_id_meta_data,person_meta_data,raw_reads,filtered_reads,merged_reads,total_tabled_haplotype_reads,no_chimeras_haplotype_reads,total_reads_all_noncontrol_haplotypes,number_of_haplotypes)

# export the final combined data set as a CSV
write_csv(combined_data,"/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/HistB_sample_summary.csv")


