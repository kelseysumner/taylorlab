# -------------------------------- #
#  Look at prelminary descriptives #
#       parasite populations       #
#        TriCEM project            #
#        March 13, 2020            #
#           K. Sumner              #
# -------------------------------- #

#### ------- load libraries -------- ####
library(tidyverse)
library(gridExtra)
library(Biostrings)
library(schoolmath)
library(adegenet)



#### ------- read in the data set -------- ####

# read in the csp haplotype data
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
csp_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# read in the data set of csp haplotypes within each sample
csp_haplotype_summary = read_rds("Desktop/clean_ids_haplotype_results/CSP/haplotype_summary/spat21_csp_summarized_haplotype_list_31DEC2019.rds")

# read in the csp edgelist
edgelist_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/old/spat21_aim2_merged_data_with_weights_4FEB2020.rds")
edgelist_data = edgelist_data %>%
  filter(!(is.na(edgelist_data$csp_haps_shared)))

# read in the human demographic data
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")

# read in the mosquito demographic data
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")


#### ------- look at descriptives of the csp haplotypes ------ ####

# add sample type tot he csp_haplotype-summary list
csp_haplotypes_subset = csp_haplotypes %>%
  select(sample_name_dbs,sample_type)
csp_haplotype_summary = left_join(csp_haplotype_summary,csp_haplotypes_subset,by="sample_name_dbs")

# create a variable for vilage
csp_haplotype_summary$village_name = rep(NA,nrow(csp_haplotype_summary))
for (i in 1:nrow(csp_haplotype_summary)) {
  if (str_detect(csp_haplotype_summary$sample_name_dbs[i],"K")){
    csp_haplotype_summary$village_name[i] = "Kinesamo"
  }
  if (str_detect(csp_haplotype_summary$sample_name_dbs[i],"M")){
    csp_haplotype_summary$village_name[i] = "Maruti"
  }
  if (str_detect(csp_haplotype_summary$sample_name_dbs[i],"S")){
    csp_haplotype_summary$village_name[i] = "Sitabicha"
  }
}

# join the date information for humans and mosquitoes
final_data = final_data %>%
  dplyr::select(sample_name_dbs,sample_id_date) %>%
  dplyr::rename(date = sample_id_date)
anoph_merged_data_head = anoph_merged_data %>%
  dplyr::select(sample_id_head,collection_date) %>%
  dplyr::rename(sample_name_dbs = sample_id_head,date = collection_date) %>%
  filter(!(is.na(sample_name_dbs)))
anoph_merged_data_abdomen = anoph_merged_data %>%
  dplyr::select(sample_id_abdomen,collection_date) %>%
  dplyr::rename(sample_name_dbs = sample_id_abdomen,date = collection_date) %>%
  filter(!(is.na(sample_name_dbs)))
all_dem_data = rbind(final_data,anoph_merged_data_head,anoph_merged_data_abdomen)
csp_haplotype_summary = left_join(csp_haplotype_summary,all_dem_data,by="sample_name_dbs")
length(which(is.na(csp_haplotype_summary$date)))
csp_haplotype_summary %>%
  filter(is.na(date)) %>% 
  View()
# add some dates that were missing
csp_haplotype_summary$date[which(csp_haplotype_summary$sample_name_dbs == "K01 A00030")] = "2017-07-17"
csp_haplotype_summary$date[which(csp_haplotype_summary$sample_name_dbs == "K01 H00030")] = "2017-07-17"
csp_haplotype_summary$date[which(csp_haplotype_summary$sample_name_dbs == "K01 A00047")] = "2017-08-21"
csp_haplotype_summary$date[which(csp_haplotype_summary$sample_name_dbs == "K01 H00047")] = "2017-08-21"

# look at a summary of csp haplotypes
table(csp_haplotype_summary$sample_type, useNA = "always")

# create separate data sets based on the sample type
abdomen_data = csp_haplotype_summary %>%
  filter(sample_type=="Abdomen")
head_data = csp_haplotype_summary %>%
  filter(sample_type=="Head")
human_data = csp_haplotype_summary %>%
  filter(sample_type=="Human")
abdomen_data$haplotype_list = as.character(abdomen_data$haplotype_list)
head_data$haplotype_list = as.character(head_data$haplotype_list)
human_data$haplotype_list = as.character(human_data$haplotype_list)

# create vectors of unique haplotypes within each sample type
# for mosquito abdomens
abdomen_unique_haplotypes = c()
for (i in 1:nrow(abdomen_data)) {
  split = str_split(abdomen_data$haplotype_list[i],",")[[1]]
  abdomen_unique_haplotypes = c(abdomen_unique_haplotypes,split)
}
abdomen_unique_haplotypes = unique(abdomen_unique_haplotypes)
length(abdomen_unique_haplotypes)
# for mosquito heads
head_unique_haplotypes = c()
for (i in 1:nrow(head_data)) {
  split = str_split(head_data$haplotype_list[i],",")[[1]]
  head_unique_haplotypes = c(head_unique_haplotypes,split)
}
head_unique_haplotypes = unique(head_unique_haplotypes)
length(head_unique_haplotypes)
# for humans
human_unique_haplotypes = c()
for (i in 1:nrow(human_data)) {
  split = str_split(human_data$haplotype_list[i],",")[[1]]
  human_unique_haplotypes = c(human_unique_haplotypes,split)
}
human_unique_haplotypes = unique(human_unique_haplotypes)
length(human_unique_haplotypes)

# look at the overlap in haplotypes
# for humans with all else
other = c(abdomen_unique_haplotypes,head_unique_haplotypes)
other = unique(other)
length(setdiff(human_unique_haplotypes,other))
length(setdiff(human_unique_haplotypes,other))/length(human_unique_haplotypes)
# for abdomens with all else
other = c(human_unique_haplotypes,head_unique_haplotypes)
other = unique(other)
length(setdiff(abdomen_unique_haplotypes,other))
length(setdiff(abdomen_unique_haplotypes,other))/length(abdomen_unique_haplotypes)
# for heads with all else
other = c(human_unique_haplotypes,abdomen_unique_haplotypes)
other = unique(other)
length(setdiff(head_unique_haplotypes,other))
length(setdiff(head_unique_haplotypes,other))/length(head_unique_haplotypes)



#### -------- now look at different time intervals and sampling bias -------- ####

# first make separate data sets for each village from the different sample types
# for abdomens
abdomen_k = abdomen_data %>% filter(village_name == "Kinesamo")
abdomen_m = abdomen_data %>% filter(village_name == "Maruti")
abdomen_s = abdomen_data %>% filter(village_name == "Sitabicha")
# for heads
head_k = head_data %>% filter(village_name == "Kinesamo")
head_m = head_data %>% filter(village_name == "Maruti")
head_s = head_data %>% filter(village_name == "Sitabicha")
# for humans
human_k = human_data %>% filter(village_name == "Kinesamo")
human_m = human_data %>% filter(village_name == "Maruti")
human_s = human_data %>% filter(village_name == "Sitabicha")

# order the data by date
# for abdomens
abdomen_k = abdomen_k[order(abdomen_k$date),]
abdomen_m = abdomen_m[order(abdomen_m$date),]
abdomen_s = abdomen_s[order(abdomen_s$date),]
# for heads
head_k = head_k[order(head_k$date),]
head_m = head_m[order(head_m$date),]
head_s = head_s[order(head_s$date),]
# for humans
human_k = human_k[order(human_k$date),]
human_m = human_m[order(human_m$date),]
human_s = human_s[order(human_s$date),]

# set up a standard error function
se_function = function(x){sd(x)/sqrt(length(x))}

# now create a function for different time windows for data collection
counter_function  = function(x){
  
  # set up the empty vectors
  time_collection_window = c(15:60)
  time_frame = rep(NA,length(time_collection_window))
  median_x = rep(NA,length(time_collection_window))
  mean_x = rep(NA,length(time_collection_window))
  sd_x = rep(NA,length(time_collection_window))
  se_x = rep(NA,length(time_collection_window))
  
  # start the for loop
  for (i in 1:length(time_collection_window)){
    
    # set up the first start and end dates
    start_date = min(x$date)
    end_date = start_date + time_collection_window[i] 
    
    # set up an empty vector
    count_x=c()
    
    # tally up samples in each category by looping over rows
    while (end_date <= max(x$date)){
      
      # subset data to just the data between the start and end dates
      x_subset = x[which(x$date >= start_date & x$date <= end_date),]
      
      # start counting sample in each category
      count_x = cbind(count_x,nrow(x_subset))
      
      # add a count
      start_date = start_date + 1
      end_date = end_date + 1
      
    }
    
    # export total number of samples in each category for that time frame
    time_frame[i] = time_collection_window[i]
    median_x[i] = median(count_x)
    mean_x[i] = mean(count_x)
    sd_x[i] = sd(count_x)
    se_x[i] = se_function(count_x)
    
  }
  
  # create a data frame
  df_all_k = data.frame(time_frame,median_x,mean_x,sd_x,se_x)
  return(df_all_k)
  
}

# run the function for kinesamo
abdomen_k_collection_df = counter_function(abdomen_k)
head_k_collection_df = counter_function(head_k)
human_k_collection_df = counter_function(human_k)
# run the function for maruti
abdomen_m_collection_df = counter_function(abdomen_m)
head_m_collection_df = counter_function(head_m)
human_m_collection_df = counter_function(human_m)
# run the function for sitabicha
abdomen_s_collection_df = counter_function(abdomen_s)
head_s_collection_df = counter_function(head_s)
human_s_collection_df = counter_function(human_s)

# make plots of the data collection numbers for samples
# for kinesamo
kinesamo_plot = ggplot()+
  geom_line(data=abdomen_k_collection_df,aes(x=time_frame,y=mean_x),color="#7fc97f") + 
  geom_ribbon(data = abdomen_k_collection_df, aes(x=time_frame, ymin=mean_x-1.96*se_x, ymax=mean_x+1.96*se_x), fill = "#7fc97f",alpha = 0.7) +
  geom_line(data=head_k_collection_df,aes(x=time_frame,y=mean_x),color="#beaed4") + 
  geom_ribbon(data = head_k_collection_df, aes(x=time_frame, ymin=mean_x-1.96*se_x, ymax=mean_x+1.96*se_x), fill = "#beaed4",alpha=0.7) +
  geom_line(data=human_k_collection_df,aes(x=time_frame,y=mean_x),color="#fdc086") + 
  geom_ribbon(data = human_k_collection_df, aes(x=time_frame, ymin=mean_x-1.96*se_x, ymax=mean_x+1.96*se_x), fill = "#fdc086",alpha=0.7) +
  theme_bw() +
  ylab("Mean number of samples collected") +
  xlab("Time interval (days)") +
  labs(title="Kinesamo")
kinesamo_plot  
# for maruti
maruti_plot = ggplot()+
  geom_line(data=abdomen_m_collection_df,aes(x=time_frame,y=mean_x),color="#7fc97f") + 
  geom_ribbon(data = abdomen_m_collection_df, aes(x=time_frame, ymin=mean_x-1.96*se_x, ymax=mean_x+1.96*se_x), fill = "#7fc97f",alpha = 0.7) +
  geom_line(data=head_m_collection_df,aes(x=time_frame,y=mean_x),color="#beaed4") + 
  geom_ribbon(data = head_m_collection_df, aes(x=time_frame, ymin=mean_x-1.96*se_x, ymax=mean_x+1.96*se_x), fill = "#beaed4",alpha=0.7) +
  geom_line(data=human_m_collection_df,aes(x=time_frame,y=mean_x),color="#fdc086") + 
  geom_ribbon(data = human_m_collection_df, aes(x=time_frame, ymin=mean_x-1.96*se_x, ymax=mean_x+1.96*se_x), fill = "#fdc086",alpha=0.7) +
  theme_bw() +
  ylab("Mean number of samples collected") +
  xlab("Time interval (days)") +
  labs(title="Maruti")
maruti_plot  
# for sitabicha
sitabicha_plot = ggplot()+
  geom_line(data=abdomen_s_collection_df,aes(x=time_frame,y=mean_x),color="#7fc97f") + 
  geom_ribbon(data = abdomen_s_collection_df, aes(x=time_frame, ymin=mean_x-1.96*se_x, ymax=mean_x+1.96*se_x), fill = "#7fc97f",alpha = 0.7) +
  geom_line(data=head_s_collection_df,aes(x=time_frame,y=mean_x),color="#beaed4") + 
  geom_ribbon(data = head_s_collection_df, aes(x=time_frame, ymin=mean_x-1.96*se_x, ymax=mean_x+1.96*se_x), fill = "#beaed4",alpha=0.7) +
  geom_line(data=human_s_collection_df,aes(x=time_frame,y=mean_x),color="#fdc086") + 
  geom_ribbon(data = human_s_collection_df, aes(x=time_frame, ymin=mean_x-1.96*se_x, ymax=mean_x+1.96*se_x), fill = "#fdc086",alpha=0.7) +
  theme_bw() +
  ylab("Mean number of samples collected") +
  xlab("Time interval (days)") +
  labs(title="Sitabicha")
sitabicha_plot   

# export the plots
all_plot = grid.arrange(kinesamo_plot,maruti_plot,sitabicha_plot,nrow=3)
# ggsave(all_plot, filename="/Users/kelseysumner/Desktop/kinesamo_plot.png", device="png",
 # height=8, width=6, units="in", dpi=500)

  
#### --------- calculate hamming distances between haplotypes ----- ####

# read in the csp fasta file in two file formats
dna = readDNAStringSet("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_uniqueSeqs_final_censored.fasta")
haplotype_sequences = read_tsv("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_uniqueSeqs_final_censored.fasta")

# calculate the hamming distance between sequences
snp_output = stringDist(dna, method="hamming")
snp_output = as.matrix(snp_output)
max(snp_output)
summary(snp_output)

# reorganize the haplotype sequence fasta file to be in a better format
sequence_names = rep(NA,298)
sequences = rep(NA,298)
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
new_haplotype_sequences$sequence_names = str_remove(new_haplotype_sequences$sequence_names,">")

# change the haplotype names
# for abdomens
abdomen_unique_haplotypes_clean = str_remove(abdomen_unique_haplotypes,"H")
abdomen_unique_haplotypes_clean = paste0("Seq",abdomen_unique_haplotypes_clean)
# for heads
head_unique_haplotypes_clean = str_remove(head_unique_haplotypes,"H")
head_unique_haplotypes_clean = paste0("Seq",head_unique_haplotypes_clean)
# for humans
human_unique_haplotypes_clean = str_remove(human_unique_haplotypes,"H")
human_unique_haplotypes_clean = paste0("Seq",human_unique_haplotypes_clean)

# separate the fasta files into different datasets for each sample type
abdomen_unique_fasta = new_haplotype_sequences %>%
  filter(sequence_names %in% abdomen_unique_haplotypes_clean)
head_unique_fasta = new_haplotype_sequences %>%
  filter(sequence_names %in% head_unique_haplotypes_clean)
human_unique_fasta = new_haplotype_sequences %>%
  filter(sequence_names %in% human_unique_haplotypes_clean)












