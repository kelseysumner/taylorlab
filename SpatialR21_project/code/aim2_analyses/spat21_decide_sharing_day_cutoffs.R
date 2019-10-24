# ----------------------------------------- #
#      Create aim 2 sharing day cutoffs     #
#             Mozzie Phase 1                #
#            AMA and CSP data               #
#            October 22, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(tidyverse)


#### ------- read in the data sets -------- ####

# read in the merged anpopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")

# read in the ama simplified edgelist (before any subsetting)
csp_edgelist = read_csv("Desktop/clean_ids_haplotype_results/CSP/CSP_haplotypes_edgelist_simplified_number_haps_shared.csv")


#### ------ clean up the edgelists to be in the proper format --------- ####

# first look at the columns
colnames(csp_edgelist)

# remove the X1 column
csp_edgelist = csp_edgelist %>%
  select(-"X1")

# look at how many unique observations
length(unique(csp_edgelist$from)) # 1280
length(unique(csp_edgelist$to)) # 1280
# looks correct

# subset the data set to just have heads in left column and mosquitoes in right column
table(nchar(csp_edgelist$from))
table(nchar(csp_edgelist$to))

# remove the rows where both the to and from columns are human samples
csp_edgelist = csp_edgelist %>%
  filter(!(str_detect(from,"-") & str_detect(to,"-"))) %>%
  filter(!(str_detect(from," ") & str_detect(to," "))) 

# make the first column human samples and second column mosquito samples
# create a for loop that checks to see if each sample is sharing with a mosquito or not
# switch the from column first
new_from = rep(NA,nrow(csp_edgelist))
new_to = rep(NA,nrow(csp_edgelist))
for (i in 1:nrow(csp_edgelist)){
  if (str_detect(csp_edgelist$from[i]," ")){
    new_from[i] = csp_edgelist$to[i]
    new_to[i] = csp_edgelist$from[i]
  } else {
    new_from[i] = csp_edgelist$from[i]
    new_to[i] = csp_edgelist$to[i]
  }
}
csp_edgelist$from = new_from
csp_edgelist$to = new_to

# rename the column headers in the csp edgelist
csp_edgelist = csp_edgelist %>%
  rename("sample_name_dbs"="from","sample_id_mosquito"="to","haps_shared"="weight")

# split up the edgelist into the shared mosquito heads and abdomens
csp_edgelist_head = csp_edgelist %>%
  filter(str_detect(sample_id_mosquito,"H")) %>%
  rename("sample_id_head"="sample_id_mosquito")
csp_edgelist_abdomen = csp_edgelist %>%
  filter(str_detect(sample_id_mosquito,"A")) %>%
  rename("sample_id_abdomen"="sample_id_mosquito")



#### --------- subset the human and mosquito data sets to just the variables of interest ---------- ####

# make sure main exposure and main outcome for primary case definition are factors
final_data$main_exposure_primary_case_def = as.factor(final_data$main_exposure_primary_case_def)
final_data$main_outcome_primary_case_def = as.factor(final_data$main_outcome_primary_case_def)

# select variables you need for human data
colnames(final_data)
human_data = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  select(visit_type,sample_id_date,sample_name_final,sample_name_dbs,age_cat_baseline,unq_memID,village_name,HH_ID,main_exposure_primary_case_def,main_outcome_primary_case_def) %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def))) %>%
  select(-main_exposure_primary_case_def,-main_outcome_primary_case_def,-visit_type)

# select variables you need for mosquito data
colnames(anoph_merged_data)
mosquito_data = anoph_merged_data %>%
  filter(!(is.na(sample_id_head) & is.na(sample_id_abdomen)) | sample_id_mosquito == "K01 00030" | sample_id_mosquito == "K01 00047") %>%
  select(HH_ID,collection_date,total_num_mosq_in_hh,sample_id_abdomen,sample_id_head,sample_id_mosquito)
# note: there are 15 entries where the lab didn't have mosquitoes so didn't have separate head and abdomen ids, removed these entries
# K01 00030 and K01 00047 were sequenced and pf positive but were original test samples so weren't in the normal qpcr data set
# add their information here for the data set ids
mosquito_data$sample_id_abdomen[which(mosquito_data$sample_id_mosquito=="K01 00030")] = "K01 A00030"
mosquito_data$sample_id_head[which(mosquito_data$sample_id_mosquito=="K01 00030")] = "K01 H00030"
mosquito_data$sample_id_abdomen[which(mosquito_data$sample_id_mosquito=="K01 00047")] = "K01 A00047"
mosquito_data$sample_id_head[which(mosquito_data$sample_id_mosquito=="K01 00047")] = "K01 H00047"


#### ------ set up the data set for csp sharing with abdomens ------- ####

## --- write code to work with the mosquito abdomens outcome

# add the human and mosquito data sets' variables to the edgelist
# the edgelist will be the level of analysis
csp_edgelist_abdomen = left_join(csp_edgelist_abdomen,human_data,by="sample_name_dbs")
csp_edgelist_abdomen = left_join(csp_edgelist_abdomen,mosquito_data,by="sample_id_abdomen")
# check the merge
csp_edgelist_abdomen %>%
  filter(is.na(sample_name_final)) %>%
  View()
csp_edgelist_abdomen %>%
  filter(is.na(collection_date)) %>%
  View()
# the samples that didn't merge did not meet the case definition for an asymptomatic or symptomatic infection
# this observation has been shown in the code chunk above around line 110

# rename some of the variables in the data set for clarity
colnames(csp_edgelist_abdomen)
csp_edgelist_abdomen = csp_edgelist_abdomen %>%
  rename("sample_id_human" = "sample_name_dbs","HH_ID_human"="HH_ID.x","HH_ID_mosquito"="HH_ID.y","human_date"="sample_id_date","mosquito_date"="collection_date") %>%
  select(-sample_id_head,-sample_id_mosquito) 
colnames(csp_edgelist_abdomen)

# first create a variable that is the time diff between human and mosquito samples subtract human time from mosquito time
# if time date difference is positive than the mosquito was collected after the human sample
csp_edgelist_abdomen = csp_edgelist_abdomen %>%
  mutate(date_difference = mosquito_date - human_date)

# now restrict the merged data set to only shared haplotypes with the same HH_ID
length(which(csp_edgelist_abdomen$HH_ID_human==csp_edgelist_abdomen$HH_ID_mosquito)) # 3 obs are in same HH
csp_edgelist_abdomen = csp_edgelist_abdomen %>%
  filter(HH_ID_human==HH_ID_mosquito)




#### -------- calculate the cutoffs for sharing with abdomens -------- ####

# now restrict the merged data set to only shared haplotypes in the correct time frame
# 0 to 7 days
csp_abdomen_cutoff_0to7 = csp_edgelist_abdomen %>%
  filter(date_difference >= 0 & date_difference <= 7) %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = number_with_a_shared_hap/number_of_pairs) %>%
  mutate(day_range = "0 to 7 days")
csp_abdomen_cutoff_0to8 = csp_edgelist_abdomen %>%
  filter(date_difference >= 0 & date_difference <= 8) %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = number_with_a_shared_hap/number_of_pairs) %>%
  mutate(day_range = "0 to 8 days")
# 0 to 9 days
csp_abdomen_cutoff_0to9 = csp_edgelist_abdomen %>%
  filter(date_difference >= 0 & date_difference <= 9) %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = number_with_a_shared_hap/number_of_pairs) %>%
  mutate(day_range = "0 to 9 days")
# 0 to 10 days
csp_abdomen_cutoff_0to10 = csp_edgelist_abdomen %>%
  filter(date_difference >= 0 & date_difference <= 10) %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = number_with_a_shared_hap/number_of_pairs) %>%
  mutate(day_range = "0 to 10 days")
# 0 to 11 days
csp_abdomen_cutoff_0to11 = csp_edgelist_abdomen %>%
  filter(date_difference >= 0 & date_difference <= 11) %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = number_with_a_shared_hap/number_of_pairs) %>%
  mutate(day_range = "0 to 11 days")
# 0 to 12 days
csp_abdomen_cutoff_0to12 = csp_edgelist_abdomen %>%
  filter(date_difference >= 0 & date_difference <= 12) %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = number_with_a_shared_hap/number_of_pairs) %>%
  mutate(day_range = "0 to 12 days")
# 0 to 13 days
csp_abdomen_cutoff_0to13 = csp_edgelist_abdomen %>%
  filter(date_difference >= 0 & date_difference <= 13) %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = number_with_a_shared_hap/number_of_pairs) %>%
  mutate(day_range = "0 to 13 days")
# 0 to 14 days
csp_abdomen_cutoff_0to14 = csp_edgelist_abdomen %>%
  filter(date_difference >= 0 & date_difference <= 14) %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = number_with_a_shared_hap/number_of_pairs) %>%
  mutate(day_range = "0 to 14 days")
# 0 to 15 days
csp_abdomen_cutoff_0to15 = csp_edgelist_abdomen %>%
  filter(date_difference >= 0 & date_difference <= 15) %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = number_with_a_shared_hap/number_of_pairs) %>%
  mutate(day_range = "0 to 15 days")
# 0 to 16 days
csp_abdomen_cutoff_0to16 = csp_edgelist_abdomen %>%
  filter(date_difference >= 0 & date_difference <= 16) %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = number_with_a_shared_hap/number_of_pairs) %>%
  mutate(day_range = "0 to 16 days")
# 0 to 17 days
csp_abdomen_cutoff_0to17 = csp_edgelist_abdomen %>%
  filter(date_difference >= 0 & date_difference <= 17) %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = number_with_a_shared_hap/number_of_pairs) %>%
  mutate(day_range = "0 to 17 days")
# 0 to 18 days
csp_abdomen_cutoff_0to18 = csp_edgelist_abdomen %>%
  filter(date_difference >= 0 & date_difference <= 18) %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = number_with_a_shared_hap/number_of_pairs) %>%
  mutate(day_range = "0 to 18 days")

# make one large data set of all the cutoffs
all_csp_abdomen_cutoffs = rbind(csp_abdomen_cutoff_0to7,csp_abdomen_cutoff_0to8,csp_abdomen_cutoff_0to9,csp_abdomen_cutoff_0to10,csp_abdomen_cutoff_0to11,
                                csp_abdomen_cutoff_0to12,csp_abdomen_cutoff_0to13,csp_abdomen_cutoff_0to14,csp_abdomen_cutoff_0to15,csp_abdomen_cutoff_0to16,
                                csp_abdomen_cutoff_0to17,csp_abdomen_cutoff_0to18)
all_csp_abdomen_cutoffs = data.frame(all_csp_abdomen_cutoffs)

# order the day cutoffs
day_order = c("0 to 7 days","0 to 8 days","0 to 9 days","0 to 10 days","0 to 11 days","0 to 12 days","0 to 13 days","0 to 14 days","0 to 15 days","0 to 16 days","0 to 17 days","0 to 18 days")
all_csp_abdomen_cutoffs <- within(all_csp_abdomen_cutoffs, day_range <- factor(day_range, levels=day_order))

# make plots of how the number of pairs changes over time
cols_for_symptom_status = c("#F1BB7B","#D67236")
csp_abdomen_cutoff_pairs_plot = ggplot() +
  geom_line(data=all_csp_abdomen_cutoffs,aes(x=day_range,y=number_of_pairs,group=aim2_exposure,col=aim2_exposure),cex=1.5) +
  theme_bw() +
  scale_colour_manual(values=cols_for_symptom_status) +
  xlab("Day range") + 
  ylab("Number of pairs within same household and day range") + 
  labs(colour="Symptomatic status")
csp_abdomen_cutoff_pairs_plot

# export the figure
ggsave(csp_abdomen_cutoff_pairs_plot, filename="/Users/kelseysumner/Desktop/csp_abdomen_cutoff_pairs_plot.png", device="png",
       height=5, width=14, units="in", dpi=400)



# make plots of how the number of pairs with at least 1 shared haplotype changes over time
cols_for_symptom_status = c("#F1BB7B","#D67236")
csp_abdomen_cutoff_number_shared_plot = ggplot() +
  geom_line(data=all_csp_abdomen_cutoffs,aes(x=day_range,y=number_with_a_shared_hap,group=aim2_exposure,col=aim2_exposure),cex=1.5) +
  theme_bw() +
  scale_colour_manual(values=cols_for_symptom_status) +
  xlab("Day range") + 
  ylab("Number with at least 1 haplotype shared") + 
  labs(colour="Symptomatic status")
csp_abdomen_cutoff_number_shared_plot

# export the figure
ggsave(csp_abdomen_cutoff_number_shared_plot, filename="/Users/kelseysumner/Desktop/csp_abdomen_cutoff_number_shared_plot.png", device="png",
       height=5, width=14, units="in", dpi=400)




# make plots of how the proportion shared changes over time
cols_for_symptom_status = c("#F1BB7B","#D67236")
csp_abdomen_cutoff_proportion_plot = ggplot() +
  geom_line(data=all_csp_abdomen_cutoffs,aes(x=day_range,y=prop_shared,group=aim2_exposure,col=aim2_exposure),cex=1.5) +
  theme_bw() +
  scale_colour_manual(values=cols_for_symptom_status) +
  xlab("Day range") + 
  ylab("Proportion sharing at least 1 haplotype") + 
  labs(colour="Symptomatic status")
csp_abdomen_cutoff_proportion_plot

# export the figure
ggsave(csp_abdomen_cutoff_proportion_plot, filename="/Users/kelseysumner/Desktop/csp_abdomen_cutoff_proportion_plot.png", device="png",
       height=5, width=14, units="in", dpi=400)


