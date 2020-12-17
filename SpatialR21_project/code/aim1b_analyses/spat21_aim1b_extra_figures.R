# -------------------------------------------------- #
#  Create aim 1b additional figures for manuscript   #
#                  Mozzie Phase 1                    #
#                 October 20, 2020                   #
#                    K. Sumner                       #
# -------------------------------------------------- #



#### --------- load packages ----------------- ####
library(tidyverse)
library(devtools)
library(streamgraph)
library(lubridate)
library(ggalluvial)
library(gridExtra)
library(ggsci)
library(ggrepel)
library(ggridges)




#### ---------- read in the data sets ---------- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA_and_CSP/final/model data/final_model_data/spat21_aim2_merged_data_with_weights_5MAR2020.rds")
# subset the data set to samples that passed pfcsp sequencing only
model_data = model_data %>%
  filter(!(is.na(csp_haps_shared)))

# read in the merged anopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the csp haplotype data
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
csp_haplotypes <- read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# read in the csp haplotype data
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
ama_haplotypes <- read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")




#### --------- make a figure for asymptomatic and symptomatic sample collection ----------- ####

# merge in information about if it is included in the CSP analysis or not
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_csp_final_model_data_21JUL2020.rds")
csp_samples = csp_data$sample_name_dbs
final_data$in_analysis = ifelse(final_data$sample_name_dbs %in% csp_samples,"yes","no")
table(final_data$in_analysis,useNA = "always")

# make a data set of just symptomatic infections
symptomatic_df = final_data %>%
  filter(visit_type == "monthly and sick visit" | visit_type== "sick visit") %>%
  select(c(unq_memID,sample_id_date,sample_name_final,HH_ID,village_name,main_outcome_primary_case_def,in_analysis)) %>%
  mutate(symp_infection = ifelse(!(is.na(main_outcome_primary_case_def)),"symptomatic infection","no infection"))
table(symptomatic_df$symp_infection, useNA = "always")

# create a new variable that is just the month
symptomatic_df$month = paste0(lubridate::month(symptomatic_df$sample_id_date),"-",lubridate::year(symptomatic_df$sample_id_date))
table(symptomatic_df$month, useNA = "always")

# make a data set of just asymptomatic infections
asymptomatic_df = final_data %>%
  filter(!(is.na(main_exposure_primary_case_def))) %>%
  select(c(unq_memID,sample_id_date,sample_name_final,HH_ID,village_name,main_exposure_primary_case_def,in_analysis))
table(asymptomatic_df$main_exposure_primary_case_def, useNA = "always")

# create a new variable that is just the month
asymptomatic_df$month = paste0(lubridate::month(asymptomatic_df$sample_id_date),"-",lubridate::year(asymptomatic_df$sample_id_date))
table(asymptomatic_df$month, useNA = "always")

# create a combined data frame of all the data frames
colnames(asymptomatic_df)
asymptomatic_df = asymptomatic_df %>%
  select(main_exposure_primary_case_def,sample_id_date,in_analysis) %>%
  mutate(type = "Asymptomatic visit") %>%
  rename(infection_status = main_exposure_primary_case_def,date = sample_id_date)
colnames(symptomatic_df)
symptomatic_df = symptomatic_df %>%
  select(main_outcome_primary_case_def,sample_id_date,in_analysis) %>%
  mutate(type = "Symptomatic visit") %>%
  rename(infection_status = main_outcome_primary_case_def,date=sample_id_date)
colnames(symptomatic_df)
colnames(asymptomatic_df)
all_df = rbind(symptomatic_df,asymptomatic_df)
all_df$infection_status = as.character(all_df$infection_status)
all_df$infection_status[which(all_df$infection_status == "symptomatic infection")] = "Positive"
all_df$infection_status[which(all_df$infection_status == "asymptomatic infection")] = "Positive"
all_df$infection_status[which(all_df$infection_status == "no infection")] = "Negative"
all_df$infection_status[which(is.na(all_df$infection_status))] = "Negative"
table(all_df$infection_status, useNA = "always")
all_df$infection_status = as.character(all_df$infection_status)
all_df$type_status = paste(all_df$type,all_df$infection_status)
table(all_df$type_status,useNA = "always")

# try a facet plot with bars
all_df$type = as.factor(all_df$type)
all_df$type = relevel(all_df$type,ref="Symptomatic visit")
all_df_neg = all_df %>% filter(infection_status=="Negative")
all_df_pos = all_df %>% filter(infection_status=="Positive")
all_df_neg = data.frame(all_df_neg)
all_df_pos = data.frame(all_df_pos)
small_all_df = all_df %>%
  mutate(new_date = floor_date(date,"week")) %>%
  group_by(new_date,type,infection_status,in_analysis) %>%
  summarize(n=n())
# make the plot
density_all_plot = ggplot(data=small_all_df,aes(x=new_date,fill=in_analysis,y=n)) + 
  facet_grid(type ~ .,switch = "y") +
  geom_histogram(stat="identity",color="black") +
  xlab("") +
  ylab("Number of samples collected") +
  scale_fill_manual(values = c("#636363","#cccccc")) +
  theme_bw() +
  scale_x_date(date_breaks="1 month",limits = as.Date(c("2017-06-01","2018-08-01"))) + 
  scale_y_continuous(limits = c(0,80),breaks=c(0,20,40,60,80),position = "right") + 
  theme(text = element_text(size=30),axis.text.x = element_text(angle = 90),legend.position=c(0.8,0.8),legend.background = element_rect(color = "black"),legend.title = element_text(size=10)) +
  labs(fill = "Included in outcome ascertainment")
density_all_plot
ggsave(density_all_plot, filename="/Users/kelseysumner/Desktop/sampling_all_plot_fig1_aim1b.png", device="png",
       height=12, width=20, units="in", dpi=500)



#### --------- make a figure for asymptomatic and symptomatic sample collection that doesn't show negative samples ----------- ####

# merge in information about if it is included in the CSP analysis or not
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_csp_final_model_data_21JUL2020.rds")
csp_samples = csp_data$sample_name_dbs
final_data$in_analysis = ifelse(final_data$sample_name_dbs %in% csp_samples,"yes","no")
table(final_data$in_analysis,useNA = "always")

# make a data set of just symptomatic infections
symptomatic_df = final_data %>%
  filter(visit_type == "monthly and sick visit" | visit_type== "sick visit") %>%
  select(c(unq_memID,sample_id_date,sample_name_final,HH_ID,village_name,main_outcome_primary_case_def,in_analysis)) %>%
  mutate(symp_infection = ifelse(!(is.na(main_outcome_primary_case_def)),"symptomatic infection","no infection"))
table(symptomatic_df$symp_infection, useNA = "always")

# create a new variable that is just the month
symptomatic_df$month = paste0(lubridate::month(symptomatic_df$sample_id_date),"-",lubridate::year(symptomatic_df$sample_id_date))
table(symptomatic_df$month, useNA = "always")

# make a data set of just asymptomatic infections
asymptomatic_df = final_data %>%
  filter(!(is.na(main_exposure_primary_case_def))) %>%
  select(c(unq_memID,sample_id_date,sample_name_final,HH_ID,village_name,main_exposure_primary_case_def,in_analysis))
table(asymptomatic_df$main_exposure_primary_case_def, useNA = "always")

# create a new variable that is just the month
asymptomatic_df$month = paste0(lubridate::month(asymptomatic_df$sample_id_date),"-",lubridate::year(asymptomatic_df$sample_id_date))
table(asymptomatic_df$month, useNA = "always")

# subset to just infections
asymptomatic_df = asymptomatic_df %>% filter(main_exposure_primary_case_def == "asymptomatic infection")
symptomatic_df = symptomatic_df %>% filter(symp_infection == "symptomatic infection")

# create a combined data frame of all the data frames
colnames(asymptomatic_df)
asymptomatic_df = asymptomatic_df %>%
  select(main_exposure_primary_case_def,sample_id_date,in_analysis) %>%
  mutate(type = "Asymptomatic visit") %>%
  rename("infection_status"="main_exposure_primary_case_def")
colnames(symptomatic_df)
symptomatic_df = symptomatic_df %>%
  select(symp_infection,sample_id_date,in_analysis) %>%
  mutate(type = "Symptomatic visit")
symptomatic_df = rename(symptomatic_df,"infection_status"="symp_infection")
colnames(symptomatic_df)
colnames(asymptomatic_df)
all_df = rbind(symptomatic_df,asymptomatic_df)
all_df$infection_status = as.character(all_df$infection_status)
all_df$infection_status[which(all_df$infection_status == "symptomatic infection")] = "Positive"
all_df$infection_status[which(all_df$infection_status == "asymptomatic infection")] = "Positive"
table(all_df$infection_status, useNA = "always")
all_df$infection_status = as.character(all_df$infection_status)
all_df$type_status = paste(all_df$type,all_df$infection_status)
table(all_df$type_status,useNA = "always")

# try a facet plot with bars
all_df$type = as.factor(all_df$type)
all_df$type = relevel(all_df$type,ref="Symptomatic visit")
all_df_pos = all_df %>% filter(infection_status=="Positive")
all_df_pos = data.frame(all_df_pos)
small_all_df = all_df %>%
  mutate(new_date = floor_date(sample_id_date,"week")) %>%
  group_by(new_date,type,in_analysis) %>%
  summarize(n=n())
# make the plot
density_all_plot = ggplot(data=small_all_df,aes(x=new_date,fill=in_analysis,y=n)) + 
  facet_grid(type ~ .,switch = "y") +
  geom_histogram(stat="identity",color="black",alpha=0.8) +
  xlab("") +
  ylab("Number of falciparum positive samples collected") +
  scale_fill_manual(values = c("#cccccc","#636363")) +
  theme_bw() +
  scale_x_date(date_breaks="1 month",limits = as.Date(c("2017-06-01","2018-08-01"))) + 
  scale_y_continuous(limits = c(0,80),breaks=c(0,20,40,60,80),position = "right") + 
  theme(text = element_text(size=30),axis.text.x = element_text(angle = 90),legend.position=c(0.77,0.85),legend.background = element_rect(color = "black")) +
  labs(fill = "Included in outcome ascertainment")
ggsave(density_all_plot, filename="/Users/kelseysumner/Desktop/sampling_all_plot_fig1_aim1b.png", device="png",
       height=12, width=20, units="in", dpi=500)













