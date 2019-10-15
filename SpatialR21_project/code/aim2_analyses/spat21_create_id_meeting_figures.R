# ----------------------------------------- #
# Create October 2019 ID Meeting Figures    #
#             Mozzie Phase 1                #
#            October 1, 2019                #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(tableone)
library(stringr)
library(ggplot2)


#### ---------- load in the data sets ---------- ####

# read in the merged anpopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")

# read in the pfama1 haplotype data set
ama_haplotype_df = read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# read in the pfcsp haplotype data set
csp_haplotype_df = read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")



#### ------- first document descriptives ------- ####

# make sure main exposure and main outcome for primary case definition are factors
final_data$main_exposure_primary_case_def = as.factor(final_data$main_exposure_primary_case_def)
final_data$main_outcome_primary_case_def = as.factor(final_data$main_outcome_primary_case_def)

# table total monthly follow-up per participant
total_follow_up_df = final_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  group_by(unq_memID) %>%
  summarise(total_follow_up = n())
# 239 ppts, that's correct

# look at the number of households
length(unique(final_data$HH_ID))

# calculate the average household size
all_households_householdsize_data = final_data%>%
  group_by(HH_ID) %>%
  summarize(n=n_distinct(unq_memID))
mean(all_households_householdsize_data$n)
sd(all_households_householdsize_data$n)

# look at total person-months of observation
length(which(final_data$visit_type=="monthly visit" | final_data$visit_type=="monthly and sick visit"))

# calculate gender
# females
participant_data_female = final_data %>%
  filter(gender == "female") %>%
  group_by(unq_memID) %>%
  summarize(n=n())
# males
participant_data_male = final_data %>%
  filter(gender == "male") %>%
  group_by(unq_memID) %>%
  summarize(n=n())
# look at the intercept
intersect(participant_data_female$unq_memID,participant_data_male$unq_memID) # 0, correct

# look at the age categories
participant_data = final_data %>%
  group_by(unq_memID,age_cat_baseline) %>%
  summarize(n=n()) %>%
  group_by(age_cat_baseline) %>%
  summarize(totaln = n())

# calculate how many people sleep under net regularly
participant_data = final_data %>%
  group_by(unq_memID) %>%
  summarize(slept_avg=mean(slept_times, na.rm =T))
# make a variable that indicates some slept under a net more than usual
slept_under_net_regularly = ifelse(is.na(participant_data$slept_avg),NA,ifelse(participant_data$slept_avg>5,1,0))
table(slept_under_net_regularly,participant_data$slept_avg, useNA = "always")
participant_data$slept_under_net_regularly = slept_under_net_regularly
participant_data_v2 = participant_data %>%
  group_by(slept_under_net_regularly) %>%
  summarize(totaln = n())

# calculate how many P. falciparum infections occurred in humans based on rdt results
table(final_data$rdt_rst, useNA = "always")

# calculate how many P. falciparum infections occurred in humans based on rdt results
table(final_data$pf_pcr_infection_status, useNA = "always")

# calculate how many asymptomatic vs. symptomatic infections occurred
table(final_data$aim2_exposure)

# select variables you need for human data
colnames(final_data)
human_data = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  select(visit_type,sample_id_date,sample_name_final,unq_memID,age_cat_baseline,village_name,HH_ID,main_exposure_primary_case_def,main_outcome_primary_case_def) %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def))) %>%
  select(-main_exposure_primary_case_def,-main_outcome_primary_case_def)

# look at how many asymptomatic and symptomatic infections there are
table(human_data$aim2_exposure, useNA = "always")

# table total monthly follow-up per participant in the data set of just asymptomatic and symptomatic infections
total_follow_up_df = human_data %>%
  filter(visit_type == "monthly visit" | visit_type == "monthly and sick visit") %>%
  group_by(unq_memID) %>%
  summarise(total_follow_up = n())
# 239 ppts, that's correct



#### --------- create figure for moi ------------- ####

### create histograms of overall moi

# create a summarized data frame of the number of abdomens with each MOI
# for ama
ama_moi_df <- ama_haplotype_df %>% 
  filter(!(is.na(haplotype_number))) %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
ama_moi_df$haplotype_number = as.numeric(ama_moi_df$haplotype_number)
sum(ama_moi_df$n) 
# for csp
csp_moi_df <- csp_haplotype_df %>% 
  filter(!(is.na(haplotype_number))) %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
csp_moi_df$haplotype_number = as.numeric(csp_moi_df$haplotype_number)
sum(csp_moi_df$n) 

# make ama moi figure
ama_title <- expression(paste(italic("pfama1"), ": MOI"))
ama_moi_plot = ggplot() +
  geom_bar(data=ama_moi_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#F1BB7B") +
  labs(x="Multiplicity of infection", y="Number of samples", title= ama_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,320,360), limits=c(0,360)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
ama_moi_plot

# make csp moi figure
csp_title <- expression(paste(italic("pfcsp"), ": MOI"))
csp_moi_plot = ggplot() +
  geom_bar(data=csp_moi_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#D67236") +
  labs(x="Multiplicity of infection", y="Number of samples", title= csp_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,320,360), limits=c(0,360)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_moi_plot

# put both ama moi plots on same grid
figure1_total_moi = gridExtra::grid.arrange(ama_moi_plot, csp_moi_plot, ncol=2)

# export the figure
ggsave(figure1_total_moi, filename="/Users/kelseysumner/Desktop/figure1_total_moi.png", device="png",
height=10.5, width=11.2, units="in", dpi=400)



### create histograms of moi subset by sample type

# create a summarized data frame of the number of abdomens with each MOI for ama
# for humans
ama_human_df <- ama_haplotype_df %>% 
  filter(!(is.na(haplotype_number)) & sample_type=="Human") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
ama_human_df$haplotype_number = as.numeric(ama_human_df$haplotype_number)
sum(ama_human_df$n) 
# for abdomens
ama_abdomen_df <- ama_haplotype_df %>% 
  filter(!(is.na(haplotype_number)) & sample_type=="Abdomen") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
ama_abdomen_df$haplotype_number = as.numeric(ama_abdomen_df$haplotype_number)
sum(ama_abdomen_df$n)
# for heads
ama_head_df <- ama_haplotype_df %>% 
  filter(!(is.na(haplotype_number)) & sample_type=="Head") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
ama_head_df$haplotype_number = as.numeric(ama_head_df$haplotype_number)
sum(ama_head_df$n)

# make ama moi figures by sample type
# for human samples
ama_human_title <- expression(paste(italic("pfama1"), ": Human DBS"))
ama_human_plot = ggplot() +
  geom_bar(data=ama_human_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#F1BB7B") +
  labs(x="Multiplicity of infection", y="Number of samples", title= ama_human_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,320), limits=c(0,320)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
ama_human_plot
# for abdomen samples
ama_abdomen_title <- expression(paste(italic("pfama1"), ": Mosquito abdomens"))
ama_abdomen_plot = ggplot() +
  geom_bar(data=ama_abdomen_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#D67236") +
  labs(x="Multiplicity of infection", y="Number of samples", title= ama_abdomen_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,320), limits=c(0,320)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
ama_abdomen_plot
# for head samples
ama_head_title <- expression(paste(italic("pfama1"), ": Mosquito Heads"))
ama_head_plot = ggplot() +
  geom_bar(data=ama_head_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="grey") +
  labs(x="Multiplicity of infection", y="Number of samples", title= ama_head_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,320), limits=c(0,320)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
ama_head_plot

# put both ama moi plots on same grid
figure2_ama_subset_moi = gridExtra::grid.arrange(ama_human_plot,ama_abdomen_plot,ama_head_plot,ncol=3)

# export the figure
ggsave(figure2_ama_subset_moi, filename="/Users/kelseysumner/Desktop/figure2_ama_subset_moi.png", device="png",
       height=10.5, width=17, units="in", dpi=400)




# create a summarized data frame of the number of abdomens with each MOI for csp
# for humans
csp_human_df <- csp_haplotype_df %>% 
  filter(!(is.na(haplotype_number)) & sample_type=="Human") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
csp_human_df$haplotype_number = as.numeric(csp_human_df$haplotype_number)
sum(csp_human_df$n) 
# for abdomens
csp_abdomen_df <- csp_haplotype_df %>% 
  filter(!(is.na(haplotype_number)) & sample_type=="Abdomen") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
csp_abdomen_df$haplotype_number = as.numeric(csp_abdomen_df$haplotype_number)
sum(csp_abdomen_df$n)
# for heads
csp_head_df <- csp_haplotype_df %>% 
  filter(!(is.na(haplotype_number)) & sample_type=="Head") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
csp_head_df$haplotype_number = as.numeric(csp_head_df$haplotype_number)
sum(csp_head_df$n)

# make csp moi figures by sample type
# for human samples
csp_human_title <- expression(paste(italic("pfcsp"), ": Human DBS"))
csp_human_plot = ggplot() +
  geom_bar(data=csp_human_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#F1BB7B") +
  labs(x="Multiplicity of infection", y="Number of samples", title= csp_human_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,320), limits=c(0,320)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_human_plot
# for abdomen samples
csp_abdomen_title <- expression(paste(italic("pfcsp"), ": Mosquito abdomens"))
csp_abdomen_plot = ggplot() +
  geom_bar(data=csp_abdomen_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#D67236") +
  labs(x="Multiplicity of infection", y="Number of samples", title= csp_abdomen_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,320), limits=c(0,320)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_abdomen_plot
# for head samples
csp_head_title <- expression(paste(italic("pfcsp"), ": Mosquito Heads"))
csp_head_plot = ggplot() +
  geom_bar(data=csp_head_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="grey") +
  labs(x="Multiplicity of infection", y="Number of samples", title= csp_head_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,320), limits=c(0,320)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_head_plot

# put both csp moi plots on same grid
figure2_csp_subset_moi = gridExtra::grid.arrange(csp_human_plot,csp_abdomen_plot,csp_head_plot,ncol=3)

# export the figure
ggsave(figure2_csp_subset_moi, filename="/Users/kelseysumner/Desktop/figure2_csp_subset_moi.png", device="png",
       height=10.5, width=17, units="in", dpi=400)

# calculate median values
# for ama
ama_summary = ama_haplotype_df %>%
  group_by(sample_type) %>%
  summarize(median=median(haplotype_number), mean=mean(haplotype_number))
median(ama_haplotype_df$haplotype_number)
# for csp
csp_summary = csp_haplotype_df %>%
  group_by(sample_type) %>%
  summarize(median=median(haplotype_number), mean=mean(haplotype_number))
median(csp_haplotype_df$haplotype_number)

