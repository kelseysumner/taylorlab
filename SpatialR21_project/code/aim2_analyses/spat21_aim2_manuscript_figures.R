# --------------------------------------------- #
#  Create aim 2 visualizations for manuscript   #
#             Mozzie Phase 1                    #
#            January 17, 2020                   #
#                K. Sumner                      #
# --------------------------------------------- #

# color pallete: Zissou1 = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")
# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC


#### --------- load packages ----------------- ####
library(tidyverse)
library(devtools)
library(streamgraph)
library(lubridate)
library(ggalluvial)
library(gridExtra)
library(ggsci)


#### ---------- read in the data sets ---------- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/model data/spat21_final_model_data_set_21JAN2020.rds")

# read in the merged anopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the csp haplotype data
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
csp_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# read in the csp haplotype data
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
ama_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")



#### -------- make visualization 1 --------- ####

##  make a plot of the anopheles mosquitoes using the stream graph plot

# set up the data set
mosquito_data = anoph_merged_data %>%
  select(collection_date,abdominal_status) %>%
  mutate(value=rep(1,nrow(anoph_merged_data)), month_date = floor_date(collection_date, "month"), 
         new_abdominal_status = ifelse(abdominal_status=="Gravid" | abdominal_status == "Half Gravid", "Gravid", ifelse(
           abdominal_status == "Blood Fed","Blood Fed",ifelse(
             abdominal_status == "Un-identified" | abdominal_status == "Undetermined", "Undetermined", "Unfed")))) %>%
  group_by(month_date,new_abdominal_status) %>%
  tally(wt=value)

# make the plot
mosquito_plot = mosquito_data %>%
  streamgraph("new_abdominal_status","n","month_date", offset="zero", interactive = F) %>%
  sg_fill_brewer("PRGn")
mosquito_plot  


## make a plot of the mosquito infection status in abdomens using the stream graph plot

# set up the data set
mosquito_data_infected = anoph_merged_data %>%
  filter(!(is.na(pf_pcr_infection_status_sample_level_a))) %>%
  select(collection_date,pf_pcr_infection_status_sample_level_a) %>%
  mutate(value=rep(1,length(!(is.na(pf_pcr_infection_status_sample_level_a)))), month_date = floor_date(collection_date, "month")) %>%
  group_by(month_date,pf_pcr_infection_status_sample_level_a) %>%
  tally(wt=value)

# relevel the data
mosquito_data_infected$pf_pcr_infection_status_sample_level_a = relevel(mosquito_data_infected$pf_pcr_infection_status_sample_level_a,"positive")

# make the plot
mosquito_plot_infected = mosquito_data_infected %>%
  streamgraph("pf_pcr_infection_status_sample_level_a","n","month_date", offset="zero", interactive = F) %>%
  sg_fill_manual(values = c("#D3DDDC","#F21A00"))
mosquito_plot_infected  

# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC

# make an additional df for plot
mosquito_df_for_plot = mosquito_data_infected %>%
  mutate(symp_status = ifelse(pf_pcr_infection_status_sample_level_a == "positive","infection","no infection")) %>%
  mutate(new_date = paste0(month(month_date),"-",year(month_date)))
mosquito_df_for_plot$symp_status = as.factor(mosquito_df_for_plot$symp_status)
mosquito_df_for_plot$symp_status = relevel(mosquito_df_for_plot$symp_status,ref="no infection")

# make an additional plot of the mosquitoes infection status but using ggplot
month_order = c("6-2017","7-2017","8-2017","9-2017","10-2017","11-2017","12-2017","1-2018","2-2018","3-2018","4-2018","5-2018","6-2018","7-2018")
mosquito_df_for_plot <- within(mosquito_df_for_plot, new_date <- factor(new_date, levels=month_order))
plot1 = ggplot(data = mosquito_df_for_plot,aes(x=new_date,y=n,fill=symp_status)) +
  geom_bar(stat="identity",colour="black")+
  scale_fill_manual(values=c("#D3DDDC","#F21A00")) +
  theme_bw() +
  xlab("Month")+
  ylab("Number of mosquitoes")+
  labs(fill="Infection status")
plot1
ggsave(plot1, filename="/Users/kelseysumner/Desktop/figure1_plot.png", device="png",
       height=5.25, width=11, units="in", dpi=500)

#### -------- make visualization 2 --------- ####

# merge in human dates with the csp_haplotypes data set
dates_df = final_data %>%
  select(sample_name_dbs,sample_id_date)
csp_haplotype_merge = left_join(csp_haplotypes,dates_df, by = "sample_name_dbs")

# check the merge
setdiff(csp_haplotypes$sample_name_dbs,csp_haplotype_merge$sample_name_dbs)
length(which(is.na(csp_haplotype_merge$sample_id_date)))

# fix dates 
anoph_merged_data$sample_id_abdomen[which(anoph_merged_data$sample_id_mosquito=="K01 00030")] = "K01 A00030"
anoph_merged_data$sample_id_head[which(anoph_merged_data$sample_id_mosquito=="K01 00030")] = "K01 H00030"
anoph_merged_data$sample_id_abdomen[which(anoph_merged_data$sample_id_mosquito=="K01 00047")] = "K01 A00047"
anoph_merged_data$sample_id_head[which(anoph_merged_data$sample_id_mosquito=="K01 00047")] = "K01 H00047"

# now merge in mosquito dates with csp_haplotypes_merge data set
dates_df_ab = anoph_merged_data %>%
  select(sample_id_abdomen,collection_date) %>%
  rename(sample_name_dbs = sample_id_abdomen)
dates_df_he = anoph_merged_data %>%
  select(sample_id_head,collection_date) %>%
  rename(sample_name_dbs = sample_id_head)
csp_haplotype_merge = left_join(csp_haplotype_merge,dates_df_ab,by="sample_name_dbs")
csp_haplotype_merge = left_join(csp_haplotype_merge,dates_df_he,by="sample_name_dbs")

# check the merge
colnames(csp_haplotype_merge)
csp_haplotype_merge$date_all = ifelse(!(is.na(csp_haplotype_merge$sample_id_date)),csp_haplotype_merge$sample_id_date,ifelse(
    !(is.na(csp_haplotype_merge$collection_date.x)),csp_haplotype_merge$collection_date.x,csp_haplotype_merge$collection_date.y))
csp_haplotype_merge$date_all = as_date(csp_haplotype_merge$date_all)
csp_haplotype_merge %>%
  select(sample_id_date,collection_date.x,collection_date.y,date_all,sample_name_dbs) %>%
  View()

# create a new variable that is just the month
csp_haplotype_merge$month = paste0(lubridate::month(csp_haplotype_merge$date_all),"-",lubridate::year(csp_haplotype_merge$date_all))
table(csp_haplotype_merge$month, useNA = "always")

# make a HH_ID variable
HH_ID = rep(NA,nrow(csp_haplotype_merge))
for (i in 1:nrow(csp_haplotype_merge)) {
  if (str_detect(csp_haplotype_merge$sample_name_dbs[i]," ")){
    HH_ID[i] = str_split(csp_haplotype_merge$sample_name_dbs[i]," ")[[1]][1]
  }
  if (str_detect(csp_haplotype_merge$sample_name_dbs[i],"-")){
    HH_ID[i] = str_split(csp_haplotype_merge$sample_name_dbs[i],"-")[[1]][1]
  }
}
table(HH_ID, useNA = "always")
csp_haplotype_merge$HH_ID = HH_ID

# only keep the haplotype columns, location, and month
colnames(csp_haplotype_merge)
csp_haplotype_merge = csp_haplotype_merge %>%
  select(-c(sample_type,haplotype_number,haplotype_reads,sample_id_date,collection_date.x,collection_date.y,MiSeq.ID,Run,sample_name_dbs,date_all))
colnames(csp_haplotype_merge)

# create a data frame summarizing each haplotype and the months it is present
# trying gathering the code to long format
long_csp_merged = gather(data=csp_haplotype_merge, "haplotype","readdepth", -month,-HH_ID)

# remove all rows with reads_present equal to 0
long_csp_merged = long_csp_merged[-which(long_csp_merged$readdepth == 0),]

# summarize the new data set by month
month_summary = long_csp_merged %>% 
  group_by(month,haplotype) %>%
  summarize(n_samples=n())

# summarize the new data set by location
location_summary = long_csp_merged %>%
  group_by(month,haplotype,HH_ID) %>%
  summarise(n_1 = n_distinct(HH_ID)) %>%
  select(month,haplotype,n_1) %>%
  summarise(n_households=sum(n_1,na.rm=T))

# merge the month and location summaries
merged_summary = left_join(month_summary,location_summary,by=c("month","haplotype"))

# check the output
length(which(csp_haplotype_merge$month == "1-2018" & csp_haplotype_merge$H1 > 0))
length(which(csp_haplotype_merge$month == "1-2018" & csp_haplotype_merge$H10 > 0))
unique(csp_haplotype_merge[which(csp_haplotype_merge$month == "1-2018" & csp_haplotype_merge$H1 > 0),c("HH_ID")])
unique(csp_haplotype_merge[which(csp_haplotype_merge$month == "1-2018" & csp_haplotype_merge$H10 > 0),c("HH_ID")])

# set order for x-axis for months
table(merged_summary$month, useNA = "always")
month_order = c("6-2017","7-2017","8-2017","9-2017","10-2017","11-2017","12-2017","1-2018","2-2018","3-2018","4-2018","5-2018","6-2018","7-2018")
merged_summary <- within(merged_summary, month <- factor(month, levels=month_order))

# set order for y-axis based on how many months each haplotype is present
months_hap_present_summary = long_csp_merged %>%
  group_by(haplotype,month) %>%
  summarise(n_present_1 = n_distinct(month)) %>%
  select(haplotype,n_present_1) %>%
  summarise(n_present = sum(n_present_1,na.rm=T))
haplotype_order = months_hap_present_summary[order(months_hap_present_summary$n_present),]
merged_summary <- within(merged_summary, haplotype <- factor(haplotype, levels=haplotype_order$haplotype))

# make a figure of the csp haplotypes present over time across all samples (regardless if human or mosquito)
csp_month_plot = ggplot(merged_summary, aes(x=month, y=haplotype, size=n_samples, color=n_households)) +
  geom_point() +
  scale_colour_gradient(low = "#fcbba1", high = "#67000d") +
  labs(x = "Month and year",y="Haplotype", color = "Number of households", size = "Number of samples") + 
  theme_bw()
csp_month_plot

# export the plot
# ggsave(csp_month_plot, filename="/Users/kelseysumner/Desktop/spat21_aim2_csp_month_plot.png", device="png",
      # height=35, width=11.2, units="in", dpi=500)


#### -------- make a streamgraph plot of asymptomatic and symptomatic infections over time ------ ####

# subset the dataframe to infections of interest
colnames(final_data)
human_data = final_data %>%
  dplyr::select(visit_type,sample_id_date,sample_name_final,sample_name_dbs,age_cat_baseline,unq_memID,village_name,HH_ID,main_exposure_primary_case_def,main_outcome_primary_case_def,pfr364Q_std_combined,age_all_baseline) %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def))) %>%
  mutate(infection_status = ifelse(is.na(aim2_exposure),"no infection",aim2_exposure))
table(human_data$infection_status, useNA = "always")
table(human_data$aim2_exposure, useNA = "always")

# set up the data to have months correct
human_data_infected = human_data %>%
  select(sample_id_date,infection_status) %>%
  mutate(value=rep(1,length(!(is.na(infection_status)))), month_date = floor_date(sample_id_date, "month")) %>%
  group_by(month_date,infection_status) %>%
  tally(wt=value)
human_data_infected$infection_status = as.factor(human_data_infected$infection_status)

# relevel the data
human_data_infected$infection_status = relevel(human_data_infected$infection_status,"symptomatic infection")

# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC

# make the plot
human_plot_infected = human_data_infected %>%
  streamgraph("infection_status","n","month_date", offset="zero", interactive = F) %>%
  sg_fill_manual(values = c("#E1AF00","#D3DDDC","#3B9AB2"))
human_plot_infected  



#### ------ now make figure 3 - the alluvial plot ------- ####

# make a plot of how malaria infection status changes over time (from having an asymptomatic or symptomatic infection to having no infection during that month)

# select variables you need for human data and make a variable that represents whether or not you have an infection (regardless of symptomatic status)
colnames(final_data)
human_data = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_exposure_primary_case_def == "no infection") %>%
  dplyr::select(visit_type,sample_id_date,sample_name_final,sample_name_dbs,age_cat_baseline,unq_memID,village_name,HH_ID,main_exposure_primary_case_def,main_outcome_primary_case_def,pfr364Q_std_combined,age_all_baseline) %>%
  rename(infection_status = main_exposure_primary_case_def)

# create a new variable that is just the month
human_data$month = paste0(lubridate::month(human_data$sample_id_date),"-",lubridate::year(human_data$sample_id_date))
table(human_data$month, useNA = "always")

# cut down the data set to just the variables of interest
plot_human_data = human_data %>%
  select(infection_status,month,unq_memID) %>%
  group_by(month,infection_status,unq_memID) %>%
  summarize(n=n())
plot_human_data_withperc = plot_human_data %>%
  group_by(month) %>%
  mutate(perc_n=n/sum(n))

# set order for x-axis for months
table(plot_human_data_withperc, useNA = "always")
month_order = c("6-2017","7-2017","8-2017","9-2017","10-2017","11-2017","12-2017","1-2018","2-2018","3-2018","4-2018","5-2018","6-2018","7-2018")
plot_human_data_withperc <- within(plot_human_data_withperc, month <- factor(month, levels=month_order))

# reorder so asymptomatic infections are on the bottom
plot_human_data_withperc$infection_status = relevel(as.factor(plot_human_data_withperc$infection_status),"no infection")

# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC

# now make an alluvial plot of how infection status changes over time
figure3_plot = ggplot(plot_human_data_withperc,
       aes(x = month, stratum = infection_status, alluvium = unq_memID,
           y = perc_n,
           fill = infection_status, label = infection_status)) +
  geom_flow(na.rm=T,alpha=0.4) +
  geom_stratum() +
  scale_fill_manual(values=c("#D3DDDC","#E1AF00")) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants")+
  labs(fill="Infection status")
figure3_plot
  
ggsave(figure3_plot, filename="/Users/kelseysumner/Desktop/plot_alluvial.png", device="png",
       height=5.25, width=11, units="in", dpi=500)



#### -------- make a plot of symptomatic infections over time ------- ####

# make a data set of just symptomatic infections
symptomatic_df = final_data %>%
  filter(visit_type == "monthly and sick visit" | visit_type== "sick visit") %>%
  select(c(unq_memID,sample_id_date,sample_name_final,HH_ID,village_name,main_outcome_primary_case_def)) %>%
  mutate(symp_infection = ifelse(!(is.na(main_outcome_primary_case_def)),"symptomatic infection","no infection"))
table(symptomatic_df$symp_infection, useNA = "always")

# create a new variable that is just the month
symptomatic_df$month = paste0(lubridate::month(symptomatic_df$sample_id_date),"-",lubridate::year(symptomatic_df$sample_id_date))
table(symptomatic_df$month, useNA = "always")

# cut down the data set to just the variables of interest
plot_human_data_symp = symptomatic_df %>%
  select(symp_infection,month,unq_memID) %>%
  group_by(month,symp_infection) %>%
  summarize(n=n())

# set order for x-axis for months
table(plot_human_data_symp, useNA = "always")
month_order = c("6-2017","7-2017","8-2017","9-2017","10-2017","11-2017","12-2017","1-2018","2-2018","3-2018","4-2018","5-2018","6-2018","7-2018")
plot_human_data_symp <- within(plot_human_data_symp, month <- factor(month, levels=month_order))

# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC

# make a stacked bar plot of the symptomatic infections tested over time
plot4 = ggplot(data = plot_human_data_symp,aes(x=month,y=n,fill=symp_infection)) +
  geom_bar(stat="identity",colour="black")+
  scale_fill_manual(values=c("#D3DDDC","#3B9AB2")) +
  theme_bw() +
  xlab("Month")+
  ylab("Number of participants")+
  labs(fill="Infection status")
plot4

# export the plot
ggsave(plot4, filename="/Users/kelseysumner/Desktop/figure4_plot_stackedsymp.png", device="png",
  height=5.25, width=11, units="in", dpi=500)


#### ------ make a plot of the mois for humans and mosquito abdomens for csp ------ ####

### create histograms of moi subset by sample type

human_data_exposure = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  dplyr::select(visit_type,sample_id_date,sample_name_final,sample_name_dbs,age_cat_baseline,unq_memID,village_name,HH_ID,main_exposure_primary_case_def,main_outcome_primary_case_def,pfr364Q_std_combined,age_all_baseline) %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def))) %>%
  dplyr::select(-main_exposure_primary_case_def,-main_outcome_primary_case_def,-visit_type)

# merge in symptomatic info with the haplotype data set
merge_hap_human_data = left_join(csp_haplotypes,human_data_exposure,by="sample_name_dbs")

# check the merge
setdiff(csp_haplotypes$sample_name_dbs,merge_hap_human_data$sample_name_dbs)
setdiff(human_data_exposure$sample_name_dbs,merge_hap_human_data$sample_name_dbs)
length(which(is.na(merge_hap_human_data$sample_id_date)))
merge_hap_human_data %>%
  filter(is.na(merge_hap_human_data$sample_id_date)) %>%
  select(sample_name_dbs,pfr364Q_std_combined,aim2_exposure,haplotype_reads) %>%
  View()

# create a summarized data frame of the number of abdomens with each MOI for csp
# for humans asymptomatic
csp_human_df_asymp <- merge_hap_human_data %>% 
  filter(!(is.na(haplotype_number)) & sample_type == "Human" & aim2_exposure == "asymptomatic infection") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
csp_human_df_asymp$haplotype_number = as.numeric(csp_human_df_asymp$haplotype_number)
sum(csp_human_df_asymp$n) 
# for humans symptomatic
csp_human_df_symp <- merge_hap_human_data %>% 
  filter(!(is.na(haplotype_number)) & sample_type == "Human" & aim2_exposure == "symptomatic infection") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
csp_human_df_symp$haplotype_number = as.numeric(csp_human_df_symp$haplotype_number)
sum(csp_human_df_symp$n) 
# for abdomens
csp_abdomen_df <- csp_haplotypes %>% 
  filter(!(is.na(haplotype_number)) & sample_type=="Abdomen") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
csp_abdomen_df$haplotype_number = as.numeric(csp_abdomen_df$haplotype_number)
sum(csp_abdomen_df$n)

# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC

# make csp moi figures by sample type
# for human samples asymptomatic
csp_human_title_asymp <- expression(paste(italic("pfcsp"), ": Asymptomatic participants"))
csp_human_plot_asymp = ggplot() +
  geom_bar(data=csp_human_df_asymp,aes(x=haplotype_number,y=n), alpha=0.95,stat="identity",fill="#E1AF00",color="black",width = 0.8) +
  labs(x="Number of haplotypes", y="Number of samples", title= csp_human_title_asymp, pch=16) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,240,300,360), limits=c(0,320)) +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5), text = element_text(size=20))
csp_human_plot_asymp
# for human samples symptomatic
csp_human_title_symp <- expression(paste(italic("pfcsp"), ": Symptomatic participants"))
csp_human_plot_symp = ggplot() +
  geom_bar(data=csp_human_df_symp,aes(x=haplotype_number,y=n), alpha=0.95,stat="identity",fill="#3B9AB2",color="black",width=0.8) +
  labs(x="Number of haplotypes", y="", title= csp_human_title_symp, pch=16) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,240,300,360), limits=c(0,320)) +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5), text = element_text(size=20))
csp_human_plot_symp
# for abdomen samples
csp_abdomen_title <- expression(paste(italic("pfcsp"), ": Mosquito abdomens"))
csp_abdomen_plot = ggplot() +
  geom_bar(data=csp_abdomen_df,aes(x=haplotype_number,y=n), alpha=0.95,stat="identity",fill="#F21A00",color="black",width=0.8) +
  labs(x="Number of haplotypes", y="", title= csp_abdomen_title, pch=16) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,240,300,360), limits=c(0,320)) +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5), text = element_text(size=20))
csp_abdomen_plot

# put both csp moi plots on same grid
figure2_csp_subset_moi = gridExtra::grid.arrange(csp_human_plot_asymp,csp_human_plot_symp,csp_abdomen_plot,ncol=3)

# export the figure
ggsave(figure2_csp_subset_moi, filename="/Users/kelseysumner/Desktop/figure2_csp_subset_moi.png", device="png",
       height=10.5, width=17, units="in", dpi=400)

# calculate median values
# for csp
csp_asymp = merge_hap_human_data %>%
  filter(aim2_exposure=="asymptomatic infection")
summary(csp_asymp$haplotype_number)
csp_symp = merge_hap_human_data %>%
  filter(aim2_exposure == "symptomatic infection") 
summary(csp_symp$haplotype_number)
csp_mosq = merge_hap_human_data %>%
  filter(sample_type == "Abdomen")
summary(csp_mosq$haplotype_number)



#### ------ make a plot of the mois for humans and mosquito abdomens for ama ------ ####

### create histograms of moi subset by sample type

human_data_exposure = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  dplyr::select(visit_type,sample_id_date,sample_name_final,sample_name_dbs,age_cat_baseline,unq_memID,village_name,HH_ID,main_exposure_primary_case_def,main_outcome_primary_case_def,pfr364Q_std_combined,age_all_baseline) %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def))) %>%
  dplyr::select(-main_exposure_primary_case_def,-main_outcome_primary_case_def,-visit_type)

# merge in symptomatic info with the haplotype data set
merge_hap_human_data = left_join(ama_haplotypes,human_data_exposure,by="sample_name_dbs")

# check the merge
setdiff(ama_haplotypes$sample_name_dbs,merge_hap_human_data$sample_name_dbs)
setdiff(human_data_exposure$sample_name_dbs,merge_hap_human_data$sample_name_dbs)
length(which(is.na(merge_hap_human_data$sample_id_date)))
merge_hap_human_data %>%
  filter(is.na(merge_hap_human_data$sample_id_date)) %>%
  select(sample_name_dbs,pfr364Q_std_combined,aim2_exposure,haplotype_reads) %>%
  View()

# create a summarized data frame of the number of abdomens with each MOI for ama
# for humans asymptomatic
ama_human_df_asymp <- merge_hap_human_data %>% 
  filter(!(is.na(haplotype_number)) & sample_type == "Human" & aim2_exposure == "asymptomatic infection") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
ama_human_df_asymp$haplotype_number = as.numeric(ama_human_df_asymp$haplotype_number)
sum(ama_human_df_asymp$n) 
# for humans symptomatic
ama_human_df_symp <- merge_hap_human_data %>% 
  filter(!(is.na(haplotype_number)) & sample_type == "Human" & aim2_exposure == "symptomatic infection") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
ama_human_df_symp$haplotype_number = as.numeric(ama_human_df_symp$haplotype_number)
sum(ama_human_df_symp$n) 
# for abdomens
ama_abdomen_df <- ama_haplotypes %>% 
  filter(!(is.na(haplotype_number)) & sample_type=="Abdomen") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
ama_abdomen_df$haplotype_number = as.numeric(ama_abdomen_df$haplotype_number)
sum(ama_abdomen_df$n)

# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC

# make ama moi figures by sample type
# for human samples asymptomatic
ama_human_title_asymp <- expression(paste(italic("pfama1"), ": Asymptomatic participants"))
ama_human_plot_asymp = ggplot() +
  geom_bar(data=ama_human_df_asymp,aes(x=haplotype_number,y=n), alpha=0.95,stat="identity",fill="#E1AF00",color="black",width=0.8) +
  labs(x="Number of haplotypes", y="Number of samples", title= ama_human_title_asymp, pch=16) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,240,300,360), limits=c(0,320)) +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5), text = element_text(size=20))
ama_human_plot_asymp
# for human samples symptomatic
ama_human_title_symp <- expression(paste(italic("pfama1"), ": Symptomatic participants"))
ama_human_plot_symp = ggplot() +
  geom_bar(data=ama_human_df_symp,aes(x=haplotype_number,y=n), alpha=0.95,stat="identity",fill="#3B9AB2",color="black",width=0.8) +
  labs(x="Number of haplotypes", y="", title= ama_human_title_symp, pch=16) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,240,300,360), limits=c(0,320)) +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5), text = element_text(size=20))
ama_human_plot_symp
# for abdomen samples
ama_abdomen_title <- expression(paste(italic("pfama1"), ": Mosquito abdomens"))
ama_abdomen_plot = ggplot() +
  geom_bar(data=ama_abdomen_df,aes(x=haplotype_number,y=n), alpha=0.95,stat="identity",fill="#F21A00",color="black",width=0.8) +
  labs(x="Number of haplotypes", y="", title= ama_abdomen_title, pch=16) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,240,300,360), limits=c(0,320)) +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5), text = element_text(size=20))
ama_abdomen_plot

# put both ama moi plots on same grid
figure2_ama_subset_moi = gridExtra::grid.arrange(ama_human_plot_asymp,ama_human_plot_symp,ama_abdomen_plot,ncol=3)

# export the figure
ggsave(figure2_ama_subset_moi, filename="/Users/kelseysumner/Desktop/figure2_ama_subset_moi.png", device="png",
       height=10.5, width=17, units="in", dpi=400)

# calculate median values
# for ama
ama_asymp = merge_hap_human_data %>%
  filter(aim2_exposure=="asymptomatic infection")
summary(ama_asymp$haplotype_number)
ama_symp = merge_hap_human_data %>%
  filter(aim2_exposure == "symptomatic infection") 
summary(ama_symp$haplotype_number)
ama_mosq = merge_hap_human_data %>%
  filter(sample_type == "Abdomen")
summary(ama_mosq$haplotype_number)



#### ----- figure 6: dot plot of number of haplotypes shared in human-mosquito pairs ------- ####

## make a plot of the outcome of the number of haplotypes shared across human-mosquito pairs for pfcsp

# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC

# set up the data set for a dot plot
dot_plot_df_csp <- model_data %>% 
  filter(!(is.na(csp_haps_shared))) %>%
  group_by(csp_haps_shared) %>%
  summarise(n=n())

# make figures of the number of haps shared
dot_plot_csp_haps_shared = ggplot(model_data, aes(x = factor(aim2_exposure), fill = factor(aim2_exposure), y = csp_haps_shared)) +
  geom_violin(binaxis = "y", stackdir = "center",dotsize=0.5)+
  labs(y="Number of haplotypes shared", x="Symptomatic status", pch=18, title = expression(italic("pfcsp"))) +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size=25),plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#E1AF00","#3B9AB2"))
dot_plot_csp_haps_shared

# export the plot
ggsave(dot_plot_csp_haps_shared, filename="/Users/kelseysumner/Desktop/figure7_dot_plot_haps_shared_csps.png", device="png",
 height=8, width=18, units="in", dpi=400)


## make a plot of the outcome of the number of haplotypes shared across human-mosquito pairs for pfama1

# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC

# set up the data set for a dot plot
dot_plot_df_ama <- model_data %>% 
  filter(!(is.na(ama_haps_shared))) %>%
  group_by(ama_haps_shared) %>%
  summarise(n=n())

# make figures of the number of haps shared
dot_plot_ama_haps_shared = ggplot(model_data, aes(x = factor(aim2_exposure), fill = factor(aim2_exposure), y = ama_haps_shared)) +
  geom_violin(binaxis = "y", stackdir = "center",dotsize=0.5)+
  labs(y="Number of haplotypes shared", x="Symptomatic status", pch=18, title = expression(italic("pfama1"))) +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size=25),plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#E1AF00","#3B9AB2"))
dot_plot_ama_haps_shared

# export the plot
ggsave(dot_plot_ama_haps_shared, filename="/Users/kelseysumner/Desktop/figure7_dot_plot_haps_shared_ama.png", device="png",
       height=8, width=18, units="in", dpi=400)



#### -------- make a plot of the number of samples within each haplotype ----- ####


## first do this for ama

# make separate data sets for humans and mosquitoes
human_haps = ama_haplotypes %>%
  filter(sample_type=="Human")
abdomen_haps = ama_haplotypes %>%
  filter(sample_type=="Abdomen")
abdomen_haps = abdomen_haps[,c(4:459)]

# merge the final_data info for symptomatic status with the human haps
cut_data = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  select(sample_name_dbs,main_exposure_primary_case_def,main_outcome_primary_case_def) %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def)))
table(cut_data$aim2_exposure, useNA = "always")
human_haps = left_join(human_haps,cut_data,by="sample_name_dbs")
table(human_haps$aim2_exposure, useNA = "always")
colnames(human_haps)
asymp_human_haps = human_haps %>% filter(aim2_exposure == "asymptomatic infection")
symp_human_haps = human_haps %>% filter(aim2_exposure == "symptomatic infection")
asymp_human_haps = asymp_human_haps[,c(4:459)]
symp_human_haps = symp_human_haps[,c(4:459)]

# summarize the number of samples within each haplotype for the asymp human samples
haplotype.names = rep(1:ncol(asymp_human_haps))
haplotypes_in_samples = rep(NA,ncol(asymp_human_haps))
total_reads_in_samples = rep(NA,ncol(asymp_human_haps))
for (k in 1:ncol(asymp_human_haps)){
  haplotypes_in_samples[k] = length(which(asymp_human_haps[,k] > 0))
  total_reads_in_samples[k] = sum(asymp_human_haps[,k],na.rm=T)
}
asymp_human_hap_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)

# summarize the number of samples within each haplotype for the symp human samples
haplotype.names = rep(1:ncol(symp_human_haps))
haplotypes_in_samples = rep(NA,ncol(symp_human_haps))
total_reads_in_samples = rep(NA,ncol(symp_human_haps))
for (k in 1:ncol(symp_human_haps)){
  haplotypes_in_samples[k] = length(which(symp_human_haps[,k] > 0))
  total_reads_in_samples[k] = sum(symp_human_haps[,k],na.rm=T)
}
symp_human_hap_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)

# summarize the number of samples within each haplotype for the mosquito abdomen samples
haplotype.names = rep(1:ncol(abdomen_haps))
haplotypes_in_samples = rep(NA,ncol(abdomen_haps))
total_reads_in_samples = rep(NA,ncol(abdomen_haps))
for (k in 1:ncol(abdomen_haps)){
  haplotypes_in_samples[k] = length(which(abdomen_haps[,k] > 0))
  total_reads_in_samples[k] = sum(abdomen_haps[,k],na.rm=T)
}
abdomen_hap_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)

# set the haplotype order
hap_order = order(-asymp_human_hap_summary$haplotypes_across_samples)
asymp_human_hap_summary = asymp_human_hap_summary[hap_order,]
symp_human_hap_summary = symp_human_hap_summary[hap_order,]
abdomen_hap_summary = abdomen_hap_summary[hap_order,]
asymp_human_hap_summary$haplotype_ids = factor(asymp_human_hap_summary$haplotype_ids, levels=asymp_human_hap_summary$haplotype_ids[order(-asymp_human_hap_summary$haplotypes_across_samples)])
symp_human_hap_summary$haplotype_ids = factor(symp_human_hap_summary$haplotype_ids, levels=symp_human_hap_summary$haplotype_ids[order(-asymp_human_hap_summary$haplotypes_across_samples)])
abdomen_hap_summary$haplotype_ids = factor(abdomen_hap_summary$haplotype_ids, levels=abdomen_hap_summary$haplotype_ids[order(-asymp_human_hap_summary$haplotypes_across_samples)])

# make a data frame of both df combined
asymp_human_hap_summary$type = rep("Asymptomatic participant",nrow(asymp_human_hap_summary))
symp_human_hap_summary$type = rep("Symptomatic participant",nrow(symp_human_hap_summary))
abdomen_hap_summary$type = rep("Mosquito abdomen",nrow(abdomen_hap_summary))
original_combined_hap_summary = rbind(asymp_human_hap_summary,symp_human_hap_summary,abdomen_hap_summary)
# now subset the data set to just the haplotypes found in >0 samples across all sample types
total_haps = original_combined_hap_summary %>%
  group_by(haplotype_ids) %>%
  summarize(total_samples = sum(haplotypes_across_samples)) %>%
  filter(total_samples > 0) # 348
# now subset the data set to just the haplotypes found in >10 samples across all sample types
total_haps_10 = original_combined_hap_summary %>%
  group_by(haplotype_ids) %>%
  summarize(total_samples = sum(haplotypes_across_samples)) %>%
  filter(total_samples > 10) # 55
# go back to making full combined df with haplotypes found in >0 samples
abdomen_hap_summary$haplotypes_across_samples = -1*abdomen_hap_summary$haplotypes_across_samples
summary(abdomen_hap_summary$haplotypes_across_samples)
combined_hap_summary_subset_0 = rbind(asymp_human_hap_summary,symp_human_hap_summary,abdomen_hap_summary)
combined_hap_summary_subset_0 = combined_hap_summary_subset_0 %>%
  filter(combined_hap_summary_subset_0$haplotype_ids %in% total_haps$haplotype_ids)
length(unique(combined_hap_summary_subset_0$haplotype_ids)) # 348, correct
# make combined df with haplotypes found in >10 samples
summary(abdomen_hap_summary$haplotypes_across_samples)
combined_hap_summary_subset_10 = rbind(asymp_human_hap_summary,symp_human_hap_summary,abdomen_hap_summary)
combined_hap_summary_subset_10 = combined_hap_summary_subset_10 %>%
  filter(combined_hap_summary_subset_10$haplotype_ids %in% total_haps_10$haplotype_ids)
length(unique(combined_hap_summary_subset_10$haplotype_ids)) # 55, correct


# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC

# now try to make a pyramid plot
pyramid_plot_ama = ggplot(combined_hap_summary_subset_0, aes(x = haplotype_ids, y = haplotypes_across_samples, fill = type)) +   # Fill column
  geom_bar(stat = "identity", width = .6,color="black") +   # draw the bars
  coord_flip() +  # Flip axes
  scale_fill_manual(values=c("#E1AF00","#F21A00","#3B9AB2")) +
  theme_bw() +
  scale_y_continuous(breaks=c(-350,-300,-250,-200,-150,-100,-50,0,50,100,150,200,250,300,350)) +
  labs(title=expression(paste(italic("pfama1: "),"Haplotypes shared across samples")), fill = "Sample type") +
  xlab("Haplotype ID") +
  ylab("Number of samples with haplotype") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25),axis.text.y=element_blank(),
        axis.ticks.y = element_blank(), legend.position = c(1,1),legend.justification = c(1,1),legend.box.background = element_rect(colour = "black"))  +
  geom_vline(xintercept = 0,color="black",size=1.5)
pyramid_plot_ama

# export the figure with all the haplotypes
ggsave(pyramid_plot_ama, filename="/Users/kelseysumner/Desktop/pyramid_plot_ama.png", device="png",
       height=20, width=12, units="in", dpi=400)




# now try to make a pyramid plot subset to those haplotypes found in >10 samples
pyramid_plot_ama_10 = ggplot(combined_hap_summary_subset_10, aes(x = haplotype_ids, y = haplotypes_across_samples, fill = type)) +   # Fill column
  geom_bar(stat = "identity", width = .6,color="black") +   # draw the bars
  coord_flip() +  # Flip axes
  scale_fill_manual(values=c("#E1AF00","#F21A00","#3B9AB2")) +
  theme_bw() +
  scale_y_continuous(breaks=c(-350,-300,-250,-200,-150,-100,-50,0,50,100,150,200,250,300,350)) +
  labs(title=expression(paste(italic("pfama1: "),"Haplotypes shared across samples")), fill = "Sample type") +
  xlab("Haplotype ID") +
  ylab("Number of samples with haplotype") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25),axis.text.y=element_blank(),
        axis.ticks.y = element_blank(), legend.position = c(0.84, 0.87),legend.box.background = element_rect(colour = "black"))  +
  geom_vline(xintercept = 0,color="black",size=1.5)
pyramid_plot_ama_10

# export the figure with all the haplotypes
ggsave(pyramid_plot_ama_10, filename="/Users/kelseysumner/Desktop/pyramid_plot_ama_inatleast10samples.png", device="png",
       height=9, width=14, units="in", dpi=400)






## then do this for csp

# make separate data sets for humans and mosquitoes
human_haps = csp_haplotypes %>%
  filter(sample_type=="Human")
abdomen_haps = csp_haplotypes %>%
  filter(sample_type=="Abdomen")
abdomen_haps = abdomen_haps[,c(4:301)]

# merge the final_data info for symptomatic status with the human haps
cut_data = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  select(sample_name_dbs,main_exposure_primary_case_def,main_outcome_primary_case_def) %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def)))
table(cut_data$aim2_exposure, useNA = "always")
human_haps = left_join(human_haps,cut_data,by="sample_name_dbs")
table(human_haps$aim2_exposure, useNA = "always")
colnames(human_haps)
asymp_human_haps = human_haps %>% filter(aim2_exposure == "asymptomatic infection")
symp_human_haps = human_haps %>% filter(aim2_exposure == "symptomatic infection")
asymp_human_haps = asymp_human_haps[,c(4:301)]
symp_human_haps = symp_human_haps[,c(4:301)]

# summarize the number of samples within each haplotype for the asymp human samples
haplotype.names = rep(1:ncol(asymp_human_haps))
haplotypes_in_samples = rep(NA,ncol(asymp_human_haps))
total_reads_in_samples = rep(NA,ncol(asymp_human_haps))
for (k in 1:ncol(asymp_human_haps)){
  haplotypes_in_samples[k] = length(which(asymp_human_haps[,k] > 0))
  total_reads_in_samples[k] = sum(asymp_human_haps[,k],na.rm=T)
}
asymp_human_hap_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)

# summarize the number of samples within each haplotype for the symp human samples
haplotype.names = rep(1:ncol(symp_human_haps))
haplotypes_in_samples = rep(NA,ncol(symp_human_haps))
total_reads_in_samples = rep(NA,ncol(symp_human_haps))
for (k in 1:ncol(symp_human_haps)){
  haplotypes_in_samples[k] = length(which(symp_human_haps[,k] > 0))
  total_reads_in_samples[k] = sum(symp_human_haps[,k],na.rm=T)
}
symp_human_hap_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)

# summarize the number of samples within each haplotype for the mosquito abdomen samples
haplotype.names = rep(1:ncol(abdomen_haps))
haplotypes_in_samples = rep(NA,ncol(abdomen_haps))
total_reads_in_samples = rep(NA,ncol(abdomen_haps))
for (k in 1:ncol(abdomen_haps)){
  haplotypes_in_samples[k] = length(which(abdomen_haps[,k] > 0))
  total_reads_in_samples[k] = sum(abdomen_haps[,k],na.rm=T)
}
abdomen_hap_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)

# set the haplotype order
hap_order = order(-asymp_human_hap_summary$haplotypes_across_samples)
asymp_human_hap_summary = asymp_human_hap_summary[hap_order,]
symp_human_hap_summary = symp_human_hap_summary[hap_order,]
abdomen_hap_summary = abdomen_hap_summary[hap_order,]
asymp_human_hap_summary$haplotype_ids = factor(asymp_human_hap_summary$haplotype_ids, levels=asymp_human_hap_summary$haplotype_ids[order(-asymp_human_hap_summary$haplotypes_across_samples)])
symp_human_hap_summary$haplotype_ids = factor(symp_human_hap_summary$haplotype_ids, levels=symp_human_hap_summary$haplotype_ids[order(-asymp_human_hap_summary$haplotypes_across_samples)])
abdomen_hap_summary$haplotype_ids = factor(abdomen_hap_summary$haplotype_ids, levels=abdomen_hap_summary$haplotype_ids[order(-asymp_human_hap_summary$haplotypes_across_samples)])

# make a data frame of both df combined
asymp_human_hap_summary$type = rep("Asymptomatic participant",nrow(asymp_human_hap_summary))
symp_human_hap_summary$type = rep("Symptomatic participant",nrow(symp_human_hap_summary))
abdomen_hap_summary$type = rep("Mosquito abdomen",nrow(abdomen_hap_summary))
original_combined_hap_summary = rbind(asymp_human_hap_summary,symp_human_hap_summary,abdomen_hap_summary)
# now subset the data set to just the haplotypes found in >0 samples across all sample types
total_haps = original_combined_hap_summary %>%
  group_by(haplotype_ids) %>%
  summarize(total_samples = sum(haplotypes_across_samples)) %>%
  filter(total_samples > 0) # 229
# now subset the data set to just the haplotypes found in >10 samples across all sample types
total_haps_10 = original_combined_hap_summary %>%
  group_by(haplotype_ids) %>%
  summarize(total_samples = sum(haplotypes_across_samples)) %>%
  filter(total_samples > 10) # 39
# go back to making full combined df with haplotypes found in >0 samples
abdomen_hap_summary$haplotypes_across_samples = -1*abdomen_hap_summary$haplotypes_across_samples
summary(abdomen_hap_summary$haplotypes_across_samples)
combined_hap_summary_subset_0 = rbind(asymp_human_hap_summary,symp_human_hap_summary,abdomen_hap_summary)
combined_hap_summary_subset_0 = combined_hap_summary_subset_0 %>%
  filter(combined_hap_summary_subset_0$haplotype_ids %in% total_haps$haplotype_ids)
length(unique(combined_hap_summary_subset_0$haplotype_ids)) # 229, correct
# make combined df with haplotypes found in >10 samples
summary(abdomen_hap_summary$haplotypes_across_samples)
combined_hap_summary_subset_10 = rbind(asymp_human_hap_summary,symp_human_hap_summary,abdomen_hap_summary)
combined_hap_summary_subset_10 = combined_hap_summary_subset_10 %>%
  filter(combined_hap_summary_subset_10$haplotype_ids %in% total_haps_10$haplotype_ids)
length(unique(combined_hap_summary_subset_10$haplotype_ids)) # 39, correct


# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC

# now try to make a pyramid plot
pyramid_plot_csp = ggplot(combined_hap_summary_subset_0, aes(x = haplotype_ids, y = haplotypes_across_samples, fill = type)) +   # Fill column
  geom_bar(stat = "identity", width = .6,color="black") +   # draw the bars
  coord_flip() +  # Flip axes
  scale_fill_manual(values=c("#E1AF00","#F21A00","#3B9AB2")) +
  theme_bw() +
  scale_y_continuous(breaks=c(-350,-300,-250,-200,-150,-100,-50,0,50,100,150,200,250,300,350)) +
  labs(title=expression(paste(italic("pfcsp: "),"Haplotypes shared across samples")), fill = "Sample type") +
  xlab("Haplotype ID") +
  ylab("Number of samples with haplotype") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25),axis.text.y=element_blank(),
        axis.ticks.y = element_blank(), legend.position = c(1,1),legend.justification = c(1,1),legend.box.background = element_rect(colour = "black"))  +
  geom_vline(xintercept = 0,color="black",size=1.5)
pyramid_plot_csp

# export the figure with all the haplotypes
ggsave(pyramid_plot_csp, filename="/Users/kelseysumner/Desktop/pyramid_plot_csp.png", device="png",
       height=20, width=12, units="in", dpi=400)




# now try to make a pyramid plot subset to those haplotypes found in >10 samples
pyramid_plot_csp_10 = ggplot(combined_hap_summary_subset_10, aes(x = haplotype_ids, y = haplotypes_across_samples, fill = type)) +   # Fill column
  geom_bar(stat = "identity", width = .6,color="black") +   # draw the bars
  coord_flip() +  # Flip axes
  scale_fill_manual(values=c("#E1AF00","#F21A00","#3B9AB2")) +
  theme_bw() +
  scale_y_continuous(breaks=c(-350,-300,-250,-200,-150,-100,-50,0,50,100,150,200,250,300,350)) +
  labs(title=expression(paste(italic("pfcsp1: "),"Haplotypes shared across samples")), fill = "Sample type") +
  xlab("Haplotype ID") +
  ylab("Number of samples with haplotype") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25),axis.text.y=element_blank(),
        axis.ticks.y = element_blank(), legend.position = c(0.84, 0.87),legend.box.background = element_rect(colour = "black"))  +
  geom_vline(xintercept = 0,color="black",size=1.5)
pyramid_plot_csp_10

# export the figure with all the haplotypes
ggsave(pyramid_plot_csp_10, filename="/Users/kelseysumner/Desktop/pyramid_plot_csp_inatleast10samples.png", device="png",
       height=9, width=14, units="in", dpi=400)





