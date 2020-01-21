# --------------------------------------------- #
#  Create aim 2 visualizations for manuscript   #
#             Mozzie Phase 1                    #
#            January 17, 2020                   #
#                K. Sumner                      #
# --------------------------------------------- #

# color pallete: http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=5
# symptomatic (red): #b2182b
# asymptomatic (green): #238443
# mosquitoes (blue): #2166ac
# no infection (grey): #A9A9A9


#### --------- load packages ----------------- ####
library(tidyverse)
library(devtools)
library(streamgraph)
library(lubridate)
library(ggalluvial)
library(gridExtra)


#### ---------- read in the data sets ---------- ####

# read in the merged csp abdomen edgelist ready for the multilevel models
csp_abdomens = read_rds("Desktop/clean_ids_haplotype_results/CSP/model data set/spat21_csp_abdomen_sharing_final_model_data_31OCT2019.rds")

# read in the merged anopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# read in the csp haplotype data
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
csp_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

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
  sg_fill_brewer("RdBu")
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
  sg_fill_manual(values = c("#A9A9A9","#2166ac"))
mosquito_plot_infected  

# symptomatic (red): #b2182b
# asymptomatic (green): #238443
# mosquitoes (blue): #2166ac
# no infection (grey): #A9A9A9

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
  scale_fill_manual(values=c("#A9A9A9","#2166ac")) +
  theme_bw() +
  xlab("Month")+
  ylab("Number of mosquitoes")+
  labs(fill="Infection status")
plot1
# ggsave(plot1, filename="/Users/kelseysumner/Desktop/figure1_plot.png", device="png",
       # height=5.25, width=11, units="in", dpi=500)

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
  
# now make an alluvial plot of how infection status changes over time
figure3_plot = ggplot(plot_human_data_withperc,
       aes(x = month, stratum = infection_status, alluvium = unq_memID,
           y = perc_n,
           fill = infection_status, label = infection_status)) +
  geom_flow(na.rm=T,alpha=0.25) +
  geom_stratum() +
  scale_fill_manual(values=c("#A9A9A9","#238443")) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants")+
  labs(fill="Infection status")
figure3_plot
  
ggsave(figure3_plot, filename="/Users/kelseysumner/Desktop/figure3_plot_alluvial.png", device="png",
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

# make a stacked bar plot of the symptomatic infections tested over time
plot4 = ggplot(data = plot_human_data_symp,aes(x=month,y=n,fill=symp_infection)) +
  geom_bar(stat="identity",colour="black")+
  scale_fill_manual(values=c("#A9A9A9","#b2182b")) +
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

# make csp moi figures by sample type
# for human samples asymptomatic
csp_human_title_asymp <- expression(paste(italic("pfcsp"), ": Asymptomatic humans"))
csp_human_plot_asymp = ggplot() +
  geom_bar(data=csp_human_df_asymp,aes(x=haplotype_number,y=n), alpha=0.95,stat="identity",fill="#ff7f00") +
  labs(x="Number of haplotypes", y="Number of samples", title= csp_human_title_asymp, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,240,300,360), limits=c(0,320)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_human_plot_asymp
# for human samples symptomatic
csp_human_title_symp <- expression(paste(italic("pfcsp"), ": Symptomatic humans"))
csp_human_plot_symp = ggplot() +
  geom_bar(data=csp_human_df_symp,aes(x=haplotype_number,y=n), alpha=0.95,stat="identity",fill="#e31a1c") +
  labs(x="Number of haplotypes", y="Number of samples", title= csp_human_title_symp, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,240,300,360), limits=c(0,320)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_human_plot_symp
# for abdomen samples
csp_abdomen_title <- expression(paste(italic("pfcsp"), ": Mosquito abdomens"))
csp_abdomen_plot = ggplot() +
  geom_bar(data=csp_abdomen_df,aes(x=haplotype_number,y=n), alpha=0.95,stat="identity",fill="#fdd0a2") +
  labs(x="Number of haplotypes", y="Number of samples", title= csp_abdomen_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,240,300,360), limits=c(0,320)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
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

#### -------- figure 6: make layered histogram of human-mosquito pairs and haplotype sharing ------- ####

## make a plot of the haplotypes shared across human-mosquito pairs

# make a data set of the number of pairs that shared at least 1 haplotype across households
csp_abdomen_shared_pairs_in_HH = csp_abdomens %>%
  filter(haps_shared > 0) %>%
  group_by(village_name,HH_ID) %>%
  summarize(number_pairs_with_sharing = n())

# make a data set of the number of Pf+ infections in humans detected across each household
pf_infection_in_HH_mosq = final_data %>%
  filter(pf_pcr_infection_status == "positive") %>%
  group_by(HH_ID) %>%
  summarize(number_infection=n())

# make a data set of the number of Pf+ infections in mosquito abdomens detected across each household
pf_infection_in_HH_humans = anoph_merged_data %>%
  filter(pf_pcr_infection_status_sample_level_a == "positive") %>%
  group_by(HH_ID) %>%
  summarize(number_infection=n())

# make a layered plot of the pairs over time
# layer1
layered1_plot = ggplot() +
  geom_bar(data=csp_abdomen_shared_pairs_in_HH,aes(x=HH_ID,y=number_pairs_with_sharing), alpha=0.95,stat="identity",fill="grey") +
  labs(x="Households", y="Number of pairs with at least 1 shared haplotype", title= "Human-mosquito abdomen haplotype pairs", pch=18) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
layered1_plot
# layer2
layered2_plot = ggplot() +
  geom_bar(data=pf_infection_in_HH_humans,aes(x=HH_ID,y=number_infection), alpha=0.95,stat="identity",fill="#e31a1c") +
  labs(x="Households", y="Number of Pf positive infections", title= "Human infections", pch=18) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
layered2_plot
# layer3
layered3_plot = ggplot() +
  geom_bar(data=pf_infection_in_HH_mosq,aes(x=HH_ID,y=number_infection), alpha=0.95,stat="identity",fill="#fdd0a2") +
  labs(x="Households", y="Number of Pf positive infections", title= "Mosquito abdomen infections", pch=18) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
layered3_plot

# put both csp moi plots on same grid
figure6_layer_plot = gridExtra::grid.arrange(layered1_plot,layered2_plot,layered3_plot,nrow=3)

# export the figure
# ggsave(figure6_layer_plot, filename="/Users/kelseysumner/Desktop/figure2_csp_subset_moi.png", device="png",
       # height=10.5, width=17, units="in", dpi=400)


#### ----- figure 7: dot plot of number of haplotypes shared in human-mosquito pairs ------- ####

## make a plot of the outcome of the number of haplotypes shared across human-mosquito pairs

# set up the data set for a dot plot
dot_plot_df <- csp_abdomens %>% 
  group_by(haps_shared) %>%
  summarise(n=n())

# make figures of the number of haps shared
dot_plot = ggplot(csp_abdomens, aes(x = factor(village_name), fill = factor(village_name), y = haps_shared)) +
  geom_dotplot(binaxis = "y", stackdir = "center",alpha=0.8,dotsize=0.5)+
  labs(y="Number of haplotypes shared", x="Village name", pch=18) +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size=25)) +
  scale_fill_manual(values=c("#c7e9b4","#41b6c4"))
dot_plot

# export the plot
ggsave(dot_plot, filename="/Users/kelseysumner/Desktop/figure7_dot_plot.png", device="png",
 height=8, width=18, units="in", dpi=400)


#### ------- make a plot with the haplotype sharing outcome across a range of definitions for a successful transmission event --------- ####

# look at the number of human-mosquito pairs sharing across symptomatic status with different definitions for a transmission event
# at least 1 haplotype shared = transmission event
csp_abdomens_1hap = csp_abdomens %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>0)), prop_shared = (number_with_a_shared_hap/number_of_pairs)*100) %>%
  mutate(hap_range = "1 or more haplotypes")
# at least 2 haplotypes shared = transmission event
csp_abdomens_2hap = csp_abdomens %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>1)), prop_shared = (number_with_a_shared_hap/number_of_pairs)*100) %>%
  mutate(hap_range = "2 or more haplotypes")
# at least 3 haplotypes shared = transmission event
csp_abdomens_3hap = csp_abdomens %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>2)), prop_shared = (number_with_a_shared_hap/number_of_pairs)*100) %>%
  mutate(hap_range = "3 or more haplotypes")
# at least 4 haplotypes shared = transmission event
csp_abdomens_4hap = csp_abdomens %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>3)), prop_shared = (number_with_a_shared_hap/number_of_pairs)*100) %>%
  mutate(hap_range = "4 or more haplotypes")
# at least 5 haplotypes shared = transmission event
csp_abdomens_5hap = csp_abdomens %>%
  group_by(aim2_exposure) %>%
  summarize(number_of_pairs = n(), number_with_a_shared_hap = length(which(haps_shared>4)), prop_shared = (number_with_a_shared_hap/number_of_pairs)*100) %>%
  mutate(hap_range = "5 or more haplotypes")

# make a combined data frame
hap_sharing_combined_df = rbind(csp_abdomens_1hap,csp_abdomens_2hap,csp_abdomens_3hap,csp_abdomens_4hap,csp_abdomens_5hap)
hap_sharing_combined_df = data.frame(hap_sharing_combined_df)

# use the binom package to compute an exact confidence interval
library(binom)
# create an empty vector
hap_sharing_combined_df$lower_ci = rep(NA,nrow(hap_sharing_combined_df))
hap_sharing_combined_df$upper_ci = rep(NA,nrow(hap_sharing_combined_df))

# for at least 1 hap
# for asymptomatic
full = binom.confint(hap_sharing_combined_df$number_with_a_shared_hap[1],hap_sharing_combined_df$number_of_pairs[1])
hap_sharing_combined_df$lower_ci[1] = full$lower[5]
hap_sharing_combined_df$upper_ci[1] = full$upper[5]
# for symptomatic
full = binom.confint(hap_sharing_combined_df$number_with_a_shared_hap[2],hap_sharing_combined_df$number_of_pairs[2])
hap_sharing_combined_df$lower_ci[2] = full$lower[5]
hap_sharing_combined_df$upper_ci[2] = full$upper[5]

# for at least 2 hap
# for asymptomatic
full = binom.confint(hap_sharing_combined_df$number_with_a_shared_hap[3],hap_sharing_combined_df$number_of_pairs[3])
hap_sharing_combined_df$lower_ci[3] = full$lower[5]
hap_sharing_combined_df$upper_ci[3] = full$upper[5]
# for symptomatic
full = binom.confint(hap_sharing_combined_df$number_with_a_shared_hap[4],hap_sharing_combined_df$number_of_pairs[4])
hap_sharing_combined_df$lower_ci[4] = full$lower[5]
hap_sharing_combined_df$upper_ci[4] = full$upper[5]

# for at least 3 hap
# for asymptomatic
full = binom.confint(hap_sharing_combined_df$number_with_a_shared_hap[5],hap_sharing_combined_df$number_of_pairs[5])
hap_sharing_combined_df$lower_ci[5] = full$lower[5]
hap_sharing_combined_df$upper_ci[5] = full$upper[5]
# for symptomatic
full = binom.confint(hap_sharing_combined_df$number_with_a_shared_hap[6],hap_sharing_combined_df$number_of_pairs[6])
hap_sharing_combined_df$lower_ci[6] = full$lower[5]
hap_sharing_combined_df$upper_ci[6] = full$upper[5]

# for at least 4 hap
# for asymptomatic
full = binom.confint(hap_sharing_combined_df$number_with_a_shared_hap[7],hap_sharing_combined_df$number_of_pairs[7])
hap_sharing_combined_df$lower_ci[7] = full$lower[5]
hap_sharing_combined_df$upper_ci[7] = full$upper[5]
# for symptomatic
full = binom.confint(hap_sharing_combined_df$number_with_a_shared_hap[8],hap_sharing_combined_df$number_of_pairs[8])
hap_sharing_combined_df$lower_ci[8] = full$lower[5]
hap_sharing_combined_df$upper_ci[8] = full$upper[5]

# for at least 5 hap
# for asymptomatic
full = binom.confint(hap_sharing_combined_df$number_with_a_shared_hap[9],hap_sharing_combined_df$number_of_pairs[9])
hap_sharing_combined_df$lower_ci[9] = full$lower[5]
hap_sharing_combined_df$upper_ci[9] = full$upper[5]
# for symptomatic
full = binom.confint(hap_sharing_combined_df$number_with_a_shared_hap[10],hap_sharing_combined_df$number_of_pairs[10])
hap_sharing_combined_df$lower_ci[10] = full$lower[5]
hap_sharing_combined_df$upper_ci[10] = full$upper[5]

# split into an asymp and symp data sets for the plot
asymp_data = hap_sharing_combined_df %>% filter(aim2_exposure == "asymptomatic infection")
symp_data = hap_sharing_combined_df %>% filter(aim2_exposure == "symptomatic infection")
asymp_data$hap_range = as.factor(asymp_data$hap_range)
symp_data$hap_range = as.factor(symp_data$hap_range)

# make plots of how the number of pairs changes over time
csp_abdomen_hap_pairs_plot = ggplot() +
  geom_line(data=asymp_data,aes(x=hap_range,y=prop_shared,group=1),cex=1.5,col="#ff7f00") +
  geom_ribbon(data=asymp_data,aes(x=1:length(hap_range),ymin = lower_ci*100, ymax = upper_ci*100),alpha=0.2,fill="#ff7f00") +
  geom_line(data=symp_data,aes(x=hap_range,y=prop_shared,group=1),cex=1.5,col="#e31a1c") +
  geom_ribbon(data=symp_data,aes(x=1:length(hap_range),ymin = lower_ci*100, ymax = upper_ci*100),alpha=0.2,fill="#e31a1c") +
  theme_bw() +
  xlab("Number of haplotypes that signified a successful transmission event") + 
  ylab("Percentage of pairs with successful transmission event")
csp_abdomen_hap_pairs_plot

# export the plot
ggsave(csp_abdomen_hap_pairs_plot, filename="/Users/kelseysumner/Desktop/csp_abdomen_hap_pairs_over_time.png", device="png",
       height=5, width=8, units="in", dpi=400)


#### -------- make a plot of the number of samples within each haplotype ----- ####

# make separate data sets for humans and mosquitoes
human_haps = csp_haplotypes %>%
  filter(sample_type=="Human")
human_haps = human_haps[,c(4:301)]
abdomen_haps = csp_haplotypes %>%
  filter(sample_type=="Abdomen")
abdomen_haps = abdomen_haps[,c(4:301)]

# summarize the number of samples within each haplotype for the human samples
haplotype.names = rep(1:ncol(human_haps))
haplotypes_in_samples = rep(NA,ncol(human_haps))
total_reads_in_samples = rep(NA,ncol(human_haps))
for (k in 1:ncol(human_haps)){
  haplotypes_in_samples[k] = length(which(human_haps[,k] > 0))
  total_reads_in_samples[k] = sum(human_haps[,k],na.rm=T)
}
human_hap_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)

# summarize the number of samples within each haplotype for the mosquito abdomen samples
haplotype.names = rep(1:ncol(abdomen_haps))
haplotypes_in_samples = rep(NA,ncol(abdomen_haps))
total_reads_in_samples = rep(NA,ncol(abdomen_haps))
for (k in 1:ncol(abdomen_haps)){
  haplotypes_in_samples[k] = length(which(abdomen_haps[,k] > 0))
  total_reads_in_samples[k] = sum(abdomen_haps[,k],na.rm=T)
}
abdomen_hap_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)

hap_order = order(-human_hap_summary$haplotypes_across_samples)
human_hap_summary = human_hap_summary[hap_order,]
abdomen_hap_summary = abdomen_hap_summary[hap_order,]
human_hap_summary$haplotype_ids = factor(human_hap_summary$haplotype_ids, levels=human_hap_summary$haplotype_ids[order(-human_hap_summary$haplotypes_across_samples)])
abdomen_hap_summary$haplotype_ids = factor(abdomen_hap_summary$haplotype_ids, levels=abdomen_hap_summary$haplotype_ids[order(-human_hap_summary$haplotypes_across_samples)])

# make plot of human haplotypes
human_hap_plot = ggplot() +
  geom_bar(data=human_hap_summary,aes(x=haplotype_ids,y=haplotypes_across_samples),alpha=0.8,fill="#e31a1c",stat = "identity") +
  theme_bw() +
  xlab("Haplotype ID") + 
  ylab("Number of samples") +
  ggtitle("Human samples") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
human_hap_plot

# make plot of mosquito abdomen haplotypes
abdomen_hap_plot = ggplot() +
  geom_bar(data=abdomen_hap_summary,aes(x=haplotype_ids,y=haplotypes_across_samples),alpha=0.8,fill="#fdd0a2",stat = "identity") +
  theme_bw() +
  xlab("Haplotype ID") + 
  ylab("Number of samples") +
  ggtitle("Mosquito samples") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
abdomen_hap_plot


# put both csp moi plots on same grid
figure_number_samples_in_haplotypes = gridExtra::grid.arrange(human_hap_plot,abdomen_hap_plot,nrow=2)

# export the figure
ggsave(figure_number_samples_in_haplotypes, filename="/Users/kelseysumner/Desktop/figure_number_samples_in_haplotypes.png", device="png",
       height=10.5, width=17, units="in", dpi=400)




