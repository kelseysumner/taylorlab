# ----------------------------------------- #
#  Create aim 2 visualizations for astmh    #
#             Mozzie Phase 1                #
#                CSP data                   #
#            October 31, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

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

# now make an alluvial plot of how infection status changes over time
figure3_plot = ggplot(plot_human_data_withperc,
       aes(x = month, stratum = infection_status, alluvium = unq_memID,
           y = perc_n,
           fill = infection_status, label = infection_status)) +
  geom_flow(na.rm=T,alpha=0.25) +
  geom_stratum() +
  scale_fill_manual(values=c("#ff7f00","#1f78b4")) +
  theme_bw() +
  xlab("Month")+
  ylab("Proportion of participants")+
  labs(fill="Infection status")+
  theme(plot.title = element_text(size = 30, face = "bold"), text = element_text(size=25)) 
figure3_plot
  
ggsave(figure3_plot, filename="/Users/kelseysumner/Desktop/figure3_plot_alluvial.png", device="png",
       height=10.5, width=22, units="in", dpi=500)


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

# reorder the group variable
plot_human_data_symp$symp_infection = as.factor(plot_human_data_symp$symp_infection)
plot_human_data_symp$symp_infection <- relevel(plot_human_data_symp$symp_infection, 'symptomatic infection')

# make a stacked bar plot of the symptomatic infections tested over time
plot4 = ggplot(data = plot_human_data_symp,aes(x=month,y=n,fill=symp_infection)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#e31a1c","#1f78b4")) +
  theme_bw() +
  xlab("Month")+
  ylab("Number of participants")+
  labs(fill="Infection status")+
  theme(plot.title = element_text(size = 30, face = "bold"), text = element_text(size=25)) 
plot4

# export the plot
# ggsave(plot4, filename="/Users/kelseysumner/Desktop/figure4_plot_stackedsymp.png", device="png",
 # height=10.5, width=22, units="in", dpi=500)


#### ------ make a plot of the mois for humans and mosquito abdomens for csp ------ ####

### create histograms of moi subset by sample type

# create a summarized data frame of the number of abdomens with each MOI for csp
# for humans
csp_human_df <- csp_haplotypes %>% 
  filter(!(is.na(haplotype_number)) & sample_type=="Human") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
csp_human_df$haplotype_number = as.numeric(csp_human_df$haplotype_number)
sum(csp_human_df$n) 
# for abdomens
csp_abdomen_df <- csp_haplotypes %>% 
  filter(!(is.na(haplotype_number)) & sample_type=="Abdomen") %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
csp_abdomen_df$haplotype_number = as.numeric(csp_abdomen_df$haplotype_number)
sum(csp_abdomen_df$n)

# make csp moi figures by sample type
# for human samples
csp_human_title <- expression(paste(italic("pfcsp"), ": Human DBS"))
csp_human_plot = ggplot() +
  geom_bar(data=csp_human_df,aes(x=haplotype_number,y=n), alpha=0.95,stat="identity",fill="#e31a1c") +
  labs(x="Multiplicity of infection", y="Number of samples", title= csp_human_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,240,300,360), limits=c(0,320)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_human_plot
# for abdomen samples
csp_abdomen_title <- expression(paste(italic("pfcsp"), ": Mosquito abdomens"))
csp_abdomen_plot = ggplot() +
  geom_bar(data=csp_abdomen_df,aes(x=haplotype_number,y=n), alpha=0.95,stat="identity",fill="#fdd0a2") +
  labs(x="Multiplicity of infection", y="Number of samples", title= csp_abdomen_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,240,300,360), limits=c(0,320)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_abdomen_plot

# put both csp moi plots on same grid
figure2_csp_subset_moi = gridExtra::grid.arrange(csp_human_plot,csp_abdomen_plot,ncol=2)

# export the figure
ggsave(figure2_csp_subset_moi, filename="/Users/kelseysumner/Desktop/figure2_csp_subset_moi.png", device="png",
       height=10.5, width=17, units="in", dpi=400)

# calculate median values
# for csp
csp_summary = csp_haplotypes %>%
  group_by(sample_type) %>%
  summarize(median=median(haplotype_number), mean=mean(haplotype_number))
median(csp_haplotypes$haplotype_number)


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



