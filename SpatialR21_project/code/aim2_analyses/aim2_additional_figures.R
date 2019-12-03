# ----------------------------------------- #
#  Create additional aim 2 visualizations   #
#             Mozzie Phase 1                #
#                CSP data                   #
#            December 2, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(tidyverse)
library(devtools)
library(streamgraph)
library(lubridate)
library(ggalluvial)
library(gridExtra)
library(geosphere)


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

# read in the coordinates data set: NOTE - don't have coordinates for the two late entry households
coordinates_data <- read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/household_coordinates/Cohort coordinates.csv")

# read in the data set of full haplotype sharing within 14 days without the household restriction
hap_sharing_pre_household <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_edgelist_abdomen_unrestricted_by_hh_29OCT2019.rds")



#### -------- merge data sets to look at haplotypes shared across distance ------- ####

# only allow 1 hh structure per hh
length(unique(coordinates_data$HH_ID)) # 36
coordinates_data = coordinates_data[-which(coordinates_data$hhlatitude=="0.6164038"),]
coordinates_data = coordinates_data[-which(coordinates_data$hhlatitude=="0.5774058"),]

# merge csp abdomens with household coordinates data for human samples
merged_data = left_join(hap_sharing_pre_household,coordinates_data,by="HH_ID")

# change the hh_id column name
merged_data = merged_data %>%
  rename(HH_ID_human = HH_ID, hhlatitude_human = hhlatitude,hhlongitude_human = hhlongitude)

# create a new variable that is the mosquito hh_id
for (i in 1:nrow(merged_data)){
  merged_data$HH_ID[i] = str_split(merged_data$sample_id_abdomen[i]," ")[[1]][1]
}
table(merged_data$HH_ID, useNA = "always")

# now merge csp abdomens with household coordinates for the mosquitoes
merged_data = left_join(merged_data,coordinates_data,by="HH_ID")

# change column names
colnames(merged_data)
merged_data = merged_data %>%
  rename(HH_ID_mosquito = HH_ID, hhlatitude_mosquito = hhlatitude,hhlongitude_mosquito = hhlongitude) %>%
  select(-c(hhaltitude.x,hhaltitude.y,HHID.x,HHID.y,hhaccuracy.x,hhaccuracy.y))

# check missingness
length(which(is.na(merged_data$hhlatitude_human)))
length(which(is.na(merged_data$hhlongitude_human)))
length(which(is.na(merged_data$hhlatitude_mosquito)))
length(which(is.na(merged_data$hhlongitude_mosquito)))

# create a variable that is the distance between each household (in meters)
merged_data$distance = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  merged_data$distance[i] = distm(c(merged_data$hhlongitude_human[i], merged_data$hhlatitude_human[i]), c(merged_data$hhlongitude_mosquito[i], merged_data$hhlatitude_mosquito[i]), fun = distHaversine)
}
summary(merged_data$distance)



#### ------ create a plot of haplotype sharing across distance ------- ####

# split into an asymp and symp data sets for the plot
asymp_data = merged_data %>% filter(aim2_exposure == "asymptomatic infection")
symp_data = merged_data %>% filter(aim2_exposure == "symptomatic infection")

# create a plot where the x axis is distance in m between households and y axis is number of haplotypes shared
# make a density plot
density_plot = ggplot() +
  geom_density(data=asymp_data,aes(x=distance,y=haps_shared), stat = "identity", fill = "#ff7f00", colour = "#ff7f00", alpha=0.7) +
  geom_density(data=symp_data,aes(x=distance,y=haps_shared), stat = "identity", fill= "#e31a1c", colour = "#e31a1c", alpha=0.7) +
  theme_bw() +
  labs(x="Distance between human and mosquito (m)",y="Number of haplotypes shared") 
density_plot
ggsave(density_plot, filename="/Users/kelseysumner/Desktop/density_plot.png", device="png",
       height=4, width=5, units="in", dpi=500)

# make a dot plot for correlation
correlation_plot = ggplot() +
  geom_point(data=asymp_data,aes(x=distance,y=haps_shared), stat = "identity", fill = "#ff7f00", colour = "#ff7f00", alpha=0.7) +
  geom_point(data=symp_data,aes(x=distance,y=haps_shared), stat = "identity", fill= "#e31a1c", colour = "#e31a1c", alpha=0.7) +
  geom_smooth(data=asymp_data,aes(x=distance,y=haps_shared),colour = "#ff7f00",method = "glm", method.args = list(family = "gaussian"), se = TRUE,lwd=1.1) +
  geom_smooth(data=symp_data,aes(x=distance,y=haps_shared),colour = "#e31a1c",method = "glm", method.args = list(family = "gaussian"), se = TRUE,lwd=1.1) +
  theme_bw() +
  labs(x="Distance between human and mosquito (m)",y="Number of haplotypes shared") 
correlation_plot
ggsave(correlation_plot, filename="/Users/kelseysumner/Desktop/correlation_plot.png", device="png",
       height=4, width=5, units="in", dpi=500)

# create distance categories
table(merged_data$distance, useNA = "always")
summary(merged_data$distance)
merged_data$distance_categories = ifelse(merged_data$distance < 4000,"< 4000 m",
                                         ifelse(merged_data$distance >= 4000 & merged_data$distance < 8000, "4000-8000 m","> 8000 m"))
merged_data$distance_categories = factor(merged_data$distance_categories, levels = c("< 4000 m", "4000-8000 m","> 8000 m","NA"))


# make a violin plot
violin_plot = ggplot() +
  geom_violin(data=merged_data,aes(x=distance_categories,y=haps_shared), fill = "#ff7f00", colour = "#ff7f00", alpha=0.7) +
  theme_bw() +
  labs(x="Distance between human and mosquito (m)",y="Number of haplotypes shared") 
violin_plot
ggsave(violin_plot, filename="/Users/kelseysumner/Desktop/violin_plot.png", device="png",
       height=4, width=5, units="in", dpi=500)


#### -------- make a plot of the number of samples within each haplotype ----- ####

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

hap_order = order(-asymp_human_hap_summary$haplotypes_across_samples)
asymp_human_hap_summary = asymp_human_hap_summary[hap_order,]
symp_human_hap_summary = symp_human_hap_summary[hap_order,]
abdomen_hap_summary = abdomen_hap_summary[hap_order,]
asymp_human_hap_summary$haplotype_ids = factor(asymp_human_hap_summary$haplotype_ids, levels=asymp_human_hap_summary$haplotype_ids[order(-asymp_human_hap_summary$haplotypes_across_samples)])
symp_human_hap_summary$haplotype_ids = factor(symp_human_hap_summary$haplotype_ids, levels=symp_human_hap_summary$haplotype_ids[order(-asymp_human_hap_summary$haplotypes_across_samples)])
abdomen_hap_summary$haplotype_ids = factor(abdomen_hap_summary$haplotype_ids, levels=abdomen_hap_summary$haplotype_ids[order(-asymp_human_hap_summary$haplotypes_across_samples)])

# make plot of asymptomatic human haplotypes
asymp_human_hap_plot = ggplot() +
  geom_bar(data=asymp_human_hap_summary,aes(x=haplotype_ids,y=haplotypes_across_samples),alpha=0.8,fill="#ff7f00",stat = "identity") +
  theme_bw() +
  xlab("Haplotype ID") + 
  ylab("Number of samples") +
  ggtitle("Asymptomatic human samples") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
asymp_human_hap_plot

# make plot of symptomatic human haplotypes
symp_human_hap_plot = ggplot() +
  geom_bar(data=symp_human_hap_summary,aes(x=haplotype_ids,y=haplotypes_across_samples),alpha=0.8,fill="#e31a1c",stat = "identity") +
  theme_bw() +
  xlab("Haplotype ID") + 
  ylab("Number of samples") +
  ggtitle("Symptomatic human samples") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
symp_human_hap_plot

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
figure_number_samples_in_haplotypes = gridExtra::grid.arrange(asymp_human_hap_plot,symp_human_hap_plot,abdomen_hap_plot,nrow=3)

# export the figure
ggsave(figure_number_samples_in_haplotypes, filename="/Users/kelseysumner/Desktop/figure_number_samples_in_haplotypes.png", device="png",
       height=10.5, width=17, units="in", dpi=400)








