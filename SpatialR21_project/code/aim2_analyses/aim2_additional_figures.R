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











