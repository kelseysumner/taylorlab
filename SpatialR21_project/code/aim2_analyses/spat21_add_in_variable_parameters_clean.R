# ------------------------------------------- #
#  Add in variable parameters for comp model  #
#               Mozzie Phase 1                #
#                Clean version                #
#                   Aim 2                     #
#             CSP and AMA data                #
#            December 20, 2019                #
#                K. Sumner                    #
# ------------------------------------------- #

#### --------- load packages ----------------- ####
library(tidyverse)
library(msm)
require(pracma)


#### ---------- read in the data sets ---------- ####

# read in the combined ama and csp data set for mosquito abdomens - old data set used to originally create these variables
# merged_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/spat21_aim2_merged_data_with_weights_4FEB2020.rds")

# read in the model data set (already subset to P(TEt)>0 and P(TEd)>0) - new data set with some of these variables already in
merged_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/model data/spat21_final_model_data_set_11FEB2020.rds")

# read in the clean ama haplotype data
ama_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")

# read in the clean csp haplotype data
csp_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")



#### -------- calculate the prevalence of each haplotype in the general population -------- ####

## for csp

# first subset the haplotype table
csp_hap_prevalence_data = csp_haplotypes 
csp_ids = unique(merged_data$sample_id_human)
csp_ids_2 = unique(merged_data$sample_id_abdomen)
csp_ids = c(csp_ids,csp_ids_2)
csp_hap_prevalence_data = csp_hap_prevalence_data %>%
  filter(sample_name_dbs %in% csp_ids) # 1046 correct
table(csp_hap_prevalence_data$sample_type, useNA = "always") # correct
csp_hap_prevalence_data = csp_hap_prevalence_data[,c(4:301)]

# summarize the number of samples within each haplotype for the asymp human samples
csp_haplotype_names = colnames(csp_hap_prevalence_data)
csp_haplotypes_in_samples = rep(NA,ncol(csp_hap_prevalence_data))
csp_total_reads_in_samples = rep(NA,ncol(csp_hap_prevalence_data))
for (k in 1:ncol(csp_hap_prevalence_data)){
  csp_haplotypes_in_samples[k] = length(which(csp_hap_prevalence_data[,k] > 0))
  csp_total_reads_in_samples[k] = sum(csp_hap_prevalence_data[,k],na.rm=T)
}
csp_hap_summary = data.frame("csp_haplotype_ids" = csp_haplotype_names, "csp_haplotypes_across_samples" = csp_haplotypes_in_samples, "csp_total_reads_across_samples" = csp_total_reads_in_samples)

# now delete the haplotypes not present in these samples (should have 229 left)
csp_hap_summary = csp_hap_summary %>%
  filter(csp_total_reads_across_samples > 0) # 229 correct

# add a column that is the haplotype prevalence
csp_hap_summary$csp_hap_prevalence = csp_hap_summary$csp_haplotypes_across_samples/nrow(csp_hap_prevalence_data)

# write out the haplotype prevalence table
# write_csv(csp_hap_summary,"Desktop/csp_hap_prevalence_summary.csv")


## for ama

# first subset the haplotype table
ama_hap_prevalence_data = ama_haplotypes 
ama_ids = unique(merged_data$sample_id_human)
ama_ids_2 = unique(merged_data$sample_id_abdomen)
ama_ids = c(ama_ids,ama_ids_2)
ama_hap_prevalence_data = ama_hap_prevalence_data %>%
  filter(sample_name_dbs %in% ama_ids) # 901 correct
table(ama_hap_prevalence_data$sample_type, useNA = "always") # correct
ama_hap_prevalence_data = ama_hap_prevalence_data[,c(4:459)]

# summarize the number of samples within each haplotype for the asymp human samples
ama_haplotype_names = colnames(ama_hap_prevalence_data)
ama_haplotypes_in_samples = rep(NA,ncol(ama_hap_prevalence_data))
ama_total_reads_in_samples = rep(NA,ncol(ama_hap_prevalence_data))
for (k in 1:ncol(ama_hap_prevalence_data)){
  ama_haplotypes_in_samples[k] = length(which(ama_hap_prevalence_data[,k] > 0))
  ama_total_reads_in_samples[k] = sum(ama_hap_prevalence_data[,k],na.rm=T)
}
ama_hap_summary = data.frame("ama_haplotype_ids" = ama_haplotype_names, "ama_haplotypes_across_samples" = ama_haplotypes_in_samples, "ama_total_reads_across_samples" = ama_total_reads_in_samples)

# now delete the haplotypes not present in these samples (should have 229 left)
ama_hap_summary = ama_hap_summary %>%
  filter(ama_total_reads_across_samples > 0) # 348 correct

# add a column that is the haplotype prevalence
ama_hap_summary$ama_hap_prevalence = ama_hap_summary$ama_haplotypes_across_samples/nrow(ama_hap_prevalence_data)

# write out the haplotype prevalence table
# write_csv(ama_hap_summary,"Desktop/ama_hap_prevalence_summary.csv")



#### ------- calculate haplotype probability values for ama and csp -------- ####


# calculate the P(TE) for csp based on the number and prevalence of haplotypes
p_te_c = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$csp_haps_shared[i] > 0 & !(is.na(merged_data$csp_haps_shared[i]))){
    cum_prop = 1
    split_list = str_split(merged_data$csp_list_haps_shared[i],",")[[1]]
    for (j in 1:merged_data$csp_haps_shared[i]) {
      for (k in 1:nrow(csp_hap_summary)){
        if (split_list[j] == csp_hap_summary$csp_haplotype_ids[k]){
          cum_prop = cum_prop*(1-csp_hap_summary$csp_hap_prevalence[k])
          }
        }
      }
      p_te_c[i] = 1 - cum_prop
  } else {
    p_te_c[i] = 0
  }
}
# add the new variable to the data set
merged_data$p_te_c = p_te_c




# calculate the P(TE) for ama based on the number and prevalence of haplotypes
p_te_a = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$ama_haps_shared[i] > 0 & !(is.na(merged_data$ama_haps_shared[i]))){
    cum_prop = 1
    split_list = str_split(merged_data$ama_list_hap_shared[i],",")[[1]]
    for (j in 1:merged_data$ama_haps_shared[i]) {
      for (k in 1:nrow(ama_hap_summary)){
        if (split_list[j] == ama_hap_summary$ama_haplotype_ids[k]){
          cum_prop = cum_prop*(1-ama_hap_summary$ama_hap_prevalence[k])
        }
      }
    }
    p_te_a[i] = 1 - cum_prop
  } else {
    p_te_a[i] = 0
  }
}
# add the new variable to the data set
merged_data$p_te_a = p_te_a



#### ------- ALTERNATE METHOD PENALIZING HIGH MOI: calculate haplotype probability values for ama and csp -------- ####

# read in the model data set (already subset to P(TEt)>0 and P(TEd)>0)
# merged_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/model data/spat21_final_model_data_set_11FEB2020.rds")


# calculate the P(TE) for csp based on the number and prevalence of haplotypes
p_te_c_alt = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$csp_haps_shared[i] > 0 & !(is.na(merged_data$csp_haps_shared[i]))){
    cum_prop = 1
    split_list = str_split(merged_data$csp_list_haps_shared[i],",")[[1]]
    for (j in 1:merged_data$csp_haps_shared[i]) {
      for (k in 1:nrow(csp_hap_summary)){
        if (split_list[j] == csp_hap_summary$csp_haplotype_ids[k]){
          cum_prop = cum_prop*(1-csp_hap_summary$csp_hap_prevalence[k])
        }
      }
    }
    p_te_c_alt[i] = (1 - cum_prop)*(merged_data$csp_haps_shared[i]/merged_data$csp_moi[i])
  } else {
    p_te_c_alt[i] = 0
  }
}
# add the new variable to the data set
merged_data$p_te_c_alt = p_te_c_alt




# calculate the P(TE) for ama based on the number and prevalence of haplotypes
p_te_a_alt = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$ama_haps_shared[i] > 0 & !(is.na(merged_data$ama_haps_shared[i]))){
    cum_prop = 1
    split_list = str_split(merged_data$ama_list_hap_shared[i],",")[[1]]
    for (j in 1:merged_data$ama_haps_shared[i]) {
      for (k in 1:nrow(ama_hap_summary)){
        if (split_list[j] == ama_hap_summary$ama_haplotype_ids[k]){
          cum_prop = cum_prop*(1-ama_hap_summary$ama_hap_prevalence[k])
        }
      }
    }
    p_te_a_alt[i] = (1 - cum_prop)*(merged_data$ama_haps_shared[i]/merged_data$ama_moi[i])
  } else {
    p_te_a_alt[i] = 0
  }
}
# add the new variable to the data set
merged_data$p_te_a_alt = p_te_a_alt

# rescale the p_te_a and p_te_c variables
merged_data$p_te_a_alt_rescaled = (merged_data$p_te_a_alt-min(merged_data$p_te_a_alt))/(max(merged_data$p_te_a_alt)-min(merged_data$p_te_a_alt))
merged_data$p_te_c_alt_rescaled = (merged_data$p_te_c_alt-min(merged_data$p_te_c_alt))/(max(merged_data$p_te_c_alt)-min(merged_data$p_te_c_alt))

# make p_te_a_c_combo
# first combine the pfcsp and pfama1 haplotypes
p_te_a_c_combo_alt = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$p_te_a_alt_rescaled[i] != 0 & merged_data$p_te_c_alt_rescaled[i] != 0){
    p_te_a_c_combo_alt[i] = 1-(1-merged_data$p_te_a_alt_rescaled[i])*(1-merged_data$p_te_c_alt_rescaled[i])
  } else if (merged_data$p_te_a_alt_rescaled[i] != 0 & merged_data$p_te_c_alt_rescaled[i] == 0){
    p_te_a_c_combo_alt[i] = 1-(1-merged_data$p_te_a_alt_rescaled[i])
  } else if (merged_data$p_te_a_alt_rescaled[i] == 0 & merged_data$p_te_c_alt_rescaled[i] != 0){
    p_te_a_c_combo_alt[i] = 1-(1-merged_data$p_te_c_alt_rescaled[i])
  } else{
    p_te_a_c_combo_alt[i] = 0
  }
}
summary(p_te_a_c_combo_alt)
merged_data$p_te_a_c_combo_alt = p_te_a_c_combo_alt
length(which(p_te_a_c_combo_alt == 0)) # 1336
length(which(merged_data$p_te_a_alt == 0 & merged_data$p_te_c_alt == 0)) # 1336

# make a new p_te_all_alt
# have that variable conditioned so you only calculate the probability of transmission if p_te is non-zero for all 4 variables
p_te_all_alt = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$rescaled_p_te_t[i] != 0 & merged_data$rescaled_p_te_d[i] != 0 & merged_data$p_te_a_c_combo_alt[i] != 0){
    p_te_all_alt[i] = merged_data$rescaled_p_te_t[i]*merged_data$rescaled_p_te_d[i]*merged_data$p_te_a_c_combo_alt[i]
  } else {
    p_te_all_alt[i] = 0
  }
}
summary(p_te_all_alt)
merged_data$p_te_all_alt = p_te_all_alt
length(which(p_te_all_alt == 0)) # 1336
length(which(p_te_all_alt > 0)) # 2634
length(which(merged_data$rescaled_p_te_t != 0 & merged_data$rescaled_p_te_d != 0 & merged_data$p_te_a_c_combo_alt != 0)) # 2634



# now make a plot of the change in p_te_a_c_combo over moi
merged_data$p_te_a_c_combo_change = merged_data$p_te_a_c_combo_alt-merged_data$p_te_a_c_combo
hap_coding_change_plot = ggplot(data=merged_data,aes(x=mean_moi,y=p_te_a_c_combo_change)) +
  geom_point() +
  geom_smooth(method="loess") +
  xlab("Mean participant MOI") +
  ylab("Change in P(TE) for pfama1 and pfcsp combined") +
  theme_bw() +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
hap_coding_change_plot
plot(merged_data$p_te_a_alt,merged_data$p_te_a)
plot(merged_data$p_te_c_alt,merged_data$p_te_c)
summary(merged_data$p_te_a)
summary(merged_data$p_te_a_alt)
summary(merged_data$p_te_c)
summary(merged_data$p_te_c_alt)
summary(merged_data$p_te_a_c_combo)
summary(merged_data$p_te_a_c_combo_alt)
ggsave(hap_coding_change_plot, filename="/Users/kelseysumner/Desktop/hap_coding_change_plot.png", device="png",
       height=10, width=14, units="in", dpi=500)

# look at plot of new haplotypes measure over moi
hap_coding_alt_plot = ggplot(data=merged_data,aes(x=mean_moi,y=p_te_a_c_combo_alt)) +
  geom_point() +
  geom_smooth(method="loess",col="orange") +
  xlab("Mean participant MOI") +
  ylab("New formula for P(TE) for pfama1 and pfcsp combined") +
  theme_bw() +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
hap_coding_alt_plot
ggsave(hap_coding_alt_plot, filename="/Users/kelseysumner/Desktop/hap_coding_alt_plot.png", device="png",
       height=10, width=14, units="in", dpi=500)


# look at plot of old haplotypes measure over moi
hap_coding_old_plot = ggplot(data=merged_data,aes(x=mean_moi,y=p_te_a_c_combo)) +
  geom_point() +
  geom_smooth(method="loess",col="purple") +
  xlab("Mean participant MOI") +
  ylab("Old formula for P(TE) for pfama1 and pfcsp combined") +
  theme_bw() +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
hap_coding_old_plot
ggsave(hap_coding_old_plot, filename="/Users/kelseysumner/Desktop/hap_coding_old_plot.png", device="png",
       height=10, width=14, units="in", dpi=500)


# look at plot of new haplotypes measure over pfcsp haplotypes shared
hap_coding_alt_plot_csphaps = ggplot(data=merged_data,aes(x=csp_haps_shared,y=p_te_a_c_combo_alt)) +
  geom_point() +
  geom_smooth(method="loess",col="orange") +
  xlab("pfcsp haplotypes shared") +
  ylab("New formula for P(TE) for pfama1 and pfcsp combined") +
  theme_bw() +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
hap_coding_alt_plot_csphaps
ggsave(hap_coding_alt_plot_csphaps, filename="/Users/kelseysumner/Desktop/hap_coding_alt_plot_csphaps.png", device="png",
       height=10, width=14, units="in", dpi=500)

# look at plot of new haplotypes measure over pfcsp haplotypes shared
hap_coding_alt_plot_amahaps = ggplot(data=merged_data,aes(x=ama_haps_shared,y=p_te_a_c_combo_alt)) +
  geom_point() +
  geom_smooth(method="loess",col="purple") +
  xlab("pfama1 haplotypes shared") +
  ylab("New formula for P(TE) for pfama1 and pfcsp combined") +
  theme_bw() +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
hap_coding_alt_plot_amahaps
ggsave(hap_coding_alt_plot_amahaps, filename="/Users/kelseysumner/Desktop/hap_coding_alt_plot_amahaps.png", device="png",
       height=10, width=14, units="in", dpi=500)

# make density plots of old terms for p_te_a_c_combo
hap_coding_old_plot_density = ggplot(data=merged_data,aes(x=p_te_a_c_combo)) +
  geom_density(fill="purple") +
  ylab("Old formula for P(TE) for pfama1 and pfcsp combined") +
  theme_bw() +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
hap_coding_old_plot_density
ggsave(hap_coding_old_plot_density, filename="/Users/kelseysumner/Desktop/hap_coding_old_plot_density.png", device="png",
       height=10, width=14, units="in", dpi=500)

# make density plots of new terms for p_te_a_c_combo_alt
hap_coding_new_plot_density = ggplot(data=merged_data,aes(x=p_te_a_c_combo_alt)) +
  geom_density(fill="orange") +
  ylab("New formula for P(TE) for pfama1 and pfcsp combined") +
  theme_bw() +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
hap_coding_new_plot_density
ggsave(hap_coding_new_plot_density, filename="/Users/kelseysumner/Desktop/hap_coding_new_plot_density.png", device="png",
       height=10, width=14, units="in", dpi=500)




#### -------- make the probability TE curve for the time between samples ------- ####

# read back in the data set
# merged_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/interim/spat21_merged_data_interim.rds")


# create a formula for the P(TE) across time

# set up the variables
summary(merged_data$date_difference)
merged_data$date_difference = as.numeric(merged_data$date_difference)
merged_data$date_difference_flipped = merged_data$date_difference*-1
summary(merged_data$date_difference_flipped)



# calculate p(TE) 
p_te_t = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$date_difference_flipped[i] >= -14 & merged_data$date_difference_flipped[i] <= 7){
    p_te_t[i] = 1
  } else {
    p_te_t[i] = 0
  }
}
summary(p_te_t)
hist(p_te_t)
p_te_t_no_zeroes = p_te_t[which(p_te_t != 0)]
summary(p_te_t_no_zeroes)
hist(p_te_t_no_zeroes)
merged_data$p_te_t = p_te_t
p_te_t_df = merged_data %>%
  filter(p_te_t != 0)
plot(p_te_t_df$date_difference_flipped,p_te_t_df$p_te_t,xlab = "Days between human infection and mosquito collection",ylab="P(TE)")
time_plot = ggplot(data=p_te_t_df) +
  geom_line(aes(x=date_difference_flipped,y=p_te_t),linetype = "dashed") +
  xlab("Days between human infection and mosquito collection") +
  ylab("Probability of tranmission for time") +
  geom_vline(xintercept = -14,color="dark red") + 
  geom_vline(xintercept = 7,color="dark red") +
  theme_bw() +
  theme(text = element_text(size=25)) +
  scale_x_continuous(limits=c(-14,7),breaks=c(-14,-7,0,7)) +
  scale_y_continuous(limits=c(0,1))
time_plot
ggsave(time_plot, filename="/Users/kelseysumner/Desktop/theoretical_time_distribution_plot.png", device="png",
       height=8, width=12, units="in", dpi=500)






#### ------- make the probably TE curve for the distance between samples ------- ####

# create a formula for the P(TE) across distance 
# f(x) = λ {e}^{- λ x}

# look at the start values
summary(merged_data$distance)
str(merged_data$distance) # this is in m

# make distance in km
merged_data$distance_km = merged_data$distance/1000
summary(merged_data$distance_km)

# calculate using the exponential decay formula but over distance (what we use) - this is testing out the equation
# our actual equation is y=e^(-3x)
p_te_d = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$distance_km[i] >= 0 & merged_data$distance_km[i] <= 3){
    p_te_d[i] = exp(-merged_data$distance_km[i]*3)
  } else {
    p_te_d[i] = 0
  }
}
summary(p_te_d)
hist(p_te_d)
p_te_d_no_zeroes = p_te_d[which(p_te_d != 0)]
summary(p_te_d_no_zeroes)
hist(p_te_d_no_zeroes)
merged_data$p_te_d = p_te_d
p_te_d_df = merged_data %>%
  filter(p_te_d != 0)
plot(p_te_d_df$distance_km,p_te_d_df$p_te_d)

# look at a summary of p_te_d_df
summary(p_te_d_df$p_te_d)





#### ------- make a final variable of combined P(TEall) ------- ####

# rescale p_te_a and p_te_c
merged_data$rescaled_p_te_a = (merged_data$p_te_a-min(merged_data$p_te_a))/(max(merged_data$p_te_a)-min(merged_data$p_te_a))
merged_data$rescaled_p_te_c = (merged_data$p_te_c-min(merged_data$p_te_c))/(max(merged_data$p_te_c)-min(merged_data$p_te_c))

# first combine the pfcsp and pfama1 haplotypes
p_te_a_c_combo = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$rescaled_p_te_a[i] != 0 & merged_data$rescaled_p_te_c[i] != 0){
    p_te_a_c_combo[i] = 1-(1-merged_data$rescaled_p_te_a[i])*(1-merged_data$rescaled_p_te_c[i])
  } else if (merged_data$rescaled_p_te_a[i] != 0 & merged_data$rescaled_p_te_c[i] == 0){
    p_te_a_c_combo[i] = 1-(1-merged_data$rescaled_p_te_a[i])
  } else if (merged_data$rescaled_p_te_a[i] == 0 & merged_data$rescaled_p_te_c[i] != 0){
    p_te_a_c_combo[i] = 1-(1-merged_data$rescaled_p_te_c[i])
  } else{
    p_te_a_c_combo[i] = 0
  }
}
summary(p_te_a_c_combo)
merged_data$p_te_a_c_combo = p_te_a_c_combo
length(which(p_te_a_c_combo == 0)) # 1336
length(which(merged_data$p_te_a == 0 & merged_data$p_te_c == 0)) # 1336


# rescale the variables to all have to be between 0 and 1
summary(merged_data$p_te_a_c_combo)
summary(merged_data$p_te_d) 
summary(merged_data$p_te_t)
# no longer need to be rescaled


# make a final variable that is P(TEall)
# have that variable conditioned so you only calculate the probability of transmission if p_te is non-zero for all 4 variables
p_te_all = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$rescaled_p_te_t[i] != 0 & merged_data$rescaled_p_te_d[i] != 0 & merged_data$p_te_a_c_combo[i] != 0){
    p_te_all[i] = merged_data$rescaled_p_te_t[i]*merged_data$rescaled_p_te_d[i]*merged_data$p_te_a_c_combo[i]
  } else {
    p_te_all[i] = 0
  }
}
summary(p_te_all)
merged_data$p_te_all = p_te_all
length(which(p_te_all == 0)) # 1336
length(which(p_te_all > 0)) # 2634
length(which(merged_data$rescaled_p_te_t != 0 & merged_data$rescaled_p_te_d != 0 & merged_data$p_te_a_c_combo != 0)) # 2634


# export the data set 
write_csv(merged_data,"Desktop/spat21_aim2_merged_data_with_weights_14FEB2020.csv")
write_rds(merged_data,"Desktop/spat21_aim2_merged_data_with_weights_14FEB2020.rds")



#### ------- make some plots of the output -------- ####


# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC


# make a plot of p_te_t
# time plot
p_te_t_density_plot_x = ggplot(data=merged_data,aes(x=date_difference_flipped)) +
  geom_density(alpha=0.6,fill=c("#c2a5cf")) + 
  theme_bw() + 
  xlab("Days between human infection and mosquito collection") +
  ylab("Density") +
  ggtitle("Density of observations over time")
p_te_t_density_plot_x
dpb <- ggplot_build(p_te_t_density_plot_x)
x1 <- min(which(dpb$data[[1]]$x >=-14))
x2 <- max(which(dpb$data[[1]]$x <=7))
p_te_t_density_plot_x = p_te_t_density_plot_x +
  geom_area(data=data.frame(x=dpb$data[[1]]$x[x1:x2],
                            y=dpb$data[[1]]$y[x1:x2]),
            aes(x=x, y=y), fill="#762a83", colour = "black") +
  scale_x_continuous(breaks=c(400,300,200,100,7,-14,-100,-200,-300,-400)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
ggsave(p_te_t_density_plot_x, filename="/Users/kelseysumner/Desktop/p_te_t_density_plot_x.png", device="png",
       height=8, width=14, units="in", dpi=500)



# make a plot of p_te_d
p_te_d_density_plot_x = ggplot(data=merged_data,aes(x=distance_km)) +
  geom_density(alpha=0.6,fill=c("#fdae6b")) + 
  theme_bw() + 
  xlab("Distance (Km) between human infection and mosquito collection") +
  ylab("Density") +
  ggtitle("Density of observations over distance")
p_te_d_density_plot_x
dpb <- ggplot_build(p_te_d_density_plot_x)
x1 <- min(which(dpb$data[[1]]$x >=0))
x2 <- max(which(dpb$data[[1]]$x <=3))
p_te_d_density_plot_x = p_te_d_density_plot_x +
  geom_area(data=data.frame(x=dpb$data[[1]]$x[x1:x2],
                            y=dpb$data[[1]]$y[x1:x2]),
            aes(x=x, y=y), fill="#f16913", colour = "black") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25)) +
  scale_x_continuous(breaks=c(0,3,6,9,12),limits=c(0,12))
ggsave(p_te_d_density_plot_x, filename="/Users/kelseysumner/Desktop/p_te_d_density_plot_x.png", device="png",
       height=8, width=14, units="in", dpi=500)




# make a plot of the ama and csp haplotype probability values
# set up the data frame
p_te_a_df = merged_data %>%
  filter(p_te_a > 0) %>%
  select(p_te_a,ama_haps_shared)
p_te_a_df$type = rep("pfama1",nrow(p_te_a_df))
p_te_c_df = merged_data %>%
  filter(p_te_c > 0) %>%
  select(p_te_c,csp_haps_shared)
p_te_c_df$type = rep("pfcsp",nrow(p_te_c_df))
p_te_a_df$x_axis = as.factor(p_te_a_df$ama_haps_shared)
p_te_c_df$x_axis = as.factor(p_te_c_df$csp_haps_shared)
p_te_a_df$ama_haps_shared <- NULL
p_te_c_df$csp_haps_shared <- NULL
colnames(p_te_a_df)
colnames(p_te_c_df)
p_te_a_df = rename(p_te_a_df,"probability"=p_te_a)
p_te_c_df = rename(p_te_c_df,"probability"=p_te_c)
p_te_a_c_combo_df = rbind(p_te_a_df,p_te_c_df)
colnames(p_te_a_c_combo_df)
# make the plot
p_te_a_c_combo = ggplot() +
  geom_boxplot(data=p_te_a_c_combo_df,aes(x=factor(x_axis),y=probability,fill=type),alpha=0.8) + 
  theme_bw() + 
  ylab("Probability of transmission") +
  xlab("Number of haplotypes shared") +
  scale_fill_manual(values=c("#081d58","#fb9a99")) +
  labs(fill = "Sequencing target") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25), legend.position = c(0.86,0.12), legend.box.background = element_rect(colour = "black"))
p_te_a_c_combo
# another way
p_te_a_c_combo = ggplot() +
  geom_jitter(data=p_te_a_c_combo_df,aes(x=factor(x_axis),y=probability,colour=type),alpha=0.6,width=0.2) + 
  geom_boxplot(data=p_te_a_c_combo_df,aes(x=factor(x_axis),y=probability,fill=type),alpha=0.05) + 
  theme_bw() + 
  ylab("Probability of transmission") +
  xlab("Number of haplotypes shared") +
  scale_colour_manual(values=c("#081d58","#fb9a99")) +
  labs(colour = "Sequencing target",fill="Sequencing target") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25), legend.position = c(0.86,0.12), legend.box.background = element_rect(colour = "black"))
p_te_a_c_combo
ggsave(p_te_a_c_combo, filename="/Users/kelseysumner/Desktop/p_te_a_density_plot_x.png", device="png",
       height=8, width=14, units="in", dpi=500)



# make a plot of p_te_all by village
p_te_all_subset = merged_data %>%
  filter(p_te_t > 0 & p_te_d > 0)
p_te_all_village = ggplot(data=p_te_all_subset) +
  geom_boxplot(aes(x=factor(village_name),y=p_te_all,fill=village_name), alpha=0.8) +
  theme_bw()+
  xlab("Village")+
  ylab("Probability of transmission across all variables") +
  theme(legend.position = "none", text = element_text(size=25))
p_te_all_village
ggsave(p_te_all_village, filename="/Users/kelseysumner/Desktop/p_te_all_village.png", device="png",
       height=8, width=14, units="in", dpi=500)



# make a plot of p_te_all by symptomatic status
p_te_all_exposure = ggplot(data=p_te_all_subset) +
  geom_boxplot(aes(x=factor(aim2_exposure),y=p_te_all,fill=aim2_exposure), alpha=0.8) +
  theme_bw()+
  xlab("Symptomatic status")+
  scale_fill_manual(values=c("#E1AF00","#3B9AB2")) +
  ylab("Probability of transmission across all variables") +
  theme(legend.position = "none",text = element_text(size=25))
p_te_all_exposure
ggsave(p_te_all_exposure, filename="/Users/kelseysumner/Desktop/p_te_all_exposure.png", device="png",
       height=8, width=14, units="in", dpi=500)



# make a plot of p_te_all by mosquito week count
p_te_all_mosquito_collection_df = p_te_all_subset %>%
  group_by(mosquito_date) %>%
  summarize(mean_p_te_all = mean(p_te_all))
p_te_all_mosquito_collection = ggplot(data=p_te_all_mosquito_collection_df) +
  geom_bar(aes(x=mosquito_date,y=mean_p_te_all), stat="identity") +
  theme_bw()+
  xlab("Week of mosquito collection")+
  ylab("Probability of transmission across all variables") +
  scale_x_date(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p_te_all_mosquito_collection
ggsave(p_te_all_mosquito_collection, filename="/Users/kelseysumner/Desktop/p_te_all_mosquito_collection.png", device="png",
       height=8, width=16, units="in", dpi=500)

# make a plot of p_te_all by mosquito week count colored by village
p_te_all_mosquito_collection_df = p_te_all_subset %>%
  group_by(mosquito_date,village_name) %>%
  summarize(mean_p_te_all = mean(p_te_all))
p_te_all_mosquito_collection = ggplot(data=p_te_all_mosquito_collection_df) +
  geom_bar(aes(x=mosquito_date,y=mean_p_te_all,fill=village_name), stat="identity") +
  theme_bw()+
  xlab("Date of mosquito collection")+
  ylab("Mean daily probability of transmission across all variables") +
  scale_x_date(date_breaks = "1 week") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill="Village")
p_te_all_mosquito_collection
ggsave(p_te_all_mosquito_collection, filename="/Users/kelseysumner/Desktop/p_te_all_mosquito_collection.png", device="png",
       height=8, width=16, units="in", dpi=500)


# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC




