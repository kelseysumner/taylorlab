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

# read in the combined ama and csp data set for mosquito abdomens
edgelist_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/spat21_aim2_merged_data_with_weights_16DEC2019.rds")

# read in the clean ama haplotype data
ama_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")

# read in the clean csp haplotype data
csp_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")



#### -------- calculate the prevalence of each haplotype in the general population -------- ####

## for csp

# first subset the haplotype table
csp_hap_prevalence_data = csp_haplotypes 
csp_hap_prevalence_data = csp_hap_prevalence_data[,c(4:301)]

# summarize the number of samples within each haplotype for the asymp human samples
csp_haplotype_names = rep(1:ncol(csp_hap_prevalence_data))
csp_haplotypes_in_samples = rep(NA,ncol(csp_hap_prevalence_data))
csp_total_reads_in_samples = rep(NA,ncol(csp_hap_prevalence_data))
for (k in 1:ncol(csp_hap_prevalence_data)){
  csp_haplotypes_in_samples[k] = length(which(csp_hap_prevalence_data[,k] > 0))
  csp_total_reads_in_samples[k] = sum(csp_hap_prevalence_data[,k],na.rm=T)
}
csp_hap_summary = data.frame("csp_haplotype_ids" = csp_haplotype_names, "csp_haplotypes_across_samples" = csp_haplotypes_in_samples, "csp_total_reads_across_samples" = csp_total_reads_in_samples)

# add a column that is the haplotype prevalence
csp_hap_summary$csp_hap_prevalence = csp_hap_summary$csp_haplotypes_across_samples/nrow(csp_haplotypes)

# add an H to every haplotype id
csp_hap_summary$csp_haplotype_ids = paste0("H",csp_hap_summary$csp_haplotype_ids)


## for ama

# first subset the haplotype table
ama_hap_prevalence_data = ama_haplotypes 
ama_hap_prevalence_data = ama_hap_prevalence_data[,c(4:459)]

# summarize the number of samples within each haplotype for the asymp human samples
ama_haplotype_names = rep(1:ncol(ama_hap_prevalence_data))
ama_haplotypes_in_samples = rep(NA,ncol(ama_hap_prevalence_data))
ama_total_reads_in_samples = rep(NA,ncol(ama_hap_prevalence_data))
for (k in 1:ncol(ama_hap_prevalence_data)){
  ama_haplotypes_in_samples[k] = length(which(ama_hap_prevalence_data[,k] > 0))
  ama_total_reads_in_samples[k] = sum(ama_hap_prevalence_data[,k],na.rm=T)
}
ama_hap_summary = data.frame("ama_haplotype_ids" = ama_haplotype_names, "ama_haplotypes_across_samples" = ama_haplotypes_in_samples, "ama_total_reads_across_samples" = ama_total_reads_in_samples)

# add a column that is the haplotype prevalence
ama_hap_summary$ama_hap_prevalence = ama_hap_summary$ama_haplotypes_across_samples/nrow(ama_haplotypes)

# add an H to every haplotype id
ama_hap_summary$ama_haplotype_ids = paste0("H",ama_hap_summary$ama_haplotype_ids)




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



#### -------- make the probability TE curve for the time between samples ------- ####

# create a formula for the P(TE) across time

# set up the variables
summary(merged_data$date_difference)
merged_data$date_difference = as.numeric(merged_data$date_difference)
merged_data$date_difference_flipped = merged_data$date_difference*-1
summary(merged_data$date_difference_flipped)


# calculate p(TE) using the gaussian distribution formula
s = 4
mu = -5
p_te_t = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$date_difference_flipped[i] >= -18 & merged_data$date_difference_flipped[i] <= 0){
    p_te_t[i] = (1/(s * sqrt(2*pi))) * exp(-((merged_data$date_difference_flipped[i]-mu)^2)/(2*s^2))
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
plot(p_te_t_df$date_difference_flipped,p_te_t_df$p_te_t)
# calculate the area under the curve
AUC = trapz(p_te_t_df$date_difference_flipped,p_te_t_df$p_te_t) # -14.74347
sum(p_te_t_df$p_te_t) # 504.6743






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

# first combine the pfcsp and pfama1 haplotypes
p_te_a_c_combo = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$p_te_a[i] != 0 & merged_data$p_te_c[i] != 0){
    p_te_a_c_combo[i] = 1-(1-merged_data$p_te_a[i])*(1-merged_data$p_te_c[i])
  } else if (merged_data$p_te_a[i] != 0 & merged_data$p_te_c[i] == 0){
    p_te_a_c_combo[i] = 1-(1-merged_data$p_te_a[i])
  } else if (merged_data$p_te_a[i] == 0 & merged_data$p_te_c[i] != 0){
    p_te_a_c_combo[i] = 1-(1-merged_data$p_te_c[i])
  } else{
    p_te_a_c_combo[i] = 0
  }
}
summary(p_te_a_c_combo)
merged_data$p_te_a_c_combo = p_te_a_c_combo
length(which(p_te_a_c_combo == 0)) # 60797
length(which(merged_data$p_te_a == 0 & merged_data$p_te_c == 0)) # 60797


# rescale the variables to all have to be between 0 and 1
summary(merged_data$p_te_a_c_combo)
summary(merged_data$p_te_d) 
summary(merged_data$p_te_t)
# rescale for p_te_a_c_combo
merged_data$rescaled_p_te_a_c_combo = (merged_data$p_te_a_c_combo-min(merged_data$p_te_a_c_combo))/(max(merged_data$p_te_a_c_combo)-min(merged_data$p_te_a_c_combo))
hist(merged_data$p_te_a_c_combo)
hist(merged_data$rescaled_p_te_a_c_combo)
summary(merged_data$rescaled_p_te_a_c_combo)
# rescale for p_te_d
merged_data$rescaled_p_te_d = (merged_data$p_te_d-min(merged_data$p_te_d))/(max(merged_data$p_te_d)-min(merged_data$p_te_d))
hist(merged_data$p_te_d)
hist(merged_data$rescaled_p_te_d)
summary(merged_data$rescaled_p_te_d) # stays same
# rescale for p_te_t
merged_data$rescaled_p_te_t = (merged_data$p_te_t-min(merged_data$p_te_t))/(max(merged_data$p_te_t)-min(merged_data$p_te_t))
hist(merged_data$p_te_t)
hist(merged_data$rescaled_p_te_t)
summary(merged_data$rescaled_p_te_t)


# make a final variable that is P(TEall)
# have that variable conditioned so you only calculate the probability of transmission if p_te is non-zero for all 4 variables
p_te_all = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$rescaled_p_te_t[i] != 0 & merged_data$rescaled_p_te_d[i] != 0 & merged_data$rescaled_p_te_a_c_combo[i] != 0){
    p_te_all[i] = merged_data$rescaled_p_te_t[i]*merged_data$rescaled_p_te_d[i]*merged_data$rescaled_p_te_a_c_combo[i]
  } else {
    p_te_all[i] = 0
  }
}
summary(p_te_all)
merged_data$p_te_all = p_te_all
length(which(p_te_all == 0)) # 167948
length(which(p_te_all > 0)) # 2262
length(which(merged_data$rescaled_p_te_t != 0 & merged_data$rescaled_p_te_d != 0 & merged_data$rescaled_p_te_a_c_combo != 0)) # 2262


# export the data set 
# write_csv(merged_data,"Desktop/spat21_aim2_merged_data_with_weights_16DEC2019.csv")
# write_rds(merged_data,"Desktop/spat21_aim2_merged_data_with_weights_16DEC2019.rds")




