# ------------------------------------------- #
#  Add in variable parameters for comp model  #
#               Mozzie Phase 1                #
#                   Aim 2                     #
#             CSP and AMA data                #
#            December 9, 2019                 #
#                K. Sumner                    #
# ------------------------------------------- #

#### --------- load packages ----------------- ####
library(tidyverse)
library(geosphere)
library(msm)
require(pracma)


#### ---------- read in the data sets ---------- ####

# read in the clean ama haplotype data
ama_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")

# read in the clean csp haplotype data
csp_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# read in the coordinates data set
coordinates_data <- read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/household_coordinates/Cohort coordinates_updated July 2019.csv")
coordinates_data$X7 <- NULL

# add ama abdomen edgelist
ama_edgelist <- read_rds("Desktop/clean_ids_haplotype_results/AMA/edgelists_no_restrictions/spat21_ama_edgelist_abdomen_no_restrictions_3DEC2019.rds")

# add csp abdomen edgelist
csp_edgelist <- read_rds("Desktop/clean_ids_haplotype_results/CSP/edgelists_no_restrictions/spat21_csp_edgelist_abdomens_no_restrictions_3DEC2019.rds")



#### ------------- create a merged data set between ama and csp  --------------- ####

# create a dataset that has all the ama and csp data merged into one

# join the ama and csp edgelists together
merged_data = full_join(csp_edgelist,ama_edgelist,by=c("sample_id_human","sample_id_abdomen"))

# look at the colnames
colnames(merged_data)

# rename some of the colnames
merged_data = merged_data %>%
  rename(csp_haps_shared = haps_shared.x,ama_haps_shared = haps_shared.y,csp_list_haps_shared = list_haplotypes_shared.x,ama_list_hap_shared = list_haplotypes_shared.y)

# reorder the columns
merged_data = merged_data %>%
  select(sample_id_human,sample_id_abdomen,csp_haps_shared,csp_list_haps_shared,ama_haps_shared,ama_list_hap_shared,human_date.x,sample_name_final.x,
         age_cat_baseline.x,unq_memID.x,village_name.x,HH_ID_human.x,pfr364Q_std_combined.x,age_all_baseline.x,aim2_exposure.x,HH_ID_mosquito.x,
         mosquito_date.x,total_num_mosq_in_hh.x,date_difference.x,human_date.y,sample_name_final.y,
         age_cat_baseline.y,unq_memID.y,village_name.y,HH_ID_human.y,pfr364Q_std_combined.y,age_all_baseline.y,aim2_exposure.y,HH_ID_mosquito.y,
         mosquito_date.y,total_num_mosq_in_hh.y,date_difference.y)
colnames(merged_data)

# write some code that merges the two columns
# now loop through each row and move over those columns
# check colnames
colnames(merged_data)
# start for loop to combine qpcr results
for (i in 1:nrow(merged_data)){
  if (is.na(merged_data[i,7])){
    for (k in 1:13){   # this is for all data that is present in .y files but not in .x
      startpoint = 6 + k
      merged_data[i,startpoint] = merged_data[i,startpoint+13]
      merged_data[i,startpoint+13] <- NA
    }
  } else if (is.na(merged_data[i,20])){
    for (k in 1:13){ # this is for all data that is present in .x files but not in .y -> just make it NULL
      startpoint = 6 + k
      merged_data[i,startpoint+13] <- NA
    }
  } else {
    for (k in 1:13){ # both data sets are missing -> just make it NULL at .y location
      startpoint = 6 + k
      merged_data[i,startpoint+13] <- NA
    }
  } 
}
# check the output
length(which(is.na(merged_data$mosquito_date.x))) # 554 missing (remember that there were 7 Hb missing so Pf results missing but merged in)
length(which(is.na(merged_data$mosquito_date.y))) # 5803 missing
# remove the .y columns
merged_data_final = merged_data[,-c(20:32)]
merged_data_final = rename(merged_data_final,
                           human_date = human_date.x, sample_name_final = sample_name_final.x, age_cat_baseline = age_cat_baseline.x,
                           unq_memID = unq_memID.x, village_name = village_name.x, HH_ID_human = HH_ID_human.x, pfr364Q_std_combined = pfr364Q_std_combined.x,
                           age_all_baseline = age_all_baseline.x, aim2_exposure = aim2_exposure.x, HH_ID_mosquito = HH_ID_mosquito.x,
                           mosquito_date = mosquito_date.x, total_num_mosq_in_hh = total_num_mosq_in_hh.x, date_difference = date_difference.x)
colnames(merged_data_final)
# check the missing hap info
length(which(is.na(merged_data_final$csp_haps_shared))) # 11369
length(which(is.na(merged_data_final$ama_haps_shared))) # 42506
sum(merged_data_final$csp_haps_shared, na.rm = T) # 188916
sum(merged_data_final$ama_haps_shared, na.rm = T) # 74863
sum(csp_edgelist$haps_shared, na.rm = T) # 188916
sum(ama_edgelist$haps_shared, na.rm = T) # 74863
# looks good


#### ------------- calculate the distance between human and mosquito samples --------- ####


# only allow 1 hh structure per hh
length(unique(coordinates_data$HH_ID)) # 38
coordinates_data = coordinates_data[-which(coordinates_data$hhlatitude=="0.6164038"),]
coordinates_data = coordinates_data[-which(coordinates_data$hhlatitude=="0.5774058"),]
length(unique(coordinates_data$HH_ID)) # 38

# merge csp abdomens with household coordinates data for human samples
coordinates_data = coordinates_data %>% rename("HH_ID_human" = "HH_ID")
merged_data_final_v2 = left_join(merged_data_final,coordinates_data,by="HH_ID_human")

# change the hh_id column name
merged_data_final_v2 = merged_data_final_v2 %>%
  rename(hhlatitude_human = hhlatitude,hhlongitude_human = hhlongitude)

# now merge csp abdomens with household coordinates for the mosquitoes
coordinates_data = coordinates_data %>% rename("HH_ID_mosquito" = "HH_ID_human")
merged_data = left_join(merged_data_final_v2,coordinates_data,by="HH_ID_mosquito")

# change column names
colnames(merged_data)
merged_data = merged_data %>%
  rename(hhlatitude_mosquito = hhlatitude,hhlongitude_mosquito = hhlongitude) %>%
  select(-c(hhaltitude.x,hhaltitude.y,HHID.x,HHID.y,hhaccuracy.x,hhaccuracy.y))

# check missingness
length(which(is.na(merged_data$hhlatitude_human))) # 0
length(which(is.na(merged_data$hhlongitude_human))) # 0
length(which(is.na(merged_data$hhlatitude_mosquito))) # 0
length(which(is.na(merged_data$hhlongitude_mosquito))) # 0

# create a variable that is the distance between each household (in meters)
merged_data$distance = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  merged_data$distance[i] = distm(c(merged_data$hhlongitude_human[i], merged_data$hhlatitude_human[i]), c(merged_data$hhlongitude_mosquito[i], merged_data$hhlatitude_mosquito[i]), fun = distHaversine)
}
summary(merged_data$distance)


#### ----------- export the data set -------- ####

# export the merged data
write_csv(merged_data,"Desktop/spat21_ama_and_csp_edgelist.csv")
write_rds(merged_data,"Desktop/spat21_ama_and_csp_edgelist.rds")


# read those data sets back in
merged_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/spat21_ama_and_csp_edgelist.rds")



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


# export this data set
# write_rds(merged_data,"Desktop/spat21_merged_data_interim.rds")
# read back in the data set
merged_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/interim/spat21_merged_data_interim.rds")



#### ------- make the probably TE curve for the distance between samples ------- ####

# create a formula for the P(TE) across distance 
# f(x) = λ {e}^{- λ x}

# look at the start values
summary(merged_data$distance)
str(merged_data$distance) # this is in m
# make distance in km
merged_data$distance_km = merged_data$distance/1000
summary(merged_data$distance_km)

# calculate p(TE) for distance using an exponential decay formula
p_te_d = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$distance_km[i] >= 0 & merged_data$distance_km[i] <= 5){
    p_te_d[i] = exp(-2.5*merged_data$distance_km[i])
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



# another way using the exponential decay formula but over distance (what we use)
p_te_d = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$distance_km[i] >= 0 & merged_data$distance_km[i] <= 3){
    p_te_d[i] = 1-pexp(merged_data$distance_km[i],rate=3)
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



# another way using the exponential decay formula but over distance (what we use) - this is testing out the equation
# out actual equation is y=e^(-3x)
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






#### -------- make the probability TE curve for the time between samples ------- ####

# create a formula for the P(TE) across time
# f(x) = λ {e}^{- λ x}

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




# calculate p(TE) for time using an exponential decay formula
p_te_t = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$date_difference_flipped[i] >= -18 & merged_data$date_difference_flipped[i] <= 0){
    p_te_t[i] = 0.05*exp(0.05*merged_data$date_difference_flipped[i])
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



# try fitting a logistic regression model
# formula Y=1/(1+e^(-(a+bX)))
p_te_t = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$date_difference_flipped[i] >= -18 & merged_data$date_difference_flipped[i] <= 0){
    p_te_t[i] = 1/(-exp(0.4*merged_data$date_difference_flipped[i]))
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
# set to a range between 0 and 1 - looks like this isn't working
merged_data$p_te_t = sort(merged_data$p_te_t)
head(merged_data$p_te_t)
tail(merged_data$p_te_t)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
test = range01(merged_data$p_te_t)
merged_data$p_te_t = test
summary(merged_data$p_te_t)
p_te_t_df = merged_data %>%
  filter(p_te_t != 0)
plot(p_te_t_df$date_difference_flipped,p_te_t_df$p_te_t)


# try fitting a logistic regression model but with positive numbers
# formula Y=1/(1+e^(-(a+bX)))
summary(merged_data$date_difference)
length(which(merged_data$date_difference >= 0 & merged_data$date_difference <= 18))
p_te_t = rep(NA,nrow(merged_data))
for (i in 1:nrow(merged_data)){
  if (merged_data$date_difference[i] >= 0 & merged_data$date_difference[i] <= 18){
    p_te_t[i] = 1/(exp(-0.6*merged_data$date_difference[i]))
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
plot(p_te_t_df$date_difference,p_te_t_df$p_te_t)






#### ------- make a final variable of combined P(TEall) ------- ####

# first play around with a way to combine the pfcsp and pfama1 haplotypes
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
write_csv(merged_data,"Desktop/spat21_aim2_merged_data_with_weights_16DEC2019.csv")
write_rds(merged_data,"Desktop/spat21_aim2_merged_data_with_weights_16DEC2019.rds")




#### -------- make plots of the distributions --------- ####

# make a plot of p_te_t
p_te_t_df = merged_data %>%
  filter(rescaled_p_te_t != 0)
p_te_t_plot = ggplot(data=p_te_t_df,aes(x=date_difference_flipped,y=rescaled_p_te_t,colour=factor(aim2_exposure))) +
  geom_point(alpha=0.7) + 
  scale_colour_manual(values=c("#ff7f00","#e31a1c")) + 
  labs(colour="Symptomatic status") +
  theme_bw() + 
  xlab("Days human sample collected before mosquito") +
  ylab("P(TE,t)")
p_te_t_plot
ggsave(p_te_t_plot, filename="/Users/kelseysumner/Desktop/p_te_t_plot.png", device="png",
       height=4, width=7, units="in", dpi=500)


# make a plot of p_te_d
p_te_d_df = merged_data %>%
  filter(rescaled_p_te_d != 0)
p_te_d_plot = ggplot(data=p_te_d_df,aes(x=distance_km,y=rescaled_p_te_d,colour=factor(aim2_exposure))) +
  geom_point(alpha=0.7) + 
  scale_colour_manual(values=c("#ff7f00","#e31a1c")) + 
  labs(colour="Symptomatic status") +
  theme_bw() + 
  xlab("Distance between human and mosquito samples (Km)") +
  ylab("P(TE,d)")
p_te_d_plot
ggsave(p_te_d_plot, filename="/Users/kelseysumner/Desktop/p_te_d_plot.png", device="png",
       height=4, width=7, units="in", dpi=500)


# make a plot of p_te_a
p_te_a_df = merged_data %>%
  filter(rescaled_p_te_a != 0)
p_te_a_plot = ggplot(data=p_te_a_df,aes(x=ama_haps_shared,y=rescaled_p_te_a,colour=factor(aim2_exposure))) +
  geom_point(alpha=0.7) + 
  scale_colour_manual(values=c("#ff7f00","#e31a1c")) + 
  labs(colour="Symptomatic status") +
  theme_bw() + 
  xlab("Number of pfama1 haplotypes shared") +
  ylab("P(TE,a)")
p_te_a_plot
ggsave(p_te_a_plot, filename="/Users/kelseysumner/Desktop/p_te_a_plot.png", device="png",
       height=4, width=7, units="in", dpi=500)


# make a plot of p_te_c
p_te_c_df = merged_data %>%
  filter(p_te_c != 0)
p_te_c_plot = ggplot(data=p_te_c_df,aes(x=csp_haps_shared,y=p_te_c,colour=factor(aim2_exposure))) +
  geom_point(alpha=0.7) + 
  scale_colour_manual(values=c("#ff7f00","#e31a1c")) + 
  labs(colour="Symptomatic status") +
  theme_bw() + 
  xlab("Number of pfcsp haplotypes shared") +
  ylab("P(TE,c)")
p_te_c_plot
ggsave(p_te_c_plot, filename="/Users/kelseysumner/Desktop/p_te_c_plot.png", device="png",
       height=4, width=7, units="in", dpi=500)






#### ----------- create a few final plots ------------ ####


# make a plot of p_te_a_c_combo
p_te_a_c_combo_df = merged_data %>%
  filter(rescaled_p_te_a_c_combo != 0)
p_te_a_c_combo_plot = ggplot(data=p_te_a_c_combo_df,aes(x=csp_haps_shared,y=p_te_a_c_combo,colour=factor(aim2_exposure))) +
  geom_point(alpha=0.7) + 
  scale_colour_manual(values=c("#ff7f00","#e31a1c")) + 
  labs(colour="Symptomatic status") +
  theme_bw() + 
  xlab("Number of pfcsp haplotypes shared") +
  ylab("P(TE,ac_combo)")
p_te_a_c_combo_plot


# make a plot of p_te_all 
p_te_all_df = merged_data %>%
  filter(p_te_all > 0)
p_te_all_plot = ggplot(data=p_te_all_df,aes(x=p_te_all,fill=factor(aim2_exposure))) +
  geom_density(alpha=0.6) + 
  scale_fill_manual(values=c("#ff7f00","#e31a1c")) + 
  labs(fill="Symptomatic status") +
  theme_bw() + 
  xlab("P(TE,all)")
p_te_all_plot
ggsave(p_te_all_plot, filename="/Users/kelseysumner/Desktop/p_te_all_plot.png", device="png",
       height=4, width=7, units="in", dpi=500)


# make a plot of p_te_all that's not stratified
p_te_all_df = merged_data %>%
  filter(p_te_all > 0)
p_te_all_plot = ggplot(data=p_te_all_df,aes(x=p_te_all)) +
  geom_density(alpha=0.6,fill="#e31a1c") +
  theme_bw() + 
  xlab("P(TE,all)")
p_te_all_plot
ggsave(p_te_all_plot, filename="/Users/kelseysumner/Desktop/p_te_all_plot_unstratified.png", device="png",
       height=4, width=7, units="in", dpi=500)


# make a plot of p_te_t
# probability plot
p_te_t_density_plot = ggplot(data=p_te_t_df,aes(x=rescaled_p_te_t)) +
  geom_density(alpha=0.6,fill=c("#ff7f00")) + 
  theme_bw() + 
  xlab("P(TE,t)") +
  ggtitle("Density of P(TE,t)")
p_te_t_density_plot
ggsave(p_te_t_density_plot, filename="/Users/kelseysumner/Desktop/p_te_t_density_plot.png", device="png",
       height=4, width=7, units="in", dpi=500)
# time plot
p_te_t_density_plot_x = ggplot(data=p_te_t_df,aes(x=date_difference_flipped)) +
  geom_density(alpha=0.6,fill=c("#ff7f00")) + 
  theme_bw() + 
  xlab("Days between human infection and mosquito collection") +
  ggtitle("Density of observations over time")
p_te_t_density_plot_x
ggsave(p_te_t_density_plot_x, filename="/Users/kelseysumner/Desktop/p_te_t_density_plot_x.png", device="png",
       height=4, width=7, units="in", dpi=500)



# make a plot of p_te_d
# probability plot
p_te_d_density_plot = ggplot(data=p_te_d_df,aes(x=rescaled_p_te_d)) +
  geom_density(alpha=0.6,fill=c("#ff7f00")) + 
  theme_bw() + 
  xlab("P(TE,d)") +
  ggtitle("Density of P(TE,d)")
p_te_d_density_plot
ggsave(p_te_d_density_plot, filename="/Users/kelseysumner/Desktop/p_te_d_density_plot.png", device="png",
       height=4, width=7, units="in", dpi=500)
# distance plot
p_te_d_density_plot_x = ggplot(data=p_te_d_df,aes(x=distance_km)) +
  geom_density(alpha=0.6,fill=c("#ff7f00")) + 
  theme_bw() + 
  xlab("Distance (Km) between human infection and mosquito collection") +
  ggtitle("Density of observations across distance")
p_te_d_density_plot_x
ggsave(p_te_d_density_plot_x, filename="/Users/kelseysumner/Desktop/p_te_d_density_plot_x.png", device="png",
       height=4, width=7, units="in", dpi=500)



# make a plot of p_te_a
p_te_a_df = merged_data %>%
  filter(p_te_a > 0)
# probability plot
p_te_a_density_plot = ggplot(data=p_te_a_df,aes(x=p_te_a)) +
  geom_density(alpha=0.6,fill=c("#ff7f00")) + 
  theme_bw() + 
  xlab("P(TE,a)") +
  ggtitle("Density of P(TE,a)")
p_te_a_density_plot
ggsave(p_te_a_density_plot, filename="/Users/kelseysumner/Desktop/p_te_a_density_plot.png", device="png",
       height=4, width=7, units="in", dpi=500)
# moi plot
p_te_a_density_plot_x = ggplot(data=p_te_a_df,aes(x=ama_haps_shared)) +
  geom_density(alpha=0.6,fill=c("#ff7f00")) + 
  theme_bw() + 
  xlab("Number of pfama1 haplotypes shared") +
  ggtitle("Density of observations across number of pfama1 haplotypes shared")
p_te_a_density_plot_x
ggsave(p_te_a_density_plot_x, filename="/Users/kelseysumner/Desktop/p_te_a_density_plot_x.png", device="png",
       height=4, width=7, units="in", dpi=500)
# make prevalence plot


# make a plot of p_te_c
p_te_c_df = merged_data %>%
  filter(p_te_c > 0)
# probability plot
p_te_c_density_plot = ggplot(data=p_te_c_df,aes(x=p_te_c)) +
  geom_density(alpha=0.6,fill=c("#ff7f00")) + 
  theme_bw() + 
  xlab("P(TE,c)") +
  ggtitle("Density of P(TE,c)")
p_te_c_density_plot
ggsave(p_te_c_density_plot, filename="/Users/kelseysumner/Desktop/p_te_c_density_plot.png", device="png",
       height=4, width=7, units="in", dpi=500)
# moi plot
p_te_c_density_plot_x = ggplot(data=p_te_c_df,aes(x=csp_haps_shared)) +
  geom_density(alpha=0.6,fill=c("#ff7f00")) + 
  theme_bw() + 
  xlab("Number of pfcsp haplotypes shared") +
  ggtitle("Density of observations across number of pfcsp haplotypes shared")
p_te_c_density_plot_x
ggsave(p_te_c_density_plot_x, filename="/Users/kelseysumner/Desktop/p_te_c_density_plot_x.png", device="png",
       height=4, width=7, units="in", dpi=500)
# make prevalence plot





#### -------- make density plot of haplotype prevalence --------- ####

# make plots of the haplotype prevalence distributions
# for ama
p_te_a_density_plot_prev = ggplot(data=ama_hap_summary,aes(x=ama_hap_prevalence)) +
  geom_density(alpha=0.6,fill=c("#ff7f00")) + 
  theme_bw() + 
  xlab("Pfama1 haplotype prevalence") +
  ggtitle("Density of pfama1 haplotype prevalence")
p_te_a_density_plot_prev
ggsave(p_te_a_density_plot_prev, filename="/Users/kelseysumner/Desktop/p_te_a_density_plot_prev.png", device="png",
       height=4, width=7, units="in", dpi=500)
# for csp
p_te_c_density_plot_prev = ggplot(data=csp_hap_summary,aes(x=csp_hap_prevalence)) +
  geom_density(alpha=0.6,fill=c("#ff7f00")) + 
  theme_bw() + 
  xlab("Pfcsp haplotype prevalence") +
  ggtitle("Density of pfcsp haplotype prevalence")
p_te_c_density_plot_prev
ggsave(p_te_c_density_plot_prev, filename="/Users/kelseysumner/Desktop/p_te_c_density_plot_prev.png", device="png",
       height=4, width=7, units="in", dpi=500)






