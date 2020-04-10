# -------------------------------- #
#       Preliminary results        #
#         Mozzie phase 1           #
#             Aim 1B               #
#        March 20, 2020            #
#           K. Sumner              #
# -------------------------------- #

# will call repeated infections: "reinfection"

#### ------- load libraries -------- ####
library(tidyverse)
library(schoolmath)


#### ----- read in the data sets ------- ####

# read in the ama data set
ama_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/data_with_first_infection/ama_data_aim1b_20MAR2020.rds")

# read in the csp data set
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/data_with_first_infection/csp_data_aim1b_20MAR2020.rds")


####
# LOOK AT CSP DATA 
####


#### ------ for each particpant, remove the first infection ------- ####

# first order the data set by date
csp_data = dplyr::arrange(csp_data,unq_memID,sample_id_date)

# look at how many infections each participant had
num_infections_before = csp_data %>%
  group_by(unq_memID) %>%
  summarize (n= n())

# remove the first instance of infection for each person
test = slice(group_by(csp_data, unq_memID), -1)

# now look at the new amount of infections every had (for those that had 2+ infections)
num_infections_after = test %>%
  group_by(unq_memID) %>%
  summarize (n= n())

# looks like this worked correctly so apply to everything
csp_data = slice(group_by(csp_data, unq_memID), -1)

# create new variables that are "any new" vs. "none new"
csp_data$any_new_categories = ifelse(csp_data$haplotype_category == "all new","any new",
                                     ifelse(csp_data$haplotype_category == "old and new","any new","none new"))
table(csp_data$haplotype_category,csp_data$any_new_categories,useNA = "always")


# export this data set
write_csv(csp_data,"Desktop/without_first_infection_csp_data_spat21_aim1b_10APR2020.csv")
write_rds(csp_data,"Desktop/without_first_infection_csp_data_spat21_aim1b_10APR2020.rds")


#### ------- now look at summaries of the data set -------- ####

# look at a table of the exposure and outcome
table(csp_data$haplotype_category,csp_data$symptomatic_status, useNA="always")

# summary of number of infections per participant
num_infections_after = csp_data %>%
  group_by(unq_memID) %>%
  summarize (n= n())
summary(num_infections_after$n)

# make a density plot of the number of infections within individuals stratified by symptomatic status
# first set up the data
num_infections_stratified = csp_data %>%
  group_by(unq_memID,symptomatic_status) %>%
  summarize (n=n())
# then make the plot
plot1 = ggplot(data=num_infections_stratified,aes(x=n,fill=symptomatic_status)) + 
  geom_density() + facet_wrap(~symptomatic_status) + theme_bw() + theme(legend.position = "none") +
  xlab("Number of infections within each individual") + scale_x_continuous(limits=c(0,15))
# export the plot
# ggsave(plot1, filename="/Users/kelseysumner/Desktop/plot1.png", device="png",
       # height=3, width=5, units="in", dpi=500)

# make a density plot of the different haplotype categories within each participant
# first set up the data
num_hap_categories_stratified = csp_data %>%
  group_by(unq_memID,haplotype_category) %>%
  summarize (n=n())
# then make the plot
plot1 = ggplot(data=num_hap_categories_stratified,aes(x=n,fill=haplotype_category)) + 
  geom_density() + facet_wrap(~haplotype_category) + theme_bw() + theme(legend.position = "none") +
  xlab("Number of haplotype categories within each individual") + scale_x_continuous(limits=c(0,15))
# export the plot
# ggsave(plot1, filename="/Users/kelseysumner/Desktop/plot1.png", device="png",
       # height=3, width=5, units="in", dpi=500)


#### ------- now create counts of infections within 30 days of each other within participants ------- ####

# now see how many symptomatic infections were within 30 days after the asymptomatic infections for each participant
# csp data is still sorted by participant and date
date_diff = rep(NA,nrow(csp_data))
symp_30_after_asymp = rep(NA,nrow(csp_data))
for (i in 1:nrow(csp_data)){
  if (csp_data$unq_memID[i] == csp_data$unq_memID[i+1] & i != nrow(csp_data) &
      csp_data$symptomatic_status[i] == "asymptomatic infection" &
      !(is.na(csp_data$symptomatic_status[i])) &
      csp_data$symptomatic_status[i+1] == "symptomatic infection" & 
      !(is.na(csp_data$symptomatic_status[i+1]))){
    date_diff[i] = csp_data$sample_id_date[i+1] - csp_data$sample_id_date[i]
    if (date_diff[i] <= 30){
      symp_30_after_asymp[i] = "yes"
      symp_30_after_asymp[i+1] = "yes"
    }
  } 
}
csp_data$date_diff = date_diff
csp_data$symp_30_after_asymp = symp_30_after_asymp
# check the output
csp_data %>%
  select(unq_memID, symptomatic_status, sample_id_date,date_diff,symp_30_after_asymp) %>%
  View()
# now take out the date diff columns
csp_data = csp_data %>%
  select(-c(date_diff))

# now see how many infections (asymptomatic or symptomatic) were within 30 days after the symptomatic infections for each participant
# csp data is still sorted by participant and date
# also check to make sure person received study-prescribed antimalarials
date_diff = rep(NA,nrow(csp_data))
infxn_30_after_symp = rep(NA,nrow(csp_data))
for (i in 1:nrow(csp_data)){
  if (csp_data$unq_memID[i] == csp_data$unq_memID[i+1] & i != nrow(csp_data) &
      csp_data$symptomatic_status[i] == "symptomatic infection" & 
      csp_data$prescription[i] == "prescribed" & !(is.na(csp_data$unq_memID[i])) & 
      !(is.na(csp_data$unq_memID[i+1])) & !(is.na(csp_data$prescription[i])) &
      !is.na(csp_data$symptomatic_status[i])){
    date_diff[i] = csp_data$sample_id_date[i+1] - csp_data$sample_id_date[i]
    if (date_diff[i] <= 30){
      infxn_30_after_symp[i] = "yes"
      infxn_30_after_symp[i+1] = "yes"
    }
  } 
}
csp_data$date_diff = date_diff
csp_data$infxn_30_after_symp = infxn_30_after_symp
# check the output
csp_data %>%
  select(unq_memID, symptomatic_status, sample_id_date,date_diff,prescription,infxn_30_after_symp) %>%
  View()
# now take out the date diff columns
csp_data = csp_data %>%
  select(-c(date_diff))


#### -------- look at summaries of infections within individuals over time ------- ####

## first look at those people who had asymptomatic infections that became symptomatic within 30 days

# how many people had asymptomatic infections that became symptomatic <= 30 days after
pre_symptomatic_data = csp_data %>%
  filter(symp_30_after_asymp == "yes")

# export this data set
# write_csv(pre_symptomatic_data,"Desktop/spat_1b_pre_symptomatic_data_csp_24MAR2020.csv")

# now just look at the specific haplotypes over time

# cut down the dataset to the variable of interest
pre_symptomatic_data_small = pre_symptomatic_data %>%
  select(unq_memID,sample_id_date,haplotype_number,haplotype_list)

# now separate out the specific haplotypes within individuals
individual_hap_list = rep(NA,nrow(pre_symptomatic_data_small))
total_haps = c()
for (i in 1:nrow(pre_symptomatic_data_small)){
  individual_hap_list[i] = str_split(pre_symptomatic_data_small$haplotype_list[i],",")
  total_haps = c(total_haps,str_split(pre_symptomatic_data_small$haplotype_list[i],",")[[1]])
}
unique_haps = unique(total_haps)

# make new columns of the specific haplotypes
pre_symptomatic_data_small = pre_symptomatic_data_small %>% 
  `is.na<-`(unique_haps)
for (j in 1:nrow(pre_symptomatic_data_small)){
  for (k in 1:length(individual_hap_list[[j]])){
    for (l in 1:ncol(pre_symptomatic_data_small)){
      if (individual_hap_list[[j]][k] == colnames(pre_symptomatic_data_small)[l]){
        pre_symptomatic_data_small[j,l] = individual_hap_list[[j]][k]
      }
    }
  }
}


# figure out how many haplotypes were shared between pairs
pre_symp_for_loop_data = pre_symptomatic_data_small[,5:49]
haps_shared_btwn_pairs = rep(NA,nrow(pre_symp_for_loop_data))
for (i in 1:nrow(pre_symp_for_loop_data)) {
  count = 0
  for (j in 1:ncol(pre_symp_for_loop_data)) {
    if (is.odd(i) & as.character(pre_symp_for_loop_data[i,j]) == as.character(pre_symp_for_loop_data[i+1,j]) &
        !(is.na(pre_symp_for_loop_data[i,j])) & !(is.na(pre_symp_for_loop_data[i+1,j]))) {
      count = count+1
    }
  }
  haps_shared_btwn_pairs[i] = count
}
# add the column to the data set
pre_symptomatic_data_small$haps_shared_btwn_pairs = haps_shared_btwn_pairs
# now make sure the row below had the same information
for (i in 1:nrow(pre_symptomatic_data_small)) {
  if (is.even(i)){
    pre_symptomatic_data_small$haps_shared_btwn_pairs[i] = pre_symptomatic_data_small$haps_shared_btwn_pairs[i-1]
  }
}

# now calculate how many were shared out of each participant's MOI
pre_symptomatic_data_small$proportion_haps_shared = pre_symptomatic_data_small$haps_shared_btwn_pairs/pre_symptomatic_data_small$haplotype_number

# export the data set
# write_csv(pre_symptomatic_data_small,"Desktop/spat_1b_pre_symptomatic_data_csp_26MAR2020.csv")



## then look at those people who had symptomatic infections, were prescribed antimalarials, and then had an infection within 30 days

# how many people had asymptomatic infections that became symptomatic <= 30 days after
recrudescence_data = csp_data %>%
  filter(infxn_30_after_symp == "yes")

# export this data set
# write_csv(recrudescence_data,"Desktop/spat_1b_recrudescence_data_csp_24MAR2020.csv")

# now just look at the specific haplotypes over time

# cut down the dataset to the variable of interest
recrudescence_data_small = recrudescence_data %>%
  select(unq_memID,sample_id_date,haplotype_number,haplotype_list)

# now separate out the specific haplotypes within individuals
individual_hap_list = rep(NA,nrow(recrudescence_data_small))
total_haps = c()
for (i in 1:nrow(recrudescence_data_small)){
  individual_hap_list[i] = str_split(recrudescence_data_small$haplotype_list[i],",")
  total_haps = c(total_haps,str_split(recrudescence_data_small$haplotype_list[i],",")[[1]])
}
unique_haps = unique(total_haps)

# make new columns of the specific haplotypes
recrudescence_data_small = recrudescence_data_small %>% 
  `is.na<-`(unique_haps)
for (j in 1:nrow(recrudescence_data_small)){
  for (k in 1:length(individual_hap_list[[j]])){
    for (l in 1:ncol(recrudescence_data_small)){
      if (individual_hap_list[[j]][k] == colnames(recrudescence_data_small)[l]){
        recrudescence_data_small[j,l] = individual_hap_list[[j]][k]
      }
    }
  }
}

# export the data set
# write_csv(recrudescence_data_small,"Desktop/spat_1b_recrudescence_data_with_haplotypes_csp_26MAR2020.csv")

# figure out how many haplotypes were shared between pairs
pre_symp_for_loop_data = recrudescence_data_small[,5:34]
haps_shared_btwn_pairs = rep(NA,nrow(pre_symp_for_loop_data))
for (i in 1:nrow(pre_symp_for_loop_data)) {
  count = 0
  for (j in 1:ncol(pre_symp_for_loop_data)) {
    if (is.odd(i) & as.character(pre_symp_for_loop_data[i,j]) == as.character(pre_symp_for_loop_data[i+1,j]) &
        !(is.na(pre_symp_for_loop_data[i,j])) & !(is.na(pre_symp_for_loop_data[i+1,j]))) {
      count = count+1
    }
  }
  haps_shared_btwn_pairs[i] = count
}
# add the column to the data set
recrudescence_data_small$haps_shared_btwn_pairs = haps_shared_btwn_pairs
# now make sure the row below had the same information
for (i in 1:nrow(recrudescence_data_small)) {
  if (is.even(i)){
    recrudescence_data_small$haps_shared_btwn_pairs[i] = recrudescence_data_small$haps_shared_btwn_pairs[i-1]
  }
}

# now calculate how many were shared out of each participant's MOI
recrudescence_data_small$proportion_haps_shared = recrudescence_data_small$haps_shared_btwn_pairs/recrudescence_data_small$haplotype_number

# export the data set
# write_csv(recrudescence_data_small,"Desktop/spat_1b_recrudescence_data_csp_26MAR2020.csv")










####
# LOOK AT AMA DATA 
####

#### ------ for each particpant, remove the first infection ------- ####

# first order the data set by date
ama_data = dplyr::arrange(ama_data,unq_memID,sample_id_date)

# look at how many infections each participant had
num_infections_before = ama_data %>%
  group_by(unq_memID) %>%
  summarize (n= n())

# remove the first instance of infection for each person
test = slice(group_by(ama_data, unq_memID), -1)

# now look at the new amount of infections every had (for those that had 2+ infections)
num_infections_after = test %>%
  group_by(unq_memID) %>%
  summarize (n= n())

# looks like this worked correctly so apply to everything
ama_data = slice(group_by(ama_data, unq_memID), -1)

# create new variables that are "any new" vs. "none new"
ama_data$any_new_categories = ifelse(ama_data$haplotype_category == "all new","any new",
                                     ifelse(ama_data$haplotype_category == "old and new","any new","none new"))
table(ama_data$haplotype_category,ama_data$any_new_categories,useNA = "always")


# export this data set
write_csv(ama_data,"Desktop/without_first_infection_ama_data_spat21_aim1b_10APR2020.csv")
write_rds(ama_data,"Desktop/without_first_infection_ama_data_spat21_aim1b_10APR2020.rds")


#### ------- now look at summaries of the data set -------- ####

# look at a table of the exposure and outcome
table(ama_data$haplotype_category,ama_data$symptomatic_status, useNA="always")

# summary of number of infections per participant
num_infections_after = ama_data %>%
  group_by(unq_memID) %>%
  summarize (n= n())
summary(num_infections_after$n)

# make a density plot of the number of infections within individuals stratified by symptomatic status
# first set up the data
num_infections_stratified = ama_data %>%
  group_by(unq_memID,symptomatic_status) %>%
  summarize (n=n())
# then make the plot
plot1 = ggplot(data=num_infections_stratified,aes(x=n,fill=symptomatic_status)) + 
  geom_density() + facet_wrap(~symptomatic_status) + theme_bw() + theme(legend.position = "none") +
  xlab("Number of infections within each individual") + scale_x_continuous(limits=c(0,15))
# export the plot
 ggsave(plot1, filename="/Users/kelseysumner/Desktop/plot1.png", device="png",
  height=3, width=5, units="in", dpi=500)

# make a density plot of the different haplotype categories within each participant
# first set up the data
num_hap_categories_stratified = ama_data %>%
  group_by(unq_memID,haplotype_category) %>%
  summarize (n=n())
# then make the plot
plot2 = ggplot(data=num_hap_categories_stratified,aes(x=n,fill=haplotype_category)) + 
  geom_density() + facet_wrap(~haplotype_category) + theme_bw() + theme(legend.position = "none") +
  xlab("Number of haplotype categories within each individual") + scale_x_continuous(limits=c(0,15))
# export the plot
ggsave(plot2, filename="/Users/kelseysumner/Desktop/plot2.png", device="png",
  height=3, width=5, units="in", dpi=500)


#### ------- now create counts of infections within 30 days of each other within participants ------- ####

# now see how many symptomatic infections were within 30 days after the asymptomatic infections for each participant
# ama data is still sorted by participant and date
date_diff = rep(NA,nrow(ama_data))
symp_30_after_asymp = rep(NA,nrow(ama_data))
for (i in 1:nrow(ama_data)){
  if (ama_data$unq_memID[i] == ama_data$unq_memID[i+1] & i != nrow(ama_data) &
      ama_data$symptomatic_status[i] == "asymptomatic infection" &
      !(is.na(ama_data$symptomatic_status[i])) &
      ama_data$symptomatic_status[i+1] == "symptomatic infection" & 
      !(is.na(ama_data$symptomatic_status[i+1]))){
    date_diff[i] = ama_data$sample_id_date[i+1] - ama_data$sample_id_date[i]
    if (date_diff[i] <= 30){
      symp_30_after_asymp[i] = "yes"
      symp_30_after_asymp[i+1] = "yes"
    }
  } 
}
ama_data$date_diff = date_diff
ama_data$symp_30_after_asymp = symp_30_after_asymp
# check the output
ama_data %>%
  select(unq_memID, symptomatic_status, sample_id_date,date_diff,symp_30_after_asymp) %>%
  View()
# now take out the date diff columns
ama_data = ama_data %>%
  select(-c(date_diff))

# now see how many infections (asymptomatic or symptomatic) were within 30 days after the symptomatic infections for each participant
# ama data is still sorted by participant and date
# also check to make sure person received study-prescribed antimalarials
date_diff = rep(NA,nrow(ama_data))
infxn_30_after_symp = rep(NA,nrow(ama_data))
for (i in 1:nrow(ama_data)){
  if (ama_data$unq_memID[i] == ama_data$unq_memID[i+1] & i != nrow(ama_data) &
      ama_data$symptomatic_status[i] == "symptomatic infection" & 
      ama_data$prescription[i] == "prescribed" & !(is.na(ama_data$unq_memID[i])) & 
      !(is.na(ama_data$unq_memID[i+1])) & !(is.na(ama_data$prescription[i])) &
      !is.na(ama_data$symptomatic_status[i])){
    date_diff[i] = ama_data$sample_id_date[i+1] - ama_data$sample_id_date[i]
    if (date_diff[i] <= 30){
      infxn_30_after_symp[i] = "yes"
      infxn_30_after_symp[i+1] = "yes"
    }
  } 
}
ama_data$date_diff = date_diff
ama_data$infxn_30_after_symp = infxn_30_after_symp
# check the output
ama_data %>%
  select(unq_memID, symptomatic_status, sample_id_date,date_diff,prescription,infxn_30_after_symp) %>%
  View()
# now take out the date diff columns
ama_data = ama_data %>%
  select(-c(date_diff))


#### -------- look at summaries of infections within individuals over time ------- ####

## first look at those people who had asymptomatic infections that became symptomatic within 30 days

# how many people had asymptomatic infections that became symptomatic <= 30 days after
pre_symptomatic_data = ama_data %>%
  filter(symp_30_after_asymp == "yes")

# export this data set
# write_csv(pre_symptomatic_data,"Desktop/spat_1b_pre_symptomatic_data_ama_24MAR2020.csv")

# cut down the dataset to the variable of interest
pre_symptomatic_data_small = pre_symptomatic_data %>%
  select(unq_memID,sample_id_date,haplotype_number,haplotype_list)

# now separate out the specific haplotypes within individuals
individual_hap_list = rep(NA,nrow(pre_symptomatic_data_small))
total_haps = c()
for (i in 1:nrow(pre_symptomatic_data_small)){
  individual_hap_list[i] = str_split(pre_symptomatic_data_small$haplotype_list[i],",")
  total_haps = c(total_haps,str_split(pre_symptomatic_data_small$haplotype_list[i],",")[[1]])
}
unique_haps = unique(total_haps)

# make new columns of the specific haplotypes
pre_symptomatic_data_small = pre_symptomatic_data_small %>% 
  `is.na<-`(unique_haps)
for (j in 1:nrow(pre_symptomatic_data_small)){
  for (k in 1:length(individual_hap_list[[j]])){
    for (l in 1:ncol(pre_symptomatic_data_small)){
      if (individual_hap_list[[j]][k] == colnames(pre_symptomatic_data_small)[l]){
        pre_symptomatic_data_small[j,l] = individual_hap_list[[j]][k]
      }
    }
  }
}


# figure out how many haplotypes were shared between pairs
pre_symp_for_loop_data = pre_symptomatic_data_small[,5:41]
haps_shared_btwn_pairs = rep(NA,nrow(pre_symp_for_loop_data))
for (i in 1:nrow(pre_symp_for_loop_data)) {
  count = 0
  for (j in 1:ncol(pre_symp_for_loop_data)) {
    if (is.odd(i) & as.character(pre_symp_for_loop_data[i,j]) == as.character(pre_symp_for_loop_data[i+1,j]) &
        !(is.na(pre_symp_for_loop_data[i,j])) & !(is.na(pre_symp_for_loop_data[i+1,j]))) {
      count = count+1
    }
  }
  haps_shared_btwn_pairs[i] = count
}
# add the column to the data set
pre_symptomatic_data_small$haps_shared_btwn_pairs = haps_shared_btwn_pairs
# now make sure the row below had the same information
for (i in 1:nrow(pre_symptomatic_data_small)) {
  if (is.even(i)){
    pre_symptomatic_data_small$haps_shared_btwn_pairs[i] = pre_symptomatic_data_small$haps_shared_btwn_pairs[i-1]
  }
}

# now calculate how many were shared out of each participant's MOI
pre_symptomatic_data_small$proportion_haps_shared = pre_symptomatic_data_small$haps_shared_btwn_pairs/pre_symptomatic_data_small$haplotype_number

# export the data set
write_csv(pre_symptomatic_data_small,"Desktop/spat_1b_pre_symptomatic_data_ama_26MAR2020.csv")



## then look at those people who had symptomatic infections, were prescribed antimalarials, and then had an infection within 30 days

# how many people had asymptomatic infections that became symptomatic <= 30 days after
recrudescence_data = ama_data %>%
  filter(infxn_30_after_symp == "yes")

# export this data set
# write_csv(recrudescence_data,"Desktop/spat_1b_recrudescence_data_ama_24MAR2020.csv")

# now just look at the specific haplotypes over time

# cut down the dataset to the variable of interest
recrudescence_data_small = recrudescence_data %>%
  select(unq_memID,sample_id_date,haplotype_number,haplotype_list)

# now separate out the specific haplotypes within individuals
individual_hap_list = rep(NA,nrow(recrudescence_data_small))
total_haps = c()
for (i in 1:nrow(recrudescence_data_small)){
  individual_hap_list[i] = str_split(recrudescence_data_small$haplotype_list[i],",")
  total_haps = c(total_haps,str_split(recrudescence_data_small$haplotype_list[i],",")[[1]])
}
unique_haps = unique(total_haps)

# make new columns of the specific haplotypes
recrudescence_data_small = recrudescence_data_small %>% 
  `is.na<-`(unique_haps)
for (j in 1:nrow(recrudescence_data_small)){
  for (k in 1:length(individual_hap_list[[j]])){
    for (l in 1:ncol(recrudescence_data_small)){
      if (individual_hap_list[[j]][k] == colnames(recrudescence_data_small)[l]){
        recrudescence_data_small[j,l] = individual_hap_list[[j]][k]
      }
    }
  }
}

# export the data set
# write_csv(recrudescence_data_small,"Desktop/spat_1b_recrudescence_data_with_haplotypes_csp_26MAR2020.csv")

# figure out how many haplotypes were shared between pairs
pre_symp_for_loop_data = recrudescence_data_small[,5:34]
haps_shared_btwn_pairs = rep(NA,nrow(pre_symp_for_loop_data))
for (i in 1:nrow(pre_symp_for_loop_data)) {
  count = 0
  for (j in 1:ncol(pre_symp_for_loop_data)) {
    if (is.odd(i) & as.character(pre_symp_for_loop_data[i,j]) == as.character(pre_symp_for_loop_data[i+1,j]) &
        !(is.na(pre_symp_for_loop_data[i,j])) & !(is.na(pre_symp_for_loop_data[i+1,j]))) {
      count = count+1
    }
  }
  haps_shared_btwn_pairs[i] = count
}
# add the column to the data set
recrudescence_data_small$haps_shared_btwn_pairs = haps_shared_btwn_pairs
# now make sure the row below had the same information
for (i in 1:nrow(recrudescence_data_small)) {
  if (is.even(i)){
    recrudescence_data_small$haps_shared_btwn_pairs[i] = recrudescence_data_small$haps_shared_btwn_pairs[i-1]
  }
}

# now calculate how many were shared out of each participant's MOI
recrudescence_data_small$proportion_haps_shared = recrudescence_data_small$haps_shared_btwn_pairs/recrudescence_data_small$haplotype_number

# export the data set
write_csv(recrudescence_data_small,"Desktop/spat_1b_recrudescence_data_ama_26MAR2020.csv")






