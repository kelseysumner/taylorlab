# -------------------------------------- #
#           Spat21/Mozzie Study          #
#       Impute missed follow-up visits   #
#                 Aim 1A                 #
#               Human Data               #
#            Mozzie Phase 3              #
#                K. Sumner               #
#            August 18, 2020             #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)


#### ------- read in the data sets -------- ####

# read in the full data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/De-identified Phase II_v13/final_merged_data/final_data_set/phase3_spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_18AUG2020.rds")

# read in the consecutive monthly follow-up data set
followup_data = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1A/Consecutive Follow-up Tables/phase3_aim1a_consecutive_follow_up_order_df_after_censoring_18AUG2020.csv")



#### ------- set up the data sets to be in survival format -------- ####

# sleeping in a space with a net regularly
# calculate how many people sleep under net regularly
participant_data = final_data %>%
  group_by(village_name,unq_memID) %>%
  summarize(slept_avg=mean(slept_times, na.rm =T))
# make a variable that indicates some slept under a net more than usual
slept_under_net_regularly = ifelse(is.na(participant_data$slept_avg),NA,ifelse(participant_data$slept_avg>5,"yes","no"))
table(slept_under_net_regularly,participant_data$slept_avg, useNA = "always")
participant_data$slept_under_net_regularly = as.factor(slept_under_net_regularly)
participant_data_v2 = participant_data %>%
  group_by(village_name, slept_under_net_regularly) %>%
  summarize(totaln = n())
participant_data_v2$slept_under_net_regularly=as.factor(participant_data_v2$slept_under_net_regularly)
# add the variables to the final data set with all observations
participant_data$village_name <- NULL
new_data = left_join(final_data,participant_data,by="unq_memID")
length(intersect(final_data$unq_memID,participant_data$unq_memID))
setdiff(participant_data$unq_memID,final_data$unq_memID)
final_data = new_data

# make a month-year combo variable for all data entries
final_data = final_data %>% mutate(month_year = lubridate::floor_date(sample_id_date, "month"))
summary(final_data$month_year)
table(final_data$month_year, useNA = "always")

# subset the data set to just the variables of interest for aims 1A
# when you set up the survival analysis data sets, will have to have separate data sets for different follow-up time 
# for the three outcome definitions
colnames(final_data)
# for the primary case definition
survival_data_primary = final_data %>%
  select(sample_name_final,age_all_baseline,gender,village_name,unq_memID,HH_ID,visit_type,slept_under_net_regularly,sample_id_date,month_year,main_exposure_primary_case_def,main_outcome_primary_case_def)
# for the secondary stringent case definition
survival_data_secondary_stringent = final_data %>%
  select(sample_name_final,age_all_baseline,gender,village_name,unq_memID,HH_ID,visit_type,slept_under_net_regularly,sample_id_date,month_year,main_exposure_secondary_stringent_case_def,main_outcome_secondary_stringent_case_def)
# for the secondary permissive case definition
survival_data_secondary_permissive = final_data %>%
  select(sample_name_final,age_all_baseline,gender,village_name,unq_memID,HH_ID,visit_type,slept_under_net_regularly,sample_id_date,month_year,main_exposure_secondary_permissive_case_def,main_outcome_secondary_permissive_case_def)

# look at the number of participants with more than one symptomatic infection
participant_data = final_data %>%
  filter(main_outcome_primary_case_def == "symptomatic infection") %>%
  group_by(unq_memID) %>%
  summarize(n=n()) %>%
  filter(n>1)

# first order the data set by date
survival_data_primary = dplyr::arrange(survival_data_primary,unq_memID,sample_id_date)
survival_data_secondary_stringent = dplyr::arrange(survival_data_secondary_stringent,unq_memID,sample_id_date)
survival_data_secondary_permissive = dplyr::arrange(survival_data_secondary_permissive,unq_memID,sample_id_date)


## ---- impute missed follow-up visits for survival data primary

# set up a for loop of follow up dates to identify which you need to impute
old_date_to_add = c()
new_date_to_add = c()
id_to_add = c()
for (i in 1:nrow(followup_data)){
  for (j in 1:(ncol(followup_data))-1){
    if (j>2){
      if (is.na(followup_data[i,j]) & !(is.na(followup_data[i,j-1]))){
        
        # pull out the id
        id_to_add = c(id_to_add,followup_data$unq_memID[i])
        
        # pull out old date
        split_up = str_split(colnames(followup_data)[j-1],"-")[[1]]
        if (nchar(split_up[1]) == 1){
          old_date = paste0("0",split_up[1],"01",split_up[2])
        } else {
          old_date = paste0(split_up[1],"01",split_up[2])
        }
        old_date_to_add = c(old_date_to_add,old_date)
        
        # add in the new month-year date for having an imputed monthly follow-up visit
        split_up = str_split(colnames(followup_data)[j],"-")[[1]]
        if (nchar(split_up[1]) == 1){
          new_date = paste0("0",split_up[1],"01",split_up[2])
        } else {
          new_date = paste0(split_up[1],"01",split_up[2])
        }
        new_date_to_add = c(new_date_to_add,new_date)
        
      }
      
    }
    
  }
}
# make a data frame of the ids and date to impute
imputation_df = data.frame(id_to_add,old_date_to_add,new_date_to_add)
imputation_df$old_date_to_add=lubridate::mdy(imputation_df$old_date_to_add)
imputation_df$new_date_to_add=lubridate::mdy(imputation_df$new_date_to_add)

# set up some variables for the for loop
unique_participants = unique(survival_data_primary$unq_memID)

# run the insert row function
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

# for each participant, look to see where missing one month of follow-up and carry forward the previous month's exposure case definition
# now start the for loop
for (k in 1:nrow(imputation_df)){
  for (j in 1:nrow(survival_data_primary)){
    if (j>1){
      if(survival_data_primary$unq_memID[j] == imputation_df$id_to_add[k] & survival_data_primary$month_year[j] == imputation_df$old_date_to_add[k] & survival_data_primary$visit_type[j]=="monthly visit"){
        # insert a new row
        survival_data_primary = insertRow(survival_data_primary,survival_data_primary[j,],j)
        # add the imputed exposure and associated date
        survival_data_primary$month_year[j] = imputation_df$new_date_to_add[k]
        survival_data_primary$sample_id_date[j] = imputation_df$new_date_to_add[k]
        # add the new sample id 
        part = strsplit(as.character(imputation_df$new_date_to_add[k]),"-")[[1]]
        part1 = strsplit(part[1],"")[[1]]
        part1_pasted = paste0(part1[3],part1[4])
        monthly_date = paste0(part[3],part[2],part1_pasted)
        id_split = strsplit(imputation_df$id_to_add[k],"_")[[1]]
        survival_data_primary$sample_name_final[j] = paste0(id_split[1],"-",monthly_date,"-",id_split[2])
        break
      } 
    }
  }
}
# check imputation
survival_data_primary = dplyr::arrange(survival_data_primary,unq_memID,sample_id_date)
table(survival_data_primary$visit_type,survival_data_primary$main_exposure_primary_case_def,useNA = "always")
# 5998+826=6824 (new total)



## ---- impute missed follow-up visits for survival data secondary stringent

# set up a for loop of follow up dates to identify which you need to impute
old_date_to_add = c()
new_date_to_add = c()
id_to_add = c()
for (i in 1:nrow(followup_data)){
  for (j in 1:(ncol(followup_data))-1){
    if (j>2){
      if (is.na(followup_data[i,j]) & !(is.na(followup_data[i,j-1]))){
        
        # pull out the id
        id_to_add = c(id_to_add,followup_data$unq_memID[i])
        
        # pull out old date
        split_up = str_split(colnames(followup_data)[j-1],"-")[[1]]
        if (nchar(split_up[1]) == 1){
          old_date = paste0("0",split_up[1],"01",split_up[2])
        } else {
          old_date = paste0(split_up[1],"01",split_up[2])
        }
        old_date_to_add = c(old_date_to_add,old_date)
        
        # add in the new month-year date for having an imputed monthly follow-up visit
        split_up = str_split(colnames(followup_data)[j],"-")[[1]]
        if (nchar(split_up[1]) == 1){
          new_date = paste0("0",split_up[1],"01",split_up[2])
        } else {
          new_date = paste0(split_up[1],"01",split_up[2])
        }
        new_date_to_add = c(new_date_to_add,new_date)
        
      }
      
    }
    
  }
}
# make a data frame of the ids and date to impute
imputation_df = data.frame(id_to_add,old_date_to_add,new_date_to_add)
imputation_df$old_date_to_add=lubridate::mdy(imputation_df$old_date_to_add)
imputation_df$new_date_to_add=lubridate::mdy(imputation_df$new_date_to_add)

# set up some variables for the for loop
unique_participants = unique(survival_data_secondary_stringent$unq_memID)

# run the insert row function
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

# for each participant, look to see where missing one month of follow-up and carry forward the previous month's exposure case definition
# now start the for loop
for (k in 1:nrow(imputation_df)){
  for (j in 1:nrow(survival_data_secondary_stringent)){
    if (j>1){
      if(survival_data_secondary_stringent$unq_memID[j] == imputation_df$id_to_add[k] & survival_data_secondary_stringent$month_year[j] == imputation_df$old_date_to_add[k] & survival_data_secondary_stringent$visit_type[j]=="monthly visit"){
        # insert a new row
        survival_data_secondary_stringent = insertRow(survival_data_secondary_stringent,survival_data_secondary_stringent[j,],j)
        # add the imputed exposure and associated date
        survival_data_secondary_stringent$month_year[j] = imputation_df$new_date_to_add[k]
        survival_data_secondary_stringent$sample_id_date[j] = imputation_df$new_date_to_add[k]
        # add the new sample id 
        part = strsplit(as.character(imputation_df$new_date_to_add[k]),"-")[[1]]
        part1 = strsplit(part[1],"")[[1]]
        part1_pasted = paste0(part1[3],part1[4])
        monthly_date = paste0(part[3],part[2],part1_pasted)
        id_split = strsplit(imputation_df$id_to_add[k],"_")[[1]]
        survival_data_secondary_stringent$sample_name_final[j] = paste0(id_split[1],"-",monthly_date,"-",id_split[2])
        break
      } 
    }
  }
}
# check imputation
survival_data_secondary_stringent = dplyr::arrange(survival_data_secondary_stringent,unq_memID,sample_id_date)
table(survival_data_secondary_stringent$visit_type,survival_data_secondary_stringent$main_exposure_secondary_stringent_case_def,useNA = "always")
# 5998+826=6824 (new total)


## ---- impute missed follow-up visits for survival data secondary permissive

# set up a for loop of follow up dates to identify which you need to impute
old_date_to_add = c()
new_date_to_add = c()
id_to_add = c()
for (i in 1:nrow(followup_data)){
  for (j in 1:(ncol(followup_data))-1){
    if (j>2){
      if (is.na(followup_data[i,j]) & !(is.na(followup_data[i,j-1]))){
        
        # pull out the id
        id_to_add = c(id_to_add,followup_data$unq_memID[i])
        
        # pull out old date
        split_up = str_split(colnames(followup_data)[j-1],"-")[[1]]
        if (nchar(split_up[1]) == 1){
          old_date = paste0("0",split_up[1],"01",split_up[2])
        } else {
          old_date = paste0(split_up[1],"01",split_up[2])
        }
        old_date_to_add = c(old_date_to_add,old_date)
        
        # add in the new month-year date for having an imputed monthly follow-up visit
        split_up = str_split(colnames(followup_data)[j],"-")[[1]]
        if (nchar(split_up[1]) == 1){
          new_date = paste0("0",split_up[1],"01",split_up[2])
        } else {
          new_date = paste0(split_up[1],"01",split_up[2])
        }
        new_date_to_add = c(new_date_to_add,new_date)
        
      }
      
    }
    
  }
}
# make a data frame of the ids and date to impute
imputation_df = data.frame(id_to_add,old_date_to_add,new_date_to_add)
imputation_df$old_date_to_add=lubridate::mdy(imputation_df$old_date_to_add)
imputation_df$new_date_to_add=lubridate::mdy(imputation_df$new_date_to_add)

# set up some variables for the for loop
unique_participants = unique(survival_data_secondary_permissive$unq_memID)

# run the insert row function
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

# for each participant, look to see where missing one month of follow-up and carry forward the previous month's exposure case definition
# now start the for loop
for (k in 1:nrow(imputation_df)){
  for (j in 1:nrow(survival_data_secondary_permissive)){
    if (j>1){
      if(survival_data_secondary_permissive$unq_memID[j] == imputation_df$id_to_add[k] & survival_data_secondary_permissive$month_year[j] == imputation_df$old_date_to_add[k] & survival_data_secondary_permissive$visit_type[j]=="monthly visit"){
        # insert a new row
        survival_data_secondary_permissive = insertRow(survival_data_secondary_permissive,survival_data_secondary_permissive[j,],j)
        # add the imputed exposure and associated date
        survival_data_secondary_permissive$month_year[j] = imputation_df$new_date_to_add[k]
        survival_data_secondary_permissive$sample_id_date[j] = imputation_df$new_date_to_add[k]
        # add the new sample id 
        part = strsplit(as.character(imputation_df$new_date_to_add[k]),"-")[[1]]
        part1 = strsplit(part[1],"")[[1]]
        part1_pasted = paste0(part1[3],part1[4])
        monthly_date = paste0(part[3],part[2],part1_pasted)
        id_split = strsplit(imputation_df$id_to_add[k],"_")[[1]]
        survival_data_secondary_permissive$sample_name_final[j] = paste0(id_split[1],"-",monthly_date,"-",id_split[2])
        break
      } 
    }
  }
}
# check imputation
survival_data_secondary_permissive = dplyr::arrange(survival_data_secondary_permissive,unq_memID,sample_id_date)
table(survival_data_secondary_permissive$visit_type,survival_data_secondary_permissive$main_exposure_secondary_permissive_case_def,useNA = "always")
# 5998+826=6824 (new total)

# do one final check
table(survival_data_primary$visit_type,survival_data_primary$main_exposure_primary_case_def,useNA = "always")
table(survival_data_secondary_stringent$visit_type,survival_data_secondary_stringent$main_exposure_secondary_stringent_case_def,useNA = "always")
table(survival_data_secondary_permissive$visit_type,survival_data_secondary_permissive$main_exposure_secondary_permissive_case_def,useNA = "always")

# export the new data sets
write_rds(survival_data_primary,"Desktop/survival_data_primary_24AUG2020.rds")
write_rds(survival_data_secondary_stringent,"Desktop/survival_data_secondary_stringent_24AUG2020.rds")
write_rds(survival_data_secondary_permissive,"Desktop/survival_data_secondary_permissive_24AUG2020.rds")
write_csv(survival_data_primary,"Desktop/survival_data_primary_24AUG2020.csv")
write_csv(survival_data_secondary_stringent,"Desktop/survival_data_secondary_stringent_24AUG2020.csv")
write_csv(survival_data_secondary_permissive,"Desktop/survival_data_secondary_permissive_24AUG2020.csv")


