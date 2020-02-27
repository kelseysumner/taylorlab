# --------------------- #
#   Look at B-tubulin   #
#   qpcr failures in    #
#    Turkana data       #
#   February 27, 2020   #
#      K. Sumner        #
# --------------------- #


#### --------- load packages --------- ####
library(tidyverse)



#### ------- load in the data set -------- ####

# read in the qpcr data with phases 1 and 2 merged in
turkana_data = read_rds("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Merged data/All merged pcr results - round 2/EMBATALK_inventory_with_qpcr_14FEB2020.RDS")

# read in the qpcr data from phase 2 that is just the positives for Betsy
positive_data = read_csv("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Merged data/14FEB2020 Positive sample - round 2/EMBATALK_positive_samples_round2_14FEB2020.csv")


#### ------- pull the samples that had low CT values in the positive qpcr samples from phase 2 -------- ####

# now look at the positive samples from phase 2 only

# first merge in the dbs plate id info from the turkana data
subset_data = turkana_data %>%
  select(`sample ID`,`DBSplate ID`)
positive_data = left_join(positive_data,subset_data,by="sample ID")
length(which(is.na(positive_data$`DBSplate ID`)))

# now create a variable that determines whether a sample is collected from the community, travel, or facility
table(positive_data$`DBSplate ID`, useNA = "always")
positive_data$sample_type = rep(NA,nrow(positive_data))
for (i in 1:nrow(positive_data)){
  if (str_detect(positive_data$`DBSplate ID`[i],"C")){
    positive_data$sample_type[i] = "community"
  } 
  if (str_detect(positive_data$`DBSplate ID`[i],"T")){
    positive_data$sample_type[i] = "travel"
  } 
  if (str_detect(positive_data$`DBSplate ID`[i],"F")){
    positive_data$sample_type[i] = "facility"
  }
}
table(positive_data$sample_type, useNA = "always")
table(positive_data$`DBSplate ID`,positive_data$sample_type, useNA = "always")

# now create a variable that determines whether or not both replicates had CT values < 34
positive_data$under_ct_34 = ifelse(positive_data$pfr364CT1 < 34 & positive_data$pfr364CT2 < 34 & !(is.na(positive_data$pfr364CT1)) & !(is.na(positive_data$pfr364CT2)),"yes",
                                   ifelse(positive_data$pfr364CT1 < 34 & is.na(positive_data$pfr364CT2) & !(is.na(positive_data$pfr364CT1)),"yes",ifelse(
    positive_data$pfr364CT2 < 34 & is.na(positive_data$pfr364CT1) & !(is.na(positive_data$pfr364CT2)),"yes","no")))
# check the coding
table(positive_data$under_ct_34, useNA = "always")
under_34 = positive_data %>%
  filter(under_ct_34 == "yes")
over_34 = positive_data %>%
  filter(under_ct_34 == "no")
summary(under_34$pfr364CT1)
summary(under_34$pfr364CT2)
summary(over_34$pfr364CT1)
summary(over_34$pfr364CT2)

# now pull out how many fall into each category by sample type
table(positive_data$sample_type,positive_data$under_ct_34, useNA = "always")



#### --------- look at which samples failed with qpcr -------- ####

# now look at all turkana samples (first and second phases of qpcr)

# first subset the data set to just those samples that have qpcr results
turkana_data = turkana_data %>%
  filter(!(is.na(`pfr364R²`)))

# look at the wells where human beta-tubulin or pf364 ct values were NA
human_ct_1_na = turkana_data %>%
  filter(is.na(HbtubCT1))
human_ct_2_na = turkana_data %>%
  filter(is.na(HbtubCT2))
pfr_ct_1_na = turkana_data %>%
  filter(is.na(pfr364CT1))
pfr_ct_2_na = turkana_data %>%
  filter(is.na(pfr364CT2))

# now create a data set merged by well of this
merged_na = rbind(human_ct_1_na,human_ct_2_na,pfr_ct_1_na,pfr_ct_2_na)

# remove duplicate entries
merged_na_unq = unique(merged_na)

# now check to see if both replicates undetermined
merged_na_unq = merged_na_unq %>%
  select("Experiment Name","Well Position",HbtubCT1,HbtubCT2,pfr364CT1,pfr364CT2)
merged_na_unq$criteria_hb = ifelse(!(is.na(merged_na_unq$HbtubCT1)) & is.na(merged_na_unq$HbtubCT2),"fits_criteria",
  ifelse(!(is.na(merged_na_unq$HbtubCT2)) & is.na(merged_na_unq$HbtubCT1),"fits_criteria","no"))
table(merged_na_unq$criteria_hb, useNA = "always")
merged_na_unq$criteria_pf = ifelse(!(is.na(merged_na_unq$pfr364CT1)) & is.na(merged_na_unq$pfr364CT2),"fits_criteria",
                                   ifelse(!(is.na(merged_na_unq$pfr364CT2)) & is.na(merged_na_unq$pfr364CT1),"fits_criteria","no"))
table(merged_na_unq$criteria_pf, useNA = "always")
merged_na_unq = merged_na_unq %>%
  filter(criteria_hb == "fits_criteria" | criteria_pf == "fits_criteria")

# now subset to just the well position and experiment name
merged_na_unq = merged_na_unq %>%
  select(`Experiment Name`,`Well Position`)

# export this
write_csv(merged_na_unq,"Desktop/embatalk_undetermined_in_one_replicate_qpcr_data_27FEB2020.csv")



