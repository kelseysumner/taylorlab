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

# read in the qpcr data from phase 2 that is just the positives for Betsy
positive_data_1 = read_csv("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/Merged data/9JAN2020 Positive sample - round 1/EMBATALK_positive_samples_9JAN2020.csv")

# read in the new turkana inventory
new_inventory = read_csv("Desktop/Tabitha last batch 5MAR2020/embatalk_inventory_extra_5MAR2020.csv")

# read in the sequencing inventories from noah
seq_inventory_1 = read_csv("Desktop/EMBATALK NGS Run 1.csv")
seq_inventory_2 = read_csv("Desktop/EMBATALK NGS Run 2.csv")
seq_inventory_3 = read_csv("Desktop/EMBATALK NGS Run 3.csv")


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



#### ------- now read in the phase 1 positive sample and make CT chart ----- ####

# now look at positives for phase 1 only

# now create a variable that determines whether a sample is collected from the community, travel, or facility
table(positive_data_1$`DBSplate ID`, useNA = "always")
positive_data_1$sample_type = rep(NA,nrow(positive_data_1))
for (i in 1:nrow(positive_data_1)){
  if (str_detect(positive_data_1$`DBSplate ID`[i],"C")){
    positive_data_1$sample_type[i] = "community"
  } 
  if (str_detect(positive_data_1$`DBSplate ID`[i],"T")){
    positive_data_1$sample_type[i] = "travel"
  } 
  if (str_detect(positive_data_1$`DBSplate ID`[i],"F")){
    positive_data_1$sample_type[i] = "facility"
  }
}
table(positive_data_1$sample_type, useNA = "always")
table(positive_data_1$`DBSplate ID`,positive_data_1$sample_type, useNA = "always")

# now create a variable that determines whether or not both replicates had CT values < 34
positive_data_1$under_ct_34 = ifelse(positive_data_1$pfr364CT1 < 34 & positive_data_1$pfr364CT2 < 34 & !(is.na(positive_data_1$pfr364CT1)) & !(is.na(positive_data_1$pfr364CT2)),"yes",
                                   ifelse(positive_data_1$pfr364CT1 < 34 & is.na(positive_data_1$pfr364CT2) & !(is.na(positive_data_1$pfr364CT1)),"yes",ifelse(
                                     positive_data_1$pfr364CT2 < 34 & is.na(positive_data_1$pfr364CT1) & !(is.na(positive_data_1$pfr364CT2)),"yes","no")))
# check the coding
table(positive_data_1$under_ct_34, useNA = "always")
under_34 = positive_data_1 %>%
  filter(under_ct_34 == "yes")
over_34 = positive_data_1 %>%
  filter(under_ct_34 == "no")
summary(under_34$pfr364CT1)
summary(under_34$pfr364CT2)
summary(over_34$pfr364CT1)
summary(over_34$pfr364CT2)

# now pull out how many fall into each category by sample type
table(positive_data_1$sample_type,positive_data_1$under_ct_34, useNA = "always")



#### ---------- pull out Pf+ travelers information --------- ####

# pull out the Pf positive and travelers samples from the full data set
pf_data = turkana_data %>%
  filter(str_detect(`DBSplate ID`,"T") & pf_pcr_infection_status == "positive") %>%
  select(`sample ID`,`DBSplate ID`,column,row,`gDNA plate ID`)

# export the data set for Betsy and Tabitha
write_csv(pf_data,"Desktop/embatalk_positive_travelers_5MAR2020.csv")



#### ----- look at duplicates in the inventory ------- ####

# now merge together the old and new inventories
colnames(turkana_data)
colnames(new_inventory)
new_inventory = new_inventory %>%
  filter("Sample Name" != "Blank") %>%
  rename("Sample Name" = "sample ID","Column Number"="column","Row Number"="row") %>%
  select(-c(Well))
turkana_data = turkana_data %>%
  select("sample ID","column","row","DBSplate ID")
full_inventory = rbind(turkana_data,new_inventory)
intersect(turkana_data$`sample ID`,new_inventory$`sample ID`) # 4 samples intersected

# now check for duplicates
length(unique(full_inventory$`sample ID`)) # 6928 unique 
length(which(is.na(full_inventory$`sample ID`) == T)) # 0 missing
count_table = table(full_inventory$`sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 13 duplicates
dups_table
ids_to_remove = names(dups_table)

# write out info for duplicates
dup_df = full_inventory[which(full_inventory$`sample ID` %in% ids_to_remove),]
write_csv(dup_df,"Desktop/embatalk_duplicates_all_5MAR2020.csv")


#### ------ create separate data sets for over and under CT 34 for first 2 qpcr runs --------- ####

# cut down the positive data sets to just the columns of interest
colnames(positive_data)
colnames(positive_data_1)
positive_data = positive_data %>%
  select("sample ID","gDNA plate ID","column","row","sample_type","under_ct_34")
positive_data_1 = positive_data_1 %>%
  select("sample ID","gDNA plate ID","column","row","sample_type","under_ct_34")
all_positive_data = rbind(positive_data,positive_data_1)

# separate source plate - well column
# do this for the first sequence inventory
source_plate = rep(NA,nrow(seq_inventory_1))
well = rep(NA,nrow(seq_inventory_1))
for (i in 1:nrow(seq_inventory_1)){
  if (is.na(seq_inventory_1$`Source Well-Plate`[i])){
    source_plate[i] = NA
    well[i] = NA
  } else {
    source_plate[i] = str_split(seq_inventory_1$`Source Well-Plate`[i],"-")[[1]][1]
    well[i] = str_split(seq_inventory_1$`Source Well-Plate`[i],"-")[[1]][2]
  }
}
# add the new vectors to the dataset
seq_inventory_1$source_plate = source_plate
seq_inventory_1$well = well
length(which(is.na(seq_inventory_1$source_plate)))
length(which(is.na(seq_inventory_1$well)))
table(seq_inventory_1$source_plate, useNA = "always")
table(seq_inventory_1$well, useNA = "always")
seq_inventory_1$source_plate[which(seq_inventory_1$`Source Well-Plate`=="2BH4")] = "2B"
seq_inventory_1$well[which(seq_inventory_1$`Source Well-Plate`=="2BH4")] = "H4"
# do this for the second sequence inventory
source_plate = rep(NA,nrow(seq_inventory_2))
well = rep(NA,nrow(seq_inventory_2))
for (i in 1:nrow(seq_inventory_2)){
  if (is.na(seq_inventory_2$`Source Plate-Well`[i])){
    source_plate[i] = NA
    well[i] = NA
  } else {
    source_plate[i] = str_split(seq_inventory_2$`Source Plate-Well`[i],"-")[[1]][1]
    well[i] = str_split(seq_inventory_2$`Source Plate-Well`[i],"-")[[1]][2]
  }
}
# add the new vectors to the dataset
seq_inventory_2$source_plate = source_plate
seq_inventory_2$well = well
length(which(is.na(seq_inventory_2$source_plate)))
length(which(is.na(seq_inventory_2$well)))
table(seq_inventory_2$source_plate, useNA = "always")
table(seq_inventory_2$well, useNA = "always")
# do this for the third sequence inventory
source_plate = rep(NA,nrow(seq_inventory_3))
well = rep(NA,nrow(seq_inventory_3))
for (i in 1:nrow(seq_inventory_3)){
  if (is.na(seq_inventory_3$`Source Plate-Well`[i])){
    source_plate[i] = NA
    well[i] = NA
  } else {
    source_plate[i] = str_split(seq_inventory_3$`Source Plate-Well`[i],"-")[[1]][1]
    well[i] = str_split(seq_inventory_3$`Source Plate-Well`[i],"-")[[1]][2]
  }
}
# add the new vectors to the dataset
seq_inventory_3$source_plate = source_plate
seq_inventory_3$well = well
length(which(is.na(seq_inventory_3$source_plate)))
length(which(is.na(seq_inventory_3$well)))
table(seq_inventory_3$source_plate, useNA = "always")
table(seq_inventory_3$well, useNA = "always")

# cut down the sequencing inventory data sets
colnames(seq_inventory_1)
seq_inventory_1 = seq_inventory_1 %>%
  select("Sample","Pf(+) Plate","Column Letter","Row Number","source_plate","well")
seq_inventory_2 = seq_inventory_2 %>%
  select("Sample","Pf(+) Plate","Column Letter","Row Number","source_plate","well")
seq_inventory_3 = seq_inventory_3 %>%
  select("Sample","Pf(+) Plate","Column Letter","Row Number","source_plate","well")

# now combine the two sequencing data sets together
all_seq_inventory = rbind(seq_inventory_1,seq_inventory_2,seq_inventory_3)

# recode one of the samples in the seq inventory
all_seq_inventory$Sample[which(all_seq_inventory$Sample=="g0503")] = "G0503"

# rename the positive data set sample_id
colnames(all_positive_data)
all_positive_data = all_positive_data %>%
  rename("Sample" = "sample ID")

# change the positive data sample Ids to Os
all_positive_data$Sample[which(all_positive_data$Sample=="GO265")] = "G0265"
all_positive_data$Sample[which(all_positive_data$Sample=="GO270")] = "G0270"
all_positive_data$Sample[which(all_positive_data$Sample=="GO284")] = "G0284"
all_positive_data$Sample[which(all_positive_data$Sample=="GO338A")] = "G0338A"
all_positive_data$Sample[which(all_positive_data$Sample=="GO385A")] = "G0385A"
all_positive_data$Sample[which(all_positive_data$Sample=="MO268")] = "M0268"
all_seq_inventory$Sample[which(all_seq_inventory$Sample=="G0499B")] = "G0499D"
all_seq_inventory$Sample[which(all_seq_inventory$Sample=="G0266A")] = "G0226A"

# combine the positive data set and sequencing inventory
all_data_combined = left_join(all_seq_inventory,all_positive_data,by="Sample")
# check the join
length(which(is.na(all_data_combined$`Column Letter`)))
length(which(is.na(all_data_combined$column)))

# separate the data sets by sample type
community_data = all_data_combined %>%
  filter(sample_type == "community")
traveler_data = all_data_combined %>%
  filter(sample_type == "travel")
facility_data = all_data_combined %>%
  filter(sample_type == "facility")

# separate the data sets by ct values
community_data_under_ct_34 = community_data %>%
  filter(under_ct_34 == "yes")
community_data_over_ct_34 = community_data %>%
  filter(under_ct_34 == "no")
traveler_data_under_ct_34 = traveler_data %>%
  filter(under_ct_34 == "yes")
traveler_data_over_ct_34 = traveler_data %>%
  filter(under_ct_34 == "no")

# export the data sets
write_csv(community_data_under_ct_34, "Desktop/embatalk_community_data_under_ct_34_10MAR2020.csv")
write_csv(community_data_over_ct_34, "Desktop/embatalk_community_data_over_ct_34_10MAR2020.csv")
write_csv(traveler_data_under_ct_34, "Desktop/embatalk_traveler_data_under_ct_34_10MAR2020.csv")
write_csv(traveler_data_over_ct_34, "Desktop/embatalk_traveler_data_over_ct_34_10MAR2020.csv")
write_csv(facility_data, "Desktop/embatalk_facility_data_all_10MAR2020.csv")

# read in the extra turkana samples from betsy
extra_turkana_data = read_csv("Desktop/Dissertation Materials/Turkana Project/EMBATALK/Lab materials/travelers positives/for_betsy_final.csv")

# merge the extra turkana samples from betsy with the gdna plate info
small_turkana_data = turkana_data %>%
  select(`sample ID`,`gDNA plate ID`)
extra_turkana_data = left_join(extra_turkana_data,small_turkana_data,by="sample ID")

# export the extra turkana samples 
extra_turkana_data = extra_turkana_data %>%
  select(-c(`DBSplate ID.x`))
extra_turkana_data = extra_turkana_data %>%
  select(`sample ID`,`gDNA plate ID`,"row","column")
write_csv(extra_turkana_data,"Desktop/embatalk_traveler_extra_samples_10MAR2020.csv")
