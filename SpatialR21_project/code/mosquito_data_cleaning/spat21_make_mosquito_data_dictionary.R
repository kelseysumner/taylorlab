# ----------------------------------------- #
#         Spat21 Data Dictionary            #
#              Mosquito Data                #
#            January 16, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


#### ---------- read in the data sets ------------- ####

# read in the all species data set
allspecies_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/raw data/MOZZIECollectionSummary_June2017_July2018.csv")

# read in the anopheles mosquito data sets
# first the cleaned descriptive data set
anoph_descriptive_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/spat21_mosquito_anopheles_descriptive_long_data_18JAN2019.RDS")
# next the cleaned qpcr data set
anoph_qpcr_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/spat21_mosquito_qpcr_data_wide_3JAN2019.RDS")
# now the merged data
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")


#### ------- look at the variables for each data set -------- ####

## ------ allspecies_data

# look at all the column names
colnames(allspecies_data)

# Household ID
table(allspecies_data$`Household ID`, useNA = "always")
str(allspecies_data$`Household ID`)
# looks good, clean

# Repeat Instrument
table(allspecies_data$`Repeat Instrument`, useNA = "always")
str(allspecies_data$`Repeat Instrument`)
# looks good, clean

# Repeat Instance
table(allspecies_data$`Repeat Instance`, useNA = "always")
str(allspecies_data$`Repeat Instance`)
# looks good, clean

# Collection date
table(allspecies_data$`Collection date`, useNA = "always")
str(allspecies_data$`Collection date`)
# make a date variable
newdate = mdy(allspecies_data$`Collection date`)
table(newdate, useNA = "always")
allspecies_data$`Collection date` = newdate
str(allspecies_data$`Collection date`)

# Collection time
table(allspecies_data$`Collection Time`, useNA = "always")
str(allspecies_data$`Collection Time`)
# all missing, remove variable
allspecies_data$`Collection Time` <- NULL

# Village
table(allspecies_data$Village, useNA = "always")
str(allspecies_data$Village)
# make a factor
allspecies_data$Village = as.factor(allspecies_data$Village)
str(allspecies_data$Village)

# Collection done by:
table(allspecies_data$`Collection done by:`, useNA = "always")
str(allspecies_data$`Collection done by:`)
# leave as a character, looks clean

# Unfed
table(allspecies_data$Unfed, useNA = "always")
str(allspecies_data$Unfed)
# looks good, clean

# Anoph Blood Fed
table(allspecies_data$`Anoph Blood Fed`, useNA = "always")
str(allspecies_data$`Anoph Blood Fed`)
# looks good, clean

# Anoph Half Gravid
table(allspecies_data$`Anoph Half Gravid`, useNA = "always")
str(allspecies_data$`Anoph Half Gravid`)
# looks good, clean

# Anoph Gravid
table(allspecies_data$`Anoph Gravid`, useNA = "always")
str(allspecies_data$`Anoph Gravid`)
# looks good, clean

# Anoph Un-determined
table(allspecies_data$`Anoph Un-determined`, useNA = "always")
str(allspecies_data$`Anoph Un-determined`)
# looks good, clean

# Anoph Total
table(allspecies_data$`Anoph Total`, useNA = "always")
str(allspecies_data$`Anoph Total`)
# looks good, clean

# 2) Number of male anopheles
table(allspecies_data$`2) Number of male anopheles`, useNA = "always")
str(allspecies_data$`2) Number of male anopheles`)
# looks good, clean

# CulexUnfed
table(allspecies_data$CulexUnfed, useNA = "always")
str(allspecies_data$CulexUnfed)
summary(allspecies_data$CulexUnfed)
# looks good, clean

# CulexBlood fed
table(allspecies_data$`CulexBlood fed`, useNA = "always")
str(allspecies_data$`CulexBlood fed`)
summary(allspecies_data$`CulexBlood fed`)
# looks good, clean

# CulexHalf gravid
table(allspecies_data$`CulexHalf gravid`, useNA = "always")
str(allspecies_data$`CulexHalf gravid`)
summary(allspecies_data$`CulexHalf gravid`)
# looks good, clean

# CulexGravid
table(allspecies_data$`CulexGravid`, useNA = "always")
str(allspecies_data$`CulexGravid`)
summary(allspecies_data$`CulexGravid`)
# looks good, clean

# CulexUndetermined
table(allspecies_data$`CulexUndetermined`, useNA = "always")
str(allspecies_data$`CulexUndetermined`)
summary(allspecies_data$`CulexUndetermined`)
# looks good, clean

# CulexTotal
table(allspecies_data$`CulexTotal`, useNA = "always")
str(allspecies_data$`CulexTotal`)
summary(allspecies_data$`CulexTotal`)
# looks good, clean

# 4) Number of male culex
table(allspecies_data$`4) Number of male culex`, useNA = "always")
str(allspecies_data$`4) Number of male culex`)
summary(allspecies_data$`4) Number of male culex`)
# looks good, clean

# Form Checked by: 
table(allspecies_data$`Form Checked by:`, useNA = "always")
str(allspecies_data$`Form Checked by:`)
# looks good, clean

# Form checked date
table(allspecies_data$`Form checked date`, useNA = "always")
str(allspecies_data$`Form checked date`)
# change the date format
newdate = mdy(allspecies_data$`Form checked date`)
head(newdate)
head(allspecies_data$`Form checked date`)
table(allspecies_data$`Form checked date`)
allspecies_data$`Form checked date` = newdate
str(allspecies_data$`Form checked date`)

# Form entered by: 
table(allspecies_data$`Form entered by:`, useNA = "always")
str(allspecies_data$`Form entered by:`)
# looks good, clean

# Date Form Entered
table(allspecies_data$`Date Form Entered`, useNA = "always")
str(allspecies_data$`Date Form Entered`)
# change the date format
newdate = mdy(allspecies_data$`Date Form Entered`)
head(newdate)
head(allspecies_data$`Date Form Entered`)
table(allspecies_data$`Date Form Entered`)
allspecies_data$`Date Form Entered` = newdate
str(allspecies_data$`Date Form Entered`)

# Complete?
table(allspecies_data$`Complete?`, useNA = "always")
str(allspecies_data$`Complete?`)
# looks good, clean

# write out the new data set
# write_csv(allspecies_data,"spat21_allspecies_mosquito_data_18JAN2019.csv")
# write_rds(allspecies_data,"spat21_allspecies_mosquito_data_18JAN2019.RDS")


## ------ anoph_merged_data

# look at all the column names in the merged data set
colnames(anoph_merged_data)

# HH_ID
table(anoph_merged_data$HH_ID, useNA = "always")
str(anoph_merged_data$HH_ID)

# repeat_instance
table(anoph_merged_data$repeat_instance, useNA = "always")
str(anoph_merged_data$repeat_instance)

# collection_date
table(anoph_merged_data$collection_date, useNA = "always")
str(anoph_merged_data$collection_date)

# village
table(anoph_merged_data$village, useNA = "always")
str(anoph_merged_data$village)

# total_num_mosq_in_hh
table(anoph_merged_data$total_num_mosq_in_hh, useNA = "always")
str(anoph_merged_data$total_num_mosq_in_hh)

# abdominal_status
table(anoph_merged_data$abdominal_status, useNA = "always")
str(anoph_merged_data$abdominal_status)

# species_type
table(anoph_merged_data$species_type, useNA = "always")
str(anoph_merged_data$species_type)

# collection_week
table(anoph_merged_data$collection_week, useNA = "always")
str(anoph_merged_data$collection_week)

# collection_month_year_combo
table(anoph_merged_data$collection_month_year_combo, useNA = "always")
str(anoph_merged_data$collection_month_year_combo)

# sample_id_mosquito
table(anoph_merged_data$sample_id_mosquito, useNA = "always")
str(anoph_merged_data$sample_id_mosquito)

# sample_id_head
table(anoph_merged_data$sample_id_head, useNA = "always")
str(anoph_merged_data$sample_id_head)
length(which(is.na(anoph_merged_data$sample_id_head)))

# HbtubCT1_h
summary(anoph_merged_data$HbtubCT1_h)
str(anoph_merged_data$HbtubCT1_h)

# HbtubCT2_h
summary(anoph_merged_data$HbtubCT2_h)
str(anoph_merged_data$HbtubCT2_h)

# pfr364CT1_h
summary(anoph_merged_data$pfr364CT1_h)
str(anoph_merged_data$pfr364CT1_h)

# pfr364CT2_h
summary(anoph_merged_data$pfr364CT2_h)
str(anoph_merged_data$pfr364CT2_h)

# pfr364Q1_h
summary(anoph_merged_data$pfr364Q1_h)
str(anoph_merged_data$pfr364Q1_h)

# pfr364Q2_h
summary(anoph_merged_data$pfr364Q2_h)
str(anoph_merged_data$pfr364Q2_h)

# pf_pcr_infection_status_sample_level_h
summary(anoph_merged_data$pf_pcr_infection_status_sample_level_h)
str(anoph_merged_data$pf_pcr_infection_status_sample_level_h)

# pfr364Q_combined_h
summary(anoph_merged_data$pfr364Q_combined_h)
str(anoph_merged_data$pfr364Q_combined_h)

# hb_status_sample_level_h
summary(anoph_merged_data$hb_status_sample_level_h)
str(anoph_merged_data$hb_status_sample_level_h)

# sample_id_abdomen
table(anoph_merged_data$sample_id_abdomen, useNA = "always")
str(anoph_merged_data$sample_id_abdomen)
length(which(is.na(anoph_merged_data$sample_id_abdomen)))

# HbtubCT1_a
summary(anoph_merged_data$HbtubCT1_a)
str(anoph_merged_data$HbtubCT1_a)

# HbtubCT2_a
summary(anoph_merged_data$HbtubCT2_a)
str(anoph_merged_data$HbtubCT2_a)

# pfr364CT1_a
summary(anoph_merged_data$pfr364CT1_a)
str(anoph_merged_data$pfr364CT1_a)

# pfr364CT2_a
summary(anoph_merged_data$pfr364CT2_a)
str(anoph_merged_data$pfr364CT2_a)

# pfr364Q1_a
summary(anoph_merged_data$pfr364Q1_a)
str(anoph_merged_data$pfr364Q1_a)

# pfr364Q2_a
summary(anoph_merged_data$pfr364Q2_a)
str(anoph_merged_data$pfr364Q2_a)

# pf_pcr_infection_status_sample_level_a
summary(anoph_merged_data$pf_pcr_infection_status_sample_level_a)
str(anoph_merged_data$pf_pcr_infection_status_sample_level_a)

# pfr364Q_combined_a
summary(anoph_merged_data$pfr364Q_combined_a)
str(anoph_merged_data$pfr364Q_combined_a)

# hb_status_sample_level_a
summary(anoph_merged_data$hb_status_sample_level_a)
str(anoph_merged_data$hb_status_sample_level_a)

# pf_infection_status_mosquito_level
summary(anoph_merged_data$pf_infection_status_mosquito_level)
str(anoph_merged_data$pf_infection_status_mosquito_level)

# hb_status_mosquito_level
summary(anoph_merged_data$hb_status_mosquito_level)
str(anoph_merged_data$hb_status_mosquito_level)
