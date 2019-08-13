# ----------------------------------------- #
#           Make Final Inventories          #
#               Mosquito Data               #
#            Mozzie Phase 1 Data            #
#              August 6, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #


#### --------- load packages ----------------- ####
library(tidyverse)



#### -------- load in the data sets ---------- ####

# read in the original inventories
# read in the mosquito inventory
mosquito_inventory_data = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/lab_inventories/clean files/mosquito_lab_inventory_final_11JAN2019.csv")

# read in the final merged data sets
# read in the mosquito data set
anoph_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")


#### ------ merge the two inventories together ------- ####

# select variables of interest for anoph data
colnames(anoph_data)
anoph_data = anoph_data %>%
  select(-c(repeat_instance,total_num_mosq_in_hh,abdominal_status,species_type,collection_week,collection_month_year_combo))

# select variables of interest for mosquito_inventory
colnames(mosquito_inventory_data)
mosquito_inventory_data = mosquito_inventory_data %>%
  select(-c(comment,extracted,sample_received,shipment_date,sample_received))

# relabel the mosquito plate name
table(mosquito_inventory_data$gdna_plate_number, useNA = "always")
for (i in 1:nrow(mosquito_inventory_data)){
  if (is.na(mosquito_inventory_data$gdna_plate_number[i])){
    mosquito_inventory_data$gdna_plate_number[i] = "Not recorded"
  } else {
    first_split = str_split(mosquito_inventory_data$gdna_plate_number[i],"-")[[1]]
    second_split = str_split(first_split," ")[[1]]
    mosquito_inventory_data$gdna_plate_number[i] = paste0(second_split[1],"-",second_split[2])
  }
}
table(mosquito_inventory_data$gdna_plate_number,useNA = "always")
# relabel some manually
mosquito_inventory_data$gdna_plate_number[which(mosquito_inventory_data$gdna_plate_number=="Spat24M-ch.ext.gDNA")]="Spat-24M"
mosquito_inventory_data$gdna_plate_number[which(mosquito_inventory_data$gdna_plate_number=="Spat25M-ch.ext.gDNA")]="Spat-25M"
mosquito_inventory_data$gdna_plate_number[which(mosquito_inventory_data$gdna_plate_number=="Spat26M-ch.ext.gDNA")]="Spat-26M"
mosquito_inventory_data$gdna_plate_number[which(mosquito_inventory_data$gdna_plate_number=="Spat27M-ch.ext.gDNA")]="Spat-27M"
mosquito_inventory_data$gdna_plate_number[which(mosquito_inventory_data$gdna_plate_number=="Spat28M-ch.ext.gDNA")]="Spat-28M"
mosquito_inventory_data$gdna_plate_number[which(mosquito_inventory_data$gdna_plate_number=="Spat29M-ch.ext.gDNA")]="Spat-29M"
mosquito_inventory_data$gdna_plate_number[which(mosquito_inventory_data$gdna_plate_number=="Spat30M-ch.ext.gDNA")]="Spat-30M"
mosquito_inventory_data$gdna_plate_number[which(mosquito_inventory_data$gdna_plate_number=="Spat31M-ch.ext.gDNA")]="Spat-31M"
mosquito_inventory_data$gdna_plate_number[which(mosquito_inventory_data$gdna_plate_number=="Spat32M-ch.ext.Gdna")]="Spat-32M"
mosquito_inventory_data$gdna_plate_number[which(mosquito_inventory_data$gdna_plate_number=="Spat33M-ch.ext.Gdna")]="Spat-33M"
mosquito_inventory_data$gdna_plate_number[which(mosquito_inventory_data$gdna_plate_number=="Spat34M-ch.ext.Gdna")]="Spat-34M"
# check the relabel
table(mosquito_inventory_data$gdna_plate_number,useNA = "always")

# check for duplicates in the mosquito inventory
# sample_id_mosquito
length(unique(mosquito_inventory_data$sample_id_mosquito)) # 2977 unique 
length(which(is.na(mosquito_inventory_data$sample_id_mosquito) == T)) # 0 missing
count_table = table(mosquito_inventory_data$sample_id_mosquito, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table

# check for duplicates in the anoph data
# sample_id_mosquito
length(unique(anoph_data$sample_id_mosquito)) # 1494 unique 
length(which(is.na(anoph_data$sample_id_mosquito) == T)) # 0 missing
count_table = table(anoph_data$sample_id_mosquito, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table
# sample_id_abdomen
length(unique(anoph_data$sample_id_abdomen)) # 1449 unique 
length(which(is.na(anoph_data$sample_id_abdomen) == T)) # 46 missing
count_table = table(anoph_data$sample_id_abdomen, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table
# sample_id_head
length(unique(anoph_data$sample_id_head)) # 1464 unique 
length(which(is.na(anoph_data$sample_id_head) == T)) # 31 missing
count_table = table(anoph_data$sample_id_head, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table

# create separate data sets in the inventory for mosquito head and mosquito abdomen
head_inventory = mosquito_inventory_data[which(str_detect(mosquito_inventory_data$sample_id_mosquito,"H")),]
abdomen_inventory = mosquito_inventory_data[which(str_detect(mosquito_inventory_data$sample_id_mosquito,"A")),]
# check the data set creation
length(which(str_detect(mosquito_inventory_data$sample_id_mosquito,"H")))
length(which(str_detect(mosquito_inventory_data$sample_id_mosquito,"A")))

# now rename the column names in the head and abdomen inventories to indicate if head or abodmen data
# head_inventory
colnames(head_inventory)
head_inventory = rename(head_inventory,"sample_id_head"="sample_id_mosquito","gdna_extraction_date_h"="gdna_extraction_date",
                        "gdna_plate_number_h"="gdna_plate_number","gdna_column_number_h"="gdna_column_number", "gdna_row_number_h"="gdna_row_number")
colnames(head_inventory)
# abdomen_inventory
colnames(abdomen_inventory)
abdomen_inventory = rename(abdomen_inventory,"sample_id_abdomen"="sample_id_mosquito","gdna_extraction_date_a"="gdna_extraction_date",
                        "gdna_plate_number_a"="gdna_plate_number","gdna_column_number_a"="gdna_column_number", "gdna_row_number_a"="gdna_row_number")
colnames(abdomen_inventory)

# now merge together each head and abdomen inventory to anoph data
# first the abdomen info
final_data = full_join(abdomen_inventory,anoph_data,by="sample_id_abdomen")
length(intersect(abdomen_inventory$sample_id_abdomen,anoph_data$sample_id_abdomen))
length(setdiff(anoph_data$sample_id_abdomen,abdomen_inventory$sample_id_abdomen))
setdiff(anoph_data$sample_id_abdomen,abdomen_inventory$sample_id_abdomen)
# then the head info
final_data_2 = full_join(head_inventory,final_data,by="sample_id_head")
length(intersect(head_inventory$sample_id_head,final_data$sample_id_head))
length(setdiff(final_data$sample_id_head,head_inventory$sample_id_head))
setdiff(final_data$sample_id_head,head_inventory$sample_id_head)
final_data = final_data_2
# one final check
setdiff(final_data$sample_id_abdomen,abdomen_inventory$sample_id_abdomen)
setdiff(abdomen_inventory$sample_id_abdomen,final_data$sample_id_abdomen)
setdiff(final_data$sample_id_head,head_inventory$sample_id_head)
setdiff(head_inventory$sample_id_head,final_data$sample_id_head)
# looks good

# now reorganize the columns
colnames(final_data)
new_order = c("HH_ID","collection_date","village","sample_id_mosquito","pf_infection_status_mosquito_level","hb_status_mosquito_level","sample_id_head","gdna_extraction_date_h","gdna_plate_number_h","gdna_column_number_h","gdna_row_number_h",
              "HbtubCT1_h","HbtubCT2_h","pfr364CT1_h","pfr364CT2_h","pfr364Q1_h","pfr364Q2_h","pfr364Q_combined_h","pf_pcr_infection_status_sample_level_h","hb_status_sample_level_h",
              "sample_id_abdomen","gdna_extraction_date_a","gdna_plate_number_a","gdna_column_number_a","gdna_row_number_a",
              "HbtubCT1_a","HbtubCT2_a","pfr364CT1_a","pfr364CT2_a","pfr364Q1_a","pfr364Q2_a","pfr364Q_combined_a","pf_pcr_infection_status_sample_level_a","hb_status_sample_level_a")
final_data = final_data[new_order]
colnames(final_data)

# merge in the sequencing info
sequencing_inventory = read_csv("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/mosquito_sequencing_inventory/spat21_mosquito_sequencing_inventory_with_CT_values_with_pilot.csv")

# look for duplicates in the sequencing inventory
length(unique(sequencing_inventory$`Sample ID`)) # 337 unique 
length(which(is.na(sequencing_inventory$`Sample ID`) == T)) # 0 missing
count_table = table(sequencing_inventory$`Sample ID`, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table

# cut down sequencing inventory to just what you need
colnames(sequencing_inventory)
sequencing_inventory = sequencing_inventory %>%
  select(c("Sample ID"))

# remove the controls
sequencing_inventory = sequencing_inventory[-which(sequencing_inventory$`Sample ID`=="3D7 blood spot/culture"),]
sequencing_inventory = sequencing_inventory[-which(sequencing_inventory$`Sample ID`=="3D7 gDNA stock"),]
sequencing_inventory = sequencing_inventory[-which(sequencing_inventory$`Sample ID`=="3D7/7g8/Dd2"),]
sequencing_inventory = sequencing_inventory[-which(is.na(sequencing_inventory$`Sample ID`)),]

# add a column saying sent for sequencing
sequencing_inventory$sent_for_sequencing = rep("yes",nrow(sequencing_inventory))

# split up the sequencing inventory data to head and abdomens
head_seq_inventory = sequencing_inventory[which(str_detect(sequencing_inventory$`Sample ID`,"H")),]
abdomen_seq_inventory = sequencing_inventory[which(str_detect(sequencing_inventory$`Sample ID`,"A")),]

# change sample ID headers
head_seq_inventory = rename(head_seq_inventory,"sample_id_head"="Sample ID","sent_for_sequencing_h"="sent_for_sequencing")
abdomen_seq_inventory = rename(abdomen_seq_inventory,"sample_id_abdomen"="Sample ID", "sent_for_sequencing_a"="sent_for_sequencing")

# now merge in the sequencing inventory with the rest of the mosquito data
colnames(final_data)
final_data = left_join(final_data,head_seq_inventory,by="sample_id_head")
final_data = left_join(final_data,abdomen_seq_inventory,by="sample_id_abdomen")

# now reorganize the columns one last time
colnames(final_data)
new_order = c("HH_ID","collection_date","village","sample_id_mosquito","pf_infection_status_mosquito_level","hb_status_mosquito_level","sample_id_head","gdna_extraction_date_h","gdna_plate_number_h","gdna_column_number_h","gdna_row_number_h",
              "HbtubCT1_h","HbtubCT2_h","pfr364CT1_h","pfr364CT2_h","pfr364Q1_h","pfr364Q2_h","pfr364Q_combined_h","pf_pcr_infection_status_sample_level_h","hb_status_sample_level_h","sent_for_sequencing_h",
              "sample_id_abdomen","gdna_extraction_date_a","gdna_plate_number_a","gdna_column_number_a","gdna_row_number_a",
              "HbtubCT1_a","HbtubCT2_a","pfr364CT1_a","pfr364CT2_a","pfr364Q1_a","pfr364Q2_a","pfr364Q_combined_a","pf_pcr_infection_status_sample_level_a","hb_status_sample_level_a","sent_for_sequencing_a")
final_data = final_data[new_order]
colnames(final_data)

# check to make sure that when the pf status is positive, sent for sequencing is yes
# check for the abdomens
length(which(final_data$pf_pcr_infection_status_sample_level_a=="positive"))
length(which(final_data$sent_for_sequencing_a=="yes"))
length(which(final_data$pf_pcr_infection_status_sample_level_a=="positive" & final_data$sent_for_sequencing_a=="yes"))
test = final_data[which(final_data$pf_pcr_infection_status_sample_level_a=="positive"& is.na(final_data$sent_for_sequencing_a)),]
test_2 = final_data[which(is.na(final_data$pf_pcr_infection_status_sample_level_a) & final_data$sent_for_sequencing_a=="yes"),]
# check for the heads
length(which(final_data$pf_pcr_infection_status_sample_level_h=="positive"))
length(which(final_data$sent_for_sequencing_h=="yes"))
length(which(final_data$pf_pcr_infection_status_sample_level_h=="positive" & final_data$sent_for_sequencing_h=="yes"))
test = final_data[which(final_data$pf_pcr_infection_status_sample_level_h=="positive"& is.na(final_data$sent_for_sequencing_h)),]
test_2 = final_data[which(is.na(final_data$pf_pcr_infection_status_sample_level_h) & final_data$sent_for_sequencing_h=="yes"),]
# looks like some samples were sequenced but were not in the sequencing inventory

# set sample id at the mosquito level for all observations
# sample_id_mosquito
length(which(is.na(final_data$sample_id_mosquito)))
length(which(is.na(final_data$sample_id_mosquito) & !(is.na(final_data$sample_id_abdomen))))
length(which(is.na(final_data$sample_id_mosquito) & !(is.na(final_data$sample_id_head))))
for (i in 1:nrow(final_data)){
  if (!(is.na(final_data$sample_id_mosquito[i]))){
    final_data$sample_id_mosquito[i] = final_data$sample_id_mosquito[i]
  } else if (is.na(final_data$sample_id_mosquito[i]) & !(is.na(final_data$sample_id_abdomen[i]))){
    test = str_split(final_data$sample_id_abdomen[i],"")[[1]]
    final_data$sample_id_mosquito[i] = paste0(test[1],test[2],test[3],test[4],test[6],test[7],test[8],test[9],test[10])
  } else if (is.na(final_data$sample_id_mosquito[i]) & !(is.na(final_data$sample_id_head[i]))){
    test2 = str_split(final_data$sample_id_head[i],"")[[1]]
    final_data$sample_id_mosquito[i] = paste0(test2[1],test2[2],test2[3],test2[4],test2[6],test2[7],test2[8],test2[9],test2[10])
  } else {
    final_data$sample_id_mosquito[i] = final_data$sample_id_mosquito[i]
  }
}
length(which(is.na(final_data$sample_id_mosquito)))
table(nchar(final_data$sample_id_mosquito))
# check for duplicates 
length(unique(final_data$sample_id_mosquito)) # 1497 unique 
length(which(is.na(final_data$sample_id_mosquito) == T)) # 0 missing
count_table = table(final_data$sample_id_mosquito, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 49 duplicates
length(dups_table)
dups_table
# for the sample_id_mosquitoes that had 3 entries, change manually
# K01 00029
final_data$HH_ID[which(final_data$sample_id_mosquito=="K01 00029" & final_data$sample_id_head=="K01 H00029")] = "K01"
final_data$collection_date[which(final_data$sample_id_mosquito=="K01 00029" & final_data$sample_id_head=="K01 H00029")] = "2017-07-17"
final_data$village[which(final_data$sample_id_mosquito=="K01 00029" & final_data$sample_id_head=="K01 H00029")] = "Kinesamo"
final_data = final_data[-which(final_data$sample_id_mosquito=="K01 00029" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# K01 00030
final_data$HH_ID[which(final_data$sample_id_mosquito=="K01 00030" & final_data$sample_id_head=="K01 H00030")] = "K01"
final_data$collection_date[which(final_data$sample_id_mosquito=="K01 00030" & final_data$sample_id_head=="K01 H00030")] = "2017-07-17"
final_data$village[which(final_data$sample_id_mosquito=="K01 00030" & final_data$sample_id_head=="K01 H00030")] = "Kinesamo"
final_data = final_data[-which(final_data$sample_id_mosquito=="K01 00030" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# K01 00031
final_data$HH_ID[which(final_data$sample_id_mosquito=="K01 00031" & final_data$sample_id_head=="K01 H00031")] = "K01"
final_data$collection_date[which(final_data$sample_id_mosquito=="K01 00031" & final_data$sample_id_head=="K01 H00031")] = "2017-07-17"
final_data$village[which(final_data$sample_id_mosquito=="K01 00031" & final_data$sample_id_head=="K01 H00031")] = "Kinesamo"
final_data = final_data[-which(final_data$sample_id_mosquito=="K01 00031" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# K01 00032
final_data$HH_ID[which(final_data$sample_id_mosquito=="K01 00032" & final_data$sample_id_head=="K01 H00032")] = "K01"
final_data$collection_date[which(final_data$sample_id_mosquito=="K01 00032" & final_data$sample_id_head=="K01 H00032")] = "2017-07-24"
final_data$village[which(final_data$sample_id_mosquito=="K01 00032" & final_data$sample_id_head=="K01 H00032")] = "Kinesamo"
final_data = final_data[-which(final_data$sample_id_mosquito=="K01 00032" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# K01 00047
final_data$HH_ID[which(final_data$sample_id_mosquito=="K01 00047" & final_data$sample_id_head=="K01 H00047")] = "K01"
final_data$collection_date[which(final_data$sample_id_mosquito=="K01 00047" & final_data$sample_id_head=="K01 H00047")] = "2017-08-21"
final_data$village[which(final_data$sample_id_mosquito=="K01 00047" & final_data$sample_id_head=="K01 H00047")] = "Kinesamo"
final_data = final_data[-which(final_data$sample_id_mosquito=="K01 00047" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# K01 00048
final_data$HH_ID[which(final_data$sample_id_mosquito=="K01 00048" & final_data$sample_id_head=="K01 H00048")] = "K01"
final_data$collection_date[which(final_data$sample_id_mosquito=="K01 00048" & final_data$sample_id_head=="K01 H00048")] = "2017-08-21"
final_data$village[which(final_data$sample_id_mosquito=="K01 00048" & final_data$sample_id_head=="K01 H00048")] = "Kinesamo"
final_data = final_data[-which(final_data$sample_id_mosquito=="K01 00048" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# K01 00049
final_data$HH_ID[which(final_data$sample_id_mosquito=="K01 00049" & final_data$sample_id_head=="K01 H00049")] = "K01"
final_data$collection_date[which(final_data$sample_id_mosquito=="K01 00049" & final_data$sample_id_head=="K01 H00049")] = "2017-08-28"
final_data$village[which(final_data$sample_id_mosquito=="K01 00049" & final_data$sample_id_head=="K01 H00049")] = "Kinesamo"
final_data = final_data[-which(final_data$sample_id_mosquito=="K01 00049" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# M01 00001
final_data$HH_ID[which(final_data$sample_id_mosquito=="M01 00001" & final_data$sample_id_head=="M01 H00001")] = "M01"
final_data$collection_date[which(final_data$sample_id_mosquito=="M01 00001" & final_data$sample_id_head=="M01 H00001")] = "2017-06-20"
final_data$village[which(final_data$sample_id_mosquito=="M01 00001" & final_data$sample_id_head=="M01 H00001")] = "Maruti"
final_data = final_data[-which(final_data$sample_id_mosquito=="M01 00001" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# M01 00002
final_data$HH_ID[which(final_data$sample_id_mosquito=="M01 00002" & final_data$sample_id_head=="M01 H00002")] = "M01"
final_data$collection_date[which(final_data$sample_id_mosquito=="M01 00002" & final_data$sample_id_head=="M01 H00002")] = "2017-06-20"
final_data$village[which(final_data$sample_id_mosquito=="M01 00002" & final_data$sample_id_head=="M01 H00002")] = "Maruti"
final_data = final_data[-which(final_data$sample_id_mosquito=="M01 00002" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# M01 00003
final_data$HH_ID[which(final_data$sample_id_mosquito=="M01 00003" & final_data$sample_id_head=="M01 H00003")] = "M01"
final_data$collection_date[which(final_data$sample_id_mosquito=="M01 00003" & final_data$sample_id_head=="M01 H00003")] = "2017-06-20"
final_data$village[which(final_data$sample_id_mosquito=="M01 00003" & final_data$sample_id_head=="M01 H00003")] = "Maruti"
final_data = final_data[-which(final_data$sample_id_mosquito=="M01 00003" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# M01 00004
final_data$HH_ID[which(final_data$sample_id_mosquito=="M01 00004" & final_data$sample_id_head=="M01 H00004")] = "M01"
final_data$collection_date[which(final_data$sample_id_mosquito=="M01 00004" & final_data$sample_id_head=="M01 H00004")] = "2017-06-28"
final_data$village[which(final_data$sample_id_mosquito=="M01 00004" & final_data$sample_id_head=="M01 H00004")] = "Maruti"
final_data = final_data[-which(final_data$sample_id_mosquito=="M01 00004" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# M01 00005
final_data$HH_ID[which(final_data$sample_id_mosquito=="M01 00005" & final_data$sample_id_head=="M01 H00005")] = "M01"
final_data$collection_date[which(final_data$sample_id_mosquito=="M01 00005" & final_data$sample_id_head=="M01 H00005")] = "2017-06-28"
final_data$village[which(final_data$sample_id_mosquito=="M01 00005" & final_data$sample_id_head=="M01 H00005")] = "Maruti"
final_data = final_data[-which(final_data$sample_id_mosquito=="M01 00005" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# M01 00006
final_data$HH_ID[which(final_data$sample_id_mosquito=="M01 00006" & final_data$sample_id_head=="M01 H00006")] = "M01"
final_data$collection_date[which(final_data$sample_id_mosquito=="M01 00006" & final_data$sample_id_head=="M01 H00006")] = "2017-06-28"
final_data$village[which(final_data$sample_id_mosquito=="M01 00006" & final_data$sample_id_head=="M01 H00006")] = "Maruti"
final_data = final_data[-which(final_data$sample_id_mosquito=="M01 00006" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# M09 00042
final_data$HH_ID[which(final_data$sample_id_mosquito=="M09 00042" & final_data$sample_id_head=="M09 H00042")] = "M09"
final_data$collection_date[which(final_data$sample_id_mosquito=="M09 00042" & final_data$sample_id_head=="M09 H00042")] = "2017-07-18"
final_data$village[which(final_data$sample_id_mosquito=="M09 00042" & final_data$sample_id_head=="M09 H00042")] = "Maruti"
final_data = final_data[-which(final_data$sample_id_mosquito=="M09 00042" & is.na(final_data$sample_id_abdomen) & is.na(final_data$sample_id_head)),]
# check for duplicates 
length(unique(final_data$sample_id_mosquito)) # 1497 unique 
length(which(is.na(final_data$sample_id_mosquito) == T)) # 0 missing
count_table = table(final_data$sample_id_mosquito, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 49 duplicates
length(dups_table)
dups_table

# check the input
length(which(is.na(final_data$sample_id_abdomen))) # 58
length(which(is.na(final_data$sample_id_head))) # 57
# sort the sample_id_mosquito variable
final_data = final_data[order(final_data$sample_id_mosquito),]
# make sure that all the mosquito head and abdomen samples are on the same row
# write a for loop that will check for if the mosquito id is on multiple lines moves the information all to the same line
# start for loop
for (i in 1:nrow(final_data)){
  if ((final_data$sample_id_mosquito[i] == final_data$sample_id_mosquito[i+1]) & !(is.na(final_data$sample_id_mosquito[i])) & !(is.na(final_data$sample_id_mosquito[i+1])) & i != nrow(final_data)){
    for (k in 1:ncol(final_data)){
      if (is.na(final_data[i,k]) & !(is.na(final_data[i+1,k]))){
        final_data[i,k] = final_data[i+1,k]
      }
      if (is.na(final_data[i+1,k]) & !(is.na(final_data[i,k]))){
        final_data[i+1,k] = final_data[i,k]
      }
    }
  } else {
    final_data[i,] = final_data[i,]
  }
}
# check the output
colnames(final_data)
length(which(is.na(final_data$sample_id_mosquito))) # none missing
length(which(is.na(final_data$sample_id_abdomen))) # 9 missing
length(which(is.na(final_data$sample_id_head))) # 8 missing
# looks like it is working correctly
# delete the rows that are now duplicates
for (i in 1:nrow(final_data)){
  if ((final_data$sample_id_mosquito[i] == final_data$sample_id_mosquito[i+1]) & !(is.na(final_data$sample_id_mosquito[i])) & !(is.na(final_data$sample_id_mosquito[i+1])) & i != nrow(final_data)){
    final_data = final_data[-i,]
  }
}
# check the output
length(which(is.na(final_data$sample_id_abdomen))) # 9 (58-49) = 9
length(which(is.na(final_data$sample_id_head))) # 8 (57-49) = 8
# also tested a few ids in dups_table_df to see if occurred in duplicate still
# all looks good
length(unique(final_data$sample_id_mosquito)) # 1497 unique 
length(which(is.na(final_data$sample_id_mosquito) == T)) # 0 missing
count_table = table(final_data$sample_id_mosquito, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table

# code the K01 H00030, K01 A00030, K01 H00047, and K01 A00047 as positive
final_data$pf_pcr_infection_status_sample_level_a[which(final_data$sample_id_abdomen=="K01 A00030")] = "positive"
final_data$pf_pcr_infection_status_sample_level_a[which(final_data$sample_id_abdomen=="K01 A00047")] = "positive"
final_data$pf_pcr_infection_status_sample_level_h[which(final_data$sample_id_abdomen=="K01 H00030")] = "positive"
final_data$pf_pcr_infection_status_sample_level_h[which(final_data$sample_id_abdomen=="K01 H00047")] = "positive"

# check for duplicates one last time in the sample IDs for the heads and abdomens
# sample_id_head
length(unique(final_data$sample_id_head)) # 1490 unique 
length(which(is.na(final_data$sample_id_head) == T)) # 8 missing
count_table = table(final_data$sample_id_head, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table
# sample_id_abdomen
length(unique(final_data$sample_id_abdomen)) # 1489 unique 
length(which(is.na(final_data$sample_id_abdomen) == T)) # 9 missing
count_table = table(final_data$sample_id_abdomen, useNA = "always")
dups_table = count_table[which(count_table > 1)] # 0 duplicates
length(dups_table)
dups_table

# now clean up the remaining columns
colnames(final_data)
# HH_ID
table(final_data$HH_ID, useNA = "always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$HH_ID[i])){
    final_data$HH_ID[i] = str_split(final_data$sample_id_mosquito[i]," ")[[1]][1]
  } else {
    final_data$HH_ID[i] = final_data$HH_ID[i]
  }
}
table(final_data$HH_ID, useNA = "always")
# collection_date
length(which(is.na(final_data$collection_date))) # 3 collection dates missing which you'd expect because didn't merge into meta data
final_data$collection_date = as.character(final_data$collection_date)
for (i in 1:nrow(final_data)){
  if (is.na(final_data$collection_date[i])){
    final_data$collection_date[i] = "Not recorded"
  } else {
    final_data$collection_date[i] = final_data$collection_date[i]
  }
}
length(which(is.na(final_data$collection_date)))
length(which(final_data$collection_date=="Not recorded"))
# village
length(which(is.na(final_data$village)))
table(final_data$village, useNA = "always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$village[i])){
    if (str_detect(final_data$sample_id_mosquito[i],"K")){
      final_data$village[i] = "Kinesamo"
    }
    if (str_detect(final_data$sample_id_mosquito[i],"M")){
      final_data$village[i] = "Maruti"
    }
    if (str_detect(final_data$sample_id_mosquito[i],"S")){
      final_data$village[i] = "Sitabicha"
    }
  } else {
    final_data$village[i] = final_data$village[i]
  }
}
table(final_data$village, useNA = "always")
# sample_id_mosquito
length(which(is.na(final_data$sample_id_mosquito)))
# pf_infection_status_mosquito_level
table(final_data$pf_infection_status_mosquito_level, useNA = "always")
table(final_data$pf_pcr_infection_status_sample_level_a, useNA = "always")
table(final_data$pf_pcr_infection_status_sample_level_h, useNA = "always")
length(which(is.na(final_data$pf_infection_status_mosquito_level) & is.na(final_data$pf_pcr_infection_status_sample_level_a) & is.na(final_data$pf_pcr_infection_status_sample_level_h)))
final_data$pf_infection_status_mosquito_level = as.character(final_data$pf_infection_status_mosquito_level)
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pf_infection_status_mosquito_level[i])){
    if (final_data$pf_pcr_infection_status_sample_level_a[i] == "positive" & !(is.na(final_data$pf_pcr_infection_status_sample_level_a[i]))){
      final_data$pf_infection_status_mosquito_level[i] = "positive"
    } else if (final_data$pf_pcr_infection_status_sample_level_h[i] == 'positive' & !(is.na(final_data$pf_pcr_infection_status_sample_level_h[i]))){
      final_data$pf_infection_status_mosquito_level[i] = "positive"
    } else if (is.na(final_data$pf_pcr_infection_status_sample_level_a[i]) & is.na(final_data$pf_pcr_infection_status_sample_level_h[i])) {
      final_data$pf_infection_status_mosquito_level[i] = "qPCR not done"
    } else {
      final_data$pf_infection_status_mosquito_level[i] = "negative"
    }
  } else {
    final_data$pf_infection_status_mosquito_level[i] = final_data$pf_infection_status_mosquito_level[i]
  }
}
table(final_data$pf_infection_status_mosquito_level, useNA = "always")
# hb_status_mosquito_level
table(final_data$hb_status_mosquito_level, useNA = "always")
table(final_data$hb_status_sample_level_a, useNA = "always")
table(final_data$hb_status_sample_level_h, useNA = "always")
length(which(is.na(final_data$hb_status_mosquito_level) & is.na(final_data$hb_status_sample_level_a) & is.na(final_data$hb_status_sample_level_h)))
final_data$hb_status_mosquito_level = as.character(final_data$hb_status_mosquito_level)
for (i in 1:nrow(final_data)){
  if (is.na(final_data$hb_status_mosquito_level[i])){
    if (final_data$hb_status_sample_level_a[i] == "positive" & !(is.na(final_data$hb_status_sample_level_a[i]))){
      final_data$hb_status_mosquito_level[i] = "positive"
    } else if (final_data$hb_status_sample_level_h[i] == 'positive' & !(is.na(final_data$hb_status_sample_level_h[i]))){
      final_data$hb_status_mosquito_level[i] = "positive"
    } else if (is.na(final_data$hb_status_sample_level_a[i]) & is.na(final_data$hb_status_sample_level_h[i])) {
      final_data$hb_status_mosquito_level[i] = "qPCR not done"
    } else {
      final_data$hb_status_mosquito_level[i] = "negative"
    }
  } else {
    final_data$hb_status_mosquito_level[i] = final_data$hb_status_mosquito_level[i]
  }
}
table(final_data$hb_status_mosquito_level, useNA = "always")
# sample_id_head
length(which(is.na(final_data$sample_id_head)))
for (i in 1:nrow(final_data)){
  if (is.na(final_data$sample_id_head[i])){
    final_data$sample_id_head[i] = "Not processed"
  } else {
    final_data$sample_id_head[i] = final_data$sample_id_head[i]
  }
}
length(which(is.na(final_data$sample_id_head)))
length(which(final_data$sample_id_head=="Not processed"))
# gdna_extraction_date_h
length(which(is.na(final_data$gdna_extraction_date_h)))
final_data$gdna_extraction_date_h = as.character(final_data$gdna_extraction_date_h)
for (i in 1:nrow(final_data)){
  if (is.na(final_data$gdna_extraction_date_h[i])){
    final_data$gdna_extraction_date_h[i] = "Not recorded"
  } else {
    final_data$gdna_extraction_date_h[i] = final_data$gdna_extraction_date_h[i]
  }
}
length(which(is.na(final_data$gdna_extraction_date_h)))
length(which(final_data$gdna_extraction_date_h=="Not recorded"))
# gdna_plate_number_h
length(which(is.na(final_data$gdna_plate_number_h)))
final_data$gdna_plate_number_h = as.character(final_data$gdna_plate_number_h)
table(final_data$gdna_plate_number_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$gdna_plate_number_h[i])){
    final_data$gdna_plate_number_h[i] = "Not recorded"
  } else {
    final_data$gdna_plate_number_h[i] = final_data$gdna_plate_number_h[i]
  }
}
table(final_data$gdna_plate_number_h, useNA="always")
# gdna_column_number_h
table(final_data$gdna_column_number_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$gdna_column_number_h[i])){
    final_data$gdna_column_number_h[i] = "Not recorded"
  } else {
    final_data$gdna_column_number_h[i] = final_data$gdna_column_number_h[i]
  }
}
table(final_data$gdna_column_number_h, useNA="always")
# gdna_row_number_h
table(final_data$gdna_row_number_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$gdna_row_number_h[i])){
    final_data$gdna_row_number_h[i] = "Not recorded"
  } else {
    final_data$gdna_row_number_h[i] = final_data$gdna_row_number_h[i]
  }
}
table(final_data$gdna_row_number_h, useNA="always")
# HbtubCT1_h
table(final_data$HbtubCT1_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$HbtubCT1_h[i])){
    final_data$HbtubCT1_h[i] = "Undefined"
  } else {
    final_data$HbtubCT1_h[i] = final_data$HbtubCT1_h[i]
  }
}
table(final_data$HbtubCT1_h, useNA="always")
# HbtubCT2_h
table(final_data$HbtubCT2_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$HbtubCT2_h[i])){
    final_data$HbtubCT2_h[i] = "Undefined"
  } else {
    final_data$HbtubCT2_h[i] = final_data$HbtubCT2_h[i]
  }
}
table(final_data$HbtubCT2_h, useNA="always")
# pfr364CT1_h
table(final_data$pfr364CT1_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pfr364CT1_h[i])){
    final_data$pfr364CT1_h[i] = "Undefined"
  } else {
    final_data$pfr364CT1_h[i] = final_data$pfr364CT1_h[i]
  }
}
table(final_data$pfr364CT1_h, useNA="always")
# pfr364CT2_h
table(final_data$pfr364CT2_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pfr364CT2_h[i])){
    final_data$pfr364CT2_h[i] = "Undefined"
  } else {
    final_data$pfr364CT2_h[i] = final_data$pfr364CT2_h[i]
  }
}
table(final_data$pfr364CT2_h, useNA="always")
# pfr364Q1_h
table(final_data$pfr364Q1_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pfr364Q1_h[i])){
    final_data$pfr364Q1_h[i] = "Undefined"
  } else {
    final_data$pfr364Q1_h[i] = final_data$pfr364Q1_h[i]
  }
}
table(final_data$pfr364Q1_h, useNA="always")
# pfr364Q2_h
table(final_data$pfr364Q2_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pfr364Q2_h[i])){
    final_data$pfr364Q2_h[i] = "Undefined"
  } else {
    final_data$pfr364Q2_h[i] = final_data$pfr364Q2_h[i]
  }
}
table(final_data$pfr364Q2_h, useNA="always")
# pfr364Q_combined_h
table(final_data$pfr364Q_combined_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pfr364Q_combined_h[i])){
    final_data$pfr364Q_combined_h[i] = "Undefined"
  } else {
    final_data$pfr364Q_combined_h[i] = final_data$pfr364Q_combined_h[i]
  }
}
table(final_data$pfr364Q_combined_h, useNA="always")
# pf_pcr_infection_status_sample_level_h
final_data$pf_pcr_infection_status_sample_level_h = as.character(final_data$pf_pcr_infection_status_sample_level_h)
table(final_data$pf_pcr_infection_status_sample_level_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pf_pcr_infection_status_sample_level_h[i])){
    final_data$pf_pcr_infection_status_sample_level_h[i] = "qPCR not done"
  } else {
    final_data$pf_pcr_infection_status_sample_level_h[i] = final_data$pf_pcr_infection_status_sample_level_h[i]
  }
}
table(final_data$pf_pcr_infection_status_sample_level_h, useNA="always")
# hb_status_sample_level_h
final_data$hb_status_sample_level_h = as.character(final_data$hb_status_sample_level_h)
table(final_data$hb_status_sample_level_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$hb_status_sample_level_h[i])){
    final_data$hb_status_sample_level_h[i] = "qPCR not done"
  } else {
    final_data$hb_status_sample_level_h[i] = final_data$hb_status_sample_level_h[i]
  }
}
table(final_data$hb_status_sample_level_h, useNA="always")
# sent_for_sequencing_h
final_data$sent_for_sequencing_h = as.character(final_data$sent_for_sequencing_h)
table(final_data$sent_for_sequencing_h, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$sent_for_sequencing_h[i])){
    final_data$sent_for_sequencing_h[i] = "no"
  } else {
    final_data$sent_for_sequencing_h[i] = final_data$sent_for_sequencing_h[i]
  }
}
table(final_data$sent_for_sequencing_h, useNA="always")
# sample_id_abdomen
length(which(is.na(final_data$sample_id_abdomen)))
for (i in 1:nrow(final_data)){
  if (is.na(final_data$sample_id_abdomen[i])){
    final_data$sample_id_abdomen[i] = "Not processed"
  } else {
    final_data$sample_id_abdomen[i] = final_data$sample_id_abdomen[i]
  }
}
length(which(is.na(final_data$sample_id_abdomen)))
length(which(final_data$sample_id_abdomen=="Not processed"))
# gdna_extraction_date_a
length(which(is.na(final_data$gdna_extraction_date_a)))
final_data$gdna_extraction_date_a = as.character(final_data$gdna_extraction_date_a)
for (i in 1:nrow(final_data)){
  if (is.na(final_data$gdna_extraction_date_a[i])){
    final_data$gdna_extraction_date_a[i] = "Not recorded"
  } else {
    final_data$gdna_extraction_date_a[i] = final_data$gdna_extraction_date_a[i]
  }
}
length(which(is.na(final_data$gdna_extraction_date_a)))
length(which(final_data$gdna_extraction_date_a=="Not recorded"))
# gdna_plate_number_a
length(which(is.na(final_data$gdna_plate_number_a)))
final_data$gdna_plate_number_a = as.character(final_data$gdna_plate_number_a)
table(final_data$gdna_plate_number_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$gdna_plate_number_a[i])){
    final_data$gdna_plate_number_a[i] = "Not recorded"
  } else {
    final_data$gdna_plate_number_a[i] = final_data$gdna_plate_number_a[i]
  }
}
table(final_data$gdna_plate_number_a, useNA="always")
# gdna_column_number_a
table(final_data$gdna_column_number_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$gdna_column_number_a[i])){
    final_data$gdna_column_number_a[i] = "Not recorded"
  } else {
    final_data$gdna_column_number_a[i] = final_data$gdna_column_number_a[i]
  }
}
table(final_data$gdna_column_number_a, useNA="always")
# gdna_row_number_a
table(final_data$gdna_row_number_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$gdna_row_number_a[i])){
    final_data$gdna_row_number_a[i] = "Not recorded"
  } else {
    final_data$gdna_row_number_a[i] = final_data$gdna_row_number_a[i]
  }
}
table(final_data$gdna_row_number_a, useNA="always")
# HbtubCT1_a
table(final_data$HbtubCT1_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$HbtubCT1_a[i])){
    final_data$HbtubCT1_a[i] = "Undefined"
  } else {
    final_data$HbtubCT1_a[i] = final_data$HbtubCT1_a[i]
  }
}
table(final_data$HbtubCT1_a, useNA="always")
# HbtubCT2_a
table(final_data$HbtubCT2_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$HbtubCT2_a[i])){
    final_data$HbtubCT2_a[i] = "Undefined"
  } else {
    final_data$HbtubCT2_a[i] = final_data$HbtubCT2_a[i]
  }
}
table(final_data$HbtubCT2_a, useNA="always")
# pfr364CT1_a
table(final_data$pfr364CT1_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pfr364CT1_a[i])){
    final_data$pfr364CT1_a[i] = "Undefined"
  } else {
    final_data$pfr364CT1_a[i] = final_data$pfr364CT1_a[i]
  }
}
table(final_data$pfr364CT1_a, useNA="always")
# pfr364CT2_a
table(final_data$pfr364CT2_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pfr364CT2_a[i])){
    final_data$pfr364CT2_a[i] = "Undefined"
  } else {
    final_data$pfr364CT2_a[i] = final_data$pfr364CT2_a[i]
  }
}
table(final_data$pfr364CT2_a, useNA="always")
# pfr364Q1_a
table(final_data$pfr364Q1_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pfr364Q1_a[i])){
    final_data$pfr364Q1_a[i] = "Undefined"
  } else {
    final_data$pfr364Q1_a[i] = final_data$pfr364Q1_a[i]
  }
}
table(final_data$pfr364Q1_a, useNA="always")
# pfr364Q2_a
table(final_data$pfr364Q2_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pfr364Q2_a[i])){
    final_data$pfr364Q2_a[i] = "Undefined"
  } else {
    final_data$pfr364Q2_a[i] = final_data$pfr364Q2_a[i]
  }
}
table(final_data$pfr364Q2_a, useNA="always")
# pfr364Q_combined_a
table(final_data$pfr364Q_combined_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pfr364Q_combined_a[i])){
    final_data$pfr364Q_combined_a[i] = "Undefined"
  } else {
    final_data$pfr364Q_combined_a[i] = final_data$pfr364Q_combined_a[i]
  }
}
table(final_data$pfr364Q_combined_a, useNA="always")
# pf_pcr_infection_status_sample_level_a
final_data$pf_pcr_infection_status_sample_level_a = as.character(final_data$pf_pcr_infection_status_sample_level_a)
table(final_data$pf_pcr_infection_status_sample_level_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$pf_pcr_infection_status_sample_level_a[i])){
    final_data$pf_pcr_infection_status_sample_level_a[i] = "qPCR not done"
  } else {
    final_data$pf_pcr_infection_status_sample_level_a[i] = final_data$pf_pcr_infection_status_sample_level_a[i]
  }
}
table(final_data$pf_pcr_infection_status_sample_level_a, useNA="always")
# hb_status_sample_level_a
final_data$hb_status_sample_level_a = as.character(final_data$hb_status_sample_level_a)
table(final_data$hb_status_sample_level_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$hb_status_sample_level_a[i])){
    final_data$hb_status_sample_level_a[i] = "qPCR not done"
  } else {
    final_data$hb_status_sample_level_a[i] = final_data$hb_status_sample_level_a[i]
  }
}
table(final_data$hb_status_sample_level_a, useNA="always")
# sent_for_sequencing_a
final_data$sent_for_sequencing_a = as.character(final_data$sent_for_sequencing_a)
table(final_data$sent_for_sequencing_a, useNA="always")
for (i in 1:nrow(final_data)){
  if (is.na(final_data$sent_for_sequencing_a[i])){
    final_data$sent_for_sequencing_a[i] = "no"
  } else {
    final_data$sent_for_sequencing_a[i] = final_data$sent_for_sequencing_a[i]
  }
}
table(final_data$sent_for_sequencing_a, useNA="always")

# write out the data set
write_csv(final_data,"mozzie_phase_1_final_mosquito_inventory_7AUG2019_searchable.csv")
write_rds(final_data,"mozzie_phase_1_final_mosquito_inventory_7AUG2019_searchable.rds")

# created an Excel sheet version for the lab to use that's pared down some from the searchable version





