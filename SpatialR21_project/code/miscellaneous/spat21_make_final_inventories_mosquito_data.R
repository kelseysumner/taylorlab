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
final_data = left_join(abdomen_inventory,anoph_data,by="sample_id_abdomen")
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
              "HbtubCT1_h","HbtubCT2_h","pfr364Q1_h","pfr364Q2_h","pfr364Q1_h","pfr364Q2_h","pfr364Q_combined_h","pf_pcr_infection_status_sample_level_h","hb_status_sample_level_h",
              "sample_id_abdomen","gdna_extraction_date_a","gdna_plate_number_a","gdna_column_number_a","gdna_row_number_a",
              "HbtubCT1_a","HbtubCT2_a","pfr364Q1_a","pfr364Q2_a","pfr364Q1_a","pfr364Q2_a","pfr364Q_combined_a","pf_pcr_infection_status_sample_level_a","hb_status_sample_level_a")
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

# now merge in the sequencing inventory with the rest of the mosquito data








