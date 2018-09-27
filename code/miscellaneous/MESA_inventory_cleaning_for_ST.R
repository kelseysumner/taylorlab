# ------------------------------------ #
#   MESA Inventory Cleaning for ST     #
#           August 28, 2018            #
#             K. Sumner                #
# ------------------------------------ #

# load in packages
library(readr) # for reading in csvs using read_csv (more efficient and better at keeping data format)
library(dplyr) # for left_join function

# read in the inventory 
inventory = read_csv("/Users/kelseysumner/Desktop/Mesa DBS and gDNA Master Inventory_2018 for WPO 28Aug2018.csv")

# create a function for something not being in the list
"%ni%" <- Negate("%in%")

# create new MESA ID column
mesa_id = rep(NA,nrow(inventory))
for (k in 1:nrow(inventory)){
  if (inventory$LabID[k] == "blank" | inventory$LabID[k] == "3D7 culture" | inventory$LabID[k] == "skipped" | inventory$LabID[k] == "Mesa_B" | inventory$LabID[k] == "Mesa_A" | is.na(inventory$LabID[k]) == T){
    mesa_id[k] = NA
  } else if ("_" %ni% strsplit(inventory$LabID[k],"")[[1]]) {
    mesa_id[k] = paste0("MPS",inventory$LabID[k])
  } else {
    part_mesa_id = strsplit(inventory$LabID[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      mesa_id[k] = paste0("MPS",inventory$LabID[k])
    }
    if (nchar(part_mesa_id) == 3){
      mesa_id[k] = paste0("MPS","0",inventory$LabID[k])
    }
    if (nchar(part_mesa_id) == 2){
      mesa_id[k] = paste0("MPS","00",inventory$LabID[k])
    }
    if (nchar(part_mesa_id) == 1){
      mesa_id[k] = paste0("MPS","000",inventory$LabID[k])
    }
  }
}
inventory$`MESA ID` <- mesa_id

# write out as a csv file
write_csv(inventory,"/Users/kelseysumner/Desktop/Mesa DBS and gDNA Master Inventory_2018 for WPO 28Aug2018_KMS.csv")



