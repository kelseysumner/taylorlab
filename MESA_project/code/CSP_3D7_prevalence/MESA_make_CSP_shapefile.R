# ----------------------------------------- #
#            Make MESA Shapefile            #
#              for ASTMH 2018               #
#             October 17, 2018              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(sp)
library(readr)
library(GISTools)
library(rgdal)
library(dplyr)


#### --- create a smaller data frame of each household and prev 3D7 CSP haplotype of all the CSP haplotypes ---- ####

# read in the merged data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")

# read in the CSP haplotype sequence table
csp_data = readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/MESA_CSP_haplotypes_final.rds")
# look at the data set
head(csp_data)
# note: this is the final cleaned CSP haplotype sequence table with the H67 column removed (that haplotype only found in controls)

# read in the CSP haplotype sample summary data set
csp_haplotypes = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/CSP_sample_summary.csv")

# make a new variable that pulls out each sample and determines if 3D7 CSP haplotype is in that sample or not
# sequence 3 (haplotype 3) appears to be the haplotype with a perfect sequence alignment match to the 3D7 CSP haplotype
csp_present = rep(NA,nrow(csp_data))
for (i in 1:nrow(csp_data)){
  if (csp_data[i,c("H3")] > 0){
    csp_present[i] = 1
  } else {
    csp_present[i] = 0
  }
}
table(csp_present,useNA = "always")
# 1 = yes, csp present   0 = no, csp not present

# make a data frame that is just the sample names and the csp_present variable
csp_present_df = data.frame(rownames(csp_data), csp_present)

# subset the csp_haplotypes data set to just the variables of interest
csp_haplotypes = csp_haplotypes[,c("lab_miseq_sample","lab_mesa_id","number_of_haplotypes")]

# create a function for something not being in the list
"%ni%" <- Negate("%in%")

## clean the csp_haplotypes data set
# look at the data set
table(csp_haplotypes$lab_mesa_id, useNA = "always")
# recode the "none1" and "none2" labids to NA
csp_haplotypes$lab_mesa_id[csp_haplotypes$lab_mesa_id == "none1"] = NA
csp_haplotypes$lab_mesa_id[csp_haplotypes$lab_mesa_id == "none2"] = NA
# check for "-" in labids
length(which(is.na(csp_haplotypes$lab_mesa_id) == T))
mesaid = rep(NA,nrow(csp_haplotypes))
for (m in 1:nrow(csp_haplotypes)){
  if ("-" %in% strsplit(csp_haplotypes$lab_mesa_id[m],"")[[1]]){
    mesaid[m] = m
  } else {
    mesaid[m] = NA
  }
}
length(na.omit(mesaid))
# now clean the labid variable and rename it to labid_old_labinventory
cleanlab_id = rep(NA,nrow(csp_haplotypes))
for (k in 1:nrow(csp_haplotypes)){
  if (is.na(csp_haplotypes$lab_mesa_id[k]) == T){
    cleanlab_id[k] = NA
  } else if ("_" %ni% strsplit(csp_haplotypes$lab_mesa_id[k],"")[[1]]) {
    if (nchar(csp_haplotypes$lab_mesa_id[k]) == 4){
      cleanlab_id[k] = paste0(csp_haplotypes$lab_mesa_id[k])
    }
    if (nchar(csp_haplotypes$lab_mesa_id[k]) == 3){
      cleanlab_id[k] = paste0("0",csp_haplotypes$lab_mesa_id[k])
    }
    if (nchar(csp_haplotypes$lab_mesa_id[k]) == 2){
      cleanlab_id[k] = paste0("00",csp_haplotypes$lab_mesa_id[k])
    }
    if (nchar(csp_haplotypes$lab_mesa_id[k]) == 1){
      cleanlab_id[k] = paste0("000",csp_haplotypes$lab_mesa_id[k])
    }
  } else {
    part_mesa_id = strsplit(csp_haplotypes$lab_mesa_id[k],"_")[[1]][1]
    if (nchar(part_mesa_id) == 4){
      cleanlab_id[k] = paste0(csp_haplotypes$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 3){
      cleanlab_id[k] = paste0("0",csp_haplotypes$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 2){
      cleanlab_id[k] = paste0("00",csp_haplotypes$lab_mesa_id[k])
    }
    if (nchar(part_mesa_id) == 1){
      cleanlab_id[k] = paste0("000",csp_haplotypes$lab_mesa_id[k])
    }
  }
}
csp_haplotypes$labid_old_labinventory = toupper(cleanlab_id)
# change controls to NA
csp_haplotypes$labid_old_labinventory[csp_haplotypes$labid_old_labinventory == "03D7"] = NA
# should now have 5 NAs
length(which(is.na(csp_haplotypes$labid_old_labinventory) == T)) # 5 NAs so looks good
# remove lab_mesa_id
csp_haplotypes$lab_mesa_id <- NULL

# now merge the csp_haplotypes data set with the subset version of the merged data 
# that has the labid_old_labinventory and studyid_case_control_new columns
merged_data = merged_data[,c("labid_old_labinventory","studyid_case_control_new")]

# merge the merged_data subset and csp_haplotypes_subset 
merged_ids = left_join(csp_haplotypes, merged_data, by = "labid_old_labinventory")

# created a new variable in merged IDs that is just the sample IDs Betsy gave the sequenced samples (S1, S2, etc.)
sids = rep(NA,nrow(merged_ids))
for (i in 1:nrow(merged_ids)){
  sids[i] = strsplit(merged_ids$lab_miseq_sample,"_")[[i]][1]
}
table(sids, useNA = "always")
merged_ids$sids = sids

# rename the sids columns for csp_present_df
names(csp_present_df)[names(csp_present_df) == 'rownames.csp_data.'] <- 'sids'

# merge this merged_ids data set with the csp_present_df with the presence/absence of the 3D7 CSP haplotype
merged_final = left_join(csp_present_df, merged_ids, by = "sids") 
merged_final$studyid_case_control_new = as.factor(merged_final$studyid_case_control_new)
merged_final$csp_present = as.numeric(merged_final$csp_present)
merged_final$number_of_haplotypes = as.numeric(merged_final$number_of_haplotypes)

# summarize data by household
csp_prev_df <- merged_final %>% 
  filter(!(is.na(studyid_case_control_new)), !(is.na(sids)), !(is.na(csp_present)), !(is.na(number_of_haplotypes))) %>%
  group_by(studyid_case_control_new) %>%
  summarise(n=n(), csp_num=sum(csp_present, na.rm=T), csp_den=sum(number_of_haplotypes, na.rm=T)) %>%
  mutate(csp_HH_prev = csp_num/csp_den)

# read back in the merged data set and immediately subset to lat/long/mesa HH ID
merged_data_2 = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")
merged_subset = merged_data_2[,c("gps_coordinates_latitude","gps_coordinates_longitude","studyid_case_control_new")]

# pull out the column of the households and match with latitude and longitude coordinates
hh_ids = data.frame(csp_prev_df$studyid_case_control_new)
names(hh_ids)[names(hh_ids) == 'csp_prev_df.studyid_case_control_new'] <- 'studyid_case_control_new'

# merge the household data sets
hh_merge = left_join(hh_ids, merged_subset, by = "studyid_case_control_new")
unique_hh_merge = unique(hh_merge)
# look at number of unique hh ids
length(unique(unique_hh_merge$studyid_case_control_new)) # 217 so merged correctly
# remove the rows with missing gps coordinates
unique_hh_merge = unique_hh_merge[-which(is.na(unique_hh_merge$gps_coordinates_latitude) | is.na(unique_hh_merge$gps_coordinates_longitude)),]
# look at number of unique hh ids
length(unique(unique_hh_merge$studyid_case_control_new)) # 216 now so one of the hh ids didn't have lat/long coordinates
# still some houses with multiple versions of latitude/longitude coordinates 
# for now, will just use the first gps coordinates listed - will ask Wendy at the conference which to use
# pull out the duplicate hh ids
dup_hh_ids = data.frame(table(unique_hh_merge$studyid_case_control_new, useNA = "always"))
dup_hh_ids = dup_hh_ids[which(dup_hh_ids$Freq>1),]
# add a new variable that is the unique row number
unique_hh_merge$rownum = c(seq(1:nrow(unique_hh_merge)))
# pull out the rows that you will keep for now so only have one set of lat/long coordinates
unique_hh_merge = unique_hh_merge[-c(121,205,209,211,213,215,217,219,221,223,225,227),]
# only have 216 unique IDs now which is correct

# now merge in the unique_hh_merge with the csp_prev_df
join_for_spatial_work = left_join(csp_prev_df, unique_hh_merge, by = "studyid_case_control_new")
# remove the row with missing lat/long coordinates and the rownum column
join_for_spatial_work$rownum = NULL
join_for_spatial_work = join_for_spatial_work[-which(is.na(join_for_spatial_work$gps_coordinates_latitude)),]


#### ----- make the shapefile from the mesa data ----- ####

# what this is doing: 
# reading in the MESA data and changing the latitude and longitude coordinates to x and y coordinates
# export the MESA data set and variables of interest as a shapefile (after creating a spatial points data frame)

## export a shape file of the spatial points data frame
# first convert the data frame to a spatial points data frame
# now convert the MESA data to a spatial points data frame
class(join_for_spatial_work)
head(join_for_spatial_work)
coordinates(join_for_spatial_work) <- c(7,6) # this pulls out the lat/long coordinates and creates a spatial points data frame
class(join_for_spatial_work) 

# now export the spatial points data frame as a shapefile
# add proj4 string
proj4string(join_for_spatial_work) <- CRS("+init=epsg:4326") # WGS 1984 projection
writeOGR(join_for_spatial_work, dsn=".", layer="mesa_csp_shapefiles", driver="ESRI Shapefile") # this is in geographical projection


