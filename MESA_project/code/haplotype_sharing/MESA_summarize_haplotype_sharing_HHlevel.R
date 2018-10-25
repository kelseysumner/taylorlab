# ----------------------------------------- #
#  Haplotype sharing at the household level #
#            October 18, 2018               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(igraph)


#### --------- calculate haplotype sharing at the household level ----------------- ####

# read in the merged data set
merged_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/clean_files_for_lab/mesa_merged_final.csv")

## CSP
# create a summary data frame of each of the households and the samples within them for the 514 1/1/18 sequenced samples
csp_df <- merged_data %>% 
  filter(CSP_number_of_haplotypes != "Removed" & !(is.na(CSP_number_of_haplotypes)) & !(is.na(studyid_case_control_new))) %>%
  group_by(studyid_case_control_new) %>%
  summarise(n=n())

# looks like there are too many people at the HH level so are changing to the look at the number of haplotypes
# that are shared at the "location" level
## CSP
# create a summary data frame of each of the village locations and the samples within them for the 514 1/1/18 sequenced samples
csp_df <- merged_data %>% 
  filter(CSP_number_of_haplotypes != "Removed" & !(is.na(CSP_number_of_haplotypes)) & !(is.na(labid_new)) & !(is.na(location))) %>%
  group_by(location, labid_new) %>%
  summarise(n=n())
# rename the lab id column
names(csp_df)[names(csp_df) == "labid_new"] <- "labid_old_labinventory"

# read in the CSP haplotype sample summary data set
csp_haplotypes = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/CSP_sample_summary.csv")

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

# merge these two data sets
csp_merge = left_join(csp_haplotypes, csp_df, by = "labid_old_labinventory")

# remove the missing values
csp_merge = csp_merge[which(is.na(csp_merge$location) == F),]

# created a new variable in merged IDs that is just the sample IDs Betsy gave the sequenced samples (S1, S2, etc.)
sids = rep(NA,nrow(csp_merge))
for (i in 1:nrow(csp_merge)){
  sids[i] = strsplit(csp_merge$lab_miseq_sample,"_")[[i]][1]
}
table(sids, useNA = "always")
csp_merge$sids = sids

# read in the CSP haplotype sequence table
csp_data = readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/MESA_CSP_haplotypes_final.rds")
# look at the data set
head(csp_data)
# make it a data frame
csp_data = data.frame(csp_data)
# note: this is the final cleaned CSP haplotype sequence table with the H67 column removed (that haplotype only found in controls)

# create a new column in the CSP haplotype sequence table that is a list of all the sample names
csp_data$sids = rownames(csp_data)

# merge the SIDs with the rownames of the CSP haplotype sequence table
haplotype_merge = left_join(csp_data, csp_merge, by = "sids")

# for each row that is similar, collapse the variable
haplotype_df <- haplotype_merge %>% 
  group_by(location) %>%
  summarise(H1=sum(H1), H1=sum(H1), H2=sum(H2), H3=sum(H3), H4=sum(H4), H5=sum(H5), H6=sum(H6)
            , H7=sum(H7), H8=sum(H8), H9=sum(H9), H10=sum(H10), H11=sum(H12), H12=sum(H12)
            , H13=sum(H13), H14=sum(H14), H15=sum(H15), H16=sum(H16), H17=sum(H17), H18=sum(H18)
            , H19=sum(H19), H20=sum(H20), H21=sum(H21), H22=sum(H22), H23=sum(H23), H24=sum(H24)
            , H25=sum(H25), H26=sum(H26), H27=sum(H27), H28=sum(H28), H29=sum(H29), H30=sum(H30)
            , H31=sum(H31), H32=sum(H32), H33=sum(H33), H34=sum(H34), H35=sum(H35), H36=sum(H36)
            , H37=sum(H37), H38=sum(H38), H39=sum(H39), H40=sum(H40), H41=sum(H41), H42=sum(H42)
            , H43=sum(H43), H44=sum(H44), H45=sum(H45), H46=sum(H46), H47=sum(H47), H48=sum(H48)
            , H49=sum(H49), H50=sum(H50), H51=sum(H51), H52=sum(H52), H53=sum(H53), H54=sum(H54)
            , H55=sum(H55), H56=sum(H56), H57=sum(H57), H58=sum(H58), H59=sum(H59), H60=sum(H60)
            , H61=sum(H61), H62=sum(H62), H63=sum(H63), H64=sum(H64), H65=sum(H65), H66=sum(H66)
            , H68=sum(H68), H69=sum(H69), H70=sum(H70), H71=sum(H71), H72=sum(H72), H73=sum(H73)
            , H74=sum(H74), H75=sum(H75), H76=sum(H76), H77=sum(H77), H78=sum(H78), H79=sum(H79)
            , H80=sum(H80), H81=sum(H81), H82=sum(H82), H83=sum(H83), H84=sum(H84), H85=sum(H85)
            , H86=sum(H86), H87=sum(H87), H88=sum(H88), H89=sum(H89))

# add rownames to the from haplotype_df
rownames(haplotype_df) = haplotype_df$location

# remove the location column from haplotype_df
haplotype_df$location <- NULL

# calculate how many haplotypes are shared across samples for each sample
# create the data frame (double loop through everything in data set)
edgelists = c(0,0,0)
samplelist = c(rownames(haplotype_df))
for (f in 1:nrow(haplotype_df)){
  sid = c(rownames(haplotype_df))
  for (g in 1:nrow(haplotype_df)) {
    gid = c(rownames(haplotype_df))
    hapsum = 0
    for (h in 1:ncol(haplotype_df)){
      if (haplotype_df[f,h] != 0 & haplotype_df[g,h] != 0) {
        haps = 1
      } else {
        haps = 0
      }
      hapsum = haps + hapsum
    }
    matrix_add <- c(sid[f],gid[g],hapsum)
    edgelists <- rbind(edgelists,matrix_add)
  }
}

# make the matrix a dataframe and write out as a csv
edgelist_df = data.frame(edgelists)

# delete the first row and add column names
edgelist_df <- edgelist_df[-1,]

# add column names
colnames(edgelist_df) = c("location_1", "location_2", "haplotypes_shared")

# make haplotype columns numeric
edgelist_df$haplotypes_shared = as.numeric(as.character(edgelist_df$haplotypes_shared))

# remove all the samples paired with themselves
indices = with(edgelist_df, which(location_1 == location_2))
edgelist_df_clean = edgelist_df[-indices,]

# convert edgelist to sociomatrix
edgelist=as.matrix(edgelist_df_clean)
g=graph.edgelist(edgelist[,1:2],directed=FALSE)

# add weighted edges to the haplotype sharing edgelist
# the weight is defined as the number of haplotypes shared for now
E(g)$weight=as.numeric(edgelist[,3])

# simplify the graph to have 1 edge between every two nodes
k = simplify(g, remove.multiple = TRUE, edge.attr.comb = "mean")
g = k

# change the simplified graph back to an edgelist
edgelist_df_final = get.data.frame(g)

# export the file as a csv
write.csv(edgelist_df_final,"CSP_haplotypes_edgelist_by_location.csv")









