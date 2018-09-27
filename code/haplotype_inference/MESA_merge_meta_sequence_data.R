# ------------------------------------ #
#       MESA Haplotype Analysis        #
#     Merge Meta and Sequence Data     #
#           August 30, 2018            #
#             K. Sumner                #
# ------------------------------------ #

# load in packages
library(readr) # for reading in csvs using read_csv (more efficient and better at keeping data format)
library(dplyr) # for left_join function


#### --------------- CLEAN META DATA SET ------------------- ####

# read in the data set
meta_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/MESA Full_dataset_all_members_v12_deidentified.csv")

# look at the data summary
dim(meta_data)
colnames(meta_data)
summary(meta_data)


#### ------------------------ AMA --------------------------- ####

# read in the data sets
sequence_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_sample_summary.csv")

# now look at the 

