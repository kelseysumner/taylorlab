# ------------------------------------ #
#       MESA Clean Meta Data Set       #
#         September 27, 2018           #
#             K. Sumner                #
# ------------------------------------ #




#### --------------- CLEAN META DATA SET ------------------- ####

# read in the data set
meta_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Meta Data/MESA Full_dataset_all_members_v12_deidentified.csv")

# look at the data summary
dim(meta_data)
colnames(meta_data)
summary(meta_data)
