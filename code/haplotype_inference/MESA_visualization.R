# ------------------------------------ #
#       MESA Haplotype Analysis        #
#            Making Visuals            #
#           August 30, 2018            #
#             K. Sumner                #
# ------------------------------------ #


# load in packages
library(readr) # for reading in csvs using read_csv (more efficient and better at keeping data format)
library(ggplot2) # for creating plots


#### --------------------------- AMA ---------------------------- ####

# read in the data set sequence data sets for now
sample_long_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_sample_summary.csv")
haplotype_long_data = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_sample_summary.csv")
# make a histogram of the number of haplotypes observed per person in a histogram


