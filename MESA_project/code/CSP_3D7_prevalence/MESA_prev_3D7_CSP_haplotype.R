# ----------------------------------------- #
#               MESA Study                  #
#        Prevalence 3D7 CSP Haplotype       #
#           September 27, 2018              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
# none loaded at the moment


#### -------- read in the data sets ----------- ####
# read in the CSP haplotype sequence table
csp_data = readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/MESA_CSP_haplotypes_final.rds")
# look at the data set
head(csp_data)
# note: this is the final cleaned CSP haplotype sequence table with the H67 column removed (that haplotype only found in controls)


#### --- calculate the prevalence of the 3D7 CSP haplotype --- ####
# sequence 3 (haplotype 3) appears to be the haplotype with a perfect sequence alignment match to the 3D7 CSP haplotype
# calculate the total number of samples with haplotype 3
numerator = length(which(csp_data[,c("H3")] > 0)) # 51
# calculate the total number of samples with haplotypes
denominator = nrow(csp_data) # 349
# calculate the prevalence of the 3D7 CSP haplotype (haplotype 3)
prevalence = numerator/denominator # 0.146







