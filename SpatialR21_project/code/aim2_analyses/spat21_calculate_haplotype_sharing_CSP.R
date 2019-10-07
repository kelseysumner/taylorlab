# ----------------------------------------- #
#         Calculate Haplotype Sharing       #
#             Spatial R21 Data              #
#       All Samples (Mosquito and Human)    #
#               CSP target                  #
#           September 24, 2019              #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(igraph)
library(readr)


#### ------------------ CSP ---------------------- ####

# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
finalfoo_all <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# edit the data set to be in the correct format
finalfoo_all = as.matrix(finalfoo_all)
rownames(finalfoo_all) = finalfoo_all[,2]
colnames(finalfoo_all)
finalfoo_all=finalfoo_all[,-c(1,2,3,302,303,304)]
colnames(finalfoo_all)

# make finalfoo_all values numeric
finalfoo_test = apply(finalfoo_all, 2, as.numeric)
rownames(finalfoo_test) = rownames(finalfoo_all)
finalfoo_all = finalfoo_test

# calculate how many haplotypes are shared across samples for each sample
# create the data frame (double loop through everything in data set)
edgelists = c(0,0,0,0)
samplelist = c(rownames(finalfoo_all))
for (f in 1:nrow(finalfoo_all)){
  sid = c(rownames(finalfoo_all))
  for (g in 1:nrow(finalfoo_all)) {
    gid = c(rownames(finalfoo_all))
    hapsum = 0
    for (h in 1:ncol(finalfoo_all)){
      if (finalfoo_all[f,h] != 0 & finalfoo_all[g,h] != 0) {
        haps = 1
      } else {
        haps = 0
      }
      hapsum = haps + hapsum
    }
    pair1haps <- names(which(finalfoo_all[f,] > 0))
    pair2haps <- names(which(finalfoo_all[g,] > 0))
    combined_hap_vecs <- length(unique(c(pair1haps, pair2haps)))
    matrix_add <- c(sid[f],gid[g],hapsum,combined_hap_vecs)
    edgelists <- rbind(edgelists,matrix_add)
  }
}

# make the matrix a dataframe and write out as a csv
edgelist_df = data.frame(edgelists)

# delete the first row and add column names
edgelist_df <- edgelist_df[-1,]

# add column names
colnames(edgelist_df) = c("sample_1", "sample_2", "haplotypes_shared", "total_haplotypes")

# make haplotype columns numeric
edgelist_df$haplotypes_shared = as.numeric(as.character(edgelist_df$haplotypes_shared))
edgelist_df$total_haplotypes = as.numeric(as.character(edgelist_df$total_haplotypes))

# calculate the proportion of haplotypes shared for each pair and add that to the data set
prop_haps = edgelist_df$haplotypes_shared/edgelist_df$total_haplotypes
edgelist_df$prop_haplotype_sharing = prop_haps

# remove all the samples paired with themselves
indices = with(edgelist_df, which(as.character(sample_1) == as.character(sample_2)))
edgelist_df_clean = edgelist_df[-indices,]

# export the edgelist with all 5 columns before removing the repeated edges
write_csv(edgelist_df_clean,"CSP_haplotypes_edgelist_repeated_edges.csv")

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
write.csv(edgelist_df_final,"CSP_haplotypes_edgelist_simplified_number_haps_shared.csv")




