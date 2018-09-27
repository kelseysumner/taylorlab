# ----------------------------------------- #
#         MESA Pairwise Comparisons         #
#             August 30, 2018               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(igraph)


#### ------------------ AMA ---------------------- ####

# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
finalfoo_all <- readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/MESA_AMA_haplotypes_final.rds")

# rename and resort the samples
# first take off the "S" and order
remove_s = strsplit(row.names(finalfoo_all),"")
minus_s = rep(NA,nrow(finalfoo_all))
for (ele in 1:nrow(finalfoo_all)){
  minus_s[ele] = paste(remove_s[[ele]][-1], collapse = "")
}
# now order the samples
row_order <- sort(as.numeric(minus_s))
row_order_s <- paste0("S",row_order)
finalfoo_all <- finalfoo_all[match(row_order_s, row.names(finalfoo_all)),]
# check the output
rownames(finalfoo_all)

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
indices = with(edgelist_df, which(sample_1 == sample_2))
edgelist_df_clean = edgelist_df[-indices,]
# check the coding
length(which(edgelist_df$prop_haplotype_sharing[indices] != 1))

# export the edgelist with all 5 columns before removing the repeated edges
write.csv(edgelist_df_clean,"AMA_haplotypes_edgelist_repeated_edges.csv")

# convert edgelist to sociomatrix
edgelist=as.matrix(edgelist_df_clean)
g=graph.edgelist(edgelist[,1:2],directed=FALSE)

# add weighted edges to the haplotype sharing edgelist
# the weight is defined as the prop_haplotype_sharing for now
E(g)$weight=as.numeric(edgelist[,5])

# simplify the graph to have 1 edge between every two nodes
k = simplify(g, remove.multiple = TRUE, edge.attr.comb = "mean")
g = k

# change the simplified graph back to an edgelist
edgelist_df_final = get.data.frame(g)

# export the file as a csv
write.csv(edgelist_df_final,"AMA_haplotypes_edgelist_simplified.csv")

# create a histogram of the haplotype proportion output for comparison
# look at distribution of haplotype sharing proportions in simplified edgelist
hist(edgelist_df_final$weight)
# look at distribution of number of haplotypes shared with repeated edges
hist(edgelist_df_clean$haplotypes_shared)

# look at those observations that had all haplotypes shared (haplotype sharing proportion == 1)
# look at this in the final, simplified edgelist (look at the edge weight which is proportion haplotype sharing)
indices_all = with(edgelist_df_final, which(weight == 1))
edgelist_all_haps_shared = edgelist_df_final[indices_all,]
# looks like 570 pairs out of 62835 pairs shared all haplotypes they possible could (haplotype sharing proportion == 1)
# 0.007479908

# look at those observations that had no haplotypes shared (haplotype sharing proportion == 0)
# look at this in the final, simplified edgelist like above (look at the edge weight which is proportion haplotype sharing)
indices_no = with(edgelist_df_final, which(weight == 0))
edgelist_no_haps_shared = edgelist_df_final[indices_no,]
# looks like 55,546 pairs out of 62835 pairs shared 0 haplotypes that they possible could (haplotype sharing proportion == 1)
# 0.8839978

# compare summaries of haplotype sharing across data sets (simplified and cleaned with repeated pairs)
summary(edgelist_df_final)
summary(edgelist_df_clean)

# look at the relation between the total number of haplotypes between a pair and the number of haplotypes shared
plot(edgelist_df_clean$total_haplotypes,edgelist_df_clean$haplotypes_shared)
cor(edgelist_df_clean$total_haplotypes,edgelist_df_clean$haplotypes_shared)
# correlation: 0.2070983

# look at the relation between the total number of haplotypes between a pair and the proportion of haplotypes shared
plot(edgelist_df_clean$total_haplotypes,edgelist_df_clean$prop_haplotype_sharing)
cor(edgelist_df_clean$total_haplotypes,edgelist_df_clean$prop_haplotype_sharing)
# correlation: -0.07710438

### create edgelist for number of haplotypes shared
# clear the working directory
# read back in the data frame with all columns
edgelist_df_clean_num = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_haplotypes_edgelist_repeated_edges.csv")

# convert edgelist to sociomatrix
edgelist=as.matrix(edgelist_df_clean_num)
g=graph.edgelist(edgelist[,1:2],directed=FALSE)

# add weighted edges to the haplotype sharing edgelist
# the weight is defined as the prop_haplotype_sharing for now
E(g)$weight=as.numeric(edgelist[,3])

# simplify the graph to have 1 edge between every two nodes
k = simplify(g, remove.multiple = TRUE, edge.attr.comb = "mean")
g = k

# change the simplified graph back to an edgelist
edgelist_df_final = get.data.frame(g)

# export the file as a csv
write.csv(edgelist_df_final,"AMA_haplotypes_edgelist_simplified_numerator.csv")

## create edgelist for total number of haplotypes between each pair
# clear the working directory
# read back in the data frame with all columns
edgelist_df_clean_den = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_haplotypes_edgelist_repeated_edges.csv")

# convert edgelist to sociomatrix
edgelist=as.matrix(edgelist_df_clean_den)
g=graph.edgelist(edgelist[,1:2],directed=FALSE)

# add weighted edges to the haplotype sharing edgelist
# the weight is defined as the prop_haplotype_sharing for now
E(g)$weight=as.numeric(edgelist[,4])

# simplify the graph to have 1 edge between every two nodes
k = simplify(g, remove.multiple = TRUE, edge.attr.comb = "mean")
g = k

# change the simplified graph back to an edgelist
edgelist_df_final = get.data.frame(g)

# export the file as a csv
write.csv(edgelist_df_final,"AMA_haplotypes_edgelist_simplified_denominator.csv")


#### ------------------ CSP ---------------------- ####

# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
finalfoo_all <- readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/MESA_CSP_haplotypes_final.rds")

# rename and resort the samples
# first take off the "S" and order
remove_s = strsplit(row.names(finalfoo_all),"")
minus_s = rep(NA,nrow(finalfoo_all))
for (ele in 1:nrow(finalfoo_all)){
  minus_s[ele] = paste(remove_s[[ele]][-1], collapse = "")
}
# now order the samples
row_order <- sort(as.numeric(minus_s))
row_order_s <- paste0("S",row_order)
finalfoo_all <- finalfoo_all[match(row_order_s, row.names(finalfoo_all)),]
# check the output
rownames(finalfoo_all)

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
indices = with(edgelist_df, which(sample_1 == sample_2))
edgelist_df_clean = edgelist_df[-indices,]
# check the coding
length(which(edgelist_df$prop_haplotype_sharing[indices] != 1))

# export the edgelist with all 5 columns before removing the repeated edges
write.csv(edgelist_df_clean,"CSP_haplotypes_edgelist_repeated_edges.csv")

# convert edgelist to sociomatrix
edgelist=as.matrix(edgelist_df_clean)
g=graph.edgelist(edgelist[,1:2],directed=FALSE)

# add weighted edges to the haplotype sharing edgelist
# the weight is defined as the prop_haplotype_sharing for now
E(g)$weight=as.numeric(edgelist[,5])

# simplify the graph to have 1 edge between every two nodes
k = simplify(g, remove.multiple = TRUE, edge.attr.comb = "mean")
g = k

# change the simplified graph back to an edgelist
edgelist_df_final = get.data.frame(g)

# export the file as a csv
write.csv(edgelist_df_final,"CSP_haplotypes_edgelist_simplified.csv")

# create a histogram of the haplotype proportion output for comparison
# look at distribution of haplotype sharing proportions in simplified edgelist
hist(edgelist_df_final$weight)
# look at distribution of number of haplotypes shared with repeated edges
hist(edgelist_df_clean$haplotypes_shared)

# look at those observations that had all haplotypes shared (haplotype sharing proportion == 1)
# look at this in the final, simplified edgelist (look at the edge weight which is proportion haplotype sharing)
indices_all = with(edgelist_df_final, which(weight == 1))
edgelist_all_haps_shared = edgelist_df_final[indices_all,]
# looks like 758 pairs out of 60726 pairs shared all haplotypes they possible could (haplotype sharing proportion == 1)
# 0.0124823

# look at those observations that had no haplotypes shared (haplotype sharing proportion == 0)
# look at this in the final, simplified edgelist like above (look at the edge weight which is proportion haplotype sharing)
indices_no = with(edgelist_df_final, which(weight == 0))
edgelist_no_haps_shared = edgelist_df_final[indices_no,]
# looks like 48,971 pairs out of 60726 pairs shared 0 haplotypes that they possible could (haplotype sharing proportion == 1)
# 0.8064256

# compare summaries of haplotype sharing across data sets (simplified and cleaned with repeated pairs)
summary(edgelist_df_final)
summary(edgelist_df_clean)

# look at the relation between the total number of haplotypes between a pair and the number of haplotypes shared
plot(edgelist_df_clean$total_haplotypes,edgelist_df_clean$haplotypes_shared)
cor(edgelist_df_clean$total_haplotypes,edgelist_df_clean$haplotypes_shared)
# 0.2906847

# look at the relation between the total number of haplotypes between a pair and the proportion of haplotypes shared
plot(edgelist_df_clean$total_haplotypes,edgelist_df_clean$prop_haplotype_sharing)
cor(edgelist_df_clean$total_haplotypes,edgelist_df_clean$prop_haplotype_sharing)
# -0.07609717

### create edgelist for number of haplotypes shared
# clear the working directory
# read back in the data frame with all columns
edgelist_df_clean_num = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/CSP_haplotypes_edgelist_repeated_edges.csv")

# convert edgelist to sociomatrix
edgelist=as.matrix(edgelist_df_clean_num)
g=graph.edgelist(edgelist[,1:2],directed=FALSE)

# add weighted edges to the haplotype sharing edgelist
# the weight is defined as the prop_haplotype_sharing for now
E(g)$weight=as.numeric(edgelist[,3])

# simplify the graph to have 1 edge between every two nodes
k = simplify(g, remove.multiple = TRUE, edge.attr.comb = "mean")
g = k

# change the simplified graph back to an edgelist
edgelist_df_final = get.data.frame(g)

# export the file as a csv
write.csv(edgelist_df_final,"CSP_haplotypes_edgelist_simplified_numerator.csv")

## create edgelist for total number of haplotypes between each pair
# clear the working directory
# read back in the data frame with all columns
edgelist_df_clean_den = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/CSP_haplotypes_edgelist_repeated_edges.csv")

# convert edgelist to sociomatrix
edgelist=as.matrix(edgelist_df_clean_den)
g=graph.edgelist(edgelist[,1:2],directed=FALSE)

# add weighted edges to the haplotype sharing edgelist
# the weight is defined as the prop_haplotype_sharing for now
E(g)$weight=as.numeric(edgelist[,4])

# simplify the graph to have 1 edge between every two nodes
k = simplify(g, remove.multiple = TRUE, edge.attr.comb = "mean")
g = k

# change the simplified graph back to an edgelist
edgelist_df_final = get.data.frame(g)

# export the file as a csv
write.csv(edgelist_df_final,"CSP_haplotypes_edgelist_simplified_denominator.csv")



#### ------------------ HistB ---------------------- ####

# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
finalfoo_all <- readRDS("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/MESA_HistB_haplotypes_final.rds")

# rename and resort the samples
# first take off the "S" and order
remove_s = strsplit(row.names(finalfoo_all),"")
minus_s = rep(NA,nrow(finalfoo_all))
for (ele in 1:nrow(finalfoo_all)){
  minus_s[ele] = paste(remove_s[[ele]][-1], collapse = "")
}
# now order the samples
row_order <- sort(as.numeric(minus_s))
row_order_s <- paste0("S",row_order)
finalfoo_all <- finalfoo_all[match(row_order_s, row.names(finalfoo_all)),]
# check the output
rownames(finalfoo_all)

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
indices = with(edgelist_df, which(sample_1 == sample_2))
edgelist_df_clean = edgelist_df[-indices,]
# check the coding
length(which(edgelist_df$prop_haplotype_sharing[indices] != 1))

# export the edgelist with all 5 columns before removing the repeated edges
write.csv(edgelist_df_clean,"HistB_haplotypes_edgelist_repeated_edges.csv")

# convert edgelist to sociomatrix
edgelist=as.matrix(edgelist_df_clean)
g=graph.edgelist(edgelist[,1:2],directed=FALSE)

# add weighted edges to the haplotype sharing edgelist
# the weight is defined as the prop_haplotype_sharing for now
E(g)$weight=as.numeric(edgelist[,5])

# simplify the graph to have 1 edge between every two nodes
k = simplify(g, remove.multiple = TRUE, edge.attr.comb = "mean")
g = k

# change the simplified graph back to an edgelist
edgelist_df_final = get.data.frame(g)

# export the file as a csv
write.csv(edgelist_df_final,"HistB_haplotypes_edgelist_simplified.csv")

# create a histogram of the haplotype proportion output for comparison
# look at distribution of haplotype sharing proportions in simplified edgelist
hist(edgelist_df_final$weight)
# look at distribution of number of haplotypes shared with repeated edges
hist(edgelist_df_clean$haplotypes_shared)

# look at those observations that had all haplotypes shared (haplotype sharing proportion == 1)
# look at this in the final, simplified edgelist (look at the edge weight which is proportion haplotype sharing)
indices_all = with(edgelist_df_final, which(weight == 1))
edgelist_all_haps_shared = edgelist_df_final[indices_all,]
# looks like 758 pairs out of 60726 pairs shared all haplotypes they possible could (haplotype sharing proportion == 1)
# 0.0124823

# look at those observations that had no haplotypes shared (haplotype sharing proportion == 0)
# look at this in the final, simplified edgelist like above (look at the edge weight which is proportion haplotype sharing)
indices_no = with(edgelist_df_final, which(weight == 0))
edgelist_no_haps_shared = edgelist_df_final[indices_no,]
# looks like 48,971 pairs out of 60726 pairs shared 0 haplotypes that they possible could (haplotype sharing proportion == 1)
# 0.8064256

# compare summaries of haplotype sharing across data sets (simplified and cleaned with repeated pairs)
summary(edgelist_df_final)
summary(edgelist_df_clean)

# look at the relation between the total number of haplotypes between a pair and the number of haplotypes shared
plot(edgelist_df_clean$total_haplotypes,edgelist_df_clean$haplotypes_shared)
cor(edgelist_df_clean$total_haplotypes,edgelist_df_clean$haplotypes_shared)
# 0.1687

# look at the relation between the total number of haplotypes between a pair and the proportion of haplotypes shared
plot(edgelist_df_clean$total_haplotypes,edgelist_df_clean$prop_haplotype_sharing)
cor(edgelist_df_clean$total_haplotypes,edgelist_df_clean$prop_haplotype_sharing)
# -0.3168

### create edgelist for number of haplotypes shared
# clear the working directory
# read back in the data frame with all columns
edgelist_df_clean_num = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/HistB_haplotypes_edgelist_repeated_edges.csv")

# convert edgelist to sociomatrix
edgelist=as.matrix(edgelist_df_clean_num)
g=graph.edgelist(edgelist[,1:2],directed=FALSE)

# add weighted edges to the haplotype sharing edgelist
# the weight is defined as the prop_haplotype_sharing for now
E(g)$weight=as.numeric(edgelist[,3])

# simplify the graph to have 1 edge between every two nodes
k = simplify(g, remove.multiple = TRUE, edge.attr.comb = "mean")
g = k

# change the simplified graph back to an edgelist
edgelist_df_final = get.data.frame(g)

# export the file as a csv
write.csv(edgelist_df_final,"HistB_haplotypes_edgelist_simplified_numerator.csv")

## create edgelist for total number of haplotypes between each pair
# clear the working directory
# read back in the data frame with all columns
edgelist_df_clean_den = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/HistB_haplotypes_edgelist_repeated_edges.csv")

# convert edgelist to sociomatrix
edgelist=as.matrix(edgelist_df_clean_den)
g=graph.edgelist(edgelist[,1:2],directed=FALSE)

# add weighted edges to the haplotype sharing edgelist
# the weight is defined as the prop_haplotype_sharing for now
E(g)$weight=as.numeric(edgelist[,4])

# simplify the graph to have 1 edge between every two nodes
k = simplify(g, remove.multiple = TRUE, edge.attr.comb = "mean")
g = k

# change the simplified graph back to an edgelist
edgelist_df_final = get.data.frame(g)

# export the file as a csv
write.csv(edgelist_df_final,"HistB_haplotypes_edgelist_simplified_denominator.csv")


#### -------- For each target, merge edgelists ---------- ####

## read in all the data sets

# AMA
# read in the data sets
AMA_edgelist_df_clean_prop = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_haplotypes_edgelist_simplified.csv")
AMA_edgelist_df_clean_num = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_haplotypes_edgelist_simplified_numerator.csv")
AMA_edgelist_df_clean_den = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_haplotypes_edgelist_simplified_denominator.csv")
# rename the weight columns for each data set
names(AMA_edgelist_df_clean_prop)[names(AMA_edgelist_df_clean_prop) == 'weight'] <- 'AMA_shared_prop'
names(AMA_edgelist_df_clean_num)[names(AMA_edgelist_df_clean_num) == 'weight'] <- 'AMA_shared_num'
names(AMA_edgelist_df_clean_den)[names(AMA_edgelist_df_clean_den) == 'weight'] <- 'AMA_shared_den'

# CSP
# read in the data sets
CSP_edgelist_df_clean_prop = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/CSP_haplotypes_edgelist_simplified.csv")
CSP_edgelist_df_clean_num = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/CSP_haplotypes_edgelist_simplified_numerator.csv")
CSP_edgelist_df_clean_den = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/CSP_haplotypes_edgelist_simplified_denominator.csv")
# rename the weight columns for each data set
names(CSP_edgelist_df_clean_prop)[names(CSP_edgelist_df_clean_prop) == 'weight'] <- 'CSP_shared_prop'
names(CSP_edgelist_df_clean_num)[names(CSP_edgelist_df_clean_num) == 'weight'] <- 'CSP_shared_num'
names(CSP_edgelist_df_clean_den)[names(CSP_edgelist_df_clean_den) == 'weight'] <- 'CSP_shared_den'

# HistB
# read in the data sets
HistB_edgelist_df_clean_prop = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/HistB_haplotypes_edgelist_simplified.csv")
HistB_edgelist_df_clean_num = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/HistB_haplotypes_edgelist_simplified_numerator.csv")
HistB_edgelist_df_clean_den = read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/HistB_haplotypes_edgelist_simplified_denominator.csv")
# rename the weight columns for each data set
names(HistB_edgelist_df_clean_prop)[names(HistB_edgelist_df_clean_prop) == 'weight'] <- 'HistB_shared_prop'
names(HistB_edgelist_df_clean_num)[names(HistB_edgelist_df_clean_num) == 'weight'] <- 'HistB_shared_num'
names(HistB_edgelist_df_clean_den)[names(HistB_edgelist_df_clean_den) == 'weight'] <- 'HistB_shared_den'


## join all the data sets one at a time matching the from and to columns

# load the dplyr library
library(dplyr)

# first use left_join to merge all the AMA edgelists
AMA_join1 = left_join(AMA_edgelist_df_clean_prop,AMA_edgelist_df_clean_num,by=c("from","to"))
AMA_join2 = left_join(AMA_join1,AMA_edgelist_df_clean_den,by=c("from","to"))
# check the AMA joins
summary(AMA_join2)
summary(AMA_edgelist_df_clean_den)
summary(AMA_edgelist_df_clean_num)
summary(AMA_edgelist_df_clean_prop)

# then merge all the CSP edgelists
CSP_join1 = left_join(CSP_edgelist_df_clean_prop,CSP_edgelist_df_clean_num,by=c("from","to"))
CSP_join2 = left_join(CSP_join1,CSP_edgelist_df_clean_den,by=c("from","to"))
# check the CSP joins
summary(CSP_join2)
summary(CSP_edgelist_df_clean_den)
summary(CSP_edgelist_df_clean_num)
summary(CSP_edgelist_df_clean_prop)

# then merge all the HistB edgelists
HistB_join1 = left_join(HistB_edgelist_df_clean_prop,HistB_edgelist_df_clean_num,by=c("from","to"))
HistB_join2 = left_join(HistB_join1,HistB_edgelist_df_clean_den,by=c("from","to"))
# check the HistB joins
summary(HistB_join2)
summary(HistB_edgelist_df_clean_den)
summary(HistB_edgelist_df_clean_num)
summary(HistB_edgelist_df_clean_prop)

# now merge the three joined data sets together
alljoin1 = full_join(AMA_join2,CSP_join2,by=c("from","to"))
alljoin2 = full_join(alljoin1,HistB_join2,by=c("from","to"))
# check the merge
summary(alljoin2)
summary(AMA_join2)
summary(CSP_join2)
summary(HistB_join2)

# rename the from and to columns
names(alljoin2)[names(alljoin2) == 'from'] <- 'Sample1'
names(alljoin2)[names(alljoin2) == 'to'] <- 'Sample2'

# export the data set
write.csv(alljoin2,"MESA_alltargets_simplified_edgelist.csv")


#### -------- calculate MOI correlations between targets --------- ####

# note: correlation between haplotype sharing proportions is the "MOI correlation" being compared

# correlation between AMA and CSP
cor.test(alljoin2$AMA_shared_prop,alljoin2$CSP_shared_prop)

# correlation between AMA and HistB
cor.test(alljoin2$AMA_shared_prop,alljoin2$HistB_shared_prop)

# correlation between CSP and HistB
cor.test(alljoin2$CSP_shared_prop,alljoin2$HistB_shared_prop)




