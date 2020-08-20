# -------------------------------------- #
#     Example network analysis code      #
#           for Ryan Simmons             #
#                K. Sumner               #
#            August 20, 2020             #
# -------------------------------------- #


#### ------ load packages ------ ####
library(tidyverse) # for general data management
library(igraph) # for sna
library(tnet) # for sna



#### ------- make a dummy edgelist and convert to sociomatrix --------- ####

# create a data frame with three columns
# column 1: first haplotype
# column 2: second haplotype
# column 3: number of times the two haplotype occurred together (to use as the edgeweight)
dummy_df = data.frame(haplotype_1 = c("H1","H2","H3","H2","H4","H5","H6","H6"),haplotype_2 = c("H2","H3","H4","H1","H5","H3","H5","H4"),edgeweight = c(20,30,25,20,90,85,70,75))

# convert the edgelist to an undirected sociomatrix
dummy_df = as.matrix(dummy_df)
g=graph.edgelist(dummy_df[,1:2],directed=F)
E(g)$weight=as.numeric(dummy_df[,3]) # adds edgeweight from third column

# simplify the graph to have 1 edge between every two nodes
# this simplifies your edgelist so each pair only occurs once (so H1-H2 and H2-H1 doesn't occur)
# only do this step if you have pairs repeated
g=simplify(g,remove.multiple=T,edge.attr.comb="mean")
# also simplify the edgelist to have one edge between every two nodes
dummy_df = get.data.frame(g) # notice how H2-H1 has now been removed



#### ------- now run a community detection algorithm, calculate modularity, and pull out the nodes within modules -------- ####

# giving an example with the louvain community detection which is commonly used
# there are many different community detection algoirthms to use and some give different modularity values

# run the louvain community detection algorithm
optimal = cluster_louvain(g, weights = dummy_df[,3])

# isolate out just the community value - this is the module/cluster each node belongs to
membership(optimal)
# to interpret: H1, H2, and H3 are in module 1, H4, H5, and H6 are in module 2




