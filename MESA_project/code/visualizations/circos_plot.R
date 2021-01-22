# ----------------------------------------- #
#     Circos plot MESA Visualizations       # 
#              for ASTMH 2018               #
#             October 4, 2018               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra) 
library(ggthemes)
library(wesanderson)
library(migest)
library(circlize)
library(tidyr)



#### ------ Figure 7: Haplotype sharing across locations ------ ####

# circular migration plot representing haplotype sharing

# source
# Abel, G.J. (2016) Estimates of global bilateral migration glows by gender between 1960 and 2015. 
# Vienna Institute of Demography Working Papers 2/2016.

# read in the data set
df0 <- read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/ASTMH/Poster/Haplotype Sharing Output/CSP_haplotypes_edgelist_by_location.csv", stringsAsFactors=FALSE)
df1 <- read.csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/ASTMH/Poster/Haplotype Sharing Output/df1_by_location.csv", stringsAsFactors=FALSE)

# create the default chord diagram
chordDiagram(x = df0)

# set up plot parameters
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))
# increasing the gaps between sectors, start at 12 o'clock, ensure no gap between the chord and the sector at the begining
# subdue warning messages and have no margins in the plot

# create the default chord diagram
figure7=chordDiagram(x = df0, grid.col = df1$col, transparency = 0.01,
             order = df1$region)

save(filename="/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/ASTMH/Poster/Figures/figure7.png", plot=figure7, device="png",
       height=10.5, width=10.5, units="in", dpi=500)


