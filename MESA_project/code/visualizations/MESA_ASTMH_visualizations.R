# ----------------------------------------- #
#            MESA Visualizations            #
#              for ASTMH 2018               #
#             Ocrober 4, 2018               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra) 
library(ggthemes)
library(wesanderson)


#### ----- figure 1: histogram of MOI across 3 targets ----- ####

## AMA
# read in the clean haplotype table
ama_haps = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/AMA_sample_summary.csv")
# create a summarized data frame of the number of people with each MOI
ama_moi_df <- ama_haps %>% 
  filter(number_of_haplotypes != "Removed") %>%
  group_by(number_of_haplotypes) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
ama_moi_df$number_of_haplotypes = as.numeric(ama_moi_df$number_of_haplotypes)

## CSP
# read in the clean MOI data set
csp_haps = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/CSP_sample_summary.csv")
# create a summarized data frame of the number of people with each MOI
csp_moi_df <- csp_haps %>% 
  filter(number_of_haplotypes != "Removed") %>%
  group_by(number_of_haplotypes) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
csp_moi_df$number_of_haplotypes = as.numeric(csp_moi_df$number_of_haplotypes)

## HistB
# read in the clean MOI data set
histb_haps = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/HistB_sample_summary.csv")
# create a summarized data frame of the number of people with each MOI
histb_moi_df <- histb_haps %>% 
  filter(number_of_haplotypes != "Removed") %>%
  group_by(number_of_haplotypes) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
histb_moi_df$number_of_haplotypes = as.numeric(histb_moi_df$number_of_haplotypes)

# plot each of the target data frames
## AMA
plot_ama <- ggplot() +
  geom_bar(data=ama_moi_df, aes(x=number_of_haplotypes, y=n), alpha=0.8, stat = "identity", fill = "#fcae91") + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,15)) +
  scale_y_continuous(limits=c(0,200)) +
  labs(x="Multiplicity of infection", y="Number of participants", title= "AMA target") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

## CSP
plot_csp <- ggplot() +
  geom_bar(data=csp_moi_df, aes(x=number_of_haplotypes, y=n), alpha=0.8, stat = "identity", fill = "#fb6a4a") + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,15)) +
  scale_y_continuous(limits=c(0,200)) +
  labs(x="Multiplicity of infection", y="", title= "CSP target")  +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

## HistB
plot_histb <- ggplot() +
  geom_bar(data=histb_moi_df, aes(x=number_of_haplotypes, y=n), alpha=0.8, stat = "identity", fill = "#33a02c") + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14), limits=c(0,15)) +
  scale_y_continuous(limits=c(0,200)) +
  labs(x="Multiplicity of infection", y="", title= "HistB target")  +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
                                  
# arrange all of the plots to be in 1 panel horizontally next to each other
# leave out HistB for now because results looks funky
gridExtra::grid.arrange(plot_ama, plot_csp, ncol=2)


#### ----- figure 2: histograms of haplotype sharing (number of people infected with each haplotype) ----- ####
## AMA
# read in the clean table
ama_fig2 = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/AMA_haplotypes/AMA/23AUG2018 AMA MESA Update/cut_zeros_haplotype_num_summary_censored.csv")

## CSP
# read in the clean table
csp_fig2 = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/CSP_haplotypes/23AUG2018 CSP MESA Update/cut_zeros_haplotype_num_summary_censored.csv")

## HistB
# read in the clean table
histb_fig2 = read_csv("/Users/kelseysumner/Desktop/Meshnick Lab/Steve Taylor's Lab/Webuye MESA Sequence Data/Mapped Cut Reads/HistB_haplotypes/23AUG2018 HistB MESA Update/cut_zeros_haplotype_num_summary_censored.csv")

# plot each of the target data frames
## AMA
plot_ama <- ggplot() +
  geom_bar(data=ama_fig2, aes(x=haplotype_ID, y=haplotypes_across_samples), alpha=0.9, stat = "identity", fill = "#fcae91", width = 0.8) + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120)) +
  labs(x="Haplotype ID", y="Number of participants", title= "AMA target") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

## CSP
plot_csp <- ggplot() +
  geom_bar(data=csp_fig2, aes(x=haplotype_ID, y=haplotypes_across_samples), alpha=0.9, stat = "identity", fill = "#fb6a4a", width = 0.8) + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(limits = c(0,120), breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120)) +
  labs(x="Haplotype ID", y="Number of participants", title= "CSP target")  +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

## HistB
plot_histb <- ggplot() +
  geom_bar(data=histb_fig2, aes(x=haplotype_ID, y=haplotypes_across_samples), alpha=0.9, stat = "identity", fill = "#33a02c") + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  labs(x="Haplotype ID", y="", title= "HistB target")  +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) 

# arrange all of the plots to be in 1 panel horizontally next to each other
# leave out HistB for now because don't trust results
gridExtra::grid.arrange(plot_ama, plot_csp, nrow=2)




