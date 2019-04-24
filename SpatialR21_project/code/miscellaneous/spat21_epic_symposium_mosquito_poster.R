# ----------------------------------------- #
#       EPIC Symposium at Duke Poster       #
#               Mosquito Data               #
#              April 22, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #


#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(tableone)
library(MHCtools)
library(dada2)
library(ggplot2)
library(stringr)
library(wesanderson)
library(gridExtra)
library(ggpubr)
library(PairedData)



#### --------- read in the anopheles demographic data set -------- ####

# read in the mosquito demographic data (merged anpopheles mosquito data set)
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")

# export the ama_merge_data
ama_merge_data = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/AMA/AMA_spat21_mosquito_haplotypes_censored_final.csv")

# export the csp_merge_data
csp_merge_data = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/CSP/CSP_spat21_mosquito_haplotypes_censored_final.csv")

# read in the ama sample summary
ama_sample_summary = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/AMA/AMA_sample_summary.csv")

# read in the csp sample summary
csp_sample_summary = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/CSP/CSP_sample_summary.csv")

# read in the ama haplotype summary
ama_haplotype_summary = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/AMA/AMA_haplotype_num_summary.csv")

# read in the csp haplotype summary
csp_haplotype_summary = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/sequence_results/haplotype_results/haplotype_output/CSP/CSP_haplotype_num_summary.csv")



#### -------- create a new column to indicate the mosquito part in the merged data sets ------------ ####

# loop through the merged data for pfama1 and indicate whether mosquito part is an abdomen or head
mosquito_part_type = rep(NA,nrow(ama_merge_data))
for (i in 1:nrow(ama_merge_data)){
  if (str_detect(ama_merge_data$sample_id[i],"A")) {
    mosquito_part_type[i] = "Abdomen"
  }
  if (str_detect(ama_merge_data$sample_id[i],"H")) {
    mosquito_part_type[i] = "Head"
  }
}
ama_merge_data$mosquito_part_type = mosquito_part_type
table(ama_merge_data$mosquito_part_type, useNA = "always")
check_data = ama_merge_data[,c(1,2,478)]

# loop through the merged data for pfcsp and indicate whether mosquito part is an abdomen or head
mosquito_part_type = rep(NA,nrow(csp_merge_data))
for (i in 1:nrow(csp_merge_data)){
  if (str_detect(csp_merge_data$sample_id[i],"A")) {
    mosquito_part_type[i] = "Abdomen"
  }
  if (str_detect(csp_merge_data$sample_id[i],"H")) {
    mosquito_part_type[i] = "Head"
  }
}
csp_merge_data$mosquito_part_type = mosquito_part_type
table(csp_merge_data$mosquito_part_type, useNA = "always")
check_data = csp_merge_data[,c(1,2,276)]

# merge in the sample summaries with the ama data
ama_sample_summary = rename(ama_sample_summary,"MiSeq.ID"="sample_names")
ama_merge_data = left_join(ama_merge_data,ama_sample_summary,by="MiSeq.ID")

# merge in the sample summaries with the csp data
csp_sample_summary = rename(csp_sample_summary,"MiSeq.ID"="sample_names")
csp_merge_data = left_join(csp_merge_data,csp_sample_summary,by="MiSeq.ID")

# create separate data sets for the mosquito heads and abdomens for ama
ama_merge_data_heads = ama_merge_data[which(ama_merge_data$mosquito_part_type == "Head"),]
ama_merge_data_abdomens = ama_merge_data[which(ama_merge_data$mosquito_part_type == "Abdomen"),]

# create separate data sets for the mosquito heads and abdomens for csp
csp_merge_data_heads = csp_merge_data[which(csp_merge_data$mosquito_part_type == "Head"),]
csp_merge_data_abdomens = csp_merge_data[which(csp_merge_data$mosquito_part_type == "Abdomen"),]



#### ------------- look at mosquito data summaries ----------- ####

# --- # create a figure of the MOI across mosquito parts 

## AMA

# create a summarized data frame of the number of abdomens with each MOI
ama_abdomen_moi_df <- ama_merge_data_abdomens %>% 
  filter(!(is.na(haplotype_number))) %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
ama_abdomen_moi_df$haplotype_number = as.numeric(ama_abdomen_moi_df$haplotype_number)
sum(ama_abdomen_moi_df$n) # 172+9 = 181

# create a summarized data frame of the number of heads with each MOI
ama_head_moi_df <- ama_merge_data_heads %>% 
  filter(!(is.na(haplotype_number))) %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
ama_head_moi_df$haplotype_number = as.numeric(ama_head_moi_df$haplotype_number)
sum(ama_head_moi_df$n) # 114+5 = 119

# make ama abdomen figure
ama_title <- expression(paste(italic("pfama1"), ": Mosquito abdomens"))
ama_abdomen_moi_plot = ggplot() +
  geom_bar(data=ama_abdomen_moi_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#F1BB7B") +
  labs(x="Multiplicity of infection", y="Number of mosquito parts", title= ama_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60), limits=c(0,60)) +
  scale_y_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
ama_abdomen_moi_plot

# make ama head figure
ama_title <- expression(paste(italic("pfama1"), ": Mosquito heads"))
ama_head_moi_plot = ggplot() +
  geom_bar(data=ama_head_moi_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#D67236") +
  labs(x="Multiplicity of infection", y="Number of mosquito parts", title= ama_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60), limits=c(0,60)) +
  scale_y_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
ama_head_moi_plot

# put both ama moi plots on same grid
figure1_ama = gridExtra::grid.arrange(ama_abdomen_moi_plot, ama_head_moi_plot, ncol=2)

# export ama moi plots
#ggsave(figure1_ama, filename="/Users/kelseysumner/Desktop/figure1_ama.png", device="png",
       #height=10.5, width=11.2, units="in", dpi=400)


## CSP

# create a summarized data frame of the number of abdomens with each MOI
csp_abdomen_moi_df <- csp_merge_data_abdomens %>% 
  filter(!(is.na(haplotype_number))) %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
csp_abdomen_moi_df$haplotype_number = as.numeric(csp_abdomen_moi_df$haplotype_number)
sum(csp_abdomen_moi_df$n) # 172+9 = 181

# create a summarized data frame of the number of heads with each MOI
csp_head_moi_df <- csp_merge_data_heads %>% 
  filter(!(is.na(haplotype_number))) %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
# make the number of haplotypes column numeric
csp_head_moi_df$haplotype_number = as.numeric(csp_head_moi_df$haplotype_number)
sum(csp_head_moi_df$n) # 114+5 = 119

# make csp abdomen figure
csp_title <- expression(paste(italic("pfcsp1"), ": Mosquito abdomens"))
csp_abdomen_moi_plot = ggplot() +
  geom_bar(data=csp_abdomen_moi_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#FD6467") +
  labs(x="Multiplicity of infection", y="Number of mosquito parts", title= csp_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60), limits=c(0,60)) +
  scale_y_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_abdomen_moi_plot

# make csp head figure
csp_title <- expression(paste(italic("pfcsp1"), ": Mosquito heads"))
csp_head_moi_plot = ggplot() +
  geom_bar(data=csp_head_moi_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#5B1A18") +
  labs(x="Multiplicity of infection", y="Number of mosquito parts", title= csp_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60), limits=c(0,60)) +
  scale_y_continuous(breaks=c(0,5,10,15,20), limits=c(0,20)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_head_moi_plot

# put both csp moi plots on same grid
figure1_csp = gridExtra::grid.arrange(csp_abdomen_moi_plot, csp_head_moi_plot, ncol=2)

# export csp moi plots
#ggsave(figure1_csp, filename="/Users/kelseysumner/Desktop/figure1_csp.png", device="png",
       #height=10.5, width=11.2, units="in", dpi=400)


# --- # create a figure of the haplotype sharing (number of people infected with each haplotype) within each mosquito part


## AMA
ama_title <- expression(paste(italic("Pfama1"), " target"))
plot_ama <- ggplot() +
  geom_bar(data=ama_haplotype_summary, aes(x=haplotype_ids, y=haplotypes_across_samples), alpha=0.9, stat = "identity", fill = "#FD6467", width = 0.8) + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  scale_x_continuous(limits = c(0,500), breaks = c(0,50,100,150,200,250,300,350,400,450,500)) +
  scale_y_continuous(limits = c(0,250), breaks = c(0,50,100,150,200,250)) +
  labs(x="Haplotype ID", y="Number of mosquito parts", title= ama_title) +
  theme_bw() +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25))
plot_ama

## CSP
csp_title <- expression(paste(italic("Pfcsp"), " target"))
plot_csp <- ggplot() +
  geom_bar(data=csp_haplotype_summary, aes(x=haplotype_ids, y=haplotypes_across_samples), alpha=0.9, stat = "identity", fill = "#5B1A18", width = 0.8) + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") + 
  scale_x_continuous(limits = c(0,500), breaks = c(0,50,100,150,200,250,300,350,400,450,500)) +
  scale_y_continuous(limits = c(0,250), breaks = c(0,50,100,150,200,250)) +
  labs(x="Haplotype ID", y="Number of mosquito parts", title= csp_title) +
  theme_bw() +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25)) 
plot_csp

# put both plots on same grid
figure2 = gridExtra::grid.arrange(plot_ama, plot_csp, nrow=2)

# export haplotype sharing plots
# ggsave(figure2, filename="/Users/kelseysumner/Desktop/figure2.png", device="png",
       # height=10.5, width=11.2, units="in", dpi=400)

# create an additional plot that is separated by the mosquito part type (head or abdomen) 

# ------ #  create empty vectors for the number of samples of each type for each haplotype

## AMA

# create vectors of the sample IDs for each type of sample (mosquito head, mosquito abdomen, human DBS)
mosquitoabdomenlist = ama_merge_data_abdomens$sample_id
mosquitoheadlist = ama_merge_data_heads$sample_id
# create a smaller data set of just the haplotypes
ama_haps = ama_merge_data[,c(5:477)]
ama_haps = ama_haps[-which(is.na(ama_haps$H1)),]
# create empty vectors
hap_mosq_abs = rep(NA,ncol(ama_haps))
hap_mosq_heads = rep(NA,ncol(ama_haps))
# loop through each haplotype and count up the number of each type of sample it has
for (i in 1:ncol(ama_haps)){
  hap_mosq_abs_count = 0
  hap_mosq_heads_count = 0
  for (j in 1:nrow(ama_haps)){
    ids = ama_merge_data$sample_id
    if (ama_haps[j,i] > 0 & ids[j] %in% mosquitoabdomenlist) {
        hap_mosq_abs_count = hap_mosq_abs_count + 1
      } else if (ama_haps[j,i] > 0 & ids[j] %in% mosquitoheadlist) {
        hap_mosq_heads_count = hap_mosq_heads_count + 1
    }
  }
  hap_mosq_abs[i] = hap_mosq_abs_count
  hap_mosq_heads[i] = hap_mosq_heads_count
}
# create a dataframe of the haplotype counts and specific types of each sample associated with those haplotype counts
fig2 = data.frame("haplotypes" = colnames(ama_haps), hap_mosq_abs, hap_mosq_heads)
# sum the number of samples with each haplotype across the three sample types
totalsum = rep(NA,nrow(fig2))
for (k in 1:nrow(fig2)){
  totalsum[k] = sum(fig2[k,2:3])
}
# add the total to the dataframe
fig2$total = totalsum
# export to excel to format for stacked bar plots
# write_csv(fig2,"Desktop/haplotype_sharing_data_by_type_ama.csv")

# changed the dataframe format in Excel for the stacked bar plots
fig2_stacked = read_csv("Desktop/haplotype_sharing_data_by_type_stacked_ama.csv")
# create the variable used for fill colors
fig2_stacked$type = as.factor(fig2_stacked$type)
summary(fig2_stacked$type)

# set color scale for ama
myColors <- c("#F1BB7B","#D67236")
names(myColors) <- levels(fig2_stacked$type)
colScale <- scale_fill_manual(name = "type",values = myColors)

# make the histogram
ama_title <- expression(paste(italic("pfama1"), " target"))
ama_plot = ggplot(fig2_stacked, aes(x=reorder(haplotypes, -total),y=hap_counts,fill=type)) + 
  geom_bar(stat="identity") + 
  colScale +
  theme_bw() +
  labs(y = "Number of samples",x="Haplotype ID", title=ama_title) +
  scale_y_continuous(limits = c(0,175), breaks = c(0,50,100,150)) +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), axis.text.x = element_text(size=4,angle=90,hjust=1),text = element_text(size=25), legend.position = "bottom") + 
  guides(fill=guide_legend(title="Type of Sample"))
 ggsave(ama_plot, filename="/Users/kelseysumner/Desktop/figure2_ama.png", device="png",
         height=4, width=20, units="in", dpi=400)



## CSP

# create vectors of the sample IDs for each type of sample (mosquito head, mosquito abdomen, human DBS)
mosquitoabdomenlist = csp_merge_data_abdomens$sample_id
mosquitoheadlist = csp_merge_data_heads$sample_id
# create a smaller data set of just the haplotypes
csp_haps = csp_merge_data[,c(5:275)]
csp_haps = csp_haps[-which(is.na(csp_haps$H1)),]
# create empty vectors
hap_mosq_abs = rep(NA,ncol(csp_haps))
hap_mosq_heads = rep(NA,ncol(csp_haps))
# loop through each haplotype and count up the number of each type of sample it has
for (i in 1:ncol(csp_haps)){
  hap_mosq_abs_count = 0
  hap_mosq_heads_count = 0
  for (j in 1:nrow(csp_haps)){
    ids = csp_merge_data$sample_id
    if (csp_haps[j,i] > 0 & ids[j] %in% mosquitoabdomenlist) {
      hap_mosq_abs_count = hap_mosq_abs_count + 1
    } else if (csp_haps[j,i] > 0 & ids[j] %in% mosquitoheadlist) {
      hap_mosq_heads_count = hap_mosq_heads_count + 1
    }
  }
  hap_mosq_abs[i] = hap_mosq_abs_count
  hap_mosq_heads[i] = hap_mosq_heads_count
}
# create a dataframe of the haplotype counts and specific types of each sample associated with those haplotype counts
fig2 = data.frame("haplotypes" = colnames(csp_haps), hap_mosq_abs, hap_mosq_heads)
# sum the number of samples with each haplotype across the three sample types
totalsum = rep(NA,nrow(fig2))
for (k in 1:nrow(fig2)){
  totalsum[k] = sum(fig2[k,2:3])
}
# add the total to the dataframe
fig2$total = totalsum
# export to excel to format for stacked bar plots
# write_csv(fig2,"Desktop/haplotype_sharing_data_by_type_csp.csv")

# changed the dataframe format in Excel for the stacked bar plots
fig2_stacked = read_csv("Desktop/haplotype_sharing_data_by_type_stacked_csp.csv")
# create the variable used for fill colors
fig2_stacked$type = as.factor(fig2_stacked$type)
summary(fig2_stacked$type)

# set color scale for csp
myColors <- c("#FD6467","#5B1A18")
names(myColors) <- levels(fig2_stacked$type)
colScale <- scale_fill_manual(name = "type",values = myColors)

# make the histogram
csp_title <- expression(paste(italic("pfcsp1"), " target"))
csp_plot = ggplot(fig2_stacked, aes(x=reorder(haplotypes, -total),y=hap_counts,fill=type)) + 
  geom_bar(stat="identity") + 
  colScale +
  theme_bw() +
  labs(y = "Number of samples",x="Haplotype ID", title=csp_title) +
  scale_y_continuous(limits = c(0,175), breaks = c(0,50,100,150)) +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), axis.text.x = element_text(size=6,angle=90,hjust=1),text = element_text(size=25), legend.position = "bottom") + 
  guides(fill=guide_legend(title="Type of Sample"))
# ggsave(csp_plot, filename="/Users/kelseysumner/Desktop/figure2_csp.png", device="png",
       # height=4, width=20, units="in", dpi=400)



#### ------- calculate basic descriptives for haplotypes ------- ####

# calculate how many unique haplotypes were found in mosquito abdomens and heads for csp
summary(ama_merge_data_abdomens$haplotype_number)
summary(ama_merge_data_heads$haplotype_number)

# calculate how many unique haplotypes were found in mosquito abdomens and heads for csp
summary(csp_merge_data_abdomens$haplotype_number)
summary(csp_merge_data_heads$haplotype_number)

# calculate how many haplotypes in abdomen shared with head for ama
fig2_stacked_ama = read_csv("Desktop/haplotype_sharing_data_by_type_ama.csv")
length(which(fig2_stacked_ama$hap_mosq_abs < fig2_stacked_ama$total & fig2_stacked_ama$hap_mosq_abs != 0)) # 149/473
nrow(fig2_stacked_ama)
# now calculate number of heads shared with abdomen for ama
length(which(fig2_stacked_ama$hap_mosq_heads < fig2_stacked_ama$total & fig2_stacked_ama$hap_mosq_heads != 0)) # 149/473
nrow(fig2_stacked_ama)

# calculate how many haplotypes in abdomen shared with head for csp
fig2_stacked_csp = read_csv("Desktop/haplotype_sharing_data_by_type_csp.csv")
length(which(fig2_stacked_csp$hap_mosq_abs < fig2_stacked_csp$total & fig2_stacked_csp$hap_mosq_abs != 0)) # 79/271
nrow(fig2_stacked_csp)


#### --------- figure out how many haplotypes were in mosquito abdomen compared to mosquito head for each mosquito ------- ####

##  AMA
# first merge in the ama abdomen haplotype info with the large mosquito data set (anoph_merged_data)
# abdomens first
ama_merge_data_abdomens = rename(ama_merge_data_abdomens,"sample_id_abdomen"="sample_id","haplotype_number_abdomens"="haplotype_number")
ama_merge_data_abdomens = ama_merge_data_abdomens[,c(2,479)]
ama_all = left_join(anoph_merged_data,ama_merge_data_abdomens,by="sample_id_abdomen")
# then heads
ama_merge_data_heads = rename(ama_merge_data_heads,"sample_id_head"="sample_id","haplotype_number_heads"="haplotype_number")
ama_merge_data_heads = ama_merge_data_heads[,c(2,479)]
ama_all = left_join(ama_all,ama_merge_data_heads,by="sample_id_head")

##  CSP
# first merge in the ama abdomen haplotype info with the large mosquito data set (anoph_merged_data)
# abdomens first
csp_merge_data_abdomens = rename(csp_merge_data_abdomens,"sample_id_abdomen"="sample_id","haplotype_number_abdomens"="haplotype_number")
csp_merge_data_abdomens = csp_merge_data_abdomens[,c(2,277)]
csp_all = left_join(anoph_merged_data,csp_merge_data_abdomens,by="sample_id_abdomen")
# then heads
csp_merge_data_heads = rename(csp_merge_data_heads,"sample_id_head"="sample_id","haplotype_number_heads"="haplotype_number")
csp_merge_data_heads = csp_merge_data_heads[,c(2,277)]
csp_all = left_join(csp_all,csp_merge_data_heads,by="sample_id_head")

# subset the data to just the mosquitoes that had both a head and abdomen have haplotypes after sequencing
# AMA
ama_all = ama_all[which(!(is.na(ama_all$haplotype_number_abdomens)) & !(is.na(ama_all$haplotype_number_heads))),]
# CSP
csp_all = csp_all[which(!(is.na(csp_all$haplotype_number_abdomens)) & !(is.na(csp_all$haplotype_number_heads))),]

# check normality of data
# AMA
# make density plots
ggdensity(ama_all$haplotype_number_abdomens)
ggdensity(ama_all$haplotype_number_heads)
# look at qq plots
ggqqplot(ama_all$haplotype_number_abdomens)
ggqqplot(ama_all$haplotype_number_heads)
# shapiro wilk test for normality: null is normal distribution
shapiro.test(ama_all$haplotype_number_abdomens-ama_all$haplotype_number_heads)
# looks like AMA is overall normal
# CSP
# make density plots
ggdensity(csp_all$haplotype_number_abdomens)
ggdensity(csp_all$haplotype_number_heads)
# look at qq plots
ggqqplot(csp_all$haplotype_number_abdomens)
ggqqplot(csp_all$haplotype_number_heads)
# shapiro wilk test for normality: null is normal distribution
shapiro.test(csp_all$haplotype_number_abdomens-csp_all$haplotype_number_heads)
# looks like CSP is overal normal
# still fine to use paired t-test because N>30, so central limit theorem applies

# compare the average number of haplotypes found in abdomens compared to heads
# AMA
# use a paired t test
t.test(ama_all$haplotype_number_abdomens,ama_all$haplotype_number_heads, paired =TRUE, conf.level = 0.95, alternative = "greater")
mean(ama_all$haplotype_number_abdomens) # 16.2
mean(ama_all$haplotype_number_heads) # 12.5
# make a box plot
ama_all_new_df = data.frame(ama_all$sample_id_abdomen,ama_all$haplotype_number_abdomens,rep("Mosquito Abdomen",nrow(ama_all)))
ama_all_new_df_2 = data.frame(ama_all$sample_id_head,ama_all$haplotype_number_heads,rep("Mosquito Head",nrow(ama_all)))
ama_all_new_df = rename(ama_all_new_df,"sample_id" = "ama_all.sample_id_abdomen","haplotype_number"="ama_all.haplotype_number_abdomens","type"="rep..Mosquito.Abdomen...nrow.ama_all..")
ama_all_new_df_2 = rename(ama_all_new_df_2,"sample_id" = "ama_all.sample_id_head","haplotype_number"="ama_all.haplotype_number_heads","type"="rep..Mosquito.Head...nrow.ama_all..")
ama_all_new_final = rbind(ama_all_new_df,ama_all_new_df_2)
# set color scale
myColors <- c("#F1BB7B","#D67236")
names(myColors) <- levels(ama_all_new_final$type)
colScale <- scale_fill_manual(name = "type",values = myColors)
# make plot
ama_title <- expression(paste(italic("pfama1"), " target"))
ama_boxplot = ggplot(data=ama_all_new_final) +
  geom_boxplot(aes(x=type,y=haplotype_number,fill=type)) +
  labs(x="Mosquito part", y="Number of haplotypes", title= ama_title) +
  colScale +
  theme_bw() +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25),legend.position="none") 
ama_boxplot
ggsave(ama_boxplot, filename="/Users/kelseysumner/Desktop/ama_boxplot.png", device="png",
 height=10.5, width=11.2, units="in", dpi=400)
# make a paired data plot
Abdomens = ama_all$haplotype_number_abdomens
Heads = ama_all$haplotype_number_heads
d <- data.frame(Abdomen = Abdomens,Head = Heads)
ama_plot2 = ggpaired(d,cond1="Abdomen",cond2="Head",fill=c("#F1BB7B","#D67236"),palette="jco", line.size = 0.4) +
  theme_bw() + 
  labs(y="Number of haplotypes", x="Mosquito part",title= ama_title) +
  scale_y_continuous(limits=c(0,60),breaks=c(0,20,40,60)) +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25),legend.position="bottom") 
ama_plot2
ggsave(ama_plot2, filename="/Users/kelseysumner/Desktop/ama_boxplot2.png", device="png",
       height=10.5, width=11.2, units="in", dpi=400)


# CSP
# use a paired t test
t.test(csp_all$haplotype_number_abdomens,csp_all$haplotype_number_heads, paired =TRUE, conf.level = 0.95, alternative = "greater")
mean(csp_all$haplotype_number_abdomens) # 10.6
mean(csp_all$haplotype_number_heads) # 9.0
# make a box plot
csp_all_new_df = data.frame(csp_all$sample_id_abdomen,csp_all$haplotype_number_abdomens,rep("Mosquito Abdomen",nrow(csp_all)))
csp_all_new_df_2 = data.frame(csp_all$sample_id_head,csp_all$haplotype_number_heads,rep("Mosquito Head",nrow(csp_all)))
csp_all_new_df = rename(csp_all_new_df,"sample_id" = "csp_all.sample_id_abdomen","haplotype_number"="csp_all.haplotype_number_abdomens","type"="rep..Mosquito.Abdomen...nrow.csp_all..")
csp_all_new_df_2 = rename(csp_all_new_df_2,"sample_id" = "csp_all.sample_id_head","haplotype_number"="csp_all.haplotype_number_heads","type"="rep..Mosquito.Head...nrow.csp_all..")
csp_all_new_final = rbind(csp_all_new_df,csp_all_new_df_2)
# set color scale
myColors <- c("#FD6467","#5B1A18")
names(myColors) <- levels(csp_all_new_final$type)
colScale <- scale_fill_manual(name = "type",values = myColors)
# make plot
csp_title <- expression(paste(italic("pfcsp"), " target"))
csp_boxplot = ggplot(data=csp_all_new_final) +
  geom_boxplot(aes(x=type,y=haplotype_number,fill=type)) +
  labs(x="Mosquito part", y="Number of haplotypes", title= csp_title) +
  colScale +
  theme_bw() +
  scale_y_continuous(limits=c(0,60),breaks=c(0,20,40,60)) +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25),legend.position="none") 
csp_boxplot
ggsave(csp_boxplot, filename="/Users/kelseysumner/Desktop/csp_boxplot.png", device="png",
       height=10.5, width=11.2, units="in", dpi=400)
# make a paired data plot
Abdomens = csp_all$haplotype_number_abdomens
Heads = csp_all$haplotype_number_heads
d <- data.frame(Abdomen = Abdomens,Head = Heads)
csp_plot2 = ggpaired(d,cond1="Abdomen",cond2="Head",fill=c("#FD6467","#5B1A18"),palette="jco", line.size = 0.4) +
  theme_bw() + 
  labs(y="Number of haplotypes",x="Mosquito part", title= csp_title) +
  scale_y_continuous(limits=c(0,60),breaks=c(0,20,40,60)) +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5), text = element_text(size=25),legend.position="bottom") 
csp_plot2
ggsave(csp_plot2, filename="/Users/kelseysumner/Desktop/csp_boxplot2.png", device="png",
       height=10.5, width=11.2, units="in", dpi=400)
  
  


