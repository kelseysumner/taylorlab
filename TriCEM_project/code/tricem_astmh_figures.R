# -------------------------------- #
#       Make ASTMH figures         #
#        TriCEM project            #
#       September 21, 2020         #
#           K. Sumner              #
# -------------------------------- #

#### ------- load libraries -------- ####
library(tidyverse)
library(lme4)
library(ggbeeswarm)
library(RVenn)



#### ------- read in the data set ------- ####

# read in the kinesamo data for the first high transmission season (06-17 to 10-17)
kinesamo_data = read_rds("Desktop/Dissertation Materials/TriCEM work/data/tricem_model_data_kinesamo_first_season_17SEP2020.rds")
# read in the maruti data for the first high transmission season (06-17 to 10-17)
maruti_data = read_rds("Desktop/Dissertation Materials/TriCEM work/data/tricem_model_data_maruti_first_season_17SEP2020.rds")

# read in the human demographic data
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")

# read in the merged anopheles mosquito data set
anoph_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Mosquito data/clean data/merged_data/spat21_mosquito_anopheles_merged_data_18JAN2019.RDS")


#### ---- make long versions of both data sets ----- ####

## -- for kinesamo data

# cut down the data set to each sample type
colnames(kinesamo_data)
# for humans
k_human = kinesamo_data %>% 
  select(starting_date,ending_date,human_haplotypes,human_num_unique,human_id_list,human_infections) %>%
  rename(haplotype_list = human_haplotypes,num_haplotypes = human_num_unique,id_list = human_id_list,num_infections = human_infections) %>%
  mutate(sample_type = rep("Human",nrow(kinesamo_data)))
# for mosquito heads
k_head = kinesamo_data %>% 
  select(starting_date,ending_date,head_haplotypes,head_num_unique,head_id_list,head_infections) %>%
  rename(haplotype_list = head_haplotypes,num_haplotypes = head_num_unique,id_list = head_id_list,num_infections = head_infections) %>%
  mutate(sample_type = rep("Mosquito Head",nrow(kinesamo_data)))
# for mosquito abdomens
k_abdomen = kinesamo_data %>% 
  select(starting_date,ending_date,abdomen_haplotypes,abdomen_num_unique,abdomen_id_list,abdomen_infections) %>%
  rename(haplotype_list = abdomen_haplotypes,num_haplotypes = abdomen_num_unique,id_list = abdomen_id_list,num_infections = abdomen_infections) %>%
  mutate(sample_type = rep("Mosquito Abdomen",nrow(kinesamo_data)))

# now combine the sample types
kinesamo_data_long = rbind(k_human,k_head,k_abdomen)


## -- for maruti data

# cut down the data set to each sample type
colnames(maruti_data)
# for humans
m_human = maruti_data %>% 
  select(starting_date,ending_date,human_haplotypes,human_num_unique,human_id_list,human_infections) %>%
  rename(haplotype_list = human_haplotypes,num_haplotypes = human_num_unique,id_list = human_id_list,num_infections = human_infections) %>%
  mutate(sample_type = rep("Human",nrow(maruti_data)))
# for mosquito heads
m_head = maruti_data %>% 
  select(starting_date,ending_date,head_haplotypes,head_num_unique,head_id_list,head_infections) %>%
  rename(haplotype_list = head_haplotypes,num_haplotypes = head_num_unique,id_list = head_id_list,num_infections = head_infections) %>%
  mutate(sample_type = rep("Mosquito Head",nrow(maruti_data)))
# for mosquito abdomens
m_abdomen = maruti_data %>% 
  select(starting_date,ending_date,abdomen_haplotypes,abdomen_num_unique,abdomen_id_list,abdomen_infections) %>%
  rename(haplotype_list = abdomen_haplotypes,num_haplotypes = abdomen_num_unique,id_list = abdomen_id_list,num_infections = abdomen_infections) %>%
  mutate(sample_type = rep("Mosquito Abdomen",nrow(maruti_data)))

# now combine the sample types
maruti_data_long = rbind(m_human,m_head,m_abdomen)



### ------ look at the overlap in haplotypes across sample types ------- ####

# calculate the proportion unique haplotypes private to humans or private to mosquitoes
# for kinesamo
prop_private_humans = rep(NA,nrow(kinesamo_data))
prop_private_mosquitoes = rep(NA,nrow(kinesamo_data))
for (i in 1:nrow(kinesamo_data)) {
  # make the haplotype lists
  mosquito_haplotypes = paste0(kinesamo_data$head_haplotypes[i],",",kinesamo_data$abdomen_haplotypes[i])
  mosquito_haplotypes = str_split(mosquito_haplotypes,",")[[1]]
  mosquito_haplotypes = unique(mosquito_haplotypes)
  human_haplotypes = kinesamo_data$human_haplotypes[i]
  human_haplotypes = str_split(human_haplotypes,",")[[1]]
  # compare the lists of haplotypes
  prop_private_humans[i] = length(setdiff(human_haplotypes,mosquito_haplotypes))/length(human_haplotypes)
  prop_private_mosquitoes[i] = length(setdiff(mosquito_haplotypes,human_haplotypes))/length(mosquito_haplotypes)
}
kinesamo_data$prop_private_humans = prop_private_humans
kinesamo_data$prop_private_mosquitoes = prop_private_mosquitoes
# for maruti
prop_private_humans = rep(NA,nrow(maruti_data))
prop_private_mosquitoes = rep(NA,nrow(maruti_data))
for (i in 1:nrow(maruti_data)) {
  # make the haplotype lists
  mosquito_haplotypes = paste0(maruti_data$head_haplotypes[i],",",maruti_data$abdomen_haplotypes[i])
  mosquito_haplotypes = str_split(mosquito_haplotypes,",")[[1]]
  mosquito_haplotypes = unique(mosquito_haplotypes)
  human_haplotypes = maruti_data$human_haplotypes[i]
  human_haplotypes = str_split(human_haplotypes,",")[[1]]
  # compare the lists of haplotypes
  prop_private_humans[i] = length(setdiff(human_haplotypes,mosquito_haplotypes))/length(human_haplotypes)
  prop_private_mosquitoes[i] = length(setdiff(mosquito_haplotypes,human_haplotypes))/length(mosquito_haplotypes)
}
maruti_data$prop_private_humans = prop_private_humans
maruti_data$prop_private_mosquitoes = prop_private_mosquitoes

# make a scatterplot of the proportion private haplotypes
# for kinesamo
scatter_plot = ggplot(data=kinesamo_data,aes(x=prop_private_humans,y=prop_private_mosquitoes)) + 
  geom_abline(intercept=0,slope=1,linetype="longdash",color="dark grey") +
  geom_point(fill="#b2df8a",color = "black",size=3,pch=21,alpha=0.6) +
  xlab("Private to humans") +
  ylab("Private to mosquitoes") +
  theme_bw() +
  scale_x_continuous(limits=c(0,1),breaks=c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(limits=c(0,1),breaks=c(0.0,0.2,0.4,0.6,0.8,1.0))
scatter_plot
ggsave(scatter_plot, filename="/Users/kelseysumner/Desktop/tricem_private_haplotypes_plot_kinesamo.png", device="png",
       height=5, width=5.5, units="in", dpi=500)
# for maruti
scatter_plot = ggplot(data=maruti_data,aes(x=prop_private_humans,y=prop_private_mosquitoes)) + 
  geom_abline(intercept=0,slope=1,linetype="longdash",color="dark grey") +
  geom_point(fill="#b2df8a",color = "black",size=3,pch=21,alpha=0.6) +
  xlab("Private to humans") +
  ylab("Private to mosquitoes") +
  theme_bw() +
  scale_x_continuous(limits=c(0,1),breaks=c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(limits=c(0,1),breaks=c(0.0,0.2,0.4,0.6,0.8,1.0))
scatter_plot
ggsave(scatter_plot, filename="/Users/kelseysumner/Desktop/tricem_private_haplotypes_plot_maruti.png", device="png",
       height=5, width=5.5, units="in", dpi=500)


# make a blank scatter plot
scatter_plot = ggplot(data=kinesamo_data,aes(x=prop_private_humans,y=prop_private_mosquitoes)) + 
  geom_abline(intercept=0,slope=1,linetype="longdash",color="dark grey") +
  xlab("Private to humans") +
  ylab("Private to mosquitoes") +
  theme_bw() +
  scale_x_continuous(limits=c(0,1),breaks=c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(limits=c(0,1),breaks=c(0.0,0.2,0.4,0.6,0.8,1.0))
scatter_plot
ggsave(scatter_plot, filename="/Users/kelseysumner/Desktop/blank_plot.png", device="png",
       height=5, width=5.5, units="in", dpi=500)


# calculate the proportion of private haplotypes comparing mosquito abdomens and heads
# for kinesamo
prop_private_abdomens = rep(NA,nrow(kinesamo_data))
prop_private_heads = rep(NA,nrow(kinesamo_data))
for (i in 1:nrow(kinesamo_data)) {
  # make the haplotype lists
  abdomen_haplotypes = kinesamo_data$abdomen_haplotypes[i]
  abdomen_haplotypes = str_split(abdomen_haplotypes,",")[[1]]
  head_haplotypes = kinesamo_data$head_haplotypes[i]
  head_haplotypes = str_split(head_haplotypes,",")[[1]]
  # compare the lists of haplotypes
  prop_private_abdomens[i] = length(setdiff(abdomen_haplotypes,head_haplotypes))/length(abdomen_haplotypes)
  prop_private_heads[i] = length(setdiff(head_haplotypes,abdomen_haplotypes))/length(head_haplotypes)
}
kinesamo_data$prop_private_abdomens = prop_private_abdomens
kinesamo_data$prop_private_heads = prop_private_heads
scatter_plot = ggplot(data=kinesamo_data,aes(x=prop_private_abdomens,y=prop_private_heads)) + 
  geom_abline(intercept=0,slope=1,linetype="longdash",color="dark grey") +
  geom_point(fill="#1f78b4",color = "black",size=3,pch=21,alpha=0.6) +
  xlab("Private to mosquito abdomens") +
  ylab("Private to mosquito heads") +
  theme_bw() +
  scale_x_continuous(limits=c(0,1),breaks=c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(limits=c(0,1),breaks=c(0.0,0.2,0.4,0.6,0.8,1.0))
scatter_plot
ggsave(scatter_plot, filename="/Users/kelseysumner/Desktop/tricem_private_haplotypes_plot_abdomenheads_kinesamo.png", device="png",
       height=5, width=5.5, units="in", dpi=500)

# for maruti
prop_private_abdomens = rep(NA,nrow(maruti_data))
prop_private_heads = rep(NA,nrow(maruti_data))
for (i in 1:nrow(maruti_data)) {
  # make the haplotype lists
  abdomen_haplotypes = maruti_data$abdomen_haplotypes[i]
  abdomen_haplotypes = str_split(abdomen_haplotypes,",")[[1]]
  head_haplotypes = maruti_data$head_haplotypes[i]
  head_haplotypes = str_split(head_haplotypes,",")[[1]]
  # compare the lists of haplotypes
  prop_private_abdomens[i] = length(setdiff(abdomen_haplotypes,head_haplotypes))/length(abdomen_haplotypes)
  prop_private_heads[i] = length(setdiff(head_haplotypes,abdomen_haplotypes))/length(head_haplotypes)
}
maruti_data$prop_private_abdomens = prop_private_abdomens
maruti_data$prop_private_heads = prop_private_heads
scatter_plot = ggplot(data=maruti_data,aes(x=prop_private_abdomens,y=prop_private_heads)) + 
  geom_abline(intercept=0,slope=1,linetype="longdash",color="dark grey") +
  geom_point(fill="#1f78b4",color = "black",size=3,pch=21,alpha=0.6) +
  xlab("Private to mosquito abdomens") +
  ylab("Private to mosquito heads") +
  theme_bw() +
  scale_x_continuous(limits=c(0,1),breaks=c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(limits=c(0,1),breaks=c(0.0,0.2,0.4,0.6,0.8,1.0))
scatter_plot
ggsave(scatter_plot, filename="/Users/kelseysumner/Desktop/tricem_private_haplotypes_plot_abdomenheads_maruti.png", device="png",
       height=5, width=5.5, units="in", dpi=500)




# now calculate the proportion of haplotypes private to each sample type
# calculate the proportion unique haplotypes private to humans or private to mosquitoes
# for kinesamo
prop_private_humans = rep(NA,nrow(kinesamo_data))
prop_private_abdomens = rep(NA,nrow(kinesamo_data))
prop_private_heads = rep(NA,nrow(kinesamo_data))
for (i in 1:nrow(kinesamo_data)) {
  # make the haplotype lists
  human_haplotypes = kinesamo_data$human_haplotypes[i]
  human_haplotypes = str_split(human_haplotypes,",")[[1]]
  abdomen_haplotypes = kinesamo_data$abdomen_haplotypes[i]
  abdomen_haplotypes = str_split(abdomen_haplotypes,",")[[1]]
  head_haplotypes = kinesamo_data$head_haplotypes[i]
  head_haplotypes = str_split(head_haplotypes,",")[[1]]
  # compare the lists of haplotypes
  private_human_1 = setdiff(human_haplotypes,abdomen_haplotypes)
  prop_private_humans[i] = length(setdiff(private_human_1,head_haplotypes))/length(human_haplotypes)
  private_abdomen_1 = setdiff(abdomen_haplotypes,human_haplotypes)
  prop_private_abdomens[i] = length(setdiff(private_abdomen_1,head_haplotypes))/length(abdomen_haplotypes)
  private_head_1 = setdiff(head_haplotypes,human_haplotypes)
  prop_private_heads[i] = length(setdiff(private_head_1,abdomen_haplotypes))/length(head_haplotypes)
}
kinesamo_data$prop_private_humans = prop_private_humans
kinesamo_data$prop_private_abdomens = prop_private_abdomens
kinesamo_data$prop_private_heads = prop_private_heads
# for maruti
prop_private_humans = rep(NA,nrow(maruti_data))
prop_private_abdomens = rep(NA,nrow(maruti_data))
prop_private_heads = rep(NA,nrow(maruti_data))
for (i in 1:nrow(maruti_data)) {
  # make the haplotype lists
  human_haplotypes = maruti_data$human_haplotypes[i]
  human_haplotypes = str_split(human_haplotypes,",")[[1]]
  abdomen_haplotypes = maruti_data$abdomen_haplotypes[i]
  abdomen_haplotypes = str_split(abdomen_haplotypes,",")[[1]]
  head_haplotypes = maruti_data$head_haplotypes[i]
  head_haplotypes = str_split(head_haplotypes,",")[[1]]
  # compare the lists of haplotypes
  private_human_1 = setdiff(human_haplotypes,abdomen_haplotypes)
  prop_private_humans[i] = length(setdiff(private_human_1,head_haplotypes))/length(human_haplotypes)
  private_abdomen_1 = setdiff(abdomen_haplotypes,human_haplotypes)
  prop_private_abdomens[i] = length(setdiff(private_abdomen_1,head_haplotypes))/length(abdomen_haplotypes)
  private_head_1 = setdiff(head_haplotypes,human_haplotypes)
  prop_private_heads[i] = length(setdiff(private_head_1,abdomen_haplotypes))/length(head_haplotypes)
}
maruti_data$prop_private_humans = prop_private_humans
maruti_data$prop_private_abdomens = prop_private_abdomens
maruti_data$prop_private_heads = prop_private_heads

# for kinesamo
# add the data to the long data set
h_df = kinesamo_data %>% 
  select(starting_date,prop_private_humans) %>% 
  mutate(sample_type = rep("Human",nrow(kinesamo_data))) %>%
  rename(prop_private = prop_private_humans)
a_df = kinesamo_data %>% 
  select(starting_date,prop_private_abdomens) %>% 
  mutate(sample_type = rep("Mosquito Abdomen",nrow(kinesamo_data))) %>%
  rename(prop_private = prop_private_abdomens)
he_df = kinesamo_data %>% 
  select(starting_date,prop_private_heads) %>% 
  mutate(sample_type = rep("Mosquito Head",nrow(kinesamo_data))) %>%
  rename(prop_private = prop_private_heads)
line_plot_df = rbind(h_df,a_df,he_df)
# now make a line plot of the proportion of private haplotypes over time
line_plot = ggplot(data=line_plot_df,aes(x=starting_date,y=prop_private,color=sample_type)) + 
  geom_line(size=1.5) +
  xlab("Starting date for time window") +
  ylab("Proportion of private haplotypes") +
  theme_bw() +
  scale_color_manual(values = c("#fb9a99","#1f78b4","#b2df8a")) +
  labs(color="Sample type") +
  scale_x_date(date_breaks = "1 week") +
  theme(legend.position = c(0.15,0.8),legend.box.background = element_rect(colour = "black"),axis.text.x = element_text(angle = 90))
line_plot
ggsave(line_plot, filename="/Users/kelseysumner/Desktop/tricem_line_plot_kinesamo.png", device="png",
       height=5, width=8, units="in", dpi=500)


# for maruti
# add the data to the long data set
h_df = maruti_data %>% 
  select(starting_date,prop_private_humans) %>% 
  mutate(sample_type = rep("Human",nrow(maruti_data))) %>%
  rename(prop_private = prop_private_humans)
a_df = maruti_data %>% 
  select(starting_date,prop_private_abdomens) %>% 
  mutate(sample_type = rep("Mosquito Abdomen",nrow(maruti_data))) %>%
  rename(prop_private = prop_private_abdomens)
he_df = maruti_data %>% 
  select(starting_date,prop_private_heads) %>% 
  mutate(sample_type = rep("Mosquito Head",nrow(maruti_data))) %>%
  rename(prop_private = prop_private_heads)
line_plot_df = rbind(h_df,a_df,he_df)
# now make a line plot of the proportion of private haplotypes over time
line_plot = ggplot(data=line_plot_df,aes(x=starting_date,y=prop_private,color=sample_type)) + 
  geom_line(size=1.5) +
  xlab("Starting date for time window") +
  ylab("Proportion of private haplotypes") +
  theme_bw() +
  scale_color_manual(values = c("#fb9a99","#1f78b4","#b2df8a")) +
  labs(color="Sample type") +
  scale_x_date(date_breaks = "1 week") +
  theme(legend.position = c(0.09,0.9),legend.box.background = element_rect(colour = "black"),axis.text.x = element_text(angle = 90))
line_plot
ggsave(line_plot, filename="/Users/kelseysumner/Desktop/tricem_line_plot_maruti.png", device="png",
       height=6.5, width=9, units="in", dpi=500)



#### -------- make plots ------- ####

## ---- figure 1: sampling of humans, mosquito heads, and mosquito abdomens over time

# make the separate data sets
abdomen_df = anoph_merged_data %>% 
  filter(!(is.na(sample_id_abdomen))) %>%
  select(sample_id_abdomen,collection_date,pf_pcr_infection_status_sample_level_a) %>%
  rename(sample_name_dbs = sample_id_abdomen, sample_id_date = collection_date, pf_pcr_infection_status = pf_pcr_infection_status_sample_level_a)
abdomen_df$sample_type = rep("Mosquito abdomen",nrow(abdomen_df))
head_df = anoph_merged_data %>% 
  filter(!(is.na(sample_id_head))) %>%
  select(sample_id_head,collection_date,pf_pcr_infection_status_sample_level_h) %>%
  rename(sample_name_dbs = sample_id_head, sample_id_date = collection_date, pf_pcr_infection_status = pf_pcr_infection_status_sample_level_h)
head_df$sample_type = rep("Mosquito head",nrow(head_df))
human_df = final_data %>% 
  filter(!(is.na(sample_name_dbs))) %>%
  select(sample_name_dbs,sample_id_date,pf_pcr_infection_status)
human_df$sample_type = rep("Human",nrow(human_df))

# combine the data sets
all_df = rbind(human_df,head_df,abdomen_df)

# add two missing mosquito entries
sample_name_dbs = c("K01 A00030","K01 H00030","K01 A00047","K01 H00047")
sample_id_date = c("2017-07-17","2017-07-17","2017-08-21","2017-08-21")
pf_pcr_infection_status = c("positive","positive","positive","positive")
sample_type = c("Mosquito abdomen","Mosquito head","Mosquito abdomen","Mosquito head")
to_add_df = data.frame(sample_name_dbs,sample_id_date,pf_pcr_infection_status,sample_type)
all_df = rbind(all_df,to_add_df)

# check the new data set
table(all_df$sample_type, useNA = "always")

# try a facet plot with bars
all_df$sample_type = as.factor(all_df$sample_type)
all_df_neg = all_df %>% filter(pf_pcr_infection_status=="negative")
all_df_pos = all_df %>% filter(pf_pcr_infection_status=="positive")
all_df_neg = data.frame(all_df_neg)
all_df_pos = data.frame(all_df_pos)
small_all_df = all_df %>%
  mutate(new_date = lubridate::floor_date(sample_id_date,"week")) %>%
  group_by(new_date,sample_type,pf_pcr_infection_status) %>%
  summarize(n=n())
# set the colors
# human (green): #b2df8a
# mosquito abdomen (dark blue): #1f78b4
# mosquito head (pink): #fb9a99
# no infection (light grey): #D3DDDC
small_all_df$color = rep(NA,nrow(small_all_df))
small_all_df$color[which(small_all_df$sample_type=="Human" & small_all_df$pf_pcr_infection_status=="positive")] = "#b2df8a"
small_all_df$color[which(small_all_df$sample_type=="Human" & small_all_df$pf_pcr_infection_status=="negative")] = "#D3DDDC"
small_all_df$color[which(small_all_df$sample_type=="Mosquito abdomen" & small_all_df$pf_pcr_infection_status=="positive")] = "#1f78b4"
small_all_df$color[which(small_all_df$sample_type=="Mosquito abdomen" & small_all_df$pf_pcr_infection_status=="negative")] = "#D3DDDC"
small_all_df$color[which(small_all_df$sample_type=="Mosquito head" & small_all_df$pf_pcr_infection_status=="positive")] = "#fb9a99"
small_all_df$color[which(small_all_df$sample_type=="Mosquito head" & small_all_df$pf_pcr_infection_status=="negative")] = "#D3DDDC"
color_order = c("#D3DDDC","#b2df8a","#1f78b4","#fb9a99")
small_all_df <- within(small_all_df,color <- factor(color,levels=color_order))
# make the plot
density_all_plot = ggplot(data=small_all_df,aes(x=new_date,fill=color,y=n)) + 
  facet_grid(sample_type ~ .,switch = "y") +
  geom_histogram(stat="identity",color="black") +
  xlab("") +
  ylab("Number of samples collected") +
  scale_fill_identity() +
  theme_bw() +
  scale_x_date(date_breaks="1 month",limits = as.Date(c("2017-06-01","2018-08-01"))) + 
  scale_y_continuous(limits = c(0,100),breaks=c(0,20,40,60,80,100),position = "right") + 
  theme(text = element_text(size=30),axis.text.x = element_text(angle = 90)) 
density_all_plot
ggsave(density_all_plot, filename="/Users/kelseysumner/Desktop/tricem_sampling_plot.png", device="png",
       height=15, width=20, units="in", dpi=500)



#### ------ make a figure of the number of infections in each sample type ------ ####

# for kinesamo
sample_order = c("Mosquito Head","Mosquito Abdomen","Human")
kinesamo_data_long <- within(kinesamo_data_long,sample_type <- factor(sample_type,levels=sample_order))
str(kinesamo_data_long$sample_type)
plot_2 = ggplot(data=kinesamo_data_long,aes(y=num_infections,x=sample_type,group=sample_type)) +
  geom_boxplot() +
  geom_quasirandom(aes(color=sample_type),groupOnX = T) +
  theme_bw() +
  ylab("Number of infections within each time window") +
  labs(fill="Sample type") +
  scale_color_manual(values = c("#fb9a99","#1f78b4","#b2df8a")) +
  xlab("")+
  theme(legend.position = "none") + 
  coord_flip()
plot_2
ggsave(plot_2, filename="/Users/kelseysumner/Desktop/number_infections_beeswarm_kinesamo.png", device="png",
       height=4, width=6, units="in", dpi=400)

# for maruti
sample_order = c("Mosquito Head","Mosquito Abdomen","Human")
maruti_data_long <- within(maruti_data_long,sample_type <- factor(sample_type,levels=sample_order))
plot_2 = ggplot(data=maruti_data_long,aes(y=num_infections,x=sample_type,group=sample_type)) +
  geom_boxplot() +
  geom_quasirandom(aes(color=sample_type),groupOnX = T) +
  theme_bw() +
  ylab("Number of infections within each time window") +
  labs(fill="Sample type") +
  scale_color_manual(values = c("#fb9a99","#1f78b4","#b2df8a")) +
  xlab("")+
  theme(legend.position = "none") + 
  coord_flip()
plot_2
ggsave(plot_2, filename="/Users/kelseysumner/Desktop/number_infections_beeswarm_maruti.png", device="png",
       height=4, width=6, units="in", dpi=400)



#### ----- make a figure to illustrate the moving 30-day window ------- ####

# check the new data set
table(all_df_pos$sample_type, useNA = "always")

# add a column for counts
all_df_positives = all_df %>%
  group_by(sample_id_date,sample_type) %>%
  summarize(n=n())

# subset the data set to just the dates of interest
all_df_positives = all_df_positives %>% filter(sample_id_date < "2017-10-15")

# first make a stacked bar plot of all positive infections
stacked_bar = ggplot(data=all_df_positives,aes(y=n,x=sample_id_date,fill=sample_type)) +
  geom_bar(stat="identity",color="black") +
  theme_bw() +
  ylab("Number of infections per day") +
  labs(fill="Sample type") +
  scale_fill_manual(values = c("#b2df8a","#1f78b4","#fb9a99")) +
  xlab("Date")+
  scale_x_date(date_breaks = "1 week") +
  scale_y_continuous(limits = c(0,200)) +
  theme(legend.position = c(0.9,0.86),legend.box.background = element_rect(colour = "black"),axis.text.x = element_text(angle = 90))
stacked_bar
ggsave(stacked_bar, filename="/Users/kelseysumner/Desktop/stacked_bar_maruti.png", device="png",
       height=6, width=10, units="in", dpi=400)



#### ----- make a beeswarm plot of the number of unique haplotypes by window ------- ###

# for kinesamo
sample_order = c("Mosquito Head","Mosquito Abdomen","Human")
kinesamo_data_long <- within(kinesamo_data_long,sample_type <- factor(sample_type,levels=sample_order))
str(kinesamo_data_long$sample_type)
plot_2 = ggplot(data=kinesamo_data_long,aes(y=num_haplotypes,x=sample_type,group=sample_type)) +
  geom_boxplot() +
  geom_quasirandom(aes(color=sample_type),groupOnX = T) +
  theme_bw() +
  ylab("Number of unique haplotypes within each time window") +
  labs(fill="Sample type") +
  scale_color_manual(values = c("#fb9a99","#1f78b4","#b2df8a")) +
  xlab("")+
  theme(legend.position = "none") + 
  coord_flip()
plot_2
ggsave(plot_2, filename="/Users/kelseysumner/Desktop/number_haplotypes_beeswarm_kinesamo.png", device="png",
       height=4, width=5, units="in", dpi=400)

# for maruti
sample_order = c("Mosquito Head","Mosquito Abdomen","Human")
maruti_data_long <- within(maruti_data_long,sample_type <- factor(sample_type,levels=sample_order))
plot_2 = ggplot(data=maruti_data_long,aes(y=num_haplotypes,x=sample_type,group=sample_type)) +
  geom_boxplot() +
  geom_quasirandom(aes(color=sample_type),groupOnX = T) +
  theme_bw() +
  ylab("Number of unique haplotypes within each time window") +
  labs(fill="Sample type") +
  scale_color_manual(values = c("#fb9a99","#1f78b4","#b2df8a")) +
  xlab("")+
  theme(legend.position = "none") + 
  coord_flip()
plot_2
ggsave(plot_2, filename="/Users/kelseysumner/Desktop/number_haplotypes_beeswarm_maruti.png", device="png",
       height=4, width=5, units="in", dpi=400)






