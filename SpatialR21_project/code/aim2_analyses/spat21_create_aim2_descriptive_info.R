# ----------------------------------------- #
#        Create aim 2 descriptive info      #
#             Mozzie Phase 1                #
#            AMA and CSP data               #
#            October 15, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(tidyverse)



#### ---------- read in the data sets ---------- ####

# read in the merged ama abdomen edgelist
ama_abdomens = read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_ama_edgelist_abdomen_15OCT2019.rds")

# read in the merged ama head edgelist
ama_heads = read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_ama_edgelist_head_15OCT2019.rds")

# read in the merged csp abdomen edgelist
csp_abdomens = read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_edgelist_abdomen_15OCT2019.rds")

# read in the merged csp head edgelist
csp_heads = read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_edgelist_head_15OCT2019.rds")

# read in the pfama1 haplotype data set
ama_haplotype_df = read_rds("Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# read in the pfcsp haplotype data set
csp_haplotype_df = read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_csp_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")


#### -------- double check that haven't lost any haplotypes ----------- ####

# for csp
foo = csp_haplotype_df[,c(4:301)]
# summarize the samples for each haplotype
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
total_reads_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
  total_reads_in_samples[k] = sum(foo[,k],na.rm=T)
}
haplotype_num_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)
# remove haplotypes with 0 reads after censoring
# none to remove for csp - looks good

# for ama
foo = ama_haplotype_df[,c(4:460)]
# summarize the samples for each haplotype
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
total_reads_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
  total_reads_in_samples[k] = sum(foo[,k],na.rm=T)
}
haplotype_num_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)
# remove haplotypes with 0 reads after censoring
# 1 to remove for ama - remove and reload
haplotype_num_summary = haplotype_num_summary[which(haplotype_num_summary$total_reads_across_samples>0),]
# enforce censoring to rds data set
ama_haplotype_df = ama_haplotype_df %>%
  select(-"H98")
# check it
foo = ama_haplotype_df[,c(4:459)]
# summarize the samples for each haplotype
haplotype.names = rep(1:ncol(foo))
haplotypes_in_samples = rep(NA,ncol(foo))
total_reads_in_samples = rep(NA,ncol(foo))
for (k in 1:ncol(foo)){
  haplotypes_in_samples[k] = length(which(foo[,k] > 0))
  total_reads_in_samples[k] = sum(foo[,k],na.rm=T)
}
haplotype_num_summary = data.frame("haplotype_ids" = haplotype.names, "haplotypes_across_samples" = haplotypes_in_samples, "total_reads_across_samples" = total_reads_in_samples)
# looks good
# write out that data set
# write_rds(ama_haplotype_df,"Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")
# write_csv(ama_haplotype_df,"Desktop/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.csv")


#### ----------- create plots of original number of haplotypes ------------- ####

### create histograms of overall moi

# create a summarized data frame of the number of abdomens with each MOI
# for ama
ama_moi_df <- ama_haplotype_df %>% 
  filter(!(is.na(haplotype_number))) %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
ama_moi_df$haplotype_number = as.numeric(ama_moi_df$haplotype_number)
sum(ama_moi_df$n) 
# for csp
csp_moi_df <- csp_haplotype_df %>% 
  filter(!(is.na(haplotype_number))) %>%
  group_by(haplotype_number) %>%
  summarise(n=n())
csp_moi_df$haplotype_number = as.numeric(csp_moi_df$haplotype_number)
sum(csp_moi_df$n) 

# make ama moi figure
ama_title <- expression(paste(italic("pfama1"), ": MOI"))
ama_moi_plot = ggplot() +
  geom_bar(data=ama_moi_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#F1BB7B") +
  labs(x="Multiplicity of infection", y="Number of samples", title= ama_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,320,360), limits=c(0,360)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
ama_moi_plot

# make csp moi figure
csp_title <- expression(paste(italic("pfcsp"), ": MOI"))
csp_moi_plot = ggplot() +
  geom_bar(data=csp_moi_df,aes(x=haplotype_number,y=n), alpha=0.8,stat="identity",fill="#D67236") +
  labs(x="Multiplicity of infection", y="Number of samples", title= csp_title, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,5,10,20), limits=c(0,20)) +
  scale_y_continuous(breaks=c(0,60,120,180,320,360), limits=c(0,360)) +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
csp_moi_plot

# put both ama moi plots on same grid
figure1_total_moi = gridExtra::grid.arrange(ama_moi_plot, csp_moi_plot, ncol=2)

# export the figure
ggsave(figure1_total_moi, filename="/Users/kelseysumner/Desktop/figure1_total_moi.png", device="png",
       height=10.5, width=11.2, units="in", dpi=400)




#### -------- look at csp descriptives ---------- ####

# look at the number of asymptomatic vs. symptomatic infections for mosquito abdomens
table(csp_abdomens$aim2_exposure, useNA = "always")
# assess median and mean number shared haplotypes in asymptomatic infections
csp_abdomens %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  summarize(mean_val = mean(haps_shared), median_val = median(haps_shared)) %>%
  View()
# assess median and mean number of shared haplotypes in symptomatic infections
csp_abdomens %>%
  filter(aim2_exposure=="symptomatic infection") %>%
  summarize(mean_val = mean(haps_shared), median_val = median(haps_shared)) %>%
  View()
# assess number of human-mosquito pairs with >= 1 haplotype shared
table(csp_abdomens$aim2_exposure,csp_abdomens$haps_shared, useNA = "always")

# look at the number of asymptomatic vs. symptomatic infections for mosquito heads
table(csp_heads$aim2_exposure, useNA = "always")
# assess median and mean number shared haplotypes in asymptomatic infections
csp_heads %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  summarize(mean_val = mean(haps_shared), median_val = median(haps_shared)) %>%
  View()
# assess median and mean number of shared haplotypes in symptomatic infections
csp_heads %>%
  filter(aim2_exposure=="symptomatic infection") %>%
  summarize(mean_val = mean(haps_shared), median_val = median(haps_shared)) %>%
  View()
# assess number of human-mosquito pairs with >= 1 haplotype shared
table(csp_heads$aim2_exposure,csp_heads$haps_shared, useNA = "always")


# look at distribution of human-mosquito abdomen pairs across covariates





#### ------------ create plots of haplotype sharing distributions --------- ####

# create a plot of the distribution of the number of shared haplotypes for mosquito abdomens
# create a summarized data frame of the number of shared haplotypes between humans and mosquito abdomens, stratified
# by other people's symptomatic status
# for asymptomatic
abdomen_df_asymptomatic <- csp_abdomens %>% 
  filter(aim2_exposure=="asymptomatic infection") %>%
  group_by(haps_shared) %>%
  summarise(n=n())
# for symptomatic 
abdomen_df_symptomatic <- csp_abdomens %>% 
  filter(aim2_exposure=="symptomatic infection") %>%
  group_by(haps_shared) %>%
  summarise(n=n())
# make the figure for asymptomatic infections
pfcsp_asymptomatic <- expression(paste(italic("pfcsp"), ": Asymptomatic infections"))
pfcsp_asymptomatic_plot = ggplot() +
  geom_bar(data=abdomen_df_asymptomatic,aes(x=haps_shared,y=n), alpha=0.8,stat="identity",fill="#F1BB7B") +
  labs(x="Number of haplotypes shared", y="Number of human-mosquito pairs", title= pfcsp_asymptomatic, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,2,4,6,8), limits=c(-1,9)) +
  scale_y_continuous(breaks=c(0,60,120), limits=c(0,120)) +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), text = element_text(size=20))
pfcsp_asymptomatic_plot
# make the figure for symptomatic infections
pfcsp_symptomatic <- expression(paste(italic("pfcsp"), ": Symptomatic infections"))
pfcsp_symptomatic_plot = ggplot() +
  geom_bar(data=abdomen_df_symptomatic,aes(x=haps_shared,y=n), alpha=0.8,stat="identity",fill="#D67236") +
  labs(x="Number of haplotypes shared", y="Number of human-mosquito pairs", title= pfcsp_symptomatic, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,2,4,6,8), limits=c(-1,9)) +
  scale_y_continuous(breaks=c(0,60,120), limits=c(0,120)) +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), text = element_text(size=20))
pfcsp_symptomatic_plot
# put both plots on same grid
figure4_abdomen_symptom_plot = gridExtra::grid.arrange(pfcsp_asymptomatic_plot, pfcsp_symptomatic_plot, ncol=2)
# export the figure
ggsave(figure4_abdomen_symptom_plot, filename="/Users/kelseysumner/Desktop/figure4_pfcsp_abdomen_symptom_plot.png", device="png",
       height=10.5, width=11.2, units="in", dpi=400)



# create a plot of the distribution of the number of shared haplotypes for mosquito head
# create a summarized data frame of the number of shared haplotypes between humans and mosquito head, stratified
# by other people's symptomatic status
# for asymptomatic
head_df_asymptomatic <- csp_heads %>% 
  filter(aim2_exposure=="asymptomatic infection") %>%
  group_by(haps_shared) %>%
  summarise(n=n())
# for symptomatic 
head_df_symptomatic <- csp_heads %>% 
  filter(aim2_exposure=="symptomatic infection") %>%
  group_by(haps_shared) %>%
  summarise(n=n())
# make the figure for asymptomatic infections
pfcsp_asymptomatic <- expression(paste(italic("pfcsp"), ": Asymptomatic infections"))
pfcsp_asymptomatic_plot_heads = ggplot() +
  geom_bar(data=head_df_asymptomatic,aes(x=haps_shared,y=n), alpha=0.8,stat="identity",fill="#F1BB7B") +
  labs(x="Number of haplotypes shared", y="Number of human-mosquito pairs", title= pfcsp_asymptomatic, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,2,4,6,8), limits=c(-1,9)) +
  scale_y_continuous(breaks=c(0,60,120,140), limits=c(0,140)) +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), text = element_text(size=20))
pfcsp_asymptomatic_plot_heads
# make the figure for symptomatic infections
pfcsp_symptomatic <- expression(paste(italic("pfcsp"), ": Symptomatic infections"))
pfcsp_symptomatic_plot_heads = ggplot() +
  geom_bar(data=head_df_symptomatic,aes(x=haps_shared,y=n), alpha=0.8,stat="identity",fill="#D67236") +
  labs(x="Number of haplotypes shared", y="Number of human-mosquito pairs", title= pfcsp_symptomatic, pch=18) +
  theme_bw() +
  scale_x_continuous(breaks=c(0,2,4,6,8), limits=c(-1,9)) +
  scale_y_continuous(breaks=c(0,60,120,140), limits=c(0,140)) +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), text = element_text(size=20))
pfcsp_symptomatic_plot_heads
# put both plots on same grid
figure4_head_symptom_plot = gridExtra::grid.arrange(pfcsp_asymptomatic_plot_heads, pfcsp_symptomatic_plot_heads, ncol=2)
# export the figure
ggsave(figure4_head_symptom_plot, filename="/Users/kelseysumner/Desktop/figure4_pfcsp_head_symptom_plot.png", device="png",
       height=10.5, width=11.2, units="in", dpi=400)







