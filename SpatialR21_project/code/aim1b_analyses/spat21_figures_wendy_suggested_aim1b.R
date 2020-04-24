# -------------------------------- #
#    Figures Wendy Suggested       #
#         Mozzie phase 1           #
#             Aim 1B               #
#        April 23, 2020            #
#           K. Sumner              #
# -------------------------------- #

# will call repeated infections: "reinfection"

#### ------- load libraries -------- ####
library(tidyverse)
library(schoolmath)
library(lme4)
library(glmmTMB)


#### ------ read in the data sets ------- ####

# read in the ama data set
ama_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/data_without_first_infection/without_first_infection_ama_data_spat21_aim1b_24APR2020.rds")

# read in the csp data set
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/data_without_first_infection/without_first_infection_csp_data_spat21_aim1b_24APR2020.rds")

# read in the full human demographic data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")
final_data = final_data %>%
  select(c(sample_name_dbs,HH_ID,gender))


#### ---- make a figures wendy suggested ------ ####

# 1.	On average, how many new haplotypes in these infections that were all new? Is it important that all the 
# haplotypes be new and if you have just one that you have seen before, then you don’t get sick? Or is it 
# important that at least half the haplotypes be new? etc
# 2.	Can you make a graph with number of new haplotypes versus likelihood of symptoms?
# 3. Can you make a graph with proportion of new haplotypes versus likelihood of symptoms? 
  

#### ---- make the first figure suggested ----- ####

# 1.	On average, how many new haplotypes in these infections that were all new? Is it important that all the 
# haplotypes be new and if you have just one that you have seen before, then you don’t get sick? Or is it 
# important that at least half the haplotypes be new? etc

# look at the average number of new haplotypes in the old and new category
# for csp
csp_data %>%
  group_by(haplotype_category) %>%
  summarize(average_new_prop = mean(proportion_new_haplotypes),average_old_prop = mean(proportion_old_haplotypes),
            median_new_prop = median(proportion_new_haplotypes),median_old_prop = median(proportion_old_haplotypes))
# for ama
ama_data %>%
  group_by(haplotype_category) %>%
  summarize(average_new_prop = mean(proportion_new_haplotypes),average_old_prop = mean(proportion_old_haplotypes),
            median_new_prop = median(proportion_new_haplotypes),median_old_prop = median(proportion_old_haplotypes))


#### --- make the second figure ----- ####

# 2.	Can you make a graph with number of new haplotypes versus likelihood of symptoms?

# make a density plot of the number of new haplotype across symptomatic status
plot2 = ggplot(csp_data, aes(x = count_new_haplotypes,fill=symptomatic_status)) + 
  geom_density() + 
  facet_wrap(~symptomatic_status) +
  xlab("Number of new haplotypes") +
  ylab("Density") +
  theme_bw() +
  scale_fill_manual(values=c("#a6cee3","#1f78b4")) +
  theme(legend.position = "")
plot2
ggsave(plot2, filename="/Users/kelseysumner/Desktop/plot2.png", device="png",
       height=4, width=7, units="in", dpi=500)

# make a density plot of the number of new haplotype across symptomatic status
small_csp = csp_data %>%
  filter(haplotype_category == "old and new")
plot2 = ggplot(small_csp, aes(x = count_new_haplotypes,fill=symptomatic_status)) + 
  geom_density() + 
  facet_wrap(~symptomatic_status) +
  xlab("Number of new haplotypes") +
  ylab("Density") +
  theme_bw() +
  scale_fill_manual(values=c("#b2df8a","#33a02c")) +
  theme(legend.position = "")
plot2
ggsave(plot2, filename="/Users/kelseysumner/Desktop/plot2b.png", device="png",
       height=4, width=7, units="in", dpi=500)


#### --- make the third figure ----- ####

# 3. Can you make a graph with proportion of new haplotypes versus likelihood of symptoms? 

# make a density plot of the number of new haplotype across symptomatic status
plot3 = ggplot(csp_data, aes(x = proportion_new_haplotypes,fill=symptomatic_status)) + 
  geom_density() + 
  facet_wrap(~symptomatic_status) +
  xlab("Proportion of new haplotypes") +
  ylab("Density") +
  theme_bw() +
  scale_fill_manual(values=c("#a6cee3","#1f78b4")) +
  theme(legend.position = "")
plot3
ggsave(plot3, filename="/Users/kelseysumner/Desktop/plot3.png", device="png",
       height=4, width=7, units="in", dpi=500)

# make a density plot of the number of new haplotype across symptomatic status
small_csp = csp_data %>%
  filter(haplotype_category == "old and new")
plot3 = ggplot(small_csp, aes(x = proportion_new_haplotypes,fill=symptomatic_status)) + 
  geom_density() + 
  facet_wrap(~symptomatic_status) +
  xlab("Proportion of new haplotypes") +
  ylab("Density") +
  theme_bw() +
  scale_fill_manual(values=c("#b2df8a","#33a02c")) +
  theme(legend.position = "")
plot3
ggsave(plot3, filename="/Users/kelseysumner/Desktop/plot3b.png", device="png",
       height=4, width=7, units="in", dpi=500)



#### ---- make a fourth figure with matched pairs like amy's method ------- ####
 

# make the plot
csp_plot = ggplot(csp_all, aes(x=symp_median_prop_nonzero, y=asymp_median_prop_nonzero)) +
  geom_point(aes(color=village_name),size=2.5,pch=21) + 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  xlab("Median proportion nonzero for symptomatic infections") +
  ylab("Median proportion nonzero for asymptomatic infections") +
  labs(color="Village name") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  scale_color_manual(values = c("#377eb8","#66bd63","#fb9a99"))
ggsave(csp_plot, filename="/Users/kelseysumner/Desktop/csp_matched_prob_plot_alt3.png", device="png",
       height=5, width=6, units="in", dpi=500)









