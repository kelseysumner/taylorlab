# -------------------------------------- #
#           Spat21/Mozzie Study          #
#  Use Amy's asymp/symp matching method  #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#            April 7, 2020               #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(tidyverse)


#### ----- read in the data sets ----- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/model data/final_model_data/spat21_aim2_merged_data_with_weights_5MAR2020.rds")

# subset the data set to samples that passed pfcsp sequencing only
csp_data = model_data %>%
  filter(!(is.na(csp_haps_shared)))

# subset the data set to samples that passed pfcsp sequencing only
ama_data = model_data %>%
  filter(!(is.na(ama_haps_shared)))



#### ------ now try coding amy's method -------- ####

# first subset to only the individuals who have at least 1 symptomatic and 1 asymptomatic infection
# for csp - end up with 65 unique participants and 1565 obs
csp_infxns_to_include_p1 = csp_data %>%
  group_by(unq_memID,aim2_exposure) %>%
  summarize(n=n())
csp_infxns_to_include_p2 = csp_infxns_to_include_p1 %>%
  group_by(unq_memID) %>%
  summarize(n=n())
csp_infxns_to_include_p3 = csp_infxns_to_include_p2 %>%
  filter(n>1)
csp_subset_data = csp_data %>%
  filter(csp_data$unq_memID %in% csp_infxns_to_include_p3$unq_memID)
# for ama - end up with 56 unique participants and 1197 obs
ama_infxns_to_include_p1 = ama_data %>%
  group_by(unq_memID,aim2_exposure) %>%
  summarize(n=n())
ama_infxns_to_include_p2 = ama_infxns_to_include_p1 %>%
  group_by(unq_memID) %>%
  summarize(n=n())
ama_infxns_to_include_p3 = ama_infxns_to_include_p2 %>%
  filter(n>1)
ama_subset_data = ama_data %>%
  filter(ama_data$unq_memID %in% ama_infxns_to_include_p3$unq_memID)

# now calculate the average p_te_all within asymptomatic and symptomatic infections
# for csp
csp_average_data = csp_subset_data %>%
  group_by(village_name,unq_memID,aim2_exposure) %>%
  summarize(average_p_te_all_csp=mean(p_te_all_csp,na.rm=T))
# for ama
ama_average_data = ama_subset_data %>%
  group_by(village_name,unq_memID,aim2_exposure) %>%
  summarize(average_p_te_all_ama=mean(p_te_all_ama,na.rm=T))  

# now split up the data sets so have separate columns for asymptomatic and symptomatic infections
# for csp
csp_asymp = csp_average_data %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("asymp_average_p_te_all_csp" = average_p_te_all_csp)
csp_symp = csp_average_data %>%
  filter(aim2_exposure == "symptomatic infection") %>%
  select(-c(village_name,aim2_exposure)) %>%
  rename("symp_average_p_te_all_csp" = average_p_te_all_csp)
csp_all = left_join(csp_asymp,csp_symp,by=c("unq_memID"))
csp_all$village_name.y <- NULL
csp_all = rename(csp_all,village_name = village_name.x)
# ama
ama_asymp = ama_average_data %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("asymp_average_p_te_all_ama" = average_p_te_all_ama)
ama_symp = ama_average_data %>%
  filter(aim2_exposure == "symptomatic infection") %>%
  select(-c(village_name,aim2_exposure)) %>%
  rename("symp_average_p_te_all_ama" = average_p_te_all_ama)
ama_all = left_join(ama_asymp,ama_symp,by=c("unq_memID"))
ama_all$village_name.y <- NULL
ama_all = rename(ama_all,village_name = village_name.x)

# now make a plot of the average probability of p_te_all by symptomatic status
# for csp
csp_plot = ggplot(csp_all, aes(x=symp_average_p_te_all_csp, y=asymp_average_p_te_all_csp)) +
  geom_point(aes(color=village_name),size=2.5,pch=21) + 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  xlab("Average probability of transmission for symptomatic infections") +
  ylab("Average probability of transmission for asymptomatic infections") +
  labs(color="Village name") +
  scale_x_continuous(limits=c(0,0.4)) +
  scale_y_continuous(limits=c(0,0.4)) +
  scale_color_manual(values = c("#377eb8","#66bd63","#fb9a99"))
ggsave(csp_plot, filename="/Users/kelseysumner/Desktop/csp_matched_prob_plot.png", device="png",
    height=5, width=6, units="in", dpi=500)
# for ama
ama_plot = ggplot(ama_all, aes(x=symp_average_p_te_all_ama, y=asymp_average_p_te_all_ama)) +
  geom_point(aes(color=village_name),size=2.5,pch=21) + 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  xlab("Average probability of transmission for symptomatic infections") +
  ylab("Average probability of transmission for asymptomatic infections") +
  labs(color="Village name") +
  scale_x_continuous(limits=c(0,0.4)) +
  scale_y_continuous(limits=c(0,0.4)) +
  scale_color_manual(values = c("#377eb8","#66bd63","#fb9a99"))
ggsave(ama_plot, filename="/Users/kelseysumner/Desktop/ama_matched_prob_plot.png", device="png",
       height=5, width=6, units="in", dpi=500)

# check the mean of means by symptomatic status
# for csp
mean(csp_all$asymp_average_p_te_all_csp)
mean(csp_all$symp_average_p_te_all_csp)
# for ama
mean(ama_all$asymp_average_p_te_all_ama)
mean(ama_all$symp_average_p_te_all_ama)

# check normality (but N>30 so not a large issue)
# for csp
d <- csp_all$asymp_average_p_te_all_csp-csp_all$symp_average_p_te_all_csp
shapiro.test(d) # normality not an issue
# for ama
d <- ama_all$asymp_average_p_te_all_ama-ama_all$symp_average_p_te_all_ama
shapiro.test(d) # normality could be an issue but N>30 so central limit theorem applies

# paired t-test
t.test(csp_all$asymp_average_p_te_all_csp, csp_all$symp_average_p_te_all_csp, paired = TRUE, alternative = "greater")
t.test(ama_all$asymp_average_p_te_all_ama, ama_all$symp_average_p_te_all_ama, paired = TRUE, alternative = "greater")




