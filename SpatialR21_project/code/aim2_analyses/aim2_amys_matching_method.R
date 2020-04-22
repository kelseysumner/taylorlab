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
library(BSDA)


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
  summarize(average_p_te_all_csp=mean(p_te_all_csp,na.rm=T), median_p_te_all_csp = median(p_te_all_csp,na.rm = T))
# for ama
ama_average_data = ama_subset_data %>%
  group_by(village_name,unq_memID,aim2_exposure) %>%
  summarize(average_p_te_all_ama=mean(p_te_all_ama,na.rm=T), median_p_te_all_ama = median(p_te_all_ama,na.rm = T))  

# now split up the data sets so have separate columns for asymptomatic and symptomatic infections
# for csp
csp_asymp = csp_average_data %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("asymp_average_p_te_all_csp" = average_p_te_all_csp,"asymp_median_p_te_all_csp" = median_p_te_all_csp)
csp_symp = csp_average_data %>%
  filter(aim2_exposure == "symptomatic infection") %>%
  select(-c(village_name,aim2_exposure)) %>%
  rename("symp_average_p_te_all_csp" = average_p_te_all_csp,"symp_median_p_te_all_csp" = median_p_te_all_csp)
csp_all = left_join(csp_asymp,csp_symp,by=c("unq_memID"))
csp_all$village_name.y <- NULL
csp_all = rename(csp_all,village_name = village_name.x)
# ama
ama_asymp = ama_average_data %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("asymp_average_p_te_all_ama" = average_p_te_all_ama,"asymp_median_p_te_all_ama" = median_p_te_all_ama)
ama_symp = ama_average_data %>%
  filter(aim2_exposure == "symptomatic infection") %>%
  select(-c(village_name,aim2_exposure)) %>%
  rename("symp_average_p_te_all_ama" = average_p_te_all_ama,"asymp_median_p_te_all_ama" = median_p_te_all_ama)
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
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
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

# check the median of means by symptomatic status
# for csp
median(csp_all$asymp_average_p_te_all_csp)
median(csp_all$symp_average_p_te_all_csp)
# for ama
median(ama_all$asymp_average_p_te_all_ama)
median(ama_all$symp_average_p_te_all_ama)

# check normality (but N>30 so not a large issue)
# for csp
d <- csp_all$asymp_average_p_te_all_csp-csp_all$symp_average_p_te_all_csp
shapiro.test(d) # normality not an issue
hist(d)
# for ama
d <- ama_all$asymp_average_p_te_all_ama-ama_all$symp_average_p_te_all_ama
shapiro.test(d) # normality could be an issue but N>30 so central limit theorem applies
hist(d)

# paired t-test
t.test(csp_all$asymp_average_p_te_all_csp, csp_all$symp_average_p_te_all_csp, paired = TRUE, alternative = "greater")
t.test(ama_all$asymp_average_p_te_all_ama, ama_all$symp_average_p_te_all_ama, paired = TRUE, alternative = "greater")

# sign test just to be careful
SIGN.test(csp_all$asymp_average_p_te_all_csp, csp_all$symp_average_p_te_all_csp,md=0,alternative = "greater")
SIGN.test(ama_all$asymp_average_p_te_all_ama, ama_all$symp_average_p_te_all_ama,md=0,alternative = "greater")



#### ----- try alternative plot 1: median value for symptomatic status ------ ####

# only do for csp right now

# now make a plot of the average probability of p_te_all by symptomatic status
csp_plot = ggplot(csp_all, aes(x=symp_median_p_te_all_csp, y=asymp_median_p_te_all_csp)) +
  geom_point(aes(color=village_name),size=2.5,pch=21) + 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  xlab("Median probability of transmission for symptomatic infections") +
  ylab("Median probability of transmission for asymptomatic infections") +
  labs(color="Village name") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  scale_color_manual(values = c("#377eb8","#66bd63","#fb9a99"))
ggsave(csp_plot, filename="/Users/kelseysumner/Desktop/csp_matched_prob_plot_alt1.png", device="png",
       height=5, width=6, units="in", dpi=500)

# check the median of medians by symptomatic status
# for csp
median(csp_all$asymp_median_p_te_all_csp)
median(csp_all$symp_median_p_te_all_csp)

# sign test
SIGN.test(csp_all$asymp_median_p_te_all_csp, csp_all$symp_median_p_te_all_csp,md=0,alternative = "greater")



#### ----- try alternative plot 2: choose highest probability of transmission value for each person's infection ------ ####

# what this does: choose highest probability of transmission value for each person's infection then take the median
# across symptomatic status

# only do for csp right now

# first write some code to choose the highest probability values
csp_high_data = csp_subset_data %>%
  group_by(village_name,unq_memID,aim2_exposure,sample_id_human) %>%
  summarize(max_p_te_all_csp = max(p_te_all_csp)) %>%
  group_by(village_name,unq_memID,aim2_exposure) %>%
  summarize(median_max_p_te_all_csp = median(max_p_te_all_csp,na.rm=T))

# now split up the data sets so have separate columns for asymptomatic and symptomatic infections
# for csp
csp_asymp = csp_high_data %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("asymp_median_max_p_te_all_csp" = median_max_p_te_all_csp,"asymp_median_max_p_te_all_csp" = median_max_p_te_all_csp)
csp_symp = csp_high_data %>%
  filter(aim2_exposure=="symptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("symp_median_max_p_te_all_csp" = median_max_p_te_all_csp,"symp_median_max_p_te_all_csp" = median_max_p_te_all_csp)
csp_all = left_join(csp_asymp,csp_symp,by=c("unq_memID"))
csp_all$village_name.y <- NULL
csp_all = rename(csp_all,village_name = village_name.x)
  
# now make a plot of the average probability of p_te_all by symptomatic status
csp_plot = ggplot(csp_all, aes(x=symp_median_max_p_te_all_csp, y=asymp_median_max_p_te_all_csp)) +
  geom_point(aes(color=village_name),size=2.5,pch=21) + 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  xlab("Median max probability of transmission for symptomatic infections") +
  ylab("Median max probability of transmission for asymptomatic infections") +
  labs(color="Village name") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  scale_color_manual(values = c("#377eb8","#66bd63","#fb9a99"))
ggsave(csp_plot, filename="/Users/kelseysumner/Desktop/csp_matched_prob_plot_alt2.png", device="png",
       height=5, width=6, units="in", dpi=500)

# check the median of medians by symptomatic status
# for csp
median(csp_all$asymp_median_max_p_te_all_csp)
median(csp_all$symp_median_max_p_te_all_csp)

# sign test
SIGN.test(csp_all$asymp_median_max_p_te_all_csp, csp_all$symp_median_max_p_te_all_csp,md=0,alternative = "greater")



#### -------- try alternative plot 3: compute proportion nonzero pairings with a mosquito ------ ####

# what this does: for each person's infection, compute the proportion of nonzero pairings with mosquito
# then take the median of that proportion across symptomatic status

# only for csp right now

# create a variable that says whether or not p_te_all_csp is non-zero or not
csp_subset_data$nonzero = ifelse(csp_subset_data$p_te_all_csp > 0,"nonzero","zero")
table(csp_subset_data$nonzero,useNA = "always")
length(which(csp_subset_data$p_te_all_csp == 0))

# for each person's infection, count the number of nonzero pairings
csp_part1_data = csp_subset_data %>%
  filter(nonzero == "nonzero") %>%
  group_by(village_name,unq_memID,aim2_exposure,sample_id_human) %>%
  summarize(numerator=n())
csp_nonzero_data = csp_subset_data %>%
  group_by(village_name,unq_memID,aim2_exposure,sample_id_human) %>%
  summarize(denominator = n())

# join the data frames
csp_all_nonzero_data = left_join(csp_nonzero_data,csp_part1_data,by=c("sample_id_human"))

# clean up the data frames
csp_all_nonzero_data$village_name.y <- NULL
csp_all_nonzero_data$aim2_exposure.y <- NULL
csp_all_nonzero_data$unq_memID.y <- NULL
csp_all_nonzero_data = csp_all_nonzero_data %>% rename(village_name = village_name.x,unq_memID=unq_memID.x,aim2_exposure=aim2_exposure.x)

# change the numerators that are NA to 0
csp_all_nonzero_data$numerator[which(is.na(csp_all_nonzero_data$numerator))] = 0

# calculate the proportion of infections that are nonzero
csp_all_nonzero_data$prop_nonzero = csp_all_nonzero_data$numerator/csp_all_nonzero_data$denominator

# now take the median of that proportion by symptomatic status
csp_final_data = csp_all_nonzero_data %>%
  group_by(village_name,unq_memID,aim2_exposure) %>%
  summarize(median_prop_nonzero = median(prop_nonzero,na.rm = T))

# now split up the data sets so have separate columns for asymptomatic and symptomatic infections
# for csp
csp_asymp = csp_final_data %>%
  filter(aim2_exposure=="asymptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("asymp_median_prop_nonzero" = median_prop_nonzero)
csp_symp = csp_final_data %>%
  filter(aim2_exposure=="symptomatic infection") %>%
  select(-c(aim2_exposure)) %>%
  rename("symp_median_prop_nonzero" = median_prop_nonzero)
csp_all = left_join(csp_asymp,csp_symp,by=c("unq_memID"))
csp_all$village_name.y <- NULL
csp_all = rename(csp_all,village_name = village_name.x)

# now make a plot of the average probability of p_te_all by symptomatic status
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

# check the median of medians by symptomatic status
# for csp
median(csp_all$asymp_median_prop_nonzero)
median(csp_all$symp_median_prop_nonzero)

# sign test
SIGN.test(csp_all$asymp_median_prop_nonzero, csp_all$symp_median_prop_nonzero,md=0,alternative = "greater")


