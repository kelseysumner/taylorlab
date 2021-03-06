# ---------------------------- #
#    Statistically compare     #
#     MOI for data set         #
#        for csp               #
#      Aim 2 manuscript        #
#      April 17, 2020          #
#           K. Sumner          #
# ---------------------------- #

#### ------- load libraries ------ ####

# load tidyverse
library(tidyverse)



#### -------- read in the data sets -------- ####

# read in the csp haplotype data
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
csp_haplotypes <- read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# read in the csp haplotype data
# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
ama_haplotypes <- read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA/spat21_AMA_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_15OCT2019.rds")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets//Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA_and_CSP/final/model data/final_model_data/spat21_aim2_merged_data_with_weights_5MAR2020.rds")
# subset the data set to samples that passed pfcsp sequencing only
model_data = model_data %>%
  filter(!(is.na(csp_haps_shared)))



#### ------ set up the data set ------ ####

# make separate data sets for humans and mosquitoes
human_haps = csp_haplotypes %>%
  filter(sample_type=="Human")
abdomen_haps = csp_haplotypes %>%
  filter(sample_type=="Abdomen")

# merge the final_data info for symptomatic status with the human haps
cut_data = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection") %>%
  select(sample_name_dbs,main_exposure_primary_case_def,main_outcome_primary_case_def) %>%
  mutate(aim2_exposure = ifelse(is.na(main_exposure_primary_case_def),as.character(main_outcome_primary_case_def),as.character(main_exposure_primary_case_def)))
table(cut_data$aim2_exposure, useNA = "always")
human_haps = left_join(human_haps,cut_data,by="sample_name_dbs")
table(human_haps$aim2_exposure, useNA = "always")
colnames(human_haps)
asymp_human_haps = human_haps %>% filter(aim2_exposure == "asymptomatic infection")
symp_human_haps = human_haps %>% filter(aim2_exposure == "symptomatic infection")

# create the data set
asymp_human_haps = asymp_human_haps %>%
  select(haplotype_number) %>%
  mutate(type = rep("Asymptomatic infection",nrow(asymp_human_haps)))
symp_human_haps = symp_human_haps %>%
  select(haplotype_number) %>%
  mutate(type = rep("Symptomatic infection",nrow(symp_human_haps)))
abdomen_haps = abdomen_haps %>%
  select(haplotype_number) %>%
  mutate(type = rep("Abdomen",nrow(abdomen_haps)))
all_data = rbind(asymp_human_haps,symp_human_haps,abdomen_haps)


#### ----- run the statistical test ------- ####

# look at a summary of the combined data set
group_by(all_data, type) %>%
  summarise(
    count = n(),
    mean = mean(haplotype_number, na.rm = TRUE),
    sd = sd(haplotype_number, na.rm = TRUE),
    median = median(haplotype_number, na.rm = TRUE),
    IQR = IQR(haplotype_number, na.rm = TRUE),
    first_quartile = quantile(haplotype_number, 0.25),
    third_quartile = quantile(haplotype_number, 0.75)
  )

# test anova assumptions of data be normally distributed and variance equal between groups
boxplot(haplotype_number ~ type, data = all_data,
        xlab = "type", ylab = "Haplotype Number",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
shapiro.test(asymp_human_haps$haplotype_number)
shapiro.test(symp_human_haps$haplotype_number)
shapiro.test(abdomen_haps$haplotype_number)
# distributions aren't normal but N>30 so CLT should apply
library(car)
leveneTest(haplotype_number ~ type, data = all_data) # variances are different so need to do kruskal wallis test

# Compute the kruskal wallis rank sum test
kruskal.test(haplotype_number ~ type, data = all_data)
# there are significant difference between the groups

# do a test for multiple pairwise-comparisons between groups
# have a bonferroni correction for multiple testing
pairwise.wilcox.test(all_data$haplotype_number, all_data$type,
                     p.adjust.method = "bonferroni") # chose bonferroni because more conservative than FDR
# results show that all pairwise comparisons statistically different

# now adjust p-values for up to 14 infections observed using the bonferroni correction (very conservative)
2.7e-14*14
2e-16*14
8.4e-10*14


#### ------- additional statistical comparisons for covariates ------- ####

# look at differences in participant age
# first make a contingency table
tbl = table(model_data$age_cat_baseline,model_data$aim2_exposure)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
2.2e-16*14

# look at differences in parasite density
# first set up the df
asymp_parasite_density = model_data %>%
  filter(aim2_exposure == "asymptomatic infection") %>%
  select(pfr364Q_std_combined) %>%
  mutate(type = rep("asymptomatic infection",nrow(asymp_parasite_density)))
symp_parasite_density = model_data %>%
  filter(aim2_exposure == "symptomatic infection") %>%
  select(pfr364Q_std_combined) %>%
  mutate(type = rep("symptomatic infection",nrow(symp_parasite_density)))
all_data = rbind(asymp_parasite_density,symp_parasite_density)
# test for normality - data not normal
shapiro.test(asymp_parasite_density$pfr364Q_std_combined)
shapiro.test(symp_parasite_density$pfr364Q_std_combined)
# test for equal variances
(sd(asymp_parasite_density$pfr364Q_std_combined))^2
(sd(symp_parasite_density$pfr364Q_std_combined))^2
leveneTest(pfr364Q_std_combined ~ type, data = all_data) # variances are different
# test if data roughly same shape - seem to be
hist(asymp_parasite_density$pfr364Q_std_combined)
hist(symp_parasite_density$pfr364Q_std_combined)
# wilcoxon-mann-whitney test
wilcox.test(pfr364Q_std_combined ~ type,data=all_data)
# correct for repeated measures
2.2e-16*14
# look at IQR
group_by(all_data, type) %>%
  summarise(
    count = n(),
    first_quartile = quantile(pfr364Q_std_combined, 0.25),
    third_quartile = quantile(pfr364Q_std_combined, 0.75)
  )

# look at differences in mosquitoes collected in week following infection
# first make a contingency table
tbl = table(model_data$mosquito_week_count_cat,model_data$aim2_exposure)
# then do a chi-squared test
chisq.test(tbl)
# test for repeated measures
2.2e-16*14

# look at differences in moi
# first set up the df
asymp_moi = model_data %>%
  filter(aim2_exposure == "asymptomatic infection") %>%
  select(csp_moi) %>%
  mutate(type = rep("asymptomatic infection",nrow(asymp_moi)))
symp_moi = model_data %>%
  filter(aim2_exposure == "symptomatic infection") %>%
  select(csp_moi) %>%
  mutate(type = rep("symptomatic infection",nrow(symp_moi)))
all_data = rbind(asymp_moi,symp_moi)
# test for normality - data not normal
shapiro.test(asymp_moi$csp_moi)
shapiro.test(symp_moi$csp_moi)
# test for equal variances
(sd(asymp_moi$csp_moi))^2
(sd(symp_moi$csp_moi))^2
leveneTest(csp_moi ~ type, data = all_data) # variances are different
# test if data roughly same shape - seem to be
hist(asymp_moi$csp_moi)
hist(symp_moi$csp_moi)
# wilcoxon-mann-whitney test
wilcox.test(csp_moi ~ type,data=all_data)
# correct for repeated measures
2.2e-16*14
# look at IQR
group_by(all_data, type) %>%
  summarise(
    count = n(),
    first_quartile = quantile(csp_moi, 0.25),
    third_quartile = quantile(csp_moi, 0.75)
  )

# look at differences in village
# first make a contingency table
tbl = table(model_data$village_name,model_data$aim2_exposure)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
2.2e-16*14

# look at differences in p_te_all_csp
# first set up the df
asymp_all = model_data %>%
  filter(aim2_exposure == "asymptomatic infection") %>%
  select(p_te_all_csp) %>%
  mutate(type = rep("asymptomatic infection",nrow(asymp_all)))
symp_all = model_data %>%
  filter(aim2_exposure == "symptomatic infection") %>%
  select(p_te_all_csp) %>%
  mutate(type = rep("symptomatic infection",nrow(symp_all)))
all_data = rbind(asymp_all,symp_all)
# test for normality - data not normal
shapiro.test(asymp_all$p_te_all_csp)
shapiro.test(symp_all$p_te_all_csp)
# test for equal variances
(sd(asymp_all$p_te_all_csp))^2
(sd(symp_all$p_te_all_csp))^2
leveneTest(p_te_all_csp ~ type, data = all_data) # variances are different
# test if data roughly same shape - seem to be
hist(asymp_all$p_te_all_csp)
hist(symp_all$p_te_all_csp)
# wilcoxon-mann-whitney test
wilcox.test(p_te_all_csp ~ type,data=all_data)
# correct for repeated measures
8.521e-15*14
# look at IQR
group_by(all_data, type) %>%
  summarise(
    count = n(),
    first_quartile = quantile(p_te_all_csp, 0.25),
    third_quartile = quantile(p_te_all_csp, 0.75)
  )

# look at differences in p_te_d
# first set up the df
asymp_all = model_data %>%
  filter(aim2_exposure == "asymptomatic infection") %>%
  select(p_te_d) %>%
  mutate(type = rep("asymptomatic infection",nrow(asymp_all)))
symp_all = model_data %>%
  filter(aim2_exposure == "symptomatic infection") %>%
  select(p_te_d) %>%
  mutate(type = rep("symptomatic infection",nrow(symp_all)))
all_data = rbind(asymp_all,symp_all)
# test for normality - data not normal
shapiro.test(asymp_all$p_te_d)
shapiro.test(symp_all$p_te_d)
# test for equal variances
(sd(asymp_all$p_te_d))^2
(sd(symp_all$p_te_d))^2
leveneTest(p_te_d ~ type, data = all_data) # variances are roughly the same
# test if data roughly same shape - seem to be
hist(asymp_all$p_te_d)
hist(symp_all$p_te_d)
# wilcoxon-mann-whitney test
wilcox.test(p_te_d ~ type,data=all_data)
# correct for repeated measures
0.8211*14
# look at IQR
group_by(all_data, type) %>%
  summarise(
    count = n(),
    first_quartile = quantile(p_te_d, 0.25),
    third_quartile = quantile(p_te_d, 0.75)
  )

# look at differences in p_te_c
# first set up the df
asymp_all = model_data %>%
  filter(aim2_exposure == "asymptomatic infection") %>%
  select(p_te_c_alt) %>%
  mutate(type = rep("asymptomatic infection",nrow(asymp_all)))
symp_all = model_data %>%
  filter(aim2_exposure == "symptomatic infection") %>%
  select(p_te_c_alt) %>%
  mutate(type = rep("symptomatic infection",nrow(symp_all)))
all_data = rbind(asymp_all,symp_all)
# test for normality - data not normal
shapiro.test(asymp_all$p_te_c_alt)
shapiro.test(symp_all$p_te_c_alt)
# test for equal variances
(sd(asymp_all$p_te_c_alt))^2
(sd(symp_all$p_te_c_alt))^2
leveneTest(p_te_c_alt ~ type, data = all_data) # variances are not the same
# test if data roughly same shape - seem to be
hist(asymp_all$p_te_c_alt)
hist(symp_all$p_te_c_alt)
# wilcoxon-mann-whitney test
wilcox.test(p_te_c_alt ~ type,data=all_data)
# correct for repeated measures
3.599e-15*14
# look at IQR
group_by(all_data, type) %>%
  summarise(
    count = n(),
    first_quartile = quantile(p_te_c_alt, 0.25),
    third_quartile = quantile(p_te_c_alt, 0.75)
  )

# look at differences in p_te_c for those that shared haplotypes
# first set up the df
asymp_all = model_data %>%
  filter(aim2_exposure == "asymptomatic infection" & p_te_c_alt > 0) %>%
  select(p_te_c_alt) %>%
  mutate(type = rep("asymptomatic infection",nrow(asymp_all)))
symp_all = model_data %>%
  filter(aim2_exposure == "symptomatic infection" &  p_te_c_alt > 0) %>%
  select(p_te_c_alt) %>%
  mutate(type = rep("symptomatic infection",nrow(symp_all)))
all_data = rbind(asymp_all,symp_all)
# test for normality - data not normal
shapiro.test(asymp_all$p_te_c_alt)
shapiro.test(symp_all$p_te_c_alt)
# test for equal variances
(sd(asymp_all$p_te_c_alt))^2
(sd(symp_all$p_te_c_alt))^2
leveneTest(p_te_c_alt ~ type, data = all_data) # variances are roughly the same
# test if data roughly same shape - seem to be
hist(asymp_all$p_te_c_alt)
hist(symp_all$p_te_c_alt)
# wilcoxon-mann-whitney test
wilcox.test(p_te_c_alt ~ type,data=all_data)
# adjust for repeated measures
0.1698*14
# look at IQR
group_by(all_data, type) %>%
  summarise(
    count = n(),
    first_quartile = quantile(p_te_c_alt, 0.25),
    third_quartile = quantile(p_te_c_alt, 0.75)
  )

# look at differences number of haplotypes shared
# first set up the df
asymp_all = model_data %>%
  filter(aim2_exposure == "asymptomatic infection") %>%
  select(csp_haps_shared) %>%
  mutate(type = rep("asymptomatic infection",nrow(asymp_all)))
symp_all = model_data %>%
  filter(aim2_exposure == "symptomatic infection") %>%
  select(csp_haps_shared) %>%
  mutate(type = rep("symptomatic infection",nrow(symp_all)))
all_data = rbind(asymp_all,symp_all)
# test for normality - data not normal
shapiro.test(asymp_all$csp_haps_shared)
shapiro.test(symp_all$csp_haps_shared)
# test for equal variances
(sd(asymp_all$csp_haps_shared))^2
(sd(symp_all$csp_haps_shared))^2
leveneTest(csp_haps_shared ~ type, data = all_data) # variances are not the same
# test if data roughly same shape - seem to be
hist(asymp_all$csp_haps_shared)
hist(symp_all$csp_haps_shared)
# wilcoxon-mann-whitney test
wilcox.test(csp_haps_shared ~ type,data=all_data)
# adjust for repeated measures
2.2e-16*14
# look at IQR
group_by(all_data, type) %>%
  summarise(
    count = n(),
    first_quartile = quantile(csp_haps_shared, 0.25),
    third_quartile = quantile(csp_haps_shared, 0.75)
  )

# look at differences number of haplotypes shared
# first set up the df
asymp_all = model_data %>%
  filter(aim2_exposure == "asymptomatic infection" & csp_haps_shared > 0) %>%
  select(csp_haps_shared) %>%
  mutate(type = rep("asymptomatic infection",nrow(asymp_all)))
symp_all = model_data %>%
  filter(aim2_exposure == "symptomatic infection" & csp_haps_shared > 0) %>%
  select(csp_haps_shared) %>%
  mutate(type = rep("symptomatic infection",nrow(symp_all)))
all_data = rbind(asymp_all,symp_all)
# test for normality - data not normal
shapiro.test(asymp_all$csp_haps_shared)
shapiro.test(symp_all$csp_haps_shared)
# test for equal variances
(sd(asymp_all$csp_haps_shared))^2
(sd(symp_all$csp_haps_shared))^2
leveneTest(csp_haps_shared ~ type, data = all_data) # variances are not the same
# test if data roughly same shape - seem to be
hist(asymp_all$csp_haps_shared)
hist(symp_all$csp_haps_shared)
# wilcoxon-mann-whitney test
wilcox.test(csp_haps_shared ~ type,data=all_data)
# adjust for repeated measures
2.2e-16*14
# look at IQR
group_by(all_data, type) %>%
  summarise(
    count = n(),
    first_quartile = quantile(csp_haps_shared, 0.25),
    third_quartile = quantile(csp_haps_shared, 0.75)
  )
