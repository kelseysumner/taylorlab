# -------------------------------------- #
#           Spat21/Mozzie Study          #
#          Risk factor analysis          #
#            Mozzie Phase 1              #
#               K. Sumner                #
#             March 4, 2021              #
# -------------------------------------- #

# good resource for trouble shooting convergence problems
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)
library(lme4)
library(ggplot2)
library(sjstats)
library(lmerTest)
library(glmmTMB)
library(tibble)


#### ----- read in the data sets ----- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA_and_CSP/final/model data/final_model_data/spat21_aim2_merged_data_with_weights_5MAR2020.rds")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")


#### ------ cut down the model data set to just the variables of interest --------- ####

# cut down to just the variables of interest and create new categorizations when needed
colnames(model_data)
model_data = model_data %>%
  select(sample_id_human,sample_id_abdomen,human_date,mosquito_date,sample_name_final,unq_memID,village_name,HH_ID_human,HH_ID_mosquito,age_cat_baseline,aim2_exposure,pfr364Q_std_combined,mosquito_week_count_cat,csp_haps_shared,ama_haps_shared,p_te_all_csp,p_te_all_ama) %>%
  mutate(parasite_density=ifelse(pfr364Q_std_combined < 1, "<1",ifelse(pfr364Q_std_combined >= 1 & pfr364Q_std_combined <= 200,"1-200",ifelse(pfr364Q_std_combined > 200,">200",NA))),
         transmission_season=ifelse(mosquito_week_count_cat=="<75 mosquitoes","low",ifelse(mosquito_week_count_cat=="75-147 mosquitoes","high",NA))) %>%
  rename(age=age_cat_baseline,infection_type=aim2_exposure) %>%
  select(-c(pfr364Q_std_combined,mosquito_week_count_cat))
colnames(model_data)


#### ----- now merge in the variables you need from the demographic data set --------- ####

# first cut down the demographic data set to the variables of interest
final_data = final_data %>%
  select(sample_name_final,gender,slept_times) %>%
  mutate(current_bed_net_usage = ifelse(slept_times > 5,"yes",ifelse(slept_times >= 0 & slept_times <= 5,"no",NA))) %>%
  rename(sex=gender) %>%
  select(-c("slept_times"))

# merge the demographic variables into the model data set
model_data = left_join(model_data,final_data,by="sample_name_final")

# check the merge
table(model_data$sex,useNA = "always")
table(model_data$current_bed_net_usage,useNA = "always")

# create a variable for non-zero p_te_all
model_data$csp_transmission = ifelse(model_data$p_te_all_csp > 0,"yes",ifelse(model_data$p_te_all_csp == 0,"no",NA))
model_data$ama_transmission = ifelse(model_data$p_te_all_ama > 0,"yes",ifelse(model_data$p_te_all_ama == 0,"no",NA))


#### ------ check covariate coding ------- ####

# first look at the columns
colnames(model_data)

# now make sure the covariates are coded correctly

# sex
str(model_data$sex)
model_data$sex = factor(model_data$sex,levels=c("female","male"))

# age
str(model_data$age)
model_data$age = factor(model_data$age,levels=c("<5 years","5-15 years",">15 years"))

# parasite density
str(model_data$parasite_density)
model_data$parasite_density = factor(model_data$parasite_density,levels=c("<1","1-200",">200"))

# current bed net usage
str(model_data$current_bed_net_usage)
model_data$current_bed_net_usage = factor(model_data$current_bed_net_usage,levels=c("no","yes"))

# transmission season
str(model_data$transmission_season)
model_data$transmission_season = factor(model_data$transmission_season,levels=c("low","high"))

# infection type
str(model_data$infection_type)

# transmission
# for csp
str(model_data$csp_transmission)
model_data$csp_transmission = factor(model_data$csp_transmission,levels=c("no","yes"))
# for ama
str(model_data$ama_transmission)
model_data$ama_transmission = factor(model_data$ama_transmission,levels=c("no","yes"))


# create a variable for transmission that just uses the mean P_te_all value
# for csp
mean_csp = mean(model_data$p_te_all_csp)
model_data$csp_transmission_mean = ifelse(model_data$p_te_all_csp >= mean_csp,"yes",ifelse(model_data$p_te_all_csp < mean_csp,"no",NA))
model_data$csp_transmission_mean = factor(model_data$csp_transmission_mean,levels=c("no","yes"))
# for ama
mean_ama = mean(model_data$p_te_all_ama)
model_data$ama_transmission_mean = ifelse(model_data$p_te_all_ama >= mean_ama,"yes",ifelse(model_data$p_te_all_ama < mean_ama,"no",NA))
model_data$ama_transmission_mean = factor(model_data$ama_transmission_mean,levels=c("no","yes"))


# create a variable for transmission that uses 0.2 as the cutoff
# for csp
model_data$csp_transmission_0.2 = ifelse(model_data$p_te_all_csp >= 0.2,"yes",ifelse(model_data$p_te_all_csp < 0.2,"no",NA))
model_data$csp_transmission_0.2 = factor(model_data$csp_transmission_0.2,levels=c("no","yes"))
# for ama
model_data$ama_transmission_0.2 = ifelse(model_data$p_te_all_ama >= 0.2,"yes",ifelse(model_data$p_te_all_ama < 0.2,"no",NA))
model_data$ama_transmission_0.2 = factor(model_data$ama_transmission_0.2,levels=c("no","yes"))


# export the data set
#write_csv(model_data,"Desktop/mozzie_rfa_data_1APR2021.csv")
#write_rds(model_data,"Desktop/mozzie_rfa_data_1APR2021.rds")


#### ------ compare PTEall across covariates for csp ------- ####

# first subset to just the csp samples (some only amplified for csp vs. ama)
model_data = model_data %>%
  filter(!(is.na(csp_haps_shared)))

# now create values for table 1 

# sex
# summaries
table(model_data$sex,useNA = "always")
model_data %>%
  group_by(sex) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
model_data %>%
  group_by(sex,csp_transmission) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
sex_model <- glmer(csp_transmission~sex+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(sex_model)
performance::icc(sex_model)
exp(-0.1765)
exp(confint(sex_model,method="Wald"))


# age
# summaries
table(model_data$age,useNA = "always")
model_data %>%
  group_by(age) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
model_data %>%
  group_by(age,csp_transmission) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
age_model <- glmer(csp_transmission~age+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(age_model)
performance::icc(age_model)
exp(-0.14489)
exp(-0.07066)
exp(confint(age_model,method="Wald"))


# parasite density
# summaries
table(model_data$parasite_density,useNA = "always")
model_data %>%
  group_by(parasite_density) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
model_data %>%
  group_by(parasite_density,csp_transmission) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
parasite_density_model <- glmer(csp_transmission~parasite_density+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(parasite_density_model)
performance::icc(parasite_density_model)
exp(-0.9767)
exp(-2.7124)
exp(confint(parasite_density_model,method="Wald"))


# current regular bed net usage
# summaries
table(model_data$current_bed_net_usage,useNA = "always")
model_data %>%
  group_by(current_bed_net_usage) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
model_data %>%
  group_by(current_bed_net_usage,csp_transmission) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
current_bed_net_usage_model <- glmer(csp_transmission~current_bed_net_usage+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(current_bed_net_usage_model)
performance::icc(current_bed_net_usage_model)
exp(-0.5667)
exp(confint(current_bed_net_usage_model,method="Wald"))


# transmission season
# summaries
table(model_data$transmission_season,useNA = "always")
model_data %>%
  group_by(transmission_season) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
model_data %>%
  group_by(transmission_season,csp_transmission) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
transmission_season_model <- glmer(csp_transmission~transmission_season+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(transmission_season_model)
performance::icc(transmission_season_model)
exp(-0.11468)
exp(confint(transmission_season_model,method="Wald"))


# infection type
# summaries
table(model_data$infection_type,useNA = "always")
model_data %>%
  group_by(infection_type) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
model_data %>%
  group_by(infection_type,csp_transmission) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
infection_type_model <- glmer(csp_transmission~infection_type+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(infection_type_model)
performance::icc(infection_type_model)
exp(1.0824)
exp(confint(infection_type_model,method="Wald"))



#### -------- create a multivariable model with covariates with p < 0.02 -------- ####

# covariates to include in multivariate model:
#  parasite density, bed net usage, transmission season, and infection type

# run the multivariate model
multivariate_model <- glmer(csp_transmission~parasite_density+current_bed_net_usage+transmission_season+infection_type+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(multivariate_model)
performance::icc(multivariate_model)
exp(-1.0156)
exp(-2.6349)
exp(-0.7107)
exp(0.3265)
exp(0.1133)
exp(confint(multivariate_model,method="Wald"))

