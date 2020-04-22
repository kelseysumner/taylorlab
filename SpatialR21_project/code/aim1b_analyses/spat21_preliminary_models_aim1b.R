# -------------------------------- #
#       Preliminary results        #
#         Mozzie phase 1           #
#             Aim 1B               #
#        March 31, 2020            #
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
ama_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/data_without_first_infection/without_first_infection_ama_data_spat21_aim1b_14APR2020.rds")

# read in the csp data set
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/data_without_first_infection/without_first_infection_csp_data_spat21_aim1b_14APR2020.rds")

# read in the full human demographic data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")
final_data = final_data %>%
  select(c(sample_name_dbs,HH_ID,gender))


#### ---- make sure the data is all in the proper format ------ ####


# add household ID to the data sets
csp_data = left_join(csp_data,final_data,by="sample_name_dbs")
ama_data = left_join(ama_data,final_data,by="sample_name_dbs")

# rescale days in study
csp_data$rescaled_days_in_study = scale(csp_data$days_in_study)
ama_data$rescaled_days_in_study = scale(ama_data$days_in_study)

# now clean up the variables to be in the proper str
# for csp
str(csp_data$sample_name_dbs)
str(csp_data$unq_memID)
str(csp_data$HH_ID)
csp_data$any_new_categories = as.factor(csp_data$any_new_categories)
levels(csp_data$any_new_categories)
csp_data$any_new_categories = relevel(csp_data$any_new_categories,ref="none new")
str(csp_data$symptomatic_status)
csp_data$symptomatic_status = as.factor(csp_data$symptomatic_status)
levels(csp_data$symptomatic_status)
str(csp_data$age_cat_baseline)
csp_data$age_cat_baseline = as.factor(csp_data$age_cat_baseline)
levels(csp_data$age_cat_baseline)
str(csp_data$village_name)
csp_data$village_name = as.factor(csp_data$village_name)
csp_data$village_name = relevel(csp_data$village_name,ref="Maruti")
str(csp_data$month)
# for ama
str(ama_data$sample_name_dbs)
str(ama_data$unq_memID)
str(ama_data$HH_ID)
ama_data$any_new_categories = as.factor(ama_data$any_new_categories)
levels(ama_data$any_new_categories)
ama_data$any_new_categories = relevel(ama_data$any_new_categories,ref="none new")
str(ama_data$symptomatic_status)
ama_data$symptomatic_status = as.factor(ama_data$symptomatic_status)
levels(ama_data$symptomatic_status)
str(ama_data$age_cat_baseline)
ama_data$age_cat_baseline = as.factor(ama_data$age_cat_baseline)
levels(ama_data$age_cat_baseline)
str(ama_data$village_name)
ama_data$village_name = as.factor(ama_data$village_name)
ama_data$village_name = relevel(ama_data$village_name,ref="Maruti")
str(ama_data$month)


#### ------ relook at summaries of the data sets ------ ####

# for pfcsp
summary(csp_data)

# for pfama1
summary(ama_data)


#### ------ make some plots of covariates ------- ####

# for csp
# for exposure
ggplot(csp_data, aes(x = symptomatic_status)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(csp_data, aes(x = age_cat_baseline)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(csp_data, aes(x = village_name)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(csp_data, aes(x = haplotype_number,fill=any_new_categories)) + geom_density() + facet_wrap(~any_new_categories) + theme_bw() + theme(legend.position="none") + xlab("pfcsp MOI")
table(csp_data$HH_ID,csp_data$any_new_categories)
table(csp_data$unq_memID,csp_data$any_new_categories)
# for outcome
ggplot(csp_data, aes(x = age_cat_baseline)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(csp_data, aes(x = village_name)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(csp_data, aes(x = haplotype_number,fill=symptomatic_status)) + geom_density() + facet_wrap(~symptomatic_status) + theme_bw() + theme(legend.position="none") + xlab("pfcsp MOI")
table(csp_data$HH_ID,csp_data$symptomatic_status)
table(csp_data$unq_memID,csp_data$symptomatic_status)

# for ama
# for exposure
ggplot(ama_data, aes(x = symptomatic_status)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(ama_data, aes(x = age_cat_baseline)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(ama_data, aes(x = village_name)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(ama_data, aes(x = haplotype_number,fill=any_new_categories)) + geom_density() + facet_wrap(~any_new_categories) + theme_bw() + theme(legend.position="none") + xlab("pfama MOI")
table(ama_data$HH_ID,ama_data$any_new_categories)
table(ama_data$unq_memID,ama_data$any_new_categories)
# for outcome
ggplot(ama_data, aes(x = age_cat_baseline)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(ama_data, aes(x = village_name)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(ama_data, aes(x = haplotype_number,fill=symptomatic_status)) + geom_density() + facet_wrap(~symptomatic_status) + theme_bw() + theme(legend.position="none") + xlab("pfcsp MOI")
table(ama_data$HH_ID,ama_data$symptomatic_status)
table(ama_data$unq_memID,ama_data$symptomatic_status)




#### ---- now run some preliminary models for csp: any new to no new ------- ####

# run a crude one level model (logistic regression for odds ratios)
csp_model_1levelcrude <- glm(symptomatic_status ~ any_new_categories,family=binomial(link = "logit"), data = csp_data)
summary(csp_model_1levelcrude)
exp(confint(csp_model_1levelcrude,method="Wald"))

# run a crude multilevel model
csp_model_crude <- glmer(symptomatic_status ~ any_new_categories + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_crude)
performance::icc(csp_model_crude)

# run a multi-level logistic regression model
csp_model_1 <- glmer(symptomatic_status ~ any_new_categories + age_cat_baseline + rescaled_days_in_study + any_new_categories*rescaled_days_in_study + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))

# make a forest plot of results without moi
table1 = exp(confint(csp_model_1,method="Wald"))
estimates = c(table1[2,3],table1[3,3],NA,table1[4,3],table1[5,3],NA,table1[6,3],table1[7,3])
lower_ci = c(table1[2,1],table1[3,1],NA,table1[4,1],table1[5,1],NA,table1[6,1],table1[7,1])
upper_ci = c(table1[2,2],table1[3,2],NA,table1[4,2],table1[5,2],NA,table1[6,2],table1[7,2])
names = c("All new haplotypes","New and recurrent haplotypes"," ","Participant age >15 years","Participant age 5-15 years","  ","Kinesamo village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("All new haplotypes","New and recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ","Kinesamo village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("All new haplotypes","New and recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ","Kinesamo village","Sitabicha village"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1),colour=c("#E1AF00","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_without_moi.png", device="png",
       height=9, width=12.5, units="in", dpi=400)



#### ---- now run some preliminary models for csp: all new to any old ------- ####

# run a crude one level model (logistic regression for odds ratios)
csp_model_1levelcrude <- glm(symptomatic_status ~ any_old_categories,family=binomial(link = "logit"), data = csp_data)
summary(csp_model_1levelcrude)
exp(confint(csp_model_1levelcrude,method="Wald"))

# run a crude multilevel model
csp_model_crude <- glmer(symptomatic_status ~ any_old_categories + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_crude)
performance::icc(csp_model_crude)

# run a multi-level logistic regression model 
csp_model_1 <- glmer(symptomatic_status ~ any_old_categories + age_cat_baseline + rescaled_days_in_study + any_old_categories*rescaled_days_in_study + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))

# make a forest plot of results without moi
table1 = exp(confint(csp_model_1,method="Wald"))
estimates = c(table1[2,3],table1[3,3],NA,table1[4,3],table1[5,3],NA,table1[6,3],table1[7,3])
lower_ci = c(table1[2,1],table1[3,1],NA,table1[4,1],table1[5,1],NA,table1[6,1],table1[7,1])
upper_ci = c(table1[2,2],table1[3,2],NA,table1[4,2],table1[5,2],NA,table1[6,2],table1[7,2])
names = c("All new haplotypes","New and recurrent haplotypes"," ","Participant age >15 years","Participant age 5-15 years","  ","Kinesamo village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("All new haplotypes","New and recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ","Kinesamo village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("All new haplotypes","New and recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ","Kinesamo village","Sitabicha village"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1),colour=c("#E1AF00","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_without_moi.png", device="png",
       height=9, width=12.5, units="in", dpi=400)


#### ---- now run some models for csp conditioned on age ------- ####

# change age cat ref
under5_csp = csp_data %>%
  filter(age_cat_baseline == "<5 years")
from5to15_csp = csp_data %>%
  filter(age_cat_baseline == "5-15 years")
over15_csp = csp_data %>%
  filter(age_cat_baseline == ">15 years")

# run the model for <5 year
csp_model_age_under5 <- glmer(symptomatic_status ~ any_old_categories + rescaled_days_in_study + any_old_categories*rescaled_days_in_study + (1|unq_memID),family=binomial(link = "logit"), data = under5_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_under5)
performance::icc(csp_model_age_under5)
exp(confint(csp_model_age_under5,method="Wald"))

# run the model for 5-15 year
csp_model_age_5to15 <- glmer(symptomatic_status ~ any_old_categories + rescaled_days_in_study + any_old_categories*rescaled_days_in_study + (1|unq_memID),family=binomial(link = "logit"), data = from5to15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_5to15)
performance::icc(csp_model_age_5to15)
exp(confint(csp_model_age_5to15,method="Wald"))

# run the model for >15 year
csp_model_age_over15 <- glmer(symptomatic_status ~ any_old_categories + rescaled_days_in_study + any_old_categories*rescaled_days_in_study + (1|unq_memID),family=binomial(link = "logit"), data = over15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_over15)
performance::icc(csp_model_age_over15)
exp(confint(csp_model_age_over15,method="Wald"))


#### ---- now run some preliminary models for ama: any new to no new ------- ####

# run a crude one level model
ama_model_1levelcrude <- glm(symptomatic_status ~ any_new_categories,family=binomial(link = "logit"), data = ama_data)
summary(ama_model_1levelcrude)
exp(confint(ama_model_1levelcrude))

# run a crude multilevel model
ama_model_crude <- glmer(symptomatic_status ~ any_new_categories + (1|unq_memID),family=binomial(link = "logit"), data = ama_data, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_crude)
performance::icc(ama_model_crude)

# run a multi-level logistic regression model 
ama_model_1 <- glmer(symptomatic_status ~ any_new_categories + age_cat_baseline + rescaled_days_in_study + any_new_categories*rescaled_days_in_study + (1|unq_memID),family=binomial(link = "logit"), data = ama_data, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_1)
performance::icc(ama_model_1)

# make a forest plot of results without moi
table1 = exp(confint(ama_model_1,method="Wald"))
estimates = c(table1[2,3],table1[3,3],NA,table1[4,3],table1[5,3],NA,table1[6,3],table1[7,3])
lower_ci = c(table1[2,1],table1[3,1],NA,table1[4,1],table1[5,1],NA,table1[6,1],table1[7,1])
upper_ci = c(table1[2,2],table1[3,2],NA,table1[4,2],table1[5,2],NA,table1[6,2],table1[7,2])
names = c("All new haplotypes","New and recurrent haplotypes"," ","Participant age >15 years","Participant age 5-15 years","  ","Kinesamo village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("All new haplotypes","New and recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ","Kinesamo village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("All new haplotypes","New and recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ","Kinesamo village","Sitabicha village"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1),colour=c("#E1AF00","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/ama_aim1b_model_without_moi.png", device="png",
       height=9, width=12.5, units="in", dpi=400)


#### ---- now run some preliminary models for ama: all new to any old ------- ####

# run a crude one level model
ama_model_1levelcrude <- glm(symptomatic_status ~ any_old_categories,family=binomial(link = "logit"), data = ama_data)
summary(ama_model_1levelcrude)
exp(confint(ama_model_1levelcrude))

# run a crude multilevel model
ama_model_crude <- glmer(symptomatic_status ~ any_old_categories + (1|unq_memID),family=binomial(link = "logit"), data = ama_data, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_crude)
performance::icc(ama_model_crude)

# run a multi-level logistic regression model 
ama_model_1 <- glmer(symptomatic_status ~ any_old_categories + age_cat_baseline + rescaled_days_in_study + any_old_categories*rescaled_days_in_study + (1|unq_memID),family=binomial(link = "logit"), data = ama_data, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_1)
performance::icc(ama_model_1)

# make a forest plot of results without moi
table1 = exp(confint(ama_model_1,method="Wald"))
estimates = c(table1[2,3],table1[3,3],NA,table1[4,3],table1[5,3],NA,table1[6,3],table1[7,3])
lower_ci = c(table1[2,1],table1[3,1],NA,table1[4,1],table1[5,1],NA,table1[6,1],table1[7,1])
upper_ci = c(table1[2,2],table1[3,2],NA,table1[4,2],table1[5,2],NA,table1[6,2],table1[7,2])
names = c("All new haplotypes","New and recurrent haplotypes"," ","Participant age >15 years","Participant age 5-15 years","  ","Kinesamo village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("All new haplotypes","New and recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ","Kinesamo village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("All new haplotypes","New and recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ","Kinesamo village","Sitabicha village"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1),colour=c("#E1AF00","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/ama_aim1b_model_without_moi.png", device="png",
       height=9, width=12.5, units="in", dpi=400)



#### ---- now run some models for ama conditioned on age ------- ####

# change age cat ref
under5_ama = ama_data %>%
  filter(age_cat_baseline == "<5 years")
from5to15_ama = ama_data %>%
  filter(age_cat_baseline == "5-15 years")
over15_ama = ama_data %>%
  filter(age_cat_baseline == ">15 years")

# run the model for <5 year
ama_model_age_under5 <- glmer(symptomatic_status ~ any_old_categories + rescaled_days_in_study + any_old_categories*rescaled_days_in_study + (1|unq_memID),family=binomial(link = "logit"), data = under5_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_under5)
performance::icc(ama_model_age_under5)
exp(confint(ama_model_age_under5,method="Wald"))

# run the model for 5-15 year
ama_model_age_5to15 <- glmer(symptomatic_status ~ any_old_categories + rescaled_days_in_study + any_old_categories*rescaled_days_in_study + (1|unq_memID),family=binomial(link = "logit"), data = from5to15_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_5to15)
performance::icc(ama_model_age_5to15)
exp(confint(ama_model_age_5to15,method="Wald"))

# run the model for >15 year
ama_model_age_over15 <- glmer(symptomatic_status ~ any_old_categories + rescaled_days_in_study + any_old_categories*rescaled_days_in_study + (1|unq_memID),family=binomial(link = "logit"), data = over15_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_over15)
performance::icc(ama_model_age_over15)
exp(confint(ama_model_age_over15,method="Wald"))




