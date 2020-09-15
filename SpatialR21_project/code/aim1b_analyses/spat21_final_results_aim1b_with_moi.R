# -------------------------------- #
#          Final results           #
#         Mozzie phase 1           #
#             Aim 1B               #
#        August 4, 2020            #
#           K. Sumner              #
# -------------------------------- #

# will call repeated infections: "reinfection"

#### ------- load libraries -------- ####
library(tidyverse)
library(schoolmath)
library(lme4)
library(glmmTMB)
library(scales)


#### ------- read in the data sets ------- ####

ama_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_ama_final_model_data_21JUL2020.rds")
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_csp_final_model_data_21JUL2020.rds")



#### ------ now look at quick descriptives of the final data sets -------- ####

# first look at number of unique haplotypes
# for csp
haplotype_list = c()
for (i in 1:nrow(csp_data)){
  first_split = str_split(csp_data$haplotype_list[i],",")[[1]]
  haplotype_list = c(haplotype_list,first_split)
}
length(unique(haplotype_list))

# for ama
haplotype_list = c()
for (i in 1:nrow(ama_data)){
  first_split = str_split(ama_data$haplotype_list[i],",")[[1]]
  haplotype_list = c(haplotype_list,first_split)
}
length(unique(haplotype_list))

# look at quick tables of asymptomatic and symptomatic infections across categories
# for csp
table(csp_data$haplotype_category,csp_data$symptomatic_status)
# for ama
table(ama_data$haplotype_category,ama_data$symptomatic_status)



#### -------- now run the new vs recurrent model ----------- ####

# run the new vs recurrent model for csp
# take out the infections with persistent haplotypes
no_persistent_data_csp = csp_data[which(!(str_detect(csp_data$haplotype_category,"persistent"))),]
table(no_persistent_data_csp$haplotype_category, useNA = "always")
no_persistent_data_csp$haplotype_category = as.character(no_persistent_data_csp$haplotype_category)
no_persistent_data_csp$haplotype_category = as.factor(no_persistent_data_csp$haplotype_category)
levels(no_persistent_data_csp$haplotype_category)
no_persistent_data_csp$haplotype_category = relevel(no_persistent_data_csp$haplotype_category,ref="all recurrent")
# now rerun the model
csp_model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                     data = no_persistent_data_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))
# all new: OR 3.15 (95% CI 1.18 to 8.37)
# new and current: OR 0.62 (95% CI 0.15 to 2.51)
# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(exp(1.1469),exp(-0.4855),NA,exp(-0.8352),exp(-1.8826),NA,exp(-0.1579),NA,exp(1.1294),NA,exp(-0.7449))
lower_ci = c(table1[3,1],table1[4,1],NA,table1[6,1],table1[5,1],NA,table1[7,1],NA,table1[8,1],NA,table1[9,1])
upper_ci = c(table1[3,2],table1[4,2],NA,table1[6,2],table1[5,2],NA,table1[7,2],NA,table1[8,2],NA,table1[9,2])
names = c("Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season", "        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season", "        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ", "High multiplicity of infection"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,3,1,1,1,1,1,1,1,1,1),colour=c("#b2182b","#ef8a62","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9),trans="log10") +
  theme_bw() +
  theme(text = element_text(size=15))
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_3_categories_no_persistent.png", device="png",
       height=6, width=10, units="in", dpi=400)


# now run the new vs recurrent model for ama
# take out the infections with persistent haplotypes
no_persistent_data_ama = ama_data[which(!(str_detect(ama_data$haplotype_category,"persistent"))),]
table(no_persistent_data_ama$haplotype_category, useNA = "always")
no_persistent_data_ama$haplotype_category = as.character(no_persistent_data_ama$haplotype_category)
no_persistent_data_ama$haplotype_category = as.factor(no_persistent_data_ama$haplotype_category)
levels(no_persistent_data_ama$haplotype_category)
no_persistent_data_ama$haplotype_category = relevel(no_persistent_data_ama$haplotype_category,ref="all recurrent")
# now rerun the model
ama_model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                     data = no_persistent_data_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_1)
performance::icc(ama_model_1)
exp(confint(ama_model_1,method="Wald"))
# make a forest plot of results
table1 = exp(confint(ama_model_1,method="Wald"))
summary(ama_model_1)
estimates = c(exp(0.7917),exp(0.1772),NA,exp(-0.1751),exp(-1.4925),NA,exp(-0.9471),NA,exp(0.7966),NA,exp(-0.6255))
lower_ci = c(table1[3,1],table1[4,1],NA,table1[6,1],table1[5,1],NA,table1[7,1],NA,table1[8,1],NA,table1[9,1])
upper_ci = c(table1[3,2],table1[4,2],NA,table1[6,2],table1[5,2],NA,table1[7,2],NA,table1[8,2],NA,table1[9,2])
names = c("Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,3,1,1,1,1,1,1,1,1,1),colour=c("#b2182b","#ef8a62","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9),trans="log10") +
  theme_bw() +
  theme(text = element_text(size=15)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/ama_aim1b_model_all_3_categories_no_persistent.png", device="png",
       height=6, width=10, units="in", dpi=400)



#### -------- test for effect measure modification by age for recurrent infections -------- ####


### for csp

# change age cat ref
under5_csp = no_persistent_data_csp %>%
  filter(age_cat_baseline == "<5 years")
from5to15_csp = no_persistent_data_csp %>%
  filter(age_cat_baseline == "5-15 years")
over15_csp = no_persistent_data_csp %>%
  filter(age_cat_baseline == ">15 years")

# run the model for <5 year
csp_model_age_under5 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = under5_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_under5)
performance::icc(csp_model_age_under5)
exp(confint(csp_model_age_under5,method="Wald"))
# model didn't converge

# run the model for 5-15 year
csp_model_age_5to15 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = from5to15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_5to15)
performance::icc(csp_model_age_5to15)
exp(confint(csp_model_age_5to15,method="Wald"))

# run the model for >15 year
csp_model_age_over15 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = over15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_over15)
performance::icc(csp_model_age_over15)
exp(confint(csp_model_age_over15,method="Wald"))


# now compare nested models 
# model with interaction term
model_1_interxn <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + haplotype_category*age_cat_baseline + moi_cat +  (1|unq_memID),family=binomial(link = "logit"), data = no_persistent_data_csp, control = glmerControl(optimizer="bobyqa"))
summary(model_1_interxn)
performance::icc(model_1_interxn)
# model without interaction term
model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat +(1|unq_memID),family=binomial(link = "logit"), data = no_persistent_data_csp, control = glmerControl(optimizer="bobyqa"))
summary(model_1)
performance::icc(model_1)
anova(model_1_interxn,model_1) # model 1 better




### for ama

# change age cat ref
under5_ama = no_persistent_data_ama %>%
  filter(age_cat_baseline == "<5 years")
from5to15_ama = no_persistent_data_ama %>%
  filter(age_cat_baseline == "5-15 years")
over15_ama = no_persistent_data_ama %>%
  filter(age_cat_baseline == ">15 years")

# run the model for <5 year
ama_model_age_under5 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = under5_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_under5)
performance::icc(ama_model_age_under5)
exp(confint(ama_model_age_under5,method="Wald"))
# model would not converge

# run the model for 5-15 year
ama_model_age_5to15 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat+ (1|unq_memID),family=binomial(link = "logit"), data = from5to15_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_5to15)
performance::icc(ama_model_age_5to15)
exp(confint(ama_model_age_5to15,method="Wald"))

# run the model for >15 year
ama_model_age_over15 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat+ (1|unq_memID),family=binomial(link = "logit"), data = over15_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_over15)
performance::icc(ama_model_age_over15)
exp(confint(ama_model_age_over15,method="Wald"))
# this model didn't converge either


# now compare nested models 
# model with interaction term
model_1_interxn <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + haplotype_category*age_cat_baseline + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = no_persistent_data_ama, control = glmerControl(optimizer="bobyqa"))
summary(model_1_interxn)
performance::icc(model_1_interxn)
# model without interaction term
model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat +  (1|unq_memID),family=binomial(link = "logit"), data = no_persistent_data_ama, control = glmerControl(optimizer="bobyqa"))
summary(model_1)
performance::icc(model_1)
anova(model_1_interxn,model_1) # model 1 better




#### --------- make table 1 of covariates across symptomatic status -------- ####

# create table 1: distribution of symptomatic status across covariates
# below shows model covariates to include and data set to use
# csp_model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
# data = no_persistent_data_csp, control = glmerControl(optimizer="bobyqa"))

## ------ do for csp first 

# make separate data sets for asymptomatic and symptomatic infections
asymp_data = no_persistent_data_csp %>%
  filter(symptomatic_status == "asymptomatic infection")
symp_data = no_persistent_data_csp %>%
  filter(symptomatic_status == "symptomatic infection")

# check the maximum number of csp infections included in this data set with no persistent haplotypes
length(unique(no_persistent_data_csp$unq_memID))
max_infections = no_persistent_data_csp %>%
  group_by(unq_memID) %>%
  summarize(n=n())
max(max_infections$n)

# haplotype category
table(no_persistent_data_csp$haplotype_category,no_persistent_data_csp$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(no_persistent_data_csp$haplotype_category,no_persistent_data_csp$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
1.168e-05*6

# age
table(no_persistent_data_csp$age_cat_baseline,no_persistent_data_csp$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(no_persistent_data_csp$age_cat_baseline,no_persistent_data_csp$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.003954*6

# number of prior malaria infections
table(no_persistent_data_csp$add_cat_number_prior_infections,no_persistent_data_csp$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(no_persistent_data_csp$add_cat_number_prior_infections,no_persistent_data_csp$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.3063*6

# transmission season (based on mosquitoes)
table(no_persistent_data_csp$mosquito_week_count_cat_add,no_persistent_data_csp$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(no_persistent_data_csp$mosquito_week_count_cat_add,no_persistent_data_csp$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.0006907*6

# multiplicity of infection
table(no_persistent_data_csp$moi_cat,no_persistent_data_csp$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(no_persistent_data_csp$moi_cat,no_persistent_data_csp$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.003616*6


## ------ now do for ama

# make separate data sets for asymptomatic and symptomatic infections
asymp_data = no_persistent_data_ama %>%
  filter(symptomatic_status == "asymptomatic infection")
symp_data = no_persistent_data_ama %>%
  filter(symptomatic_status == "symptomatic infection")

# check the maximum number of ama infections included in this data set with no persistent haplotypes
length(unique(no_persistent_data_ama$unq_memID))
max_infections = no_persistent_data_ama %>%
  group_by(unq_memID) %>%
  summarize(n=n())
max(max_infections$n)

# haplotype category
table(no_persistent_data_ama$haplotype_category,no_persistent_data_ama$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(no_persistent_data_ama$haplotype_category,no_persistent_data_ama$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.02874*8

# age
table(no_persistent_data_ama$age_cat_baseline,no_persistent_data_ama$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(no_persistent_data_ama$age_cat_baseline,no_persistent_data_ama$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.02576*8

# number of prior malaria infections
table(no_persistent_data_ama$add_cat_number_prior_infections,no_persistent_data_ama$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(no_persistent_data_ama$add_cat_number_prior_infections,no_persistent_data_ama$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.05845*8

# transmission season (based on mosquitoes)
table(no_persistent_data_ama$mosquito_week_count_cat_add,no_persistent_data_ama$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(no_persistent_data_ama$mosquito_week_count_cat_add,no_persistent_data_ama$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.03777*8

# multiplicity of infection
table(no_persistent_data_ama$moi_cat,no_persistent_data_ama$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(no_persistent_data_ama$moi_cat,no_persistent_data_ama$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.2125*8


#### ------ now re-rerun the multilevel models but comparing new vs persistent haplotypes ------- ####

# run the new vs persistent model for csp
# take out the infections with recurrent haplotypes
no_recurrent_data_csp = csp_data[which(!(str_detect(csp_data$haplotype_category,"recurrent"))),]
table(no_recurrent_data_csp$haplotype_category, useNA = "always")
no_recurrent_data_csp$haplotype_category = as.character(no_recurrent_data_csp$haplotype_category)
no_recurrent_data_csp$haplotype_category = as.factor(no_recurrent_data_csp$haplotype_category)
levels(no_recurrent_data_csp$haplotype_category)
no_recurrent_data_csp$haplotype_category = relevel(no_recurrent_data_csp$haplotype_category,ref="all persistent")
# now rerun the model
csp_model_1 <- glmmTMB(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                     data = no_recurrent_data_csp)
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))
# all new: OR 0.51 (95% CI 0.25 to 1.01)
# new and persistent: OR 0.64 (95% CI 0.25 to 1.67)
# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(table1[2,3],table1[3,3],NA,table1[5,3],table1[4,3],NA,table1[6,3],NA,table1[7,3],NA,table1[8,3])
lower_ci = c(table1[2,1],table1[3,1],NA,table1[5,1],table1[4,1],NA,table1[6,1],NA,table1[7,1],NA,table1[8,1])
upper_ci = c(table1[2,2],table1[3,2],NA,table1[5,2],table1[4,2],NA,table1[6,2],NA,table1[7,2],NA,table1[8,2])
names = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,3,1,1,1,1,1,1,1,1,1),colour=c("#2166ac","#67a9cf","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(breaks=c(0,1,2,3,4),trans="log10") +
  theme_bw()
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_3_categories_no_recurrent.png", device="png",
       height=4.5, width=7, units="in", dpi=400)


# run the new vs persistent model for ama
# take out the infections with recurrent haplotypes
no_recurrent_data_ama = ama_data[which(!(str_detect(ama_data$haplotype_category,"recurrent"))),]
table(no_recurrent_data_ama$haplotype_category, useNA = "always")
no_recurrent_data_ama$haplotype_category = as.character(no_recurrent_data_ama$haplotype_category)
no_recurrent_data_ama$haplotype_category = as.factor(no_recurrent_data_ama$haplotype_category)
levels(no_recurrent_data_ama$haplotype_category)
no_recurrent_data_ama$haplotype_category = relevel(no_recurrent_data_ama$haplotype_category,ref="all persistent")
# now rerun the model
ama_model_1 <- glmmTMB(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = no_recurrent_data_ama)
summary(ama_model_1)
performance::icc(ama_model_1)
exp(confint(ama_model_1,method="Wald"))
# all new: OR 0.12 (95% CI 0.04 to 0.42)
# new and recurrent: OR 0.22 (95% CI 0.06 to 0.86)
# make a forest plot of results
table1 = exp(confint(ama_model_1,method="Wald"))
summary(ama_model_1)
estimates = c(table1[2,3],table1[3,3],NA,table1[5,3],table1[4,3],NA,table1[6,3],NA,table1[7,3],NA,table1[8,3])
lower_ci = c(table1[2,1],table1[3,1],NA,table1[5,1],table1[4,1],NA,table1[6,1],NA,table1[7,1],NA,table1[8,1])
upper_ci = c(table1[2,2],table1[3,2],NA,table1[5,2],table1[4,2],NA,table1[6,2],NA,table1[7,2],NA,table1[8,2])
names = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Only new vs. only persistent haplotypes","New and persistent vs. only persistent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,3,1,1,1,1,1,1,1,1,1),colour=c("#2166ac","#67a9cf","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8),trans="log10") +
  theme_bw()
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/ama_aim1b_model_all_3_categories_no_recurrent.png", device="png",
       height=4.5, width=7, units="in", dpi=400)



#### -------- test for effect measure modification by age for persistent infections -------- ####


### for csp

# change age cat ref
under5_csp = no_recurrent_data_csp %>%
  filter(age_cat_baseline == "<5 years")
from5to15_csp = no_recurrent_data_csp %>%
  filter(age_cat_baseline == "5-15 years")
over15_csp = no_recurrent_data_csp %>%
  filter(age_cat_baseline == ">15 years")

# run the model for <5 year
csp_model_age_under5 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = under5_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_under5)
performance::icc(csp_model_age_under5)
exp(confint(csp_model_age_under5,method="Wald"))
# model would not converge

# run the model for 5-15 year
csp_model_age_5to15 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = from5to15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_5to15)
performance::icc(csp_model_age_5to15)
exp(confint(csp_model_age_5to15,method="Wald"))

# run the model for >15 year
csp_model_age_over15 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = over15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_over15)
performance::icc(csp_model_age_over15)
exp(confint(csp_model_age_over15,method="Wald"))


# now compare nested models 
# model with interaction term
model_1_interxn <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + haplotype_category*age_cat_baseline + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = no_recurrent_data_csp, control = glmerControl(optimizer="bobyqa"))
summary(model_1_interxn)
performance::icc(model_1_interxn)
# model without interaction term
model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = no_recurrent_data_csp, control = glmerControl(optimizer="bobyqa"))
summary(model_1)
performance::icc(model_1)
anova(model_1_interxn,model_1) # model 1 better




### for ama

# change age cat ref
under5_ama = no_recurrent_data_ama %>%
  filter(age_cat_baseline == "<5 years")
from5to15_ama = no_recurrent_data_ama %>%
  filter(age_cat_baseline == "5-15 years")
over15_ama = no_recurrent_data_ama %>%
  filter(age_cat_baseline == ">15 years")

# run the model for <5 year
ama_model_age_under5 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = under5_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_under5)
performance::icc(ama_model_age_under5)
exp(confint(ama_model_age_under5,method="Wald"))
# model would not converge

# run the model for 5-15 year
ama_model_age_5to15 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat +(1|unq_memID),family=binomial(link = "logit"), data = from5to15_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_5to15)
performance::icc(ama_model_age_5to15)
exp(confint(ama_model_age_5to15,method="Wald"))

# run the model for >15 year
ama_model_age_over15 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = over15_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_over15)
performance::icc(ama_model_age_over15)
exp(confint(ama_model_age_over15,method="Wald"))
# model failed to converge

# now compare nested models 
# model with interaction term
model_1_interxn <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + haplotype_category*age_cat_baseline + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = no_recurrent_data_ama, control = glmerControl(optimizer="bobyqa"))
summary(model_1_interxn)
performance::icc(model_1_interxn)
# model would not converge well
# model without interaction term
model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = no_recurrent_data_ama, control = glmerControl(optimizer="bobyqa"))
summary(model_1)
performance::icc(model_1)
anova(model_1_interxn,model_1)


#### ----- compare number new haplotypes in recurrent versus persistent infections -------- ####

# first look at general numbers
table(csp_data$haplotype_category, useNA = "always")
table(ama_data$haplotype_category, useNA = "always")
table(csp_data$symptomatic_status,csp_data$haplotype_category)
table(ama_data$symptomatic_status,ama_data$haplotype_category)
csp_data = arrange(csp_data,unq_memID,sample_id_date)
csp_data %>%
  group_by(haplotype_category) %>%
  summarize(mean_moi = mean(haplotype_number),median_moi = median(haplotype_number),min_moi = min(haplotype_number),max_moi = max(haplotype_number))
csp_data %>%
  group_by(symptomatic_status) %>%
  summarize(mean_moi = mean(haplotype_number),median_moi = median(haplotype_number),min_moi = min(haplotype_number),max_moi = max(haplotype_number))
csp_data %>%
  group_by(haplotype_category) %>%
  summarize(mean_count_new = mean(count_new_haplotypes),median_count_new = median(count_new_haplotypes),min_new = min(count_new_haplotypes),max_new = max(count_new_haplotypes))
csp_data %>%
  group_by(haplotype_category) %>%
  summarize(mean_count_new = mean(number_prior_infections),median_count_new = median(number_prior_infections),min_new = min(number_prior_infections),max_new = max(number_prior_infections))




