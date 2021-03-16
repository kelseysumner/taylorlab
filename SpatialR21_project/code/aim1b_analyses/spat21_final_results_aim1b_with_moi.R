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

# make a new age category variable
# for csp
table(csp_data$age_cat_baseline)
csp_data$age_cat_baseline = ifelse(csp_data$age_cat_baseline== ">15 years",">15 years","15 years or less")
table(csp_data$age_cat_baseline)
csp_data$age_cat_baseline = as.factor(csp_data$age_cat_baseline)
csp_data$age_cat_baseline = relevel(csp_data$age_cat_baseline,ref="15 years or less")
# for ama
table(ama_data$age_cat_baseline)
ama_data$age_cat_baseline = ifelse(ama_data$age_cat_baseline== ">15 years",">15 years","15 years or less")
table(ama_data$age_cat_baseline)
ama_data$age_cat_baseline = as.factor(ama_data$age_cat_baseline)
ama_data$age_cat_baseline = relevel(ama_data$age_cat_baseline,ref="15 years or less")



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
# all new: OR 3.24 (95% CI 1.20 to 8.78)
# new and current: OR 0.64 (95% CI 0.15 to 2.65)
# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(exp(1.1771),exp(-0.4477),NA,exp(-1.2089),NA,exp(-0.1795),NA,exp(1.0996),NA,exp(-0.7327))
lower_ci = c(table1[3,1],table1[4,1],NA,table1[5,1],NA,table1[6,1],NA,table1[7,1],NA,table1[8,1])
upper_ci = c(table1[3,2],table1[4,2],NA,table1[5,2],NA,table1[6,2],NA,table1[7,2],NA,table1[8,2])
names = c("Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season", "        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season", "        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ", "High multiplicity of infection"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1,1,1,1,1,1,1,1,1),colour=c("#000000","#000000","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(breaks=c(0,0.2,0.5,1,2,5,10),trans="log10") +
  theme_bw() +
  theme(text = element_text(size=15))
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_3_categories_no_persistent.png", device="png",
       height=5, width=10, units="in", dpi=400)


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
estimates = c(exp(0.7894),exp(0.1879),NA,exp(-1.3488),NA,exp(-0.9619),NA,exp(0.7899),NA,exp(-0.6269))
lower_ci = c(table1[3,1],table1[4,1],NA,table1[5,1],NA,table1[6,1],NA,table1[7,1],NA,table1[8,1])
upper_ci = c(table1[3,2],table1[4,2],NA,table1[5,2],NA,table1[6,2],NA,table1[7,2],NA,table1[8,2])
names = c("Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1,1,1,1,1,1,1,1,1),colour=c("#000000","#000000","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9),trans="log10") +
  theme_bw() +
  theme(text = element_text(size=15)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/ama_aim1b_model_all_3_categories_no_persistent.png", device="png",
       height=5, width=10, units="in", dpi=400)



#### -------- test for effect measure modification by age for recurrent infections -------- ####


### for csp

# change age cat ref
under15_csp = no_persistent_data_csp %>%
  filter(age_cat_baseline == "15 years or less")
over15_csp = no_persistent_data_csp %>%
  filter(age_cat_baseline == ">15 years")

# run the model for 15 years or less
csp_model_age_under15 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = under15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_under15)
performance::icc(csp_model_age_under15)
exp(confint(csp_model_age_under15,method="Wald"))


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


# create a plot
names = c("Participant age 15 years or less","Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes","","Participant age >15 years"," Only new vs. only recurrent haplotypes"," New and recurrent vs. only recurrent haplotypes")
estimate = c(NA,exp(1.1011),exp(-0.6023),NA,NA,exp(1.3868),exp(0.4285))
lower_ci = c(NA,1.02311204,0.11704872,NA,NA,0.443813735,0.055222858)
upper_ci = c(NA,8.8402636,2.5613124,NA,NA,36.0845486,42.6637051)
forest_plot_df = data.frame(names,estimate,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Participant age 15 years or less","Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes","","Participant age >15 years"," Only new vs. only recurrent haplotypes"," New and recurrent vs. only recurrent haplotypes"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Participant age 15 years or less","Only new vs. only recurrent haplotypes","New and recurrent vs. only recurrent haplotypes","","Participant age >15 years"," Only new vs. only recurrent haplotypes"," New and recurrent vs. only recurrent haplotypes"))
# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimate, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(colour=c(NA,"#000000","#000000",NA,NA,"#000000","#000000"),size=c(1,1,1,1,1,1,1)) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.y = element_text(face=c("plain","plain","bold","plain","plain","plain","bold"))) +
  scale_y_continuous(trans="log10",breaks=c(0,0.1,1,5,10,20,40))
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_new_age_stratified.png", device="png",
       height=4, width=10, units="in", dpi=400)




### for ama

# change age cat ref
under15_ama = no_persistent_data_ama %>%
  filter(age_cat_baseline == "15 years or less")
over15_ama = no_persistent_data_ama %>%
  filter(age_cat_baseline == ">15 years")

# run the model for <5 year
ama_model_age_under5 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = under15_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_under5)
performance::icc(ama_model_age_under5)
exp(confint(ama_model_age_under5,method="Wald"))

# run the model for >15 year
ama_model_age_over15 <- glmer(symptomatic_status ~ haplotype_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat+ (1|unq_memID),family=binomial(link = "logit"), data = over15_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_over15)
performance::icc(ama_model_age_over15)
exp(confint(ama_model_age_over15,method="Wald"))


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
0.002724*6

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
0.01211*8

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



## ----- now look at total infections not stratified by symptomatic status for csp

# haplotype category
table(csp_data$haplotype_category)

# age category
table(csp_data$age_cat_baseline)

# number prior malaria infections
table(csp_data$add_cat_number_prior_infections)

# transmission season
table(csp_data$mosquito_week_count_cat_add)

# moi
table(csp_data$moi_cat)



## ----- now look at total infections not stratified by symptomatic status for ama

# haplotype category
table(ama_data$haplotype_category)

# age category
table(ama_data$age_cat_baseline)

# number prior malaria infections
table(ama_data$add_cat_number_prior_infections)

# transmission season
table(ama_data$mosquito_week_count_cat_add)

# moi
table(ama_data$moi_cat)





