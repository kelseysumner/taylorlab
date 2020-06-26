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
ama_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/without_first_infection_ama_data_spat21_aim1b_11JUN2020.rds")

# read in the csp data set
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/without_first_infection_all_data_spat21_aim1b_11JUN2020.rds")

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

# rescale the number of mosquitoes in week following infection (represents transmission season)
csp_data$rescaled_mosquito_week_count = scale(csp_data$mosquito_week_count)
ama_data$rescaled_mosquito_week_count = scale(ama_data$mosquito_week_count)

# rescale the number of prior malaria infections
csp_data$rescaled_number_prior_infections = scale(csp_data$number_prior_infections)
ama_data$rescaled_number_prior_infections = scale(ama_data$number_prior_infections)

# rescale MOI
csp_data$rescaled_moi = scale(csp_data$haplotype_number)
ama_data$rescaled_moi = scale(ama_data$haplotype_number)

# make a three haplotype category variable
# for csp
csp_data$three_hap_category = ifelse(csp_data$haplotype_category == "all new","all new",
                                     ifelse(str_detect(csp_data$haplotype_category,"persistent"),"any persistent","recurrent or new"))
table(csp_data$haplotype_category,csp_data$three_hap_category,useNA = "always")
table(csp_data$three_hap_category,useNA = "always")
# for ama
ama_data$three_hap_category = ifelse(ama_data$haplotype_category == "all new","all new",
                                     ifelse(str_detect(ama_data$haplotype_category,"persistent"),"any persistent","recurrent or new"))
table(ama_data$haplotype_category,ama_data$three_hap_category,useNA = "always")
table(ama_data$three_hap_category,useNA = "always")


# now clean up the variables to be in the proper str
# for csp
str(csp_data$sample_name_dbs)
str(csp_data$unq_memID)
str(csp_data$HH_ID)
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
csp_data$haplotype_category = as.factor(csp_data$haplotype_category)
csp_data$three_hap_category = as.factor(csp_data$three_hap_category)
csp_data$three_hap_category = relevel(csp_data$three_hap_category,ref="any persistent")
# for ama
str(ama_data$sample_name_dbs)
str(ama_data$unq_memID)
str(ama_data$HH_ID)
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
ama_data$haplotype_category = as.factor(ama_data$haplotype_category)
ama_data$three_hap_category = as.factor(ama_data$three_hap_category)
ama_data$three_hap_category = relevel(ama_data$three_hap_category,ref="any persistent")


#### ----- quickly do a functional form assessment for continuous variables ------ ####

# make a dummy variable for symptomatic status
csp_data$dummy_symptomatic_status = ifelse(csp_data$symptomatic_status == "asymptomatic infection",0,1)
table(csp_data$dummy_symptomatic_status,csp_data$symptomatic_status, useNA = "always")

# age: will keep categorical for interpretability and comoparability

# number_prior_infections
# plot the lowess graph 
ggplot(data=csp_data) + geom_smooth(aes(x=rescaled_number_prior_infections,y=dummy_symptomatic_status),method="loess")
ggplot(data=csp_data) + geom_smooth(aes(x=number_prior_infections,y=dummy_symptomatic_status),method="loess")
# look at linear coding
model1=glmer(symptomatic_status~rescaled_number_prior_infections + (1|unq_memID),family=binomial(link = "logit"),data=csp_data,control = glmerControl(optimizer="bobyqa")) 
summary(model1) # AIC: 507.4
# look at quadratic coding
csp_data$quad_number_prior_infections = csp_data$rescaled_number_prior_infections*csp_data$rescaled_number_prior_infections
model2=glmer(symptomatic_status~rescaled_number_prior_infections + quad_number_prior_infections + (1|unq_memID),family=binomial(link = "logit"),data=csp_data,control = glmerControl(optimizer="bobyqa")) 
summary(model2) # AIC: 507.9
anova(model1,model2) # model 1 better
# look at cubic coding
csp_data$cub_number_prior_infections = csp_data$rescaled_number_prior_infections*csp_data$rescaled_number_prior_infections*csp_data$rescaled_number_prior_infections
model3=glmer(symptomatic_status~rescaled_number_prior_infections + quad_number_prior_infections + cub_number_prior_infections + (1|unq_memID),family=binomial(link = "logit"),data=csp_data,control = glmerControl(optimizer="bobyqa")) 
summary(model3) # AIC: 509.1
anova(model1,model3) # model 1 better
# look at categorical coding
table(csp_data$number_prior_infections)
csp_data$cat_number_prior_infections = ifelse(csp_data$number_prior_infections == 1,"one infection","greater than one infection")
table(csp_data$number_prior_infections,csp_data$cat_number_prior_infections)
csp_data$cat_number_prior_infections = as.factor(csp_data$cat_number_prior_infections)
model4=glmer(symptomatic_status~cat_number_prior_infections + (1|unq_memID),family=binomial(link = "logit"),data=csp_data,control = glmerControl(optimizer="bobyqa")) 
summary(model4) # AIC: 507.2
# additional categorical coding
csp_data$add_cat_number_prior_infections = ifelse(csp_data$number_prior_infections < 4,"3 infections or less","more than 3 infections")
table(csp_data$number_prior_infections,csp_data$add_cat_number_prior_infections)
csp_data$add_cat_number_prior_infections = as.factor(csp_data$add_cat_number_prior_infections)
model4=glmer(symptomatic_status~add_cat_number_prior_infections + (1|unq_memID),family=binomial(link = "logit"),data=csp_data,control = glmerControl(optimizer="bobyqa")) 
summary(model4) # AIC: 504.6
# clean up the variables
csp_data$cat_number_prior_infections <- NULL
csp_data$quad_number_prior_infections <- NULL
csp_data$cub_number_prior_infections <- NULL


# mosquito_week_count
# plot the lowess graph 
ggplot(data=csp_data) + geom_smooth(aes(x=mosquito_week_count,y=dummy_symptomatic_status),method="loess")
# look at the number of mosquitoes collected by month
ggplot(data=csp_data) + geom_smooth(aes(y=mosquito_week_count,x=sample_id_date),method="loess")
# look at linear coding
model1=glmer(symptomatic_status~rescaled_mosquito_week_count + (1|unq_memID),family=binomial(link = "logit"),data=csp_data,control = glmerControl(optimizer="bobyqa")) 
summary(model1) # AIC: 492.8
# look at quadratic coding
csp_data$quad_mosquito_week_count = csp_data$rescaled_mosquito_week_count*csp_data$rescaled_mosquito_week_count
model2=glmer(symptomatic_status~rescaled_mosquito_week_count + quad_mosquito_week_count + (1|unq_memID),family=binomial(link = "logit"),data=csp_data,control = glmerControl(optimizer="bobyqa")) 
summary(model2) # AIC: 493.9
anova(model1,model2) # model 1 better
# look at cubic coding
csp_data$cub_mosquito_week_count = csp_data$rescaled_mosquito_week_count*csp_data$rescaled_mosquito_week_count*csp_data$rescaled_mosquito_week_count
model3=glmer(symptomatic_status~rescaled_mosquito_week_count + quad_mosquito_week_count + cub_mosquito_week_count + (1|unq_memID),family=binomial(link = "logit"),data=csp_data,control = glmerControl(optimizer="bobyqa")) 
summary(model3) # AIC: 495.8
anova(model1,model3) # model 1 better
# look at categorical coding
table(csp_data$mosquito_week_count)
csp_data$mosquito_week_count_cat = ifelse(csp_data$mosquito_week_count <= 25,"25 mosquitoes or less","more than 25 mosquitoes")
table(csp_data$mosquito_week_count,csp_data$mosquito_week_count_cat)
csp_data$mosquito_week_count_cat = as.factor(csp_data$mosquito_week_count_cat)
model4=glmer(symptomatic_status~mosquito_week_count_cat + (1|unq_memID),family=binomial(link = "logit"),data=csp_data,control = glmerControl(optimizer="bobyqa")) 
summary(model4) # AIC: 502.7
# look at additional categorical coding
csp_data$mosquito_week_count_cat_add = ifelse(csp_data$mosquito_week_count <= 50,"50 or less mosquitoes","more than 50 mosquitoes")
table(csp_data$mosquito_week_count,csp_data$mosquito_week_count_cat_add)
csp_data$mosquito_week_count_cat_add = as.factor(csp_data$mosquito_week_count_cat_add)
model4=glmer(symptomatic_status~mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"),data=csp_data,control = glmerControl(optimizer="bobyqa")) 
summary(model4) # AIC: 494.4
# clean up the variables
csp_data$mosquito_week_count_cat <- NULL
csp_data$quad_mosquito_week_count <- NULL
csp_data$cub_mosquito_week_count <- NULL

# make sure the variables are coded correctly
colnames(csp_data)
str(csp_data$add_cat_number_prior_infections)
str(csp_data$mosquito_week_count_cat_add)
csp_data$mosquito_week_count_cat_add = relevel(csp_data$mosquito_week_count_cat_add,ref="50 or less mosquitoes")

# now add these variables for ama
# number of prior infections
ama_data$add_cat_number_prior_infections = ifelse(ama_data$number_prior_infections < 4,"3 infections or less","more than 3 infections")
ama_data$add_cat_number_prior_infections = as.factor(ama_data$add_cat_number_prior_infections)
# mosquito week count
ama_data$mosquito_week_count_cat_add = ifelse(ama_data$mosquito_week_count <= 50,"50 or less mosquitoes","more than 50 mosquitoes")
ama_data$mosquito_week_count_cat_add = as.factor(ama_data$mosquito_week_count_cat_add)
ama_data$mosquito_week_count_cat_add = relevel(ama_data$mosquito_week_count_cat_add,ref="50 or less mosquitoes")


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
ggplot(csp_data, aes(x = number_prior_infections)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(csp_data, aes(x = add_cat_number_prior_infections)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(csp_data, aes(x = mosquito_week_count_cat_add)) + geom_density() + facet_wrap(~any_new_categories)
# for outcome
ggplot(csp_data, aes(x = age_cat_baseline)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(csp_data, aes(x = village_name)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(csp_data, aes(x = haplotype_number,fill=symptomatic_status)) + geom_density() + facet_wrap(~symptomatic_status) + theme_bw() + theme(legend.position="none") + xlab("pfcsp MOI")
table(csp_data$HH_ID,csp_data$symptomatic_status)
table(csp_data$unq_memID,csp_data$symptomatic_status)
ggplot(csp_data, aes(x = number_prior_infections)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(csp_data, aes(x = mosquito_week_count)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(csp_data, aes(x = add_cat_number_prior_infections)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(csp_data, aes(x = mosquito_week_count_cat_add)) + geom_density() + facet_wrap(~symptomatic_status)

# for ama
# for exposure
ggplot(ama_data, aes(x = symptomatic_status)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(ama_data, aes(x = age_cat_baseline)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(ama_data, aes(x = village_name)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(ama_data, aes(x = haplotype_number,fill=any_new_categories)) + geom_density() + facet_wrap(~any_new_categories) + theme_bw() + theme(legend.position="none") + xlab("pfama MOI")
table(ama_data$HH_ID,ama_data$any_new_categories)
table(ama_data$unq_memID,ama_data$any_new_categories)
ggplot(ama_data, aes(x = number_prior_infections)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(ama_data, aes(x = mosquito_week_count)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(ama_data, aes(x = add_cat_number_prior_infections)) + geom_density() + facet_wrap(~any_new_categories)
ggplot(ama_data, aes(x = mosquito_week_count_cat_add)) + geom_density() + facet_wrap(~any_new_categories)
# for outcome
ggplot(ama_data, aes(x = age_cat_baseline)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(ama_data, aes(x = village_name)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(ama_data, aes(x = haplotype_number,fill=symptomatic_status)) + geom_density() + facet_wrap(~symptomatic_status) + theme_bw() + theme(legend.position="none") + xlab("pfcsp MOI")
table(ama_data$HH_ID,ama_data$symptomatic_status)
table(ama_data$unq_memID,ama_data$symptomatic_status)
ggplot(ama_data, aes(x = number_prior_infections)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(ama_data, aes(x = mosquito_week_count)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(ama_data, aes(x = add_cat_number_prior_infections)) + geom_density() + facet_wrap(~symptomatic_status)
ggplot(ama_data, aes(x = mosquito_week_count_cat_add)) + geom_density() + facet_wrap(~symptomatic_status)



#### -------- look at all three haplotype categories ------ ####

# look at a summary of the three haplotype categories
table(csp_data$three_hap_category,useNA = "always")

# run a crude multilevel model
csp_model_crude <- glmer(symptomatic_status ~ three_hap_category + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_crude)
performance::icc(csp_model_crude)

# run a multi-level logistic regression model
csp_data$haplotype_category = relevel(csp_data$haplotype_category,ref="all new")
levels(csp_data$haplotype_category)
csp_model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), 
                     data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))
# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(exp(0.8263),exp(-0.8574),exp(-0.2176),exp(-1.8150),exp(-1.8118),exp(-17.9210),NA,exp(-0.1949),exp(-1.1434),NA,exp(-0.2089),NA,exp(0.7332))
lower_ci = c(table1[3,1],table1[4,1],table1[5,1],table1[6,1],table1[7,1],table1[8,1],NA,table1[10,1],table1[9,1],NA,table1[11,1],NA,table1[12,1])
upper_ci = c(table1[3,2],table1[4,2],table1[5,2],table1[6,2],table1[7,2],table1[8,2],NA,table1[10,2],table1[9,2],NA,table1[11,2],NA,table1[12,2])
names = c("All persistent vs. all new haplotypes","All recurrent vs. all new haplotypes","New and persistent vs. all new haplotypes","New and recurrent vs. all new haplotypes","New/Recurrent/Persistent vs. all new haplotypes","Recurrent and persistent vs. all new haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("All persistent vs. all new haplotypes","All recurrent vs. all new haplotypes","New and persistent vs. all new haplotypes","New and recurrent vs. all new haplotypes","New/Recurrent/Persistent vs. all new haplotypes","Recurrent and persistent vs. all new haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("All persistent vs. all new haplotypes","All recurrent vs. all new haplotypes","New and persistent vs. all new haplotypes","New and recurrent vs. all new haplotypes","New/Recurrent/Persistent vs. all new haplotypes","Recurrent and persistent vs. all new haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(2,2,2,2,2,2,1,1,1,1,1,1,1),colour=c("#54278f","#9e9ac8","#54278f","#9e9ac8","#54278f","#9e9ac8","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=15)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_categories.png", device="png",
       height=6, width=10, units="in", dpi=400)
# now make the forest plot removing the wide confidence interval for recurrent and persistent
# make a forest plot of results
estimates = c(exp(0.8263),exp(-0.8574),exp(-0.2176),exp(-1.8150),exp(-1.8118),NA,exp(-0.1949),exp(-1.1434),NA,exp(-0.2089),NA,exp(0.7332))
lower_ci = c(table1[3,1],table1[4,1],table1[5,1],table1[6,1],table1[7,1],NA,table1[10,1],table1[9,1],NA,table1[11,1],NA,table1[12,1])
upper_ci = c(table1[3,2],table1[4,2],table1[5,2],table1[6,2],table1[7,2],NA,table1[10,2],table1[9,2],NA,table1[11,2],NA,table1[12,2])
names =  c("All persistent vs. all new haplotypes","All recurrent vs. all new haplotypes","New and persistent vs. all new haplotypes","New and recurrent vs. all new haplotypes","New/Recurrent/Persistent vs. all new haplotypes","    ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("All persistent vs. all new haplotypes","All recurrent vs. all new haplotypes","New and persistent vs. all new haplotypes","New and recurrent vs. all new haplotypes","New/Recurrent/Persistent vs. all new haplotypes","    ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("All persistent vs. all new haplotypes","All recurrent vs. all new haplotypes","New and persistent vs. all new haplotypes","New and recurrent vs. all new haplotypes","New/Recurrent/Persistent vs. all new haplotypes","    ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(2,2,2,2,2,1,1,1,1,1,1,1),colour=c("#54278f","#9e9ac8","#54278f","#9e9ac8","#54278f","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=15)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_categories_minus_bigci.png", device="png",
       height=6, width=10, units="in", dpi=400)

# take out the infections with persistent haplotypes
no_persistent_data = csp_data[which(!(str_detect(csp_data$haplotype_category,"persistent"))),]
table(no_persistent_data$haplotype_category, useNA = "always")
no_persistent_data$haplotype_category = as.character(no_persistent_data$haplotype_category)
no_persistent_data$haplotype_category = as.factor(no_persistent_data$haplotype_category)
levels(no_persistent_data$haplotype_category)
no_persistent_data$haplotype_category = relevel(no_persistent_data$haplotype_category,ref="all recurrent")
# now rerun the model
csp_model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), 
                     data = no_persistent_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))
# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(exp(1.05926),exp(-1.00540),NA,exp(-0.76494),exp(-1.76790),NA,exp(-0.06387),NA,exp(1.08654))
lower_ci = c(table1[3,1],table1[4,1],NA,table1[6,1],table1[5,1],NA,table1[7,1],NA,table1[8,1])
upper_ci = c(table1[3,2],table1[4,2],NA,table1[6,2],table1[5,2],NA,table1[7,2],NA,table1[8,2])
names = c("All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,3,1,1,1,1,1,1,1),colour=c("#54278f","#9e9ac8","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8),trans="log10") +
  theme_bw() +
  theme(text = element_text(size=15))
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_3_categories_no_persistent.png", device="png",
       height=6, width=10, units="in", dpi=400)


# run a multi-level logistic regression model with an interaction term for age
csp_model_1b <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + haplotype_category*age_cat_baseline + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1b)
performance::icc(csp_model_1b)
anova(csp_model_1,csp_model_1b) # model 1 better
exp(confint(csp_model_1b,method="Wald"))

# run a multi-level logistic regression model with an interaction term for prior infections
csp_model_1c <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + haplotype_category*add_cat_number_prior_infections + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1c)
performance::icc(csp_model_1c)
anova(csp_model_1,csp_model_1c) # model 1 better
exp(confint(csp_model_1c,method="Wald"))

# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(exp(0.13082),exp(-0.86673),NA,exp(-0.08476),exp(-1.03685),NA,exp(-0.56599),NA,exp(0.87584))
lower_ci = c(table1[3,1],table1[4,1],NA,table1[6,1],table1[5,1],NA,table1[7,1],NA,table1[8,1])
upper_ci = c(table1[3,2],table1[4,2],NA,table1[6,2],table1[5,2],NA,table1[7,2],NA,table1[8,2])
names = c("All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,3,1,1,1,1,1,1,1),colour=c("#54278f","#9e9ac8","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=15)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_3_categories.png", device="png",
       height=6, width=10, units="in", dpi=400)





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
csp_model_1 <- glmer(symptomatic_status ~ any_new_categories + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))

# run a multi-level logistic regression model with an interaction term for age
csp_model_1b <- glmer(symptomatic_status ~ any_new_categories + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + any_new_categories*age_cat_baseline + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1b)
performance::icc(csp_model_1b)
anova(csp_model_1,csp_model_1b) # model 1 better
exp(confint(csp_model_1b,method="Wald"))

# run a multi-level logistic regression model with an interaction term for prior infections
csp_model_1c <- glmer(symptomatic_status ~ any_new_categories + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + any_new_categories*add_cat_number_prior_infections + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1c)
performance::icc(csp_model_1c)
anova(csp_model_1,csp_model_1c) # model 1 better
exp(confint(csp_model_1c,method="Wald"))


# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(exp(-0.40347),NA,exp(-0.03402),exp(-1.04893),NA,exp(-0.65798),NA,exp(0.87165))
lower_ci = c(table1[3,1],NA,table1[5,1],table1[4,1],NA,table1[6,1],NA,table1[7,1])
upper_ci = c(table1[3,2],NA,table1[5,2],table1[4,2],NA,table1[6,2],NA,table1[7,2])
names = c("Any new vs. all recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Any new vs. all recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Any new vs. all recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1),colour=c("#238443","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=15)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_any_new.png", device="png",
       height=6, width=10, units="in", dpi=400)



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
csp_model_1 <- glmer(symptomatic_status ~ any_old_categories + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))

# run a multi-level logistic regression model with an interaction term for age
csp_model_1b <- glmer(symptomatic_status ~ any_old_categories + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + any_old_categories*age_cat_baseline + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1b)
performance::icc(csp_model_1b)
anova(csp_model_1,csp_model_1b) # model 1 better
exp(confint(csp_model_1b,method="Wald"))

# run a multi-level logistic regression model with an interaction term for prior infections
csp_model_1c <- glmer(symptomatic_status ~ any_old_categories + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + any_old_categories*add_cat_number_prior_infections + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1c)
performance::icc(csp_model_1c)
anova(csp_model_1,csp_model_1c) # model 1 better
exp(confint(csp_model_1c,method="Wald"))


# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(exp(0.60628),NA,exp(-0.03891),exp(-1.04922),NA,exp(-0.45380),NA,exp(0.83755))
lower_ci = c(table1[3,1],NA,table1[5,1],table1[4,1],NA,table1[6,1],NA,table1[7,1])
upper_ci = c(table1[3,2],NA,table1[5,2],table1[4,2],NA,table1[6,2],NA,table1[7,2])
names = c("All new vs. any recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("All new vs. any recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("All new vs. any recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1),colour=c("#225ea8","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=15)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_new.png", device="png",
       height=6, width=10, units="in", dpi=400)


#### ---- now run some models for csp conditioned on age ------- ####

# change age cat ref
under5_csp = csp_data %>%
  filter(age_cat_baseline == "<5 years")
from5to15_csp = csp_data %>%
  filter(age_cat_baseline == "5-15 years")
over15_csp = csp_data %>%
  filter(age_cat_baseline == ">15 years")

# run the model for <5 year
csp_model_age_under5 <- glmer(symptomatic_status ~ any_old_categories + (1|unq_memID),family=binomial(link = "logit"), data = under5_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_under5)
performance::icc(csp_model_age_under5)
exp(confint(csp_model_age_under5,method="Wald"))

# run the model for 5-15 year
csp_model_age_5to15 <- glmer(symptomatic_status ~ any_old_categories + (1|unq_memID),family=binomial(link = "logit"), data = from5to15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_5to15)
performance::icc(csp_model_age_5to15)
exp(confint(csp_model_age_5to15,method="Wald"))

# run the model for >15 year
csp_model_age_over15 <- glmer(symptomatic_status ~ any_old_categories + (1|unq_memID),family=binomial(link = "logit"), data = over15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_over15)
performance::icc(csp_model_age_over15)
exp(confint(csp_model_age_over15,method="Wald"))
# this version had a singular fit so trying glmmtmb version
library(glmmTMB)
csp_model_age_over15 <- glmmTMB(symptomatic_status ~ any_old_categories + (1|unq_memID),family=binomial(link = "logit"), data = over15_csp)
summary(csp_model_age_over15) # produces same values
performance::icc(csp_model_age_over15)
exp(confint(csp_model_age_over15,method="Wald"))

# create a plot
names = c("Participant age <5 years","Participant age 5-15 years","Participant age >15 years")
estimate = c(exp(1.704),exp(0.6709),exp(0.6688))
lower_ci = c(0.641895798,1.0569320,0.70631396)
upper_ci = c(47.0224205,3.6201857,5.3942070)
forest_plot_df = data.frame(names,estimate,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Participant age <5 years","Participant age 5-15 years","Participant age >15 years"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Participant age <5 years","Participant age 5-15 years","Participant age >15 years"))
# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimate, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1,1),colour=c("#238443","#74c476","#225ea8")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_new_age_stratified.png", device="png",
       height=6, width=10, units="in", dpi=400)

# now compare nested models 
# model with interaction term
model_1_interxn <- glmer(symptomatic_status ~ any_old_categories + age_cat_baseline + any_old_categories*age_cat_baseline + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(model_1_interxn)
performance::icc(model_1_interxn)
# model without interaction term
model_1 <- glmer(symptomatic_status ~ any_old_categories + age_cat_baseline + (1|unq_memID),family=binomial(link = "logit"), data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(model_1)
performance::icc(model_1)
anova(model_1_interxn,model_1) # model 1 better




#### ------ run some models for csp of just all new vs. all old -------- ####

# subset the data set to all new or all old
csp_sub_data = csp_data %>% filter(haplotype_category == "all new" | haplotype_category == "all old")

# make haplotype category a factor
csp_sub_data$haplotype_category = factor(csp_sub_data$haplotype_category,levels=c("all old","all new"))

# run the crude multi-level model
csp_model_crude <- glmer(symptomatic_status ~ haplotype_category + (1|unq_memID),family=binomial(link = "logit"), data = csp_sub_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_crude)

# run the full multi-level model
csp_model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), data = csp_sub_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1)

# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(exp(0.1210),NA,exp(-0.2465),exp(-1.1731),NA,exp(-0.6499),NA,exp(0.7347))
lower_ci = c(table1[3,1],NA,table1[5,1],table1[4,1],NA,table1[6,1],NA,table1[7,1])
upper_ci = c(table1[3,2],NA,table1[5,2],table1[4,2],NA,table1[6,2],NA,table1[7,2])
names = c("All new haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("All new haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("All new haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1),colour=c("#a6cee3","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_new_v_all_old.png", device="png",
       height=6, width=10, units="in", dpi=400)


#### ------ now do an analysis of the proportion of new haplotypes ------ ####

# first subset the data set to only infections that have both new and recurrent haplotypes
csp_mixed = csp_data %>% filter(haplotype_category == "old and new")

# run the crude multi-level model
csp_model_crude <- glmer(symptomatic_status ~ proportion_new_haplotypes + (1|unq_memID),family=binomial(link = "logit"), data = csp_mixed, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_crude)

# run the full multi-level model
csp_model_1 <- glmer(symptomatic_status ~ proportion_new_haplotypes + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), data = csp_mixed, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1)

# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(exp(-1.4117),NA,exp(0.4306),exp(-0.9421),NA,exp(-0.3515),NA,exp(1.6253))
lower_ci = c(table1[3,1],NA,table1[5,1],table1[4,1],NA,table1[6,1],NA,table1[7,1])
upper_ci = c(table1[3,2],NA,table1[5,2],table1[4,2],NA,table1[6,2],NA,table1[7,2])
names = c("Proportion new haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Proportion new haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Proportion new haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1),colour=c("#756bb1","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_prop_new_haplotypes.png", device="png",
       height=6, width=10, units="in", dpi=400)



#### ------ recreate the analysis subset to polyclonal vs monoclonal infections for csp ------ ####

# subset the data set to monoclonal and polyclonal
csp_mono = csp_data %>% filter(haplotype_number == 1)
csp_poly = csp_data %>% filter(haplotype_number > 1)

# run the full multi-level model for monoclonal infections for any new vs. all old
csp_model_mono <- glmer(symptomatic_status ~ any_new_categories + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), data = csp_mono, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_mono)

# run the full multi-level model for polyclonal infections for any new vs. all old
csp_model_poly <- glmer(symptomatic_status ~ any_new_categories + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), data = csp_poly, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_poly)

# run the full multi-level model for monoclonal infections for all new vs. no new
csp_model_mono <- glmer(symptomatic_status ~ any_old_categories + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), data = csp_mono, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_mono)

# run the full multi-level model for polyclonal infections for all new vs. no new
csp_model_poly <- glmer(symptomatic_status ~ any_old_categories + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), data = csp_poly, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_poly)





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



#### ---- look more into persistent infections ----- ####

# just look at persistent infections
persistent_only = csp_data[which(str_detect(csp_data$haplotype_category,"persistent")),]

# now look at the participant level in all infections
test = csp_data %>%
  select(unq_memID,sample_id_date,haplotype_category)

# read in the csp data set for potential pre-symptomatic infections
pre_symp_data = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/CSP_subsets/pre_symptomatic/spat_1b_pre_symptomatic_data_csp_24MAR2020.csv")
pre_symp_data_p2 = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/CSP_subsets/pre_symptomatic/spat_1b_pre_symptomatic_data_csp_26MAR2020.csv")

# merge the two pre_symp_data sets together
pre_symp_data_p2 = pre_symp_data_p2 %>% select(haps_shared_btwn_pairs,unq_memID,sample_id_date,proportion_haps_shared)
all_pre_symp_data = left_join(pre_symp_data,pre_symp_data_p2,by=c("unq_memID","sample_id_date"))

# figure out how many participants had possible pre-symptomatic infections
all_pre_symp_data$pre_symptomatic = ifelse(all_pre_symp_data$haps_shared_btwn_pairs > 0 & all_pre_symp_data$symptomatic_status == "asymptomatic infection","yes",NA)
table(all_pre_symp_data$pre_symptomatic, useNA = "always")

# now merge in the pre-symptomatic infections and remove them
all_pre_symp_data = all_pre_symp_data %>% select(sample_name_dbs,pre_symptomatic,proportion_haps_shared)
all_data = left_join(csp_data,all_pre_symp_data,by="sample_name_dbs")

# now remove pre-symptomatic infections
all_data = all_data %>% filter(is.na(pre_symptomatic))

# now rerun the models
# run a multi-level logistic regression model
all_data$haplotype_category = as.factor(all_data$haplotype_category)
all_data$haplotype_category = relevel(all_data$haplotype_category,ref="all new")
levels(all_data$haplotype_category)
csp_model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), 
                     data = all_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))

# now run a model with moi added in
csp_model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + 
                       haplotype_number + (1|unq_memID),family=binomial(link = "logit"), 
                     data = csp_data, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1)


