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
csp_model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), 
                     data = no_persistent_data_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))
# all new: OR 1.06 (95% CI 1.11 to 7.50)
# new and current: OR -1.01 (95% CI 0.11 to 1.24)
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
  geom_pointrange(size=c(3,3,1,1,1,1,1,1,1),colour=c("#b2182b","#ef8a62","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
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


# now run the new vs recurrent model for ama
# take out the infections with persistent haplotypes
no_persistent_data_ama = ama_data[which(!(str_detect(ama_data$haplotype_category,"persistent"))),]
table(no_persistent_data_ama$haplotype_category, useNA = "always")
no_persistent_data_ama$haplotype_category = as.character(no_persistent_data_ama$haplotype_category)
no_persistent_data_ama$haplotype_category = as.factor(no_persistent_data_ama$haplotype_category)
levels(no_persistent_data_ama$haplotype_category)
no_persistent_data_ama$haplotype_category = relevel(no_persistent_data_ama$haplotype_category,ref="all recurrent")
# now rerun the model
ama_model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + (1|unq_memID),family=binomial(link = "logit"), 
                     data = no_persistent_data_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_1)
performance::icc(ama_model_1)
exp(confint(ama_model_1,method="Wald"))
# make a forest plot of results
table1 = exp(confint(ama_model_1,method="Wald"))
summary(ama_model_1)
estimates = c(exp(0.6811),exp(-0.2676),NA,exp(-0.1784),exp(-1.4490),NA,exp(-0.8799),NA,exp(0.7391))
lower_ci = c(table1[3,1],table1[4,1],NA,table1[6,1],table1[5,1],NA,table1[7,1],NA,table1[8,1])
upper_ci = c(table1[3,2],table1[4,2],NA,table1[6,2],table1[5,2],NA,table1[7,2],NA,table1[8,2])
names = c("All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes"," ","Participant age 5-15 years","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season"))
# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,3,1,1,1,1,1,1,1),colour=c("#b2182b","#ef8a62","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8),trans="log10") +
  theme_bw() +
  theme(text = element_text(size=15)) 
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/ama_aim1b_model_all_3_categories_no_persistent.png", device="png",
       height=6, width=10, units="in", dpi=400)



#### -------- test for effect measure modification by age -------- ####


### for csp

# change age cat ref
under5_csp = no_persistent_data_csp %>%
  filter(age_cat_baseline == "<5 years")
from5to15_csp = no_persistent_data_csp %>%
  filter(age_cat_baseline == "5-15 years")
over15_csp = no_persistent_data_csp %>%
  filter(age_cat_baseline == ">15 years")

# run the model for <5 year
csp_model_age_under5 <- glmer(symptomatic_status ~ haplotype_category + (1|unq_memID),family=binomial(link = "logit"), data = under5_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_under5)
performance::icc(csp_model_age_under5)
exp(confint(csp_model_age_under5,method="Wald"))

# run the model for 5-15 year
csp_model_age_5to15 <- glmer(symptomatic_status ~ haplotype_category + (1|unq_memID),family=binomial(link = "logit"), data = from5to15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_5to15)
performance::icc(csp_model_age_5to15)
exp(confint(csp_model_age_5to15,method="Wald"))

# run the model for >15 year
csp_model_age_over15 <- glmer(symptomatic_status ~ haplotype_category + (1|unq_memID),family=binomial(link = "logit"), data = over15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_over15)
performance::icc(csp_model_age_over15)
exp(confint(csp_model_age_over15,method="Wald"))


# create a plot
names = c("Participant age <5 years","All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes","","Participant age 5-15 years"," All new vs. all recurrent haplotypes"," New and recurrent vs. all recurrent haplotypes"," ","Participant age >15 years","  All new vs. all recurrent haplotypes","  New and recurrent vs. all recurrent haplotypes")
estimate = c(NA,exp(1.211),exp(-1.499),NA,NA,exp(1.1767),exp(-0.7150),NA,NA,exp(1.3959),exp(-0.7419))
lower_ci = c(NA,0.06984692,0.00818501,NA,NA,1.19108882,0.11332282,NA,NA,0.473774438,0.028794117)
upper_ci = c(NA,161.178548,6.094928,NA,NA,8.8336888,2.1116154,NA,NA,34.4239338,7.8751353)
forest_plot_df = data.frame(names,estimate,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Participant age <5 years","All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes","","Participant age 5-15 years"," All new vs. all recurrent haplotypes"," New and recurrent vs. all recurrent haplotypes"," ","Participant age >15 years","  All new vs. all recurrent haplotypes","  New and recurrent vs. all recurrent haplotypes"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Participant age <5 years","All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes","","Participant age 5-15 years"," All new vs. all recurrent haplotypes"," New and recurrent vs. all recurrent haplotypes"," ","Participant age >15 years","  All new vs. all recurrent haplotypes","  New and recurrent vs. all recurrent haplotypes"))
# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimate, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(colour=c(NA,"#2166ac","#67a9cf",NA,NA,"#2166ac","#67a9cf",NA,NA,"#2166ac","#67a9cf"),size=c(2,2,2,2,2,2,2,2,2,2,2)) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.y = element_text(face=c("plain","plain","bold","plain","plain","plain","bold","plain","plain","plain","bold"))) +
  scale_y_continuous(labels = comma,trans="log10")
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_new_age_stratified.png", device="png",
       height=6, width=10, units="in", dpi=400)

# now compare nested models 
# model with interaction term
model_1_interxn <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + haplotype_category*age_cat_baseline + (1|unq_memID),family=binomial(link = "logit"), data = no_persistent_data_csp, control = glmerControl(optimizer="bobyqa"))
summary(model_1_interxn)
performance::icc(model_1_interxn)
# model without interaction term
model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + (1|unq_memID),family=binomial(link = "logit"), data = no_persistent_data_csp, control = glmerControl(optimizer="bobyqa"))
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
ama_model_age_under5 <- glmer(symptomatic_status ~ haplotype_category + (1|unq_memID),family=binomial(link = "logit"), data = under5_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_under5)
performance::icc(ama_model_age_under5)
exp(confint(ama_model_age_under5,method="Wald"))

# run the model for 5-15 year
ama_model_age_5to15 <- glmer(symptomatic_status ~ haplotype_category + (1|unq_memID),family=binomial(link = "logit"), data = from5to15_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_5to15)
performance::icc(ama_model_age_5to15)
exp(confint(ama_model_age_5to15,method="Wald"))

# run the model for >15 year
ama_model_age_over15 <- glmer(symptomatic_status ~ haplotype_category + (1|unq_memID),family=binomial(link = "logit"), data = over15_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_over15)
performance::icc(ama_model_age_over15)
exp(confint(ama_model_age_over15,method="Wald"))


# create a plot
names = c("Participant age <5 years","All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes","","Participant age 5-15 years"," All new vs. all recurrent haplotypes"," New and recurrent vs. all recurrent haplotypes"," ","Participant age >15 years","  All new vs. all recurrent haplotypes","  New and recurrent vs. all recurrent haplotypes")
estimate = c(NA,exp(0.5554),exp(-2.0709),NA,NA,exp(0.87926),exp(-0.06679),NA,NA,exp(9.00688),exp(5.00628))
lower_ci = c(NA,2.514864e-02,4.461510e-04,NA,NA,0.63142099,0.18487844,NA,NA,6.174115e+02,1.454258e+02)
upper_ci = c(NA,120.75800,35.62332,NA,NA,9.1916379,4.7326445,NA,NA,1.078197e+05,1.533766e+02)
forest_plot_df = data.frame(names,estimate,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Participant age <5 years","All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes","","Participant age 5-15 years"," All new vs. all recurrent haplotypes"," New and recurrent vs. all recurrent haplotypes"," ","Participant age >15 years","  All new vs. all recurrent haplotypes","  New and recurrent vs. all recurrent haplotypes"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Participant age <5 years","All new vs. all recurrent haplotypes","New and recurrent vs. all recurrent haplotypes","","Participant age 5-15 years"," All new vs. all recurrent haplotypes"," New and recurrent vs. all recurrent haplotypes"," ","Participant age >15 years","  All new vs. all recurrent haplotypes","  New and recurrent vs. all recurrent haplotypes"))
# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimate, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(colour=c(NA,"#2166ac","#67a9cf",NA,NA,"#2166ac","#67a9cf",NA,NA,"#2166ac","#67a9cf"),size=c(2,2,2,2,2,2,2,2,2,2,2)) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  theme_bw() +
  theme(text = element_text(size=15),axis.text.y = element_text(face=c("plain","plain","bold","plain","plain","plain","bold","plain","plain","plain","bold"))) +
  scale_y_continuous(labels = comma,trans="log10")
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/ama_aim1b_model_all_new_age_stratified.png", device="png",
       height=6, width=10, units="in", dpi=400)

# now compare nested models 
# model with interaction term
model_1_interxn <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + haplotype_category*age_cat_baseline + (1|unq_memID),family=binomial(link = "logit"), data = no_persistent_data_ama, control = glmerControl(optimizer="bobyqa"))
summary(model_1_interxn)
performance::icc(model_1_interxn)
# model without interaction term
model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + (1|unq_memID),family=binomial(link = "logit"), data = no_persistent_data_ama, control = glmerControl(optimizer="bobyqa"))
summary(model_1)
performance::icc(model_1)
anova(model_1_interxn,model_1) # model 1 better



#### ----- look at a subset of haplotypes that are only in infections after taking AL ------- ####

# first organize the data sets by person and then date
ama_data = arrange(ama_data,unq_memID,sample_id_date)
csp_data = arrange(csp_data,unq_memID,sample_id_date)

# now loop through each person and see if they were prescribed AL in the study
unq_memID_start_date = ama_data[match(unique(ama_data$unq_memID), ama_data$unq_memID),]
after_al = rep(NA,nrow(ama_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(ama_data)){
    if (unq_memID_start_date$unq_memID[i] == ama_data$unq_memID[j]){
      if (ama_data$prescription[j-1]=="prescribed"){
        after_al[j] = "yes"
      } else {
        after_al[j] = "no"
      }
    }
  }
}
summary(after_al)  
ama_data$after_al = after_al





