# --------------------------------------- #
#    Final persistent infection analysis  #
#             Mozzie phase 1              #
#                 Aim 1B                  #
#            October 18, 2020             #
#               K. Sumner                 #
# --------------------------------------- #

# will call repeated infections: "reinfection"

#### ------- load libraries -------- ####
library(tidyverse)
library(car)
library(ggbeeswarm)
library(lme4)
library(glmmTMB)



#### ------ read in the data sets ------- ####

# read in the ama data set with the first infection
ama_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/with first infection/ama_data_aim1b_11JUN2020.rds")

# read in the csp data set with the first infection
csp_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/with first infection/csp_data_aim1b_11JUN2020.rds")

# read in the csp pre-symptomatic data set
csp_pre = read_rds("Desktop/csp_asymp_presymp_22MAR2021.rds")

# read in the csp pre-symptomatic data set
ama_pre = read_rds("Desktop/ama_asymp_presymp_22MAR2021.rds")

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


#### ------ figure out time between persistent infections ------- ####

# calculate number of days between persistent infections
ama_data = arrange(ama_data,unq_memID,sample_id_date)
csp_data = arrange(csp_data,unq_memID,sample_id_date)


# calculate the time between each infection for each person
# for ama
unq_memID_start_date = ama_data[match(unique(ama_data$unq_memID), ama_data$unq_memID),]
days_btwn_infxns = rep(NA,nrow(ama_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(ama_data)){
    if (unq_memID_start_date$unq_memID[i] == ama_data$unq_memID[j]){
      if (ama_data$sample_id_date[j] == unq_memID_start_date$sample_id_date[i]){
        days_btwn_infxns[j] = 0
      } else {
        days_btwn_infxns[j] = ama_data$sample_id_date[j] - ama_data$sample_id_date[j-1]
      }
    }
  }
}
summary(days_btwn_infxns)  
ama_data$days_btwn_infxns = days_btwn_infxns
# for csp
unq_memID_start_date = csp_data[match(unique(csp_data$unq_memID), csp_data$unq_memID),]
days_btwn_infxns = rep(NA,nrow(csp_data))
for (i in 1:nrow(unq_memID_start_date)){
  for (j in 1:nrow(csp_data)){
    if (unq_memID_start_date$unq_memID[i] == csp_data$unq_memID[j]){
      if (csp_data$sample_id_date[j] == unq_memID_start_date$sample_id_date[i]){
        days_btwn_infxns[j] = 0
      } else {
        days_btwn_infxns[j] = csp_data$sample_id_date[j] - csp_data$sample_id_date[j-1]
      }
    }
  }
}
summary(days_btwn_infxns)  
csp_data$days_btwn_infxns = days_btwn_infxns


#### ------ look at the days before infections for symptomatic infections ------- ####

# this is a way to look at pre-symptomatic infections

# first order the data set by date
symptomatic_csp_data = dplyr::arrange(csp_data,unq_memID,sample_id_date)
symptomatic_ama_data = dplyr::arrange(ama_data,unq_memID,sample_id_date)

# look at how many infections each participant had
num_infections_before = symptomatic_csp_data %>%
  group_by(unq_memID) %>%
  summarize (n= n())
num_infections_before = symptomatic_ama_data %>%
  group_by(unq_memID) %>%
  summarize (n= n())

# looks like this worked correctly so apply to everything
symptomatic_csp_data = slice(group_by(symptomatic_csp_data, unq_memID), -1)
symptomatic_ama_data = slice(group_by(symptomatic_ama_data, unq_memID), -1)




#### ------ subset infections to those with consecutive infections within 30 days and with only persistent haplotypes --------- ####

# first subset to infections within 30 days
csp_30days = symptomatic_csp_data %>% filter(days_btwn_infxns <= 30)
ama_30days = symptomatic_ama_data %>% filter(days_btwn_infxns <= 30)
summary(csp_30days$days_btwn_infxns)
summary(ama_30days$days_btwn_infxns)

# take out possible recrudescent infections
# 3 infections for csp
csp_30days = csp_30days[-which(csp_30days$unq_memID == "K01_5" & csp_30days$sample_id_date == "2017-07-06"),]
csp_30days = csp_30days[-which(csp_30days$unq_memID == "K01_7" & csp_30days$sample_id_date == "2017-08-03"),]
csp_30days = csp_30days[-which(csp_30days$unq_memID == "M14_2" & csp_30days$sample_id_date == "2017-08-17"),]
# 1 infection for ama
ama_30days = ama_30days[-which(ama_30days$unq_memID == "K01_7" & ama_30days$sample_id_date == "2017-08-03"),]


#### ----- make a beeswarm plot ------ ####


# subset to just infections with persistent haplotypes
ama_persistent_data = ama_30days %>% filter(str_detect(haplotype_category,"persistent"))
csp_persistent_data = csp_30days %>% filter(str_detect(haplotype_category,"persistent"))

# do kruskal-wallis test instead
kruskal.test(days_btwn_infxns ~ haplotype_category, data=csp_persistent_data)
kruskal.test(days_btwn_infxns ~ haplotype_category, data=ama_persistent_data)

# relabel the categories
# for csp
csp_persistent_data$haplotype_category[which(csp_persistent_data$haplotype_category == "recurrent and persistent")] = "Recurrent and persistent"
csp_persistent_data$haplotype_category[which(csp_persistent_data$haplotype_category == "new, recurrent, and persistent")] = "New, recurrent, and persistent"
csp_persistent_data$haplotype_category[which(csp_persistent_data$haplotype_category == "new and persistent")] = "New and persistent"
csp_persistent_data$haplotype_category[which(csp_persistent_data$haplotype_category == "all persistent")] = "Only persistent"
csp_persistent_data$haplotype_category = factor(csp_persistent_data$haplotype_category,levels=c("Only persistent","New and persistent","Recurrent and persistent","New, recurrent, and persistent"))
# for ama
ama_persistent_data$haplotype_category[which(ama_persistent_data$haplotype_category == "recurrent and persistent")] = "Recurrent and persistent"
ama_persistent_data$haplotype_category[which(ama_persistent_data$haplotype_category == "new, recurrent, and persistent")] = "New, recurrent, and persistent"
ama_persistent_data$haplotype_category[which(ama_persistent_data$haplotype_category == "new and persistent")] = "New and persistent"
ama_persistent_data$haplotype_category[which(ama_persistent_data$haplotype_category == "all persistent")] = "Only persistent"
ama_persistent_data$haplotype_category = factor(ama_persistent_data$haplotype_category,levels=c("Only persistent","New and persistent","Recurrent and persistent","New, recurrent, and persistent"))

# make a beeswarm plot of the days between infections for persistent categories
csp_bees = ggplot(data=csp_persistent_data,aes(x=haplotype_category,y=days_btwn_infxns)) + 
  geom_boxplot() +
  geom_quasirandom(aes(fill=haplotype_category,shape=symptomatic_status),alpha=0.8,size=1.75,color="#000000") + 
  theme_bw() +
  xlab("") +
  ylab("Number of days since previous asymptomatic infection") +
  scale_fill_manual(values = c("#cccccc","#969696","#636363","#252525")) +
  scale_shape_manual(values = c(21, 24)) +
  coord_flip() +
  labs(shape = "Symptomatic status") +
  theme(legend.position = "top") +
  guides(fill = FALSE)
csp_bees
ama_bees = ggplot(data=ama_persistent_data,aes(x=haplotype_category,y=days_btwn_infxns)) + 
  geom_boxplot() +
  geom_quasirandom(aes(fill=haplotype_category,shape=symptomatic_status),alpha=0.8,size=1.75,color="#000000") + 
  theme_bw() +
  xlab("") +
  ylab("Number of days since previous asymptomatic infection") +
  scale_fill_manual(values = c("#cccccc","#969696","#636363","#252525")) +
  scale_shape_manual(values = c(21, 24)) +
  coord_flip() +
  labs(shape = "Symptomatic status") +
  theme(legend.position = "top") +
  guides(fill = FALSE)
ama_bees
ggsave(csp_bees, filename="/Users/kelseysumner/Desktop/csp_beeswarm_plot.png", device="png",
       height=4, width=6.5, units="in", dpi=400)
ggsave(ama_bees, filename="/Users/kelseysumner/Desktop/ama_beeswarm_plot.png", device="png",
       height=4, width=6.5, units="in", dpi=400)

# compare the proportion of observations that are symptomatic across categories
table(csp_persistent_data$symptomatic_status,csp_persistent_data$haplotype_category,useNA = "always")
table(ama_persistent_data$symptomatic_status,ama_persistent_data$haplotype_category,useNA = "always")

# compare the proportion of observations that are symptomatic within 14 days compared to greater than 14 days
length(which(csp_persistent_data$symptomatic_status=="symptomatic infection" & csp_persistent_data$days_btwn_infxns <= 14)) # 20/30 = 66.7%
length(which(ama_persistent_data$symptomatic_status=="symptomatic infection" & ama_persistent_data$days_btwn_infxns <= 14)) # 18/28 = 64.3%




#### ----- run models ------ ####

# for csp
# take out the infections with recurrent haplotypes
all_persistent_data_csp = csp_30days[which((str_detect(csp_30days$haplotype_category,"persistent"))),]
table(all_persistent_data_csp$haplotype_category, useNA = "always")
all_persistent_data_csp$haplotype_category = as.character(all_persistent_data_csp$haplotype_category)
all_persistent_data_csp$haplotype_category = as.factor(all_persistent_data_csp$haplotype_category)
levels(all_persistent_data_csp$haplotype_category)
all_persistent_data_csp$haplotype_category = relevel(all_persistent_data_csp$haplotype_category,ref="all persistent")
# merge in covariates
csp_cov_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_csp_final_model_data_21JUL2020.rds")
csp_cov_data = csp_cov_data %>% select(sample_name_dbs,add_cat_number_prior_infections,mosquito_week_count_cat_add,moi_cat)
all_persistent_data_csp = left_join(all_persistent_data_csp,csp_cov_data,by="sample_name_dbs")
all_persistent_data_csp = rename(all_persistent_data_csp,unq_memID = unq_memID.x)
all_persistent_data_csp$unq_memID.y <- NULL
length(which(is.na(all_persistent_data_csp$moi_cat)))
all_persistent_data_csp$symptomatic_status = as.factor(all_persistent_data_csp$symptomatic_status)
levels(all_persistent_data_csp$symptomatic_status)
# create a new category comparing infections with only persistent haplotypes to those with other types of haplotypes in addition to persistent ones
all_persistent_data_csp$persistent_category = ifelse(all_persistent_data_csp$haplotype_category=="all persistent","Only persistent haplotypes","Mix of persistent and other haplotypes")
table(all_persistent_data_csp$persistent_category,all_persistent_data_csp$haplotype_category,useNA = "always")
table(all_persistent_data_csp$persistent_category,all_persistent_data_csp$symptomatic_status,useNA = "always") # good positivity
all_persistent_data_csp$persistent_category = as.factor(all_persistent_data_csp$persistent_category)
all_persistent_data_csp$persistent_category = relevel(all_persistent_data_csp$persistent_category,ref="Only persistent haplotypes")
# now rerun the model
csp_model_1 <- glmmTMB(symptomatic_status ~ persistent_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = all_persistent_data_csp)
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))
# mix persistent compared to only persistent: OR 0.77 (95% CI 0.21 to 2.75)
# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(table1[2,3],NA,table1[3,3],NA,table1[4,3],NA,table1[5,3],NA,table1[6,3])
lower_ci = c(table1[2,1],NA,table1[3,1],NA,table1[4,1],NA,table1[5,1],NA,table1[6,1])
upper_ci = c(table1[2,2],NA,table1[3,2],NA,table1[4,2],NA,table1[5,2],NA,table1[6,2])
names = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
library(ggplot2)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1,1,1,1,1,1,1,1),colour=c("#000000","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10", breaks = c(0,0.2,0.5,1,2,5,10)) +
  theme_bw()
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_model_all_3_categories_all_persistent_subsetinfxns.png", device="png",
       height=4.5, width=7, units="in", dpi=400)

# for ama
# take out the infections with recurrent haplotypes
all_persistent_data_ama = ama_30days[which((str_detect(ama_30days$haplotype_category,"persistent"))),]
table(all_persistent_data_ama$haplotype_category, useNA = "always")
all_persistent_data_ama$haplotype_category = as.character(all_persistent_data_ama$haplotype_category)
all_persistent_data_ama$haplotype_category = as.factor(all_persistent_data_ama$haplotype_category)
levels(all_persistent_data_ama$haplotype_category)
all_persistent_data_ama$haplotype_category = relevel(all_persistent_data_ama$haplotype_category,ref="all persistent")
# merge in covariates
ama_cov_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 1B/Data/persistent data/without first infection/aim1b_ama_final_model_data_21JUL2020.rds")
ama_cov_data = ama_cov_data %>% select(sample_name_dbs,add_cat_number_prior_infections,mosquito_week_count_cat_add,moi_cat)
all_persistent_data_ama = left_join(all_persistent_data_ama,ama_cov_data,by="sample_name_dbs")
all_persistent_data_ama = rename(all_persistent_data_ama,unq_memID = unq_memID.x)
all_persistent_data_ama$unq_memID.y <- NULL
length(which(is.na(all_persistent_data_ama$moi_cat)))
all_persistent_data_ama$symptomatic_status = as.factor(all_persistent_data_ama$symptomatic_status)
levels(all_persistent_data_ama$symptomatic_status)
# create a new category comparing infections with only persistent haplotypes to those with other types of haplotypes in addition to persistent ones
all_persistent_data_ama$persistent_category = ifelse(all_persistent_data_ama$haplotype_category=="all persistent","Only persistent haplotypes","Mix of persistent and other haplotypes")
table(all_persistent_data_ama$persistent_category,all_persistent_data_ama$haplotype_category,useNA = "always")
table(all_persistent_data_ama$persistent_category,all_persistent_data_ama$symptomatic_status,useNA = "always") # good positivity
all_persistent_data_ama$persistent_category = as.factor(all_persistent_data_ama$persistent_category)
all_persistent_data_ama$persistent_category = relevel(all_persistent_data_ama$persistent_category,ref="Only persistent haplotypes")
# now rerun the model
ama_model_1 <- glmmTMB(symptomatic_status ~ persistent_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = all_persistent_data_ama)
summary(ama_model_1)
performance::icc(ama_model_1)
exp(confint(ama_model_1,method="Wald"))
# mix persistent compared to only persistent: OR 0.24 (95% CI 0.06 to 1.00)
# make a forest plot of results
table1 = exp(confint(ama_model_1,method="Wald"))
summary(ama_model_1)
estimates = c(table1[2,3],NA,table1[3,3],NA,table1[4,3],NA,table1[5,3],NA,table1[6,3])
lower_ci = c(table1[2,1],NA,table1[3,1],NA,table1[4,1],NA,table1[5,1],NA,table1[6,1])
upper_ci = c(table1[2,2],NA,table1[3,2],NA,table1[4,2],NA,table1[5,2],NA,table1[6,2])
names = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
library(ggplot2)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1,1,1,1,1,1,1,1),colour=c("#000000","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10", breaks = c(0,0.2,0.5,1,2,5,10)) +
  theme_bw()
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/ama_aim1b_model_all_3_categories_all_persistent_subsetinfxns.png", device="png",
       height=4.5, width=7, units="in", dpi=400)


#### -------- test effect measure modification by age for persistent infections ------- ####


### for csp

# change age cat ref
under15_csp = all_persistent_data_csp %>%
  filter(age_cat_baseline == "15 years or less")
over15_csp = all_persistent_data_csp %>%
  filter(age_cat_baseline == ">15 years")

# run the model for 15 years or less
csp_model_age_under15 <- glmer(symptomatic_status ~ persistent_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = under15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_under15)
performance::icc(csp_model_age_under15)
exp(confint(csp_model_age_under15,method="Wald"))


# run the model for >15 year
csp_model_age_over15 <- glmer(symptomatic_status ~ persistent_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = over15_csp, control = glmerControl(optimizer="bobyqa"))
summary(csp_model_age_over15)
performance::icc(csp_model_age_over15)
exp(confint(csp_model_age_over15,method="Wald"))


# now compare nested models 
# model with interaction term
model_1_interxn <- glmer(symptomatic_status ~ persistent_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + haplotype_category*age_cat_baseline + moi_cat +  (1|unq_memID),family=binomial(link = "logit"), data = all_persistent_data_csp, control = glmerControl(optimizer="bobyqa"))
summary(model_1_interxn)
performance::icc(model_1_interxn)
# model without interaction term
model_1 <- glmer(symptomatic_status ~ persistent_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat +(1|unq_memID),family=binomial(link = "logit"), data = all_persistent_data_csp, control = glmerControl(optimizer="bobyqa"))
summary(model_1)
performance::icc(model_1)
anova(model_1_interxn,model_1) # model 1 better
# really having some convergence issues can't compare



### for ama

# change age cat ref
under15_ama = all_persistent_data_ama %>%
  filter(age_cat_baseline == "15 years or less")
over15_ama = all_persistent_data_ama %>%
  filter(age_cat_baseline == ">15 years")

# run the model for 15 years or less
ama_model_age_under15 <- glmer(symptomatic_status ~ persistent_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = under15_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_under15)
performance::icc(ama_model_age_under15)
exp(confint(ama_model_age_under15,method="Wald"))


# run the model for >15 year
ama_model_age_over15 <- glmer(symptomatic_status ~ persistent_category + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), data = over15_ama, control = glmerControl(optimizer="bobyqa"))
summary(ama_model_age_over15)
performance::icc(ama_model_age_over15)
exp(confint(ama_model_age_over15,method="Wald"))


# now compare nested models 
# model with interaction term
model_1_interxn <- glmer(symptomatic_status ~ persistent_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + haplotype_category*age_cat_baseline + moi_cat +  (1|unq_memID),family=binomial(link = "logit"), data = all_persistent_data_ama, control = glmerControl(optimizer="bobyqa"))
summary(model_1_interxn)
performance::icc(model_1_interxn)
# model without interaction term
model_1 <- glmer(symptomatic_status ~ persistent_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat +(1|unq_memID),family=binomial(link = "logit"), data = all_persistent_data_ama, control = glmerControl(optimizer="bobyqa"))
summary(model_1)
performance::icc(model_1)
anova(model_1_interxn,model_1) # model 1 better
# really having some convergence issues can't compare


#### --------- make table 1 of covariates across symptomatic status -------- ####

# create table 1: distribution of symptomatic status across covariates
# below shows model covariates to include and data set to use
# csp_model_1 <- glmer(symptomatic_status ~ haplotype_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
# data = all_persistent_data_csp, control = glmerControl(optimizer="bobyqa"))

## ------ do for csp first 

# make separate data sets for asymptomatic and symptomatic infections
asymp_data = all_persistent_data_csp %>%
  filter(symptomatic_status == "asymptomatic infection")
symp_data = all_persistent_data_csp %>%
  filter(symptomatic_status == "symptomatic infection")

# check the maximum number of csp infections included in this data set with no persistent haplotypes
length(unique(all_persistent_data_csp$unq_memID))
max_infections = all_persistent_data_csp %>%
  group_by(unq_memID) %>%
  summarize(n=n())
max(max_infections$n)

# haplotype category
table(all_persistent_data_csp$persistent_category,all_persistent_data_csp$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(all_persistent_data_csp$persistent_category,all_persistent_data_csp$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.0003401*6

# age
table(all_persistent_data_csp$age_cat_baseline,all_persistent_data_csp$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(all_persistent_data_csp$age_cat_baseline,all_persistent_data_csp$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.6058*6

# number of prior malaria infections
table(all_persistent_data_csp$add_cat_number_prior_infections,all_persistent_data_csp$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(all_persistent_data_csp$add_cat_number_prior_infections,all_persistent_data_csp$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.1335*6

# transmission season (based on mosquitoes)
table(all_persistent_data_csp$mosquito_week_count_cat_add,all_persistent_data_csp$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(all_persistent_data_csp$mosquito_week_count_cat_add,all_persistent_data_csp$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.1259*6

# multiplicity of infection
table(all_persistent_data_csp$moi_cat,all_persistent_data_csp$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(all_persistent_data_csp$moi_cat,all_persistent_data_csp$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
3.034e-05*6


## ------ now do for ama

# make separate data sets for asymptomatic and symptomatic infections
asymp_data = all_persistent_data_ama %>%
  filter(symptomatic_status == "asymptomatic infection")
symp_data = all_persistent_data_ama %>%
  filter(symptomatic_status == "symptomatic infection")

# check the maximum number of ama infections included in this data set with no persistent haplotypes
length(unique(all_persistent_data_ama$unq_memID))
max_infections = all_persistent_data_ama %>%
  group_by(unq_memID) %>%
  summarize(n=n())
max(max_infections$n)

# haplotype category
table(all_persistent_data_ama$persistent_category,all_persistent_data_ama$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(all_persistent_data_ama$persistent_category,all_persistent_data_ama$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.0001305*6

# age
table(all_persistent_data_ama$age_cat_baseline,all_persistent_data_ama$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(all_persistent_data_ama$age_cat_baseline,all_persistent_data_ama$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.9193*6

# number of prior malaria infections
table(all_persistent_data_ama$add_cat_number_prior_infections,all_persistent_data_ama$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(all_persistent_data_ama$add_cat_number_prior_infections,all_persistent_data_ama$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.04144*6

# transmission season (based on mosquitoes)
table(all_persistent_data_ama$mosquito_week_count_cat_add,all_persistent_data_ama$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(all_persistent_data_ama$mosquito_week_count_cat_add,all_persistent_data_ama$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.1531*6

# multiplicity of infection
table(all_persistent_data_ama$moi_cat,all_persistent_data_ama$symptomatic_status, useNA = "always")
# first make a contingency table
tbl = table(all_persistent_data_ama$moi_cat,all_persistent_data_ama$symptomatic_status)
# then do a chi-squared test
chisq.test(tbl)
# correct for repeated measures
0.001011*6



#### ------- run a regression model for persistent infections removing double-counted pre-symptomatic infections ------ ####

## first for csp

# first remove asymptomatic infections that are in the pre-symptomatic list
pre_symptomatic_analysis_csp = all_persistent_data_csp[-which(all_persistent_data_csp$sample_name_dbs %in% csp_pre$sample_name_dbs),]
intersect(all_persistent_data_csp$sample_name_dbs,csp_pre$sample_name_dbs)
# this removed 4 asymptomatic infections
# looks good

# now rerun the model
csp_model_1 <- glmmTMB(symptomatic_status ~ persistent_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = pre_symptomatic_analysis_csp)
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))
# mix persistent compared to only persistent: 0.78 (95% CI: 0.21 to 2.88)
# make a forest plot of results
table1 = exp(confint(csp_model_1,method="Wald"))
summary(csp_model_1)
estimates = c(table1[2,3],NA,table1[3,3],NA,table1[4,3],NA,table1[5,3],NA,table1[6,3])
lower_ci = c(table1[2,1],NA,table1[3,1],NA,table1[4,1],NA,table1[5,1],NA,table1[6,1])
upper_ci = c(table1[2,2],NA,table1[3,2],NA,table1[4,2],NA,table1[5,2],NA,table1[6,2])
names = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
library(ggplot2)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1,1,1,1,1,1,1,1),colour=c("#000000","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10", breaks = c(0,0.2,0.5,1,2,5,10)) +
  theme_bw()
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/csp_aim1b_all_persistent_sensitivity_analysis.png", device="png",
       height=4.5, width=7, units="in", dpi=400)



## then for ama

# first remove asymptomatic infections that are in the pre-symptomatic list
pre_symptomatic_analysis_ama = all_persistent_data_ama[-which(all_persistent_data_ama$sample_name_dbs %in% ama_pre$sample_name_dbs),]
intersect(all_persistent_data_ama$sample_name_dbs,ama_pre$sample_name_dbs)
# this removed 3 asymptomatic infections
# looks good

# now rerun the model
ama_model_1 <- glmmTMB(symptomatic_status ~ persistent_category + age_cat_baseline + add_cat_number_prior_infections + mosquito_week_count_cat_add + moi_cat + (1|unq_memID),family=binomial(link = "logit"), 
                       data = pre_symptomatic_analysis_ama)
summary(ama_model_1)
performance::icc(ama_model_1)
exp(confint(ama_model_1,method="Wald"))
# mix persistent compared to only persistent: 0.24 (95% CI: 0.06 to 1.00)
# make a forest plot of results
table1 = exp(confint(ama_model_1,method="Wald"))
summary(ama_model_1)
estimates = c(table1[2,3],NA,table1[3,3],NA,table1[4,3],NA,table1[5,3],NA,table1[6,3])
lower_ci = c(table1[2,1],NA,table1[3,1],NA,table1[4,1],NA,table1[5,1],NA,table1[6,1])
upper_ci = c(table1[2,2],NA,table1[3,2],NA,table1[4,2],NA,table1[5,2],NA,table1[6,2])
names = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Mixed types of haplotypes vs. only persistent haplotypes"," ","Participant age >15 years","  ",">3 prior malaria infections","     ","High transmission season","        ","High multiplicity of infection"))
# create a forest plot
library(forcats)
library(ggplot2)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(1,1,1,1,1,1,1,1,1),colour=c("#000000","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds of symptomatic malaria (95% CI)") +
  scale_y_continuous(trans="log10", breaks = c(0,0.2,0.5,1,2,5,10)) +
  theme_bw()
fp
# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/ama_aim1b_all_persistent_sensitivity_analysis.png", device="png",
       height=4.5, width=7, units="in", dpi=400)




#### ------ now rerun the models but removing symptomatic infections that were <15 days after an asymptomatic infection -------- ####

## first for csp

# first remove asymptomatic infections that are in the pre-symptomatic list
removing_symptomatic_14days_csp = all_persistent_data_csp[-which(all_persistent_data_csp$symptomatic_status == "symptomatic infection" & all_persistent_data_csp$days_btwn_infxns < 15),]
# this removes 20 symptomatic infections

# now rerun the model
csp_model_1 <- glm(symptomatic_status ~ persistent_category,family=binomial(link = "logit"), 
                       data = removing_symptomatic_14days_csp)
summary(csp_model_1)
performance::icc(csp_model_1)
exp(confint(csp_model_1,method="Wald"))
exp(-0.4716)
# with small sample sizes, causes huge estimates

# look at a crude table
table(removing_symptomatic_14days_csp$persistent_category,removing_symptomatic_14days_csp$symptomatic_status,useNA = "always")
crude_odds_exposed = (7/(93))/(1-(7/93))
crude_odds_unexposed = (3/(26))/(1-(3/26))
crude_odds_ratio = crude_odds_exposed/crude_odds_unexposed
crude_odds_ratio

