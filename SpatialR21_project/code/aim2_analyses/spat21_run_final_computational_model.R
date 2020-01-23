# -------------------------------------- #
#           Spat21/Mozzie Study          #
#     Run final computational model      #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#           January 14, 2020             #
# -------------------------------------- #

# good resource for trouble shooting convergence problems
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)
library(betareg)
library(lme4)
library(ggplot2)



#### ----- read in the data sets ----- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/model data/spat21_final_model_data_set_21JAN2020.rds")



#### ------ check covariate coding ------- ####

# check the covariates
str(model_data$sample_id_human)
str(model_data$HH_ID_human)
str(model_data$unq_memID)
str(model_data$p_te_all)
str(model_data$age_cat_baseline)
model_data$age_cat_baseline = as.factor(model_data$age_cat_baseline)
str(model_data$village_name)
model_data$village_name = as.factor(model_data$village_name)
model_data$village_name = relevel(model_data$village_name,ref = "Maruti")
str(model_data$mosquito_week_count_cat)
model_data$mosquito_week_count_cat = as.factor(model_data$mosquito_week_count_cat)
str(model_data$pfr364Q_std_combined_rescaled)
str(model_data$aim2_exposure)
model_data$aim2_exposure = as.factor(model_data$aim2_exposure)
model_data$aim2_exposure = relevel(model_data$aim2_exposure,ref = "symptomatic infection")

# create a variable for parasite density cubed
summary(model_data$pfr364Q_std_combined)
model_data$pfr364Q_std_combined_cubic = model_data$pfr364Q_std_combined_rescaled*model_data$pfr364Q_std_combined_rescaled*model_data$pfr364Q_std_combined_rescaled
summary(model_data$pfr364Q_std_combined_cubic)

# create a variable for parasite density ln transformed
model_data$pfr364Q_std_combined_ln = log(model_data$pfr364Q_std_combined)
skewness(model_data$pfr364Q_std_combined_ln)
kurtosis(model_data$pfr364Q_std_combined_ln)
# fixed high skew and kurtosis but still has scaling problems
model_data$pfr364Q_std_combined_ln = scale(model_data$pfr364Q_std_combined_ln)
summary(model_data$pfr364Q_std_combined_ln)
skewness(model_data$pfr364Q_std_combined_ln)
kurtosis(model_data$pfr364Q_std_combined_ln)



#### -------- create two copies for everyone for logistic regression ------- ####

# first make two copies of everyone
copy_data = model_data

# now make a new variable in the original data set that is having a transmission event or not
model_data$transmission_event = rep("yes",nrow(model_data)) # here, everyone has transmission event
model_data$transmission_weight = model_data$p_te_all

# now make a new variable in the copied data set that is not having a transmission event
copy_data$transmission_event = rep("no",nrow(copy_data)) # here, no one has transmission event
copy_data$transmission_weight = 1-model_data$p_te_all # inverse weight for those that didn't have a transmission event

# now combine the original and copy data sets
combined_data = rbind(model_data,copy_data)

# make the transmission event variable a factor
combined_data$transmission_event = as.factor(combined_data$transmission_event)
combined_data$transmission_event = relevel(combined_data$transmission_event,ref="no")




#### -------- now run a crude logistic regression model ------- ####

# this is a regular logistic regression model and not multi-level model with the original model data set
regular_logistic = glm(p_te_all ~ aim2_exposure, family=binomial(link="logit"), data=model_data)
summary(regular_logistic)

# this is a regular logistic regression model and not multi-level model with the combined data set jess suggested
# that has two observations for each person
combo_logistic = glm(transmission_event ~ aim2_exposure, family=binomial(link="logit"), data=combined_data, weights=transmission_weight)
summary(combo_logistic)
table(combined_data$transmission_event)
table(combined_data$transmission_weight)
# both these models produce the exact same answer

# now try running a multi-level logistic regression model on the original model data set
multi_model <- glmer(p_te_all~aim2_exposure+age_cat_baseline+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(multi_model)
# this one did not have trouble converging

# now run a multi-level logistic regression model on the combined data set jess suggested
# that has two observations for each person
combo_multi_model <- glmer(transmission_event~aim2_exposure+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = combined_data, weights=transmission_weight)
summary(combo_multi_model)
# this one did not have trouble converging

# test out the model 2 below but with the combined_data data set
modeltest <- glmer(transmission_event~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = combined_data, weights=transmission_weight)
summary(modeltest)
exp(confint(modeltest,devtol=1e-5)) # won't compute confidence intervals
modeltest_2 <- glmer(transmission_event~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = combined_data, weights=transmission_weight)
summary(modeltest_2)
exp(confint(modeltest_2,devtol=1e-5))
anova(modeltest,modeltest_2)


#### ------ run the final models and do model selection ------- ####

# run the original multi-level model with all covariates and interaction term
model1 <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+age_cat_baseline*aim2_exposure+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model1)
# model had some trouble converging
exp(0.16055)
exp(-0.65805)

# run the model with all covariates but interaction removed
model2 <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(model2)
# model had some trouble converging
anova(model1,model2) # model 2 is better - no interaction between main exposure and age
# check singularity
tt <- getME(model2,"theta")
ll <- getME(model2,"lower")
min(tt[ll==0]) # 0.3410 which is not close to 0 so singularity is not a problem
# If the fit is singular or near-singular, there might be a higher chance of a false positive, 
# a higher chance that the model has actually misconverged, and a reasonable argument that the random effects model should be simplified
# check gradient calculations
derivs1 <- model2@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1)) # 0.1802444, this is small but still larger than typical tolerance of 0.001
max(pmin(abs(sc_grad1),abs(derivs1$gradient))) # still 0.1802444, so gradient is a little of an issue
# now try the model restarting the model based on the previous fit but bumping up the number of iterations to allow it to converge
ss <- getME(model2,c("theta","fixef"))
model2_update <- update(model2,start=ss,control=glmerControl(optCtrl=list(maxfun=2e6))) # still had warnings on gradient
# try loading this new package and looking at different optimizers
library(optimx)
library(dfoptim)
library(reshape2)
library(plyr)
aa <- allFit(model2)
is.OK <- sapply(aa,is,"merMod")
aa.OK <- aa[is.OK]
lapply(aa.OK,function(x) x@optinfo$conv$lme4$messages) # bobyqa and nlminbwrap did not fail to converge
(lliks <- sort(sapply(aa.OK,logLik))) # all have same log-likelihood basically regardless on if converged
# look at the coefficients across different models
aa.fixef <- t(sapply(aa.OK,fixef))
aa.fixef.m <- melt(aa.fixef)
summary(unlist(daply(aa.fixef.m,"Var2",summarise,sd(value)/abs(mean(value))))) # some variability in the coefficients
# now try rerunning the model with one of the optimizers that worked
model2 <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model2)
exp(1.016267)
anova(model1,model2)

# now run the model removing village
model3 <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), control = glmerControl(optimizer="bobyqa"), data = model_data)
summary(model3)
exp(1.07790)
anova(model2,model3)
# model 2 is better
# check gradient calculations
derivs1 <- model3@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1)) # 0.02283682, this is small but still larger than typical tolerance of 0.001

# now run the model removing parasite density but adding back in village
model4 <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model4)
anova(model2,model4) 
exp(1.0189)
# check gradient calculations
derivs1 <- model4@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1)) # 0.3823 so having convergence problems because > 0.001 tolerance

# now run the model removing parasite density and village
model5 <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model5)
exp(1.0756)
anova(model2,model5) # model 2 is better
anova(model4,model5) # model 4 is better
# model 2 is better but this one didn't have convergence issues
# check gradient calculations
derivs1 <- model5@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1)) # 1.242775e-05 so smaller than typical tolerance of 0.001, which is good

# now run the model removing parasite density and village and age
model6 <- glmer(p_te_all~aim2_exposure+mosquito_week_count_cat+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model6)
exp(1.0579)
anova(model2,model6) # model 2 is better
# model 2 is better but this one didn't have convergence issues
# check gradient calculations
derivs1 <- model6@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1)) # 3.497157e-05 so smaller than typical tolerance of 0.001, which is good

# now run the model removing parasite density and village and mosquito_week_count
model7 <- glmer(p_te_all~aim2_exposure+age_cat_baseline+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model7)
anova(model2,model7) # model 2 is better
# model 2 is better but this one didn't have convergence issues

# now run the crude model with no covariates
model8 <- glmer(p_te_all~aim2_exposure+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model8)
exp(1.1126)
anova(model2,model8) # model 2 is better
# model 2 is better but this one didn't have convergence issues

# a few more model comparisons
anova(model4,model5) # model 4 better than 5
anova(model3,model5)
anova(model5,model6) # model 5 better
anova(model5,model7) # model 5 better
anova(model5,model8) # model 5 better


# summary:
# deciding between model 2 and model 5
# model 2 has all original covariates except interaction term
# model 5 has covariates minus the interaction term, parasite density, and village name
# model 2 technically has a better fit via the chi-squared test but had some convergence problems
# model 2 asymptomatic OR estimate: exp(1.13595) = 3.1141
# model 5 asymptomatic OR estimate: exp(1.0756) = 2.9318
# because optimizers that did work had same loglikelihood and convergence values low, think it is okay to go with model 2
# however, it looks like the parasite density and village name covariates didn't have a big change in the main outcome (<10%)
# decide to go with model 5


#### ------ create a forest plot of the final model output ------ ####

# create a data frame of model 5 output
summary(model2)
estimates = c(exp(1.016267),exp(-0.776090),exp(-0.687479),exp(0.220296),exp(-1.477334),exp(-0.002337),exp(-0.526726),exp(-1.113527))
exp(confint.merMod(model2,method="Wald"))
lower_ci = c(1.8799560,0.2823813,0.3100777,0.9953401,0.0897681,0.8705264,0.3718746,0.1552902)
upper_ci = c(4.0604196,0.7499986,0.8154418,1.5608992,0.5803393,1.1433747,0.9377678,0.6944786)
names = c("Asymptomatic infection","Age 5-15 years","Age >15 years","50-99 mosquitoes","100-147 mosquitoes","Parasite density","Village: Kinesamo","Village: Sitabicha")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = as.factor(forest_plot_df$names)

# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=names, y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=2) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds Ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp

# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/forest_plot_aim2_model_continuous_outcome.png", device="png",
       height=9, width=12.5, units="in", dpi=400)


#### ------- make a plot of p_te_all stratified -------- ####

# make a plot of p_te_all 
p_te_all_plot = ggplot(data=model_data,aes(x=p_te_all,fill=relevel(aim2_exposure,ref="asymptomatic infection"))) +
  geom_density(alpha=0.6) + 
  scale_fill_manual(values=c("#ff7f00","#e31a1c")) + 
  labs(fill="Symptomatic status") +
  theme_bw() + 
  xlab("P(TE,all)")
p_te_all_plot
ggsave(p_te_all_plot, filename="/Users/kelseysumner/Desktop/p_te_all_plot.png", device="png",
       height=4, width=7, units="in", dpi=500)


#### ------ make a plot of the odds ratios of p_te_all coded binary -------- ####

# look at a summary of the outcome variable
summary(model_data$p_te_all)
length(which(is.na(model_data$p_te_all)))
hist(model_data$p_te_all)


# make a binary variable for <0.1 or >= 0.1
model_data$outcome_binary_lessthan0.1 = ifelse(model_data$p_te_all < 0.1,"less than 0.1","greater than 0.1")
table(model_data$outcome_binary_lessthan0.1,model_data$p_te_all,useNA = "always")
table(model_data$outcome_binary_lessthan0.1, useNA = "always")
model_data$outcome_binary_lessthan0.1 = factor(model_data$outcome_binary_lessthan0.1)
levels(model_data$outcome_binary_lessthan0.1)
model_data$outcome_binary_lessthan0.1 = relevel(model_data$outcome_binary_lessthan0.1,ref = "less than 0.1")

# make a binary variable for <0.2 or >= 0.2
model_data$outcome_binary_lessthan0.2 = ifelse(model_data$p_te_all < 0.2,"less than 0.2","greater than 0.2")
table(model_data$outcome_binary_lessthan0.2,model_data$p_te_all,useNA = "always")
table(model_data$outcome_binary_lessthan0.2, useNA = "always")
model_data$outcome_binary_lessthan0.2 = factor(model_data$outcome_binary_lessthan0.2)
levels(model_data$outcome_binary_lessthan0.2)
model_data$outcome_binary_lessthan0.2 = relevel(model_data$outcome_binary_lessthan0.2,ref = "less than 0.2")

# make a binary variable for <0.3 or >= 0.3
model_data$outcome_binary_lessthan0.3 = ifelse(model_data$p_te_all < 0.3,"less than 0.3","greater than 0.3")
table(model_data$outcome_binary_lessthan0.3,model_data$p_te_all,useNA = "always")
table(model_data$outcome_binary_lessthan0.3, useNA = "always")
model_data$outcome_binary_lessthan0.3 = factor(model_data$outcome_binary_lessthan0.3)
levels(model_data$outcome_binary_lessthan0.3)
model_data$outcome_binary_lessthan0.3 = relevel(model_data$outcome_binary_lessthan0.3,ref = "less than 0.3")

# make a binary variable for <0.4 or >= 0.4
model_data$outcome_binary_lessthan0.4 = ifelse(model_data$p_te_all < 0.4,"less than 0.4","greater than 0.4")
table(model_data$outcome_binary_lessthan0.4,model_data$p_te_all,useNA = "always")
table(model_data$outcome_binary_lessthan0.4, useNA = "always")
model_data$outcome_binary_lessthan0.4 = factor(model_data$outcome_binary_lessthan0.4)
levels(model_data$outcome_binary_lessthan0.4)
model_data$outcome_binary_lessthan0.4 = relevel(model_data$outcome_binary_lessthan0.4,ref = "less than 0.4")

# make a binary variable for <0.5 or >= 0.5
model_data$outcome_binary_lessthan0.5 = ifelse(model_data$p_te_all < 0.5,"less than 0.5","greater than 0.5")
table(model_data$outcome_binary_lessthan0.5,model_data$p_te_all,useNA = "always")
table(model_data$outcome_binary_lessthan0.5, useNA = "always")
model_data$outcome_binary_lessthan0.5 = factor(model_data$outcome_binary_lessthan0.5)
levels(model_data$outcome_binary_lessthan0.5)
model_data$outcome_binary_lessthan0.5 = relevel(model_data$outcome_binary_lessthan0.5,ref = "less than 0.5")

# make a binary variable for <0.6 or >= 0.6
model_data$outcome_binary_lessthan0.6 = ifelse(model_data$p_te_all < 0.6,"less than 0.6","greater than 0.6")
table(model_data$outcome_binary_lessthan0.6,model_data$p_te_all,useNA = "always")
table(model_data$outcome_binary_lessthan0.6, useNA = "always")
model_data$outcome_binary_lessthan0.6 = factor(model_data$outcome_binary_lessthan0.6)
levels(model_data$outcome_binary_lessthan0.6)
model_data$outcome_binary_lessthan0.6 = relevel(model_data$outcome_binary_lessthan0.6,ref = "less than 0.6")

# make a binary variable for <0.7 or >= 0.7
model_data$outcome_binary_lessthan0.7 = ifelse(model_data$p_te_all < 0.7,"less than 0.7","greater than 0.7")
table(model_data$outcome_binary_lessthan0.7,model_data$p_te_all,useNA = "always")
table(model_data$outcome_binary_lessthan0.7, useNA = "always")
model_data$outcome_binary_lessthan0.7 = factor(model_data$outcome_binary_lessthan0.7)
levels(model_data$outcome_binary_lessthan0.7)
model_data$outcome_binary_lessthan0.7 = relevel(model_data$outcome_binary_lessthan0.7,ref = "less than 0.7")

# make a binary variable for <0.8 or >= 0.8
model_data$outcome_binary_lessthan0.8 = ifelse(model_data$p_te_all < 0.8,"less than 0.8","greater than 0.8")
table(model_data$outcome_binary_lessthan0.8,model_data$p_te_all,useNA = "always")
table(model_data$outcome_binary_lessthan0.8, useNA = "always")
model_data$outcome_binary_lessthan0.8 = factor(model_data$outcome_binary_lessthan0.8)
levels(model_data$outcome_binary_lessthan0.8)
model_data$outcome_binary_lessthan0.8 = relevel(model_data$outcome_binary_lessthan0.8,ref = "less than 0.8")

# make a binary variable for <0.9 or >= 0.9
model_data$outcome_binary_lessthan0.9 = ifelse(model_data$p_te_all < 0.9,"less than 0.9","greater than 0.9")
table(model_data$outcome_binary_lessthan0.9,model_data$p_te_all,useNA = "always")
table(model_data$outcome_binary_lessthan0.9, useNA = "always")
model_data$outcome_binary_lessthan0.9 = factor(model_data$outcome_binary_lessthan0.9)
levels(model_data$outcome_binary_lessthan0.9)
model_data$outcome_binary_lessthan0.9 = relevel(model_data$outcome_binary_lessthan0.9,ref = "less than 0.9")

# binary outcome <0.1 with a logistic model
model.1 <- glmer(outcome_binary_lessthan0.1~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.1)
exp(0.56121)
exp(confint(model.1,method="Wald"))
# converged

# binary outcome <0.2 with a logistic model
model.2 <- glmer(outcome_binary_lessthan0.2~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.2)
exp(0.69727)
exp(confint(model.2, method="Wald"))
# converged

# binary outcome <0.3 with a logistic model
model.3 <- glmer(outcome_binary_lessthan0.3~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.3)
exp(0.64544)
exp(confint(model.3, method="Wald"))
# converged

# binary outcome <0.4 with a logistic model
model.4 <- glmer(outcome_binary_lessthan0.4~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.4)
exp(1.07765)
exp(confint(model.4, method="Wald")) # can't compute confidence interval
# converged

# binary outcome <0.5 with a logistic model
model.5 <- glmer(outcome_binary_lessthan0.5~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.5)
exp(1.18379)
exp(confint(model.5, method="Wald")) # can't compute confidence interval
# converged

# binary outcome <0.6 with a logistic model
model.6 <- glmer(outcome_binary_lessthan0.6~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.6)
exp(1.19655)
exp(confint(model.6, method="Wald")) # can't compute confidence interval
# converged

# binary outcome <0.7 with a logistic model
model.7 <- glmer(outcome_binary_lessthan0.7~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.7)
exp(1.39538)
exp(confint(model.7, method="Wald")) # can't compute confidence interval
# converged

# binary outcome <0.8 with a logistic model
model.8 <- glmer(outcome_binary_lessthan0.8~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.8)
exp(0.7480)
exp(confint(model.8, method="Wald")) # can't compute confidence interval
# model not very identifiable

# binary outcome <0.9 with a logistic model
model.9 <- glmer(outcome_binary_lessthan0.9~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.9)
# model did not work

# read in the model results
model_results = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/computational_model_materials/aim2_binary_outcome_final.csv")

# make a plot of the results
model_results$binary_outcome = as.factor(model_results$binary_outcome)
model_plot = ggplot(data=model_results, aes(x=binary_outcome, y=estimate)) + 
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.1) +
  geom_line() +
  geom_point() + 
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12),trans="log10") +
  theme_bw() +
  ylab("Point estimate - Odds ratio (95% CI)") +
  xlab("Binary outcome coding") + 
  coord_flip() +
  geom_hline(yintercept=1,linetype="dashed")
# try another way to make this plot
model_plot = ggplot(data=model_results,aes(x=binary_outcome,y=estimate,group=1),cex=1.5,col="#ff7f00") +
  geom_line(data=model_results,aes(x=binary_outcome,y=estimate,group=1),cex=1.5,col="#ff7f00") +
  geom_ribbon(data=model_results,aes(x=1:length(binary_outcome),ymin = lower_ci, ymax = upper_ci),alpha=0.2,fill="#ff7f00") +
  theme_bw() +
  xlab("Binary outcome coding") + 
  ylab("Point estimate - Odds ratio (95% CI)") + 
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12),trans="log10") +
  geom_hline(yintercept=1,linetype="dashed")
model_plot

ggsave(model_plot, filename="/Users/kelseysumner/Desktop/binary_coding_model_plot.png", device="png",
       height=7, width=8, units="in", dpi=500)




