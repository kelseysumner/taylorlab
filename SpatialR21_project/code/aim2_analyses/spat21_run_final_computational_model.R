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
combo_multi_model <- glmer(transmission_event~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = combined_data, weights=transmission_weight)
summary(combo_multi_model)
# this one had trouble converging




#### ------ run the final models and do model selection ------- ####

# run the original multi-level model with all covariates and interaction term
model1 <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+age_cat_baseline*aim2_exposure+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(model1)
# model had some trouble converging
exp(0.10853)
exp(-0.73427)

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

# now run the model removing village
model3 <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(model3)
# model had some trouble converging
anova(model2,model3)
# model 2 is better
# check gradient calculations
derivs1 <- model3@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1)) # 0.02283682, this is small but still larger than typical tolerance of 0.001

# now run the model removing parasite density but adding back in village
model4 <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(model4)
anova(model2,model4) 
# model 4 is better but this had convergence issues
# check gradient calculations
derivs1 <- model4@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1)) # 0.3823 so having convergence problems because > 0.001 tolerance

# now run the model removing parasite density and village
model5 <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(model5)
anova(model2,model5) # model 2 is better
# model 2 is better but this one didn't have convergence issues
# check gradient calculations
derivs1 <- model5@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1)) # 1.242775e-05 so smaller than typical tolerance of 0.001, which is good



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
