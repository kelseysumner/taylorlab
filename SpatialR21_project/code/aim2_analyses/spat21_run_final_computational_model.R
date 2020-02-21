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
library(sjstats)
library(lmerTest)


#### ----- read in the data sets ----- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/spat21_aim2_merged_data_with_weights_14FEB2020.rds")



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
str(model_data$mean_moi_category)



#### ------ make some plots of covariates ------- ####

# make a plot of p_te_all over the exposure
ggplot(model_data, aes(x = p_te_all)) + geom_density() + facet_wrap(~aim2_exposure)
ggplot(model_data, aes(x = pfr364Q_std_combined_rescaled)) + geom_density() + facet_wrap(~aim2_exposure)
ggplot(model_data, aes(x = age_cat_baseline)) + geom_density() + facet_wrap(~aim2_exposure)
ggplot(model_data, aes(x = mean_moi)) + geom_density() + facet_wrap(~aim2_exposure)
ggplot(model_data, aes(x = mosquito_week_count_cat)) + geom_density() + facet_wrap(~aim2_exposure)
ggplot(model_data, aes(x = village_name)) + geom_density() + facet_wrap(~aim2_exposure)
ggplot(model_data, aes(x = HH_ID_human)) + geom_density() + facet_wrap(~aim2_exposure)
table(model_data$HH_ID_human,model_data$aim2_exposure)
table(model_data$unq_memID,model_data$aim2_exposure)
table(model_data$sample_id_human,model_data$aim2_exposure)



#### ------- check for overdispersion ------- ####

# test the model with the original p(TEall) coding
model2_all_2r <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model2_all_2r)
performance::icc(model2_all_2r)
model2_all_1r <- glmer(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+(1|unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model2_all_1r)
performance::icc(model2_all_1r)

# test the crude model with the origianl p(TEall) coding 
model2_all_crude <- glmer(p_te_all~aim2_exposure+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model2_all_crude)

# test the model with original p(TEall) coding and not multilevel
model2_all_nmlm <- glm(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name,family=binomial(link = "logit"), data = model_data)
summary(model2_all_nmlm)
performance::icc(model2_all_crude)

# test the model with the alternative p(TEall) coding
model2_all_alt <- glmer(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model2_all_alt)
performance::icc(model2_all_alt)

# test the model with alternative p(TEall) coding and not multilevel
model2_all_alt_nmlm <- glm(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name,family=binomial(link = "logit"), data = model_data)
summary(model2_all_alt_nmlm)

# test the crude model with the alternative p(TEall) coding
model2_all_alt_crude <- glmer(p_te_all_alt~aim2_exposure+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model2_all_alt_crude)
performance::icc(model2_all_alt_crude)

# look at a summary of p_te_all and p_te_alt by household
tmp = merged_data %>%
  group_by(HH_ID_human) %>%
  summarize(mean_p_te_all = mean(p_te_all))
tmp_alt = merged_data %>%
  group_by(HH_ID_human) %>%
  summarize(mean_p_te_all_alt = mean(p_te_all_alt))

# look at a summary of p_te_all and p_te_alt by person
tmp = merged_data %>%
  group_by(unq_memID) %>%
  summarize(mean_p_te_all = mean(p_te_all))
tmp_alt = merged_data %>%
  group_by(unq_memID) %>%
  summarize(mean_p_te_all_alt = mean(p_te_all_alt))

# run a multilevel logistic model with new p(TEall) coding but with glmmtmb package
model2_tmb <- glmmTMB(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat +pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(model2_tmb)
performance::icc(model2_tmb)

# run a multilevel logistic model with new p(TEall) coding but with glmmadmb package
model2_tmb <- glmm(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat +pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(model2_tmb)
performance::icc(model2_tmb)

# overdispersion function from: https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
overdisp_fun <- function(model) {
  ## number of variance parameters in an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1)/2
  }
  # The next two lines calculate the residual degrees of freedom
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf <- nrow(model.frame(model)) - model.df
  # extracts the Pearson residuals
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  # Generates a p-value. If less than 0.05, the data are overdispersed.
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}
overdisp_fun(model2_all) # data are not overdispersed
overdisp_fun(model2_all_alt) # data are not overdispersed


# another function to check for dispersion using code from Ben Bolker
dispersion_glmer <- function(model) {
  ## number of variance parameters in
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
dispfun <- function(m) {
  r <- residuals(m,type="pearson")
  n <- df.residual(m)
  dsq <- sum(r^2)
  c(dsq=dsq,n=n,disp=dsq/n)
}
dispersion_glmer(model2_all_alt)
dispfun(model2_all_alt)



#### -------- create two copies for everyone for logistic regression ------- ####

# first make two copies of everyone
copy_data = model_data

# now make a new variable in the original data set that is having a transmission event or not
model_data$transmission_event = rep("yes",nrow(model_data)) # here, everyone has transmission event
model_data$transmission_weight = model_data$p_te_all_alt

# now make a new variable in the copied data set that is not having a transmission event
copy_data$transmission_event = rep("no",nrow(copy_data)) # here, no one has transmission event
copy_data$transmission_weight = 1-model_data$p_te_all_alt # inverse weight for those that didn't have a transmission event

# now combine the original and copy data sets
combined_data = rbind(model_data,copy_data)

# make the transmission event variable a factor
combined_data$transmission_event = as.factor(combined_data$transmission_event)
combined_data$transmission_event = relevel(combined_data$transmission_event,ref="no")




#### -------- now run a crude logistic regression model ------- ####

# this is a regular logistic regression model and not multi-level model with the original model data set
regular_logistic = glm(p_te_all_alt ~ aim2_exposure, family=binomial(link="logit"), data=model_data)
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
combo_multi_model <- glmer(transmission_event~age_cat_baseline+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = combined_data, weights=transmission_weight)
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


#### --------- try a zero inflated binomial or beta model ------- ####

# load packages
library("glmmTMB") 
library("bbmle")

# check for zero inflation
100*sum(model_data$p_te_all_alt == 0)/nrow(model_data) # 33.7% of data zeroes - quite a lot

# try fitting a crude version fo the zero inflated binomial model
model_zero <- glmmTMB(p_te_all_alt~aim2_exposure+(1|HH_ID_human/unq_memID),ziformula=~1,family=binomial(link = "logit"), data = model_data)
summary(model_zero)

# try the model we want with all confounders but not zero inflated
model_zero_2 <- glmmTMB(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(model_zero_2)
performance::icc(model_zero_2)

# try the model we want with all confounders but not zero inflated and with a dispersion parameter
model_zero_2 <- glmmTMB(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"),dispformula = ~unq_memID, data = model_data)
summary(model_zero_2)
performance::icc(model_zero_2)

# try a zero-inflated beta model with the =1 value removed
model_data_test = model_data %>%
  filter(p_te_all_alt < 1)
model_zero_beta <- glmmTMB(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),ziformula=~.,beta_family(link = "logit"), data = model_data_test)
summary(model_zero_beta)

# try a zero-inflated beta model with the =1 value removed and with the number of haps shared to the right of the zero inflation term
model_data$sum_haps_shared = model_data$csp_haps_shared+model_data$ama_haps_shared
model_data_test = model_data %>%
  filter(p_te_all_alt < 1)
model_zero_beta <- glmmTMB(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),ziformula=~sum_haps_shared,beta_family(link = "logit"), data = model_data_test)
summary(model_zero_beta)

# try a zero-inflated beta model with the =1 value removed and with the intercept to the right of the zero inflation term
model_zero_beta <- glmmTMB(p_te_all~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),ziformula=~1,beta_family(link = "logit"), data = model_data_test)
summary(model_zero_beta)

# also try a zero-one-inflated beta distribution with a different package
library(gamlss)
model_beta = gamlss(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+re(random=~1|HH_ID_human/unq_memID),family=BEINF,data=na.omit(model_data))
summary(model_beta)
# not really sure what this is doing

# also try a zero-inflated beta distribution with a different package
model_beta = gamlss(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+re(random=~1|HH_ID_human/unq_memID),family=BEZI,data=na.omit(model_data_test))
summary(model_beta)
# not really sure what this is doing

# also try a multilevel logistic regression with a different package
model_logistic = gamlss(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+re(random=~1|HH_ID_human/unq_memID),family=LO(),data=na.omit(model_data_test))
summary(model_logistic)


#### ------ run the final models and do model selection ------- ####

# run the original multi-level model with all covariates and interaction term
model1 <- glmer(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+age_cat_baseline*aim2_exposure+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model1)
exp(-0.097120)
exp(-0.235954)

# run the model with all covariates but interaction removed
model2 <- glmer(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
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
model2 <- glmer(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model2)
exp(0.64476)
anova(model1,model2)

# now run the model removing village
model3 <- glmer(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled +pfr364Q_std_combined_rescaled+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), control = glmerControl(optimizer="bobyqa"), data = model_data)
summary(model3)
exp(0.72747)
anova(model2,model3)
anova(model3,model1)
# model 2 is better
# check gradient calculations
derivs1 <- model3@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1)) # very small

# now run the model removing parasite density but adding back in village
model4 <- glmer(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ mean_moi_rescaled + village_name +(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model4)
anova(model2,model4) 
anova(model3,model4)
anova(model1,model4)
exp(0.58842)
# check gradient calculations
derivs1 <- model4@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1)) 

# now run the model removing mean moi
model5 <- glmer(p_te_all_alt~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+ pfr364Q_std_combined_rescaled + village_name + (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model5)
exp(1.12549)
anova(model2,model5) # model 2 is better
anova(model5,model3)
anova(model5,model4)
# model 2 is better but this one didn't have convergence issues
# check gradient calculations
derivs1 <- model5@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1)) 

# now run the model removing age
model6 <- glmer(p_te_all_alt~aim2_exposure+mean_moi_rescaled+mosquito_week_count_cat+ pfr364Q_std_combined_rescaled + village_name +(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model6)
exp(1.06053)
anova(model2,model6) # model 6 is better
# model 2 is better but this one didn't have convergence issues
# check gradient calculations
derivs1 <- model6@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))

# now run the model removing mosquito_week_count
model7 <- glmer(p_te_all_alt~aim2_exposure+mean_moi_rescaled+ pfr364Q_std_combined_rescaled + age_cat_baseline + village_name +(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model7)
exp(0.78294)
anova(model2,model7) # model 2 is better

# now run the crude model with no covariates
model8 <- glmer(p_te_all_alt~aim2_exposure+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data_test, control = glmerControl(optimizer="bobyqa"))
summary(model8)
exp(1.2122)
anova(model2,model8) # model 2 is better
# model 2 is better but this one didn't have convergence issues
 
# a few more model comparisons
anova(model4,model5) # model 4 better than 5
anova(model3,model5)
anova(model5,model6) # model 5 better
anova(model5,model7) # model 5 better
anova(model5,model8) # model 5 better


# summary:
# deciding to go with model 2


#### ------ create a forest plot of the final model output ------ ####

# create a data frame of model 2 output
model2 <- glmer(p_te_all~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+ mean_moi_category+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model2)
estimates = c(exp(0.79024),exp(0.18640),exp(-0.41343),exp(-0.32096),exp(0.24330),exp(1.97667),exp(3.23172),exp(-0.69801),exp(-0.71680))
exp(confint.merMod(model2,method="Wald"))
lower_ci = c(1.53832984,1.06555452,0.45154626,0.49833749,1.05136983,5.24167214,18.21097254,0.32433396,0.26443791)
upper_ci = c(3.15750835,1.36247946,0.96871560,1.05607530,1.54728538,9.94130768,35.21333310,0.76335046,0.90171365)
names = c("Asymptomatic infection","Participant asexual parasite density","Participant age 5-15 years","Participant age >15 years","75-147 mosquitoes","2.5-6 mean MOI","6.5-15.5 mean MOI","Kinesamo village","Sitabicha village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","Participant asexual parasite density","Participant age 5-15 years","Participant age >15 years","75-147 mosquitoes","2.5-6 mean MOI","6.5-15.5 mean MOI","Kinesamo village","Sitabicha village"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","Participant asexual parasite density","Participant age 5-15 years","Participant age >15 years","75-147 mosquitoes","2.5-6 mean MOI","6.5-15.5 mean MOI","Kinesamo village","Sitabicha village"))

# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1,1),colour=c("#E1AF00","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp

# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/forest_plot_aim2_model_continuous_outcome.png", device="png",
       height=9, width=12.5, units="in", dpi=400)


#### ------- make a plot of p_te_all stratified -------- ####

# make a density plot of p_te_all 
p_te_all_plot = ggplot(data=model_data,aes(x=p_te_all_alt,fill=aim2_exposure)) +
  geom_density(alpha=0.6) + 
  scale_fill_manual(values=c("#E1AF00","#3B9AB2")) + 
  labs(fill="Symptomatic status") +
  theme_bw() + 
  xlab("Probability of transmission across all variables") +
  ylab("Density") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25), legend.position = c(0.84, 0.87),legend.box.background = element_rect(colour = "black"))
p_te_all_plot
ggsave(p_te_all_plot, filename="/Users/kelseysumner/Desktop/p_te_all_plot_density.png", device="png",
       height=8, width=14, units="in", dpi=500)


# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00


# make a violin plot of p_te_all stratified by participant's symptomatic status
p_te_all_plot = ggplot(data=model_data,aes(x=aim2_exposure,y=p_te_all_alt,fill=aim2_exposure)) +
  geom_violin(alpha=0.8) + 
  scale_fill_manual(values=c("#E1AF00","#3B9AB2")) + 
  theme_bw() + 
  theme(legend.position = "none") +
  xlab("") +
  ylab("Probability of transmission across all variables") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25))
p_te_all_plot
ggsave(p_te_all_plot, filename="/Users/kelseysumner/Desktop/p_te_all_plot_violin.png", device="png",
       height=10, width=14, units="in", dpi=500)



#### ------ make a plot of the odds ratios of p_te_all coded binary -------- ####

# look at a summary of the outcome variable
summary(model_data$p_te_all_alt)
length(which(is.na(model_data$p_te_all_alt)))
hist(model_data$p_te_all_alt)

# make a binary variable for 0 or >0
model_data$outcome_binary_lessthan0 = ifelse(model_data$p_te_all_alt > 0,"greater than 0.00","equal to 0.00")
table(model_data$outcome_binary_lessthan0,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0, useNA = "always")
model_data$outcome_binary_lessthan0 = factor(model_data$outcome_binary_lessthan0)
levels(model_data$outcome_binary_lessthan0)
model_data$outcome_binary_lessthan0 = relevel(model_data$outcome_binary_lessthan0,ref = "equal to 0.00")

# make a binary variable for <0.05 or >= 0.05
model_data$outcome_binary_lessthan0.05 = ifelse(model_data$p_te_all_alt < 0.05,"less than 0.05","greater than 0.05")
table(model_data$outcome_binary_lessthan0.05,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.05, useNA = "always")
model_data$outcome_binary_lessthan0.05 = factor(model_data$outcome_binary_lessthan0.05)
levels(model_data$outcome_binary_lessthan0.05)
model_data$outcome_binary_lessthan0.05 = relevel(model_data$outcome_binary_lessthan0.05,ref = "less than 0.05")

# make a binary variable for <0.1 or >= 0.1
model_data$outcome_binary_lessthan0.1 = ifelse(model_data$p_te_all_alt < 0.1,"less than 0.1","greater than 0.1")
table(model_data$outcome_binary_lessthan0.1,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.1, useNA = "always")
model_data$outcome_binary_lessthan0.1 = factor(model_data$outcome_binary_lessthan0.1)
levels(model_data$outcome_binary_lessthan0.1)
model_data$outcome_binary_lessthan0.1 = relevel(model_data$outcome_binary_lessthan0.1,ref = "less than 0.1")

# make a binary variable for <0.15 or >= 0.15
model_data$outcome_binary_lessthan0.15 = ifelse(model_data$p_te_all_alt < 0.15,"less than 0.15","greater than 0.15")
table(model_data$outcome_binary_lessthan0.15,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.15, useNA = "always")
model_data$outcome_binary_lessthan0.15 = factor(model_data$outcome_binary_lessthan0.15)
levels(model_data$outcome_binary_lessthan0.15)
model_data$outcome_binary_lessthan0.15 = relevel(model_data$outcome_binary_lessthan0.15,ref = "less than 0.15")

# make a binary variable for <0.2 or >= 0.2
model_data$outcome_binary_lessthan0.2 = ifelse(model_data$p_te_all_alt < 0.2,"less than 0.2","greater than 0.2")
table(model_data$outcome_binary_lessthan0.2,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.2, useNA = "always")
model_data$outcome_binary_lessthan0.2 = factor(model_data$outcome_binary_lessthan0.2)
levels(model_data$outcome_binary_lessthan0.2)
model_data$outcome_binary_lessthan0.2 = relevel(model_data$outcome_binary_lessthan0.2,ref = "less than 0.2")

# make a binary variable for <0.25 or >= 0.25
model_data$outcome_binary_lessthan0.25 = ifelse(model_data$p_te_all_alt < 0.25,"less than 0.25","greater than 0.25")
table(model_data$outcome_binary_lessthan0.25,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.25, useNA = "always")
model_data$outcome_binary_lessthan0.25 = factor(model_data$outcome_binary_lessthan0.25)
levels(model_data$outcome_binary_lessthan0.25)
model_data$outcome_binary_lessthan0.25 = relevel(model_data$outcome_binary_lessthan0.25,ref = "less than 0.25")

# make a binary variable for <0.3 or >= 0.3
model_data$outcome_binary_lessthan0.3 = ifelse(model_data$p_te_all_alt < 0.3,"less than 0.3","greater than 0.3")
table(model_data$outcome_binary_lessthan0.3,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.3, useNA = "always")
model_data$outcome_binary_lessthan0.3 = factor(model_data$outcome_binary_lessthan0.3)
levels(model_data$outcome_binary_lessthan0.3)
model_data$outcome_binary_lessthan0.3 = relevel(model_data$outcome_binary_lessthan0.3,ref = "less than 0.3")

# make a binary variable for <0.35 or >= 0.35
model_data$outcome_binary_lessthan0.35 = ifelse(model_data$p_te_all_alt < 0.35,"less than 0.35","greater than 0.35")
table(model_data$outcome_binary_lessthan0.35,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.35, useNA = "always")
model_data$outcome_binary_lessthan0.35 = factor(model_data$outcome_binary_lessthan0.35)
levels(model_data$outcome_binary_lessthan0.35)
model_data$outcome_binary_lessthan0.35 = relevel(model_data$outcome_binary_lessthan0.35,ref = "less than 0.35")

# make a binary variable for <0.4 or >= 0.4
model_data$outcome_binary_lessthan0.4 = ifelse(model_data$p_te_all_alt < 0.4,"less than 0.4","greater than 0.4")
table(model_data$outcome_binary_lessthan0.4,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.4, useNA = "always")
model_data$outcome_binary_lessthan0.4 = factor(model_data$outcome_binary_lessthan0.4)
levels(model_data$outcome_binary_lessthan0.4)
model_data$outcome_binary_lessthan0.4 = relevel(model_data$outcome_binary_lessthan0.4,ref = "less than 0.4")

# make a binary variable for <0.45 or >= 0.45
model_data$outcome_binary_lessthan0.45 = ifelse(model_data$p_te_all_alt < 0.45,"less than 0.45","greater than 0.45")
table(model_data$outcome_binary_lessthan0.45,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.45, useNA = "always")
model_data$outcome_binary_lessthan0.45 = factor(model_data$outcome_binary_lessthan0.45)
levels(model_data$outcome_binary_lessthan0.45)
model_data$outcome_binary_lessthan0.45 = relevel(model_data$outcome_binary_lessthan0.45,ref = "less than 0.45")

# make a binary variable for <0.5 or >= 0.5
model_data$outcome_binary_lessthan0.5 = ifelse(model_data$p_te_all_alt < 0.5,"less than 0.5","greater than 0.5")
table(model_data$outcome_binary_lessthan0.5,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.5, useNA = "always")
model_data$outcome_binary_lessthan0.5 = factor(model_data$outcome_binary_lessthan0.5)
levels(model_data$outcome_binary_lessthan0.5)
model_data$outcome_binary_lessthan0.5 = relevel(model_data$outcome_binary_lessthan0.5,ref = "less than 0.5")

# make a binary variable for <0.55 or >= 0.55
model_data$outcome_binary_lessthan0.55 = ifelse(model_data$p_te_all_alt < 0.55,"less than 0.55","greater than 0.55")
table(model_data$outcome_binary_lessthan0.55,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.55, useNA = "always")
model_data$outcome_binary_lessthan0.55 = factor(model_data$outcome_binary_lessthan0.55)
levels(model_data$outcome_binary_lessthan0.55)
model_data$outcome_binary_lessthan0.55 = relevel(model_data$outcome_binary_lessthan0.55,ref = "less than 0.55")

# make a binary variable for <0.6 or >= 0.6
model_data$outcome_binary_lessthan0.6 = ifelse(model_data$p_te_all_alt < 0.6,"less than 0.6","greater than 0.6")
table(model_data$outcome_binary_lessthan0.6,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.6, useNA = "always")
model_data$outcome_binary_lessthan0.6 = factor(model_data$outcome_binary_lessthan0.6)
levels(model_data$outcome_binary_lessthan0.6)
model_data$outcome_binary_lessthan0.6 = relevel(model_data$outcome_binary_lessthan0.6,ref = "less than 0.6")

# make a binary variable for <0.65 or >= 0.65
model_data$outcome_binary_lessthan0.65 = ifelse(model_data$p_te_all_alt < 0.65,"less than 0.65","greater than 0.65")
table(model_data$outcome_binary_lessthan0.65,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.65, useNA = "always")
model_data$outcome_binary_lessthan0.65 = factor(model_data$outcome_binary_lessthan0.65)
levels(model_data$outcome_binary_lessthan0.65)
model_data$outcome_binary_lessthan0.65 = relevel(model_data$outcome_binary_lessthan0.65,ref = "less than 0.65")

# make a binary variable for <0.7 or >= 0.7
model_data$outcome_binary_lessthan0.7 = ifelse(model_data$p_te_all_alt < 0.7,"less than 0.7","greater than 0.7")
table(model_data$outcome_binary_lessthan0.7,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.7, useNA = "always")
model_data$outcome_binary_lessthan0.7 = factor(model_data$outcome_binary_lessthan0.7)
levels(model_data$outcome_binary_lessthan0.7)
model_data$outcome_binary_lessthan0.7 = relevel(model_data$outcome_binary_lessthan0.7,ref = "less than 0.7")

# make a binary variable for <0.75 or >= 0.75
model_data$outcome_binary_lessthan0.75 = ifelse(model_data$p_te_all_alt < 0.75,"less than 0.75","greater than 0.75")
table(model_data$outcome_binary_lessthan0.75,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.75, useNA = "always")
model_data$outcome_binary_lessthan0.75 = factor(model_data$outcome_binary_lessthan0.75)
levels(model_data$outcome_binary_lessthan0.75)
model_data$outcome_binary_lessthan0.75 = relevel(model_data$outcome_binary_lessthan0.75,ref = "less than 0.75")

# make a binary variable for <0.8 or >= 0.8
model_data$outcome_binary_lessthan0.8 = ifelse(model_data$p_te_all_alt < 0.8,"less than 0.8","greater than 0.8")
table(model_data$outcome_binary_lessthan0.8,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.8, useNA = "always")
model_data$outcome_binary_lessthan0.8 = factor(model_data$outcome_binary_lessthan0.8)
levels(model_data$outcome_binary_lessthan0.8)
model_data$outcome_binary_lessthan0.8 = relevel(model_data$outcome_binary_lessthan0.8,ref = "less than 0.8")

# make a binary variable for <0.85 or >= 0.85
model_data$outcome_binary_lessthan0.85 = ifelse(model_data$p_te_all_alt < 0.85,"less than 0.85","greater than 0.85")
table(model_data$outcome_binary_lessthan0.85,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.85, useNA = "always")
model_data$outcome_binary_lessthan0.85 = factor(model_data$outcome_binary_lessthan0.85)
levels(model_data$outcome_binary_lessthan0.85)
model_data$outcome_binary_lessthan0.85 = relevel(model_data$outcome_binary_lessthan0.85,ref = "less than 0.85")

# make a binary variable for <0.9 or >= 0.9
model_data$outcome_binary_lessthan0.9 = ifelse(model_data$p_te_all_alt < 0.9,"less than 0.9","greater than 0.9")
table(model_data$outcome_binary_lessthan0.9,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.9, useNA = "always")
model_data$outcome_binary_lessthan0.9 = factor(model_data$outcome_binary_lessthan0.9)
levels(model_data$outcome_binary_lessthan0.9)
model_data$outcome_binary_lessthan0.9 = relevel(model_data$outcome_binary_lessthan0.9,ref = "less than 0.9")

# make a binary variable for <0.95 or >= 0.95
model_data$outcome_binary_lessthan0.95 = ifelse(model_data$p_te_all_alt < 0.95,"less than 0.95","greater than 0.95")
table(model_data$outcome_binary_lessthan0.95,model_data$p_te_all_alt,useNA = "always")
table(model_data$outcome_binary_lessthan0.95, useNA = "always")
model_data$outcome_binary_lessthan0.95 = factor(model_data$outcome_binary_lessthan0.95)
levels(model_data$outcome_binary_lessthan0.95)
model_data$outcome_binary_lessthan0.95 = relevel(model_data$outcome_binary_lessthan0.95,ref = "less than 0.95")

# binary outcome 0 with a logistic model - this is basically a hurdle model
model0 <- glmer(outcome_binary_lessthan0~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model0)
exp(confint(model0,method="Wald"))
# singular

# binary outcome <0.05 with a logistic model
model.05 <- glmer(outcome_binary_lessthan0.05~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.05)
exp(0.211405)
exp(confint(model.05,method="Wald"))
# converged

# binary outcome <0.1 with a logistic model
model.1 <- glmer(outcome_binary_lessthan0.1~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.1)
exp(0.30025)
exp(confint(model.1,method="Wald"))
# converged

# binary outcome <0.15 with a logistic model
model.15 <- glmer(outcome_binary_lessthan0.15~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.15)
exp(0.18703)
exp(confint(model.15,method="Wald"))
# converged

# binary outcome <0.2 with a logistic model
model.2 <- glmer(outcome_binary_lessthan0.2~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.2)
exp(0.26484)
exp(confint(model.2, method="Wald"))
# converged

# binary outcome <0.25 with a logistic model
model.25 <- glmer(outcome_binary_lessthan0.25~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.25)
exp(0.31428)
exp(confint(model.25, method="Wald"))
# converged

# binary outcome <0.3 with a logistic model
model.3 <- glmer(outcome_binary_lessthan0.3~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.3)
exp(0.38542)
exp(confint(model.3, method="Wald"))
# converged

# binary outcome <0.35 with a logistic model
model.35 <- glmer(outcome_binary_lessthan0.35~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.35)
exp(0.570961)
exp(confint(model.35, method="Wald"))
# converged

# binary outcome <0.4 with a logistic model
model.4 <- glmer(outcome_binary_lessthan0.4~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.4)
exp(0.57531)
exp(confint(model.4, method="Wald")) # can't compute confidence interval
# converged

# binary outcome <0.45 with a logistic model
model.45 <- glmer(outcome_binary_lessthan0.45~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.45)
exp(0.45738)
exp(confint(model.45, method="Wald")) # can't compute confidence interval
# converged

# binary outcome <0.5 with a logistic model
model.5 <- glmer(outcome_binary_lessthan0.5~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.5)
exp(0.80277)
exp(confint(model.5, method="Wald")) # can't compute confidence interval
# converged

# binary outcome <0.55 with a logistic model
model.55 <- glmer(outcome_binary_lessthan0.55~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.55)
exp(0.93852)
exp(confint(model.55, method="Wald")) # can't compute confidence interval
# converged

# binary outcome <0.6 with a logistic model
model.6 <- glmer(outcome_binary_lessthan0.6~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.6)
exp(1.1002)
exp(confint(model.6, method="Wald")) # can't compute confidence interval
# converged

# binary outcome <0.65 with a logistic model
model.65 <- glmer(outcome_binary_lessthan0.65~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.65)
# model did not converge

# binary outcome <0.7 with a logistic model
model.7 <- glmer(outcome_binary_lessthan0.7~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.7)
# converged but small sample sizes

# binary outcome <0.75 with a logistic model
model.75 <- glmer(outcome_binary_lessthan0.75~aim2_exposure+age_cat_baseline+mosquito_week_count_cat+pfr364Q_std_combined_rescaled+village_name+mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data, control = glmerControl(optimizer="bobyqa"))
summary(model.75)
# converged but small sample sizes

# read in the model results
model_results = read_csv("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/computational_model_materials/aim2_binary_outcome_final.csv")

# make a plot of the results
model_results$binary_outcome = as.factor(model_results$binary_outcome)
model_plot = ggplot(data=model_results, aes(x=binary_outcome, y=estimate)) + 
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.1) +
  geom_line() +
  geom_point() + 
  scale_y_continuous(breaks=c(0,1,2,3,4),trans="log10") +
  theme_bw() +
  ylab("Point estimate - Odds ratio (95% CI)") +
  xlab("Binary outcome coding") + 
  coord_flip() +
  geom_hline(yintercept=1,linetype="dashed")
# try another way to make this plot
model_plot = ggplot(data=model_results,aes(x=binary_outcome,y=estimate,group=1),cex=1.5,col="#E1AF00") +
  geom_line(data=model_results,aes(x=binary_outcome,y=estimate,group=1),cex=1.5,col="#E1AF00") +
  geom_ribbon(data=model_results,aes(x=1:length(binary_outcome),ymin = lower_ci, ymax = upper_ci),alpha=0.2,fill="#E1AF00") +
  theme_bw() +
  xlab("Binary outcome coding") + 
  ylab("Point estimate - Odds ratio (95% CI)") + 
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7),trans="log10") +
  geom_hline(yintercept=1,linetype="dashed") + 
  coord_flip()
model_plot
# third way to do the plot
model_plot = ggplot(data=model_results,aes(x=binary_outcome,y=estimate,group=1),cex=1.5,col="#E1AF00") +
  geom_smooth(data=model_results,aes(x=binary_outcome,y=estimate,group=1),cex=1.5,col="#E1AF00",fill="#E1AF00") +
  theme_bw() +
  xlab("Binary outcome coding") +
  ylab("Odds ratio (95% CI)") + 
  scale_y_continuous(breaks=c(0.0,1.0,2.0,3.0,4.0),trans="log10") +
  geom_hline(yintercept=1,linetype="dashed") + 
  coord_flip() +
  theme(text = element_text(size=25)) 
model_plot


ggsave(model_plot, filename="/Users/kelseysumner/Desktop/binary_coding_model_plot.png", device="png",
       height=7, width=8, units="in", dpi=500)


