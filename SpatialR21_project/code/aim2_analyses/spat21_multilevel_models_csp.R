# ----------------------------------------- #
#     Create aim 2 multi-level models       #
#             Mozzie Phase 1                #
#                CSP data                   #
#            October 14, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(lme4)
library(lmer)
library(MASS)
library(ggplot2)



#### ---------- read in the data sets ---------- ####

# read in the merged csp abdomen edgelist ready for the multilevel models
csp_abdomens = read_rds("Desktop/clean_ids_haplotype_results/CSP/model data set/spat21_csp_abdomen_sharing_final_model_data_31OCT2019.rds")


#### --------- run multi-level model for csp_abdomens ---------- ####

# look at the column names
colnames(csp_abdomens)

# make sure the exposure and outcome are coded correctly
str(csp_abdomens$aim2_exposure)
csp_abdomens$aim2_exposure = as.factor(csp_abdomens$aim2_exposure)
str(csp_abdomens$haps_shared)
str(csp_abdomens$village_name)
csp_abdomens$village_name = as.factor(csp_abdomens$village_name)
str(csp_abdomens$total_num_mosq_in_hh)
str(csp_abdomens$age_cat_baseline)
csp_abdomens$age_cat_baseline = as.factor(csp_abdomens$age_cat_baseline)
str(csp_abdomens$pfr364Q_std_combined_cat)
csp_abdomens$aim2_exposure = relevel(csp_abdomens$aim2_exposure,"symptomatic infection")

#### ----- work with the regular models ------ ####

# create a regular poisson model
regular_poisson = glm(haps_shared ~ aim2_exposure + age_cat_baseline + pfr364Q_std_combined_cat + village_name + aim2_exposure*age_cat_baseline, family=c("poisson"), data=csp_abdomens)
summary(regular_poisson)
# test if good model fit
1-pchisq(deviance(regular_poisson),df.residual(regular_poisson)) # p > 0.05 so not good model fit

# test if data is overdisperse by running a quasipoisson model and seeing if results differ
quasi_poisson = glm(haps_shared ~ aim2_exposure + age_cat_baseline + pfr364Q_std_combined_cat + village_name + aim2_exposure*age_cat_baseline, family=c("quasipoisson"), data=csp_abdomens)
summary(quasi_poisson)
# test if good model fit
1-pchisq(deviance(quasi_poisson),df.residual(quasi_poisson)) # p > 0.05 so not good model fit
# standard errors went down a little bit so could have mild overdispersion

# try a negative binomial model that accounts for overdispersion
neg_binomial = glm.nb(haps_shared ~ aim2_exposure + age_cat_baseline + pfr364Q_std_combined_cat + village_name + aim2_exposure*age_cat_baseline, data=csp_abdomens)
summary(neg_binomial)
# test if good model fit
1-pchisq(deviance(neg_binomial),df.residual(neg_binomial)) # p>0.05 so not good model fit
# looks like negative binomial better not that much better fit than poisson based on AIC (5 point difference)



#### ------ make the multi-level models ------- ####

# fit a random-intercept poisson model with covariates 
random_intercept_model_covariates <- glmmPQL(haps_shared~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+aim2_exposure*age_cat_baseline+village_name, random = ~1|HH_ID/unq_memID,family=poisson, data = csp_abdomens)
summary(random_intercept_model_covariates)

# check for overdispersion
# fit a random-intercept quasipoisson model with covariates 
random_intercept_model_covariates_quasipoisson <- glmmPQL(haps_shared~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+aim2_exposure*age_cat_baseline+village_name, random = ~1|HH_ID/unq_memID,family=quasipoisson, data = csp_abdomens)
summary(random_intercept_model_covariates_quasipoisson)
# does not appear to be overdispersion because standard errors are same as poisson

# another random-intercept poisson model with covariates but use glmer function instead
model1 <- glmer(haps_shared~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+aim2_exposure*age_cat_baseline+village_name + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(model1)
1-pchisq(deviance(model1),df.residual(model1)) # p > 0.05 so not good model fit
# looks like no interaction between main exposure and age

# now run the model without the interaction term
model2 <- glmer(haps_shared~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+village_name + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(model2)
# run anova to see which is a better fit
anova(model2,model1)
1-pchisq(deviance(model2),df.residual(model2)) # p > 0.05 so not good model fit
# aic and bic are lower for model 2 but log likelihood ratio test doesn't show big difference with model with interaction term compared to model withought
# interaction term was not statistically significant though so does not appear that it helped model fit

# now run the model without village
model3 <- glmer(haps_shared~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(model3)
# run anova to see which is a better fit
anova(model3,model1)
# seems like model 3 is not a drastically better fit but has slightly lower AIC and BIC

# now run the model without the age categories
model4 <- glmer(haps_shared~aim2_exposure+pfr364Q_std_combined_cat + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(model4)
# run anova to see which is a better fit
anova(model4,model1)
# still a slightly lower AIC and BIC but still not a significantly better fit

# look at the crude model
crude_model <- glmer(haps_shared~aim2_exposure + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(crude_model)
1-pchisq(deviance(crude_model),df.residual(crude_model)) # p < 0.05 so best model fit
anova(crude_model,model1) # model 1 is a better fit
anova(crude_model,model2) # model 2 is a better fit

##  decision: go with model 2 without the interaction term for age_cat

# calculate the estimate stratified by village
kinesamo = csp_abdomens %>%
  filter(village_name == "Kinesamo")
maruti = csp_abdomens %>%
  filter(village_name == "Maruti")
# for kinesamo
model2_kinesamo <- glmer(haps_shared~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+ (1|HH_ID/unq_memID),family=poisson, data = kinesamo)
summary(model2_kinesamo)
1-pchisq(deviance(model2_kinesamo),df.residual(model2_kinesamo)) # p < 0.05 so good model fit
confint(model2_kinesamo, method="profile",devtol=1e-5)
# for maruti
model2_maruti <- glmer(haps_shared~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline + (1|HH_ID/unq_memID),family=poisson, data = maruti)
summary(model2_maruti)
1-pchisq(deviance(model2_maruti),df.residual(model2_maruti)) # p > 0.05 so not good model fit

# another way to compare maruti and sitabicha
model2 <- glmer(haps_shared~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+village_name + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(model2)
exp(confint(model2))
exp(0.2316) # 1.26 (0.9737103 to 1.6466483)
csp_abdomens$village_name = relevel(csp_abdomens$village_name,"Maruti")
model2_kin <- glmer(haps_shared~aim2_exposure+pfr364Q_std_combined_cat+age_cat_baseline+village_name + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(model2_kin)
exp(confint(model2))
exp(0.2316) # exact same

# create a data frame of model 2 output
estimates = c(exp(0.2316),exp(1.1373),exp(0.1186),exp(0.1114),exp(-0.1288))
lower_ci = c(0.9737103,2.3380111,0.7647209,0.8130134,0.3716901)
upper_ci = c(1.6466483,4.4111152,1.7059564,1.5748148,2.2043146)
names = c("Asymptomatic infection","Under RDT detection limit","Age <5 years","Age 5-15 years","Maruti village")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = as.factor(forest_plot_df$names)

# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=names, y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange() + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Covariate") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
fp

# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/forest_plot.png", device="png",
       height=5, width=5, units="in", dpi=400)


