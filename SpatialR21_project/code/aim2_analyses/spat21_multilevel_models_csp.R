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
library(dplyr)



#### ---------- read in the data sets ---------- ####

# read in the merged csp abdomen edgelist ready for the multilevel models
csp_abdomens = read_rds("Desktop/clean_ids_haplotype_results/CSP/model data set/spat21_csp_abdomen_sharing_final_model_data_31OCT2019.rds")

# load in the data set (the haplotypes after chimeras have been removed and haplotypes censored - seqtab_final.rds)
csp_haplotypes <- read_rds("Desktop/clean_ids_haplotype_results/CSP/spat21_CSP_haplotype_table_censored_final_version_with_moi_and_ids_CLEANVERSION_30SEPT2019.rds")

# read in the full human data set
final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1OCT2019.rds")



#### ----- merge moi into the data set ------ ####

# subset the csp_haplotpes data set to sample id and moi
csp_haplotypes = csp_haplotypes %>% 
  select(sample_name_dbs,haplotype_number) %>%
  rename("sample_id_human" = "sample_name_dbs","moi"="haplotype_number")

# merge the csp_abdomens and csp_haplotypes data sets to get moi
csp_abdomens = left_join(csp_abdomens,csp_haplotypes,by="sample_id_human")
length(which(is.na(csp_abdomens$moi)))
str(csp_abdomens$moi)



### ------ check to see who had antimalarials recently ------- ####

# tally up the people that had antimalarials at a monthly visit at least 14 days before their infection
had_antimalarials_recently = final_data %>%
  filter(main_exposure_primary_case_def == "asymptomatic infection" & mal_illness == "yes" & ill_med == "yes" &
           (medicine_ACT_hum_monthly_data == "yes" | medicine_Qui_hum_monthly_data == "yes" | medicine_SP_hum_monthly_data == "yes" | 
              medicine_OACT_hum_monthly_data == "yes" | medicine_AMO_hum_monthly_data == "yes" | medicine_SPT_hum_monthly_data == "yes" |
              medicine_CIP_hum_monthly_data == "yes" | medicine_PAN_hum_monthly_data == "yes" | medicine_DNT_hum_monthly_data == "yes" |
              medicine_OTH_hum_monthly_data == "yes")) %>%
  select(main_exposure_primary_case_def,med_date,recover,sample_id_date,ill_med,sample_name_dbs) %>%
  mutate(time_since_antimalarials_taken = sample_id_date - med_date) %>%
  filter(time_since_antimalarials_taken < 15 | is.na(time_since_antimalarials_taken)) %>%
  rename(sample_id_human = sample_name_dbs)

# see if any of these asymptomatic infections are in the paired data set
test = left_join(csp_abdomens,had_antimalarials_recently,by="sample_id_human")
test = test %>%
  filter(ill_med=="yes")
# the self-reported use of antimalarials isn't that reputable

# let's see how many people had a positive RDT at a symptomatic visit and received an antimalarial within 14 days of the asymptomatic infection
# first reorder the data set
final_data = final_data[with(final_data, order(final_data$unq_memID, final_data$sample_id_date)),]
final_data %>%
  select(sample_name_final,sample_id_date,unq_memID) %>%
  View()

# then do a for loop
received_antimalarial = rep(NA,nrow(final_data))
date_diff = rep(NA,nrow(final_data))
for (i in 1:nrow(final_data)){
  if (final_data$unq_memID[i] == final_data$unq_memID[i+1] & final_data$prescription[i] == "prescribed" & !(is.na(final_data$prescription[i])) & i != nrow(final_data) &
      final_data$main_exposure_primary_case_def[i+1] == "asymptomatic infection" & !(is.na(final_data$main_exposure_primary_case_def[i+1]))){
    date_diff[i] = final_data$sample_id_date[i+1] - final_data$sample_id_date[i]
    if (date_diff[i] < 15){
      received_antimalarial[i+1] = "yes"
    }
  } 
}
final_data$date_diff = date_diff
final_data$received_antimalarial = received_antimalarial
final_data %>%
  select(visit_type,sample_name_final,sample_id_date,unq_memID,main_exposure_primary_case_def,main_outcome_primary_case_def,date_diff,received_antimalarial) %>%
  filter(received_antimalarial == "yes") %>%
  View()
# looks like there are 23 asymptomatic infections that should be censored

test = final_data %>%
  select(age_all_baseline,unq_memID,sample_id_date,sample_name_final,pf_pcr_infection_status,rdt_rst,main_exposure_primary_case_def,main_outcome_primary_case_def,prescription,taken_al)

#### --------- set up the data ---------- ####

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
random_intercept_model_covariates <- glmmPQL(haps_shared~aim2_exposure+moi+moi_quad+age_cat_baseline+aim2_exposure*age_cat_baseline+village_name, random = ~1|HH_ID/unq_memID,family=poisson, data = csp_abdomens)
summary(random_intercept_model_covariates)

# check for overdispersion
# fit a random-intercept quasipoisson model with covariates 
random_intercept_model_covariates_quasipoisson <- glmmPQL(haps_shared~aim2_exposure+moi+age_cat_baseline+aim2_exposure*age_cat_baseline+village_name, random = ~1|HH_ID/unq_memID,family=quasipoisson, data = csp_abdomens)
summary(random_intercept_model_covariates_quasipoisson)
# does not appear to be overdispersion because standard errors are same as poisson

# another random-intercept poisson model with covariates but use glmer function instead
model1 <- glmer(haps_shared~aim2_exposure+moi+age_cat_baseline+aim2_exposure*age_cat_baseline+village_name + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(model1)
1-pchisq(deviance(model1),df.residual(model1)) # p > 0.05 so not good model fit
# looks like no interaction between main exposure and age

# now run the model without the interaction term
model2 <- glmer(haps_shared~aim2_exposure+moi+age_cat_baseline+village_name + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(model2)
# run anova to see which is a better fit
anova(model2,model1)
1-pchisq(deviance(model2),df.residual(model2)) # p > 0.05 so not good model fit
# aic and bic are lower for model 2 but log likelihood ratio test doesn't show big difference with model with interaction term compared to model withought
# interaction term was not statistically significant though so does not appear that it helped model fit

# now run the model without village
model3 <- glmer(haps_shared~aim2_exposure+moi+age_cat_baseline + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(model3)
# run anova to see which is a better fit
anova(model3,model1)
anova(model3,model2)
1-pchisq(deviance(model3),df.residual(model3)) # p > 0.05 so not good model fit
# seems like model 3 is not a drastically better fit but has slightly lower AIC and BIC

# now run the model without the age categories
model4 <- glmer(haps_shared~aim2_exposure+moi + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(model4)
# run anova to see which is a better fit
anova(model4,model1)
anova(model4,model3)
# still a slightly lower AIC and BIC but still not a significantly better fit

# look at the crude model
crude_model <- glmer(haps_shared~aim2_exposure + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(crude_model)
1-pchisq(deviance(crude_model),df.residual(crude_model)) # p < 0.05 so best model fit
anova(crude_model,model1) # model 1 is a better fit
anova(crude_model,model2) # model 2 is a better fit
anova(crude_model,model3) # model 3 is a better fit
anova(crude_model,model4) # model 4 is a better fit

##  decision: go with model 3 with aim2_exposure, age, and moi because controls for confounding in the DAG

# create a data frame of model 4 output
summary(model3)
estimates = c(exp(0.29888),exp(0.18301),exp(-0.02634),exp(0.08375))
exp(confint(model3))
lower_ci = c(1.0434545,1.1633398,0.6941831,0.8396665)
upper_ci = c(1.7618341,1.2423616,1.3834605,1.4269567)
names_f = c("Z   Asymptomatic infection","Number of haplotypes in human infection","Age <5 years","Age 5-15 years")
forest_plot_df = data.frame(names_f,estimates,lower_ci,upper_ci)

# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=names_f, y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=2) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Covariate") + ylab("Rate Ratio (95% CI)") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp

# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/forest_plot.png", device="png",
       height=9, width=12.5, units="in", dpi=400)



#### ------ play around with the concept of doing a logistic regression model ------- ####

# make the outcome a binary outcome based on sharing at least one haplotype
csp_abdomens$binary_outcome_one_hap = ifelse(csp_abdomens$haps_shared>=1,1,0) # 1 is a transmission event, 0 is not
table(csp_abdomens$binary_outcome_one_hap,csp_abdomens$haps_shared, useNA = "always")

# make a second outcome that's binary based on sharing at least two haplotypes
csp_abdomens$binary_outcome_two_hap = ifelse(csp_abdomens$haps_shared>=2,1,0) # 1 is a transmission event, 0 is not
table(csp_abdomens$binary_outcome_two_hap,csp_abdomens$haps_shared, useNA = "always")

# now try running a multi-level logistic regression model for 1 hap binary outcome
logistic_1hap <- glmer(binary_outcome_one_hap~aim2_exposure+age_cat_baseline+moi+village_name + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(logistic_1hap)
1-pchisq(deviance(logistic_1hap),df.residual(logistic_1hap)) # p > 0.05 so not good model fit


logistic_1hap <- glmer(binary_outcome_two_hap~aim2_exposure+moi + (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens)
summary(logistic_1hap)



#### ------ run a secondary model that is just for asymptomatic infections and parasite density ------- ####

# subset the data set to just asymptomatic infections
asymp_data = csp_abdomens %>%
  filter(aim2_exposure == "asymptomatic infection")

# now run a poisson model that has the exposure be parasite density instead of sympomatic status and outcome haplotypes shared
model1a <- glmer(haps_shared~pfr364Q_std_combined_cat+moi+age_cat_baseline+pfr364Q_std_combined_cat*age_cat_baseline+village_name + (1|HH_ID/unq_memID),family=poisson, data = asymp_data)
summary(model1a)
1-pchisq(deviance(model1a),df.residual(model1a)) # p > 0.05 so not good model fit
# looks like no interaction between main exposure and age

# now run a model that takes out the interaction term with age
model2a <- glmer(haps_shared~pfr364Q_std_combined_cat+moi+age_cat_baseline+village_name + (1|HH_ID/unq_memID),family=poisson, data = asymp_data)
summary(model2a)
1-pchisq(deviance(model2a),df.residual(model2a)) # p > 0.05 so not good model fit
# looks like the model had a little trouble converging
anova(model2a,model1a)
# stick with model 2a

# now run a model that takes out village
model3a <- glmer(haps_shared~pfr364Q_std_combined_cat+moi+age_cat_baseline+ (1|HH_ID/unq_memID),family=poisson, data = asymp_data)
summary(model3a)
anova(model3a,model2a)
# looksl ike model 3a is better

# try a model that takes out age
model4a <- glmer(haps_shared~pfr364Q_std_combined_cat+moi+ (1|HH_ID/unq_memID),family=poisson, data = asymp_data)
summary(model4a)
anova(model4a,model3a)
# model 4a is better but sticking with model 3a for comparison to the previous analysis

##  decision: go with model 3a with parasite density, age, and moi because controls for confounding in the DAG

# create a data frame of model 4 output
summary(model3a)
estimates = c(exp(0.35663),exp(0.15230),exp(-0.01747),exp(0.07910))
exp(confint(model3a))
lower_ci = c(1.0342958,1.1167254,0.7058552,0.8405492)
upper_ci = c(1.9695617,1.2184185,1.3857355,1.4275038)
names = c("Parasite density below limit of cRDT detection","Number of haplotypes in human infection","Age <5 years","Age 5-15 years")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = as.factor(forest_plot_df$names)

# create a forest plot
fp <- ggplot(data=forest_plot_df, aes(x=names, y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=2) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Covariate") + ylab("Rate Ratio (95% CI)") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp

# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/forest_plot_parasite_density.png", device="png",
       height=9, width=12.5, units="in", dpi=400)


#### ------- test out the relationship with mosquitoes collected within 2 days of human samples ------ ####

# read in the edgelist of only mosquitoes collected within 2 days
csp_abdomens_2days = read_rds("Desktop/clean_ids_haplotype_results/CSP/within 2 days mosquitoes collected/spat21_csp_edgelist_abdomen_2days_12NOV2019.rds")

# subset the csp_haplotpes data set to sample id and moi
csp_haplotypes = csp_haplotypes %>% 
  select(sample_name_dbs,haplotype_number) %>%
  rename("sample_id_human" = "sample_name_dbs","moi"="haplotype_number")

# merge the csp_abdomens and csp_haplotypes data sets to get moi
csp_abdomens_2days = left_join(csp_abdomens_2days,csp_haplotypes,by="sample_id_human")
length(which(is.na(csp_abdomens_2days$moi)))
str(csp_abdomens_2days$moi)

# rerun the model
model_2day <- glmer(haps_shared~aim2_exposure+moi+age_cat_baseline+ (1|HH_ID/unq_memID),family=poisson, data = csp_abdomens_2days)
summary(model_2day)
# model really couldn't be fit because such sparse data with only 38 obs

# look at the table of the main exposure and outcome
table(csp_abdomens_2days$aim2_exposure,csp_abdomens_2days$haps_shared,useNA = "always")
table(csp_abdomens_2days$aim2_exposure) # only had 3 asymptomatic infections and 30 symptomatic infections
# with super small numbers had 62.5% of asymptomatic infections having a shared haplotypes and 60.0% of symptomatic infections

