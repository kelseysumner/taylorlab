# -------------------------------------- #
#           Spat21/Mozzie Study          #
#            Calculate the PAF           #
#               CSP Target               # 
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#             June 2, 2020               #
# -------------------------------------- #


#### -------- load the libraries ------- ####

library(tidyverse)
library(glmmTMB)
library(AF)
library(lme4)


#### ----- load in the data sets ------ ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA_and_CSP/final/model data/final_model_data/spat21_aim2_merged_data_with_weights_5MAR2020.rds")

final_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/final merged data/final_recoded_data_set/spat21_human_final_censored_data_for_dissertation_with_exposure_outcome_1MAR2020.rds")


#### ------ check covariate coding ------- ####

# rescale csp and ama moi
model_data$csp_moi_rescaled = scale(model_data$csp_moi)
model_data$ama_moi_rescaled = scale(model_data$ama_moi)

# subset the data set to samples that passed pfcsp sequencing only
model_data = model_data %>%
  filter(!(is.na(csp_haps_shared)))

# check the covariates
str(model_data$sample_id_human)
str(model_data$HH_ID_human)
str(model_data$unq_memID)
str(model_data$p_te_all_csp)
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


#### ----- calculate the PAF ------- ####

# make a binary variable for 0 or >0
model_data$outcome_binary_lessthan0 = ifelse(model_data$p_te_all_csp > 0,"greater than 0.00","equal to 0.00")
table(model_data$outcome_binary_lessthan0,model_data$p_te_all_csp,useNA = "always")
table(model_data$outcome_binary_lessthan0, useNA = "always")
model_data$outcome_binary_lessthan0 = factor(model_data$outcome_binary_lessthan0)
levels(model_data$outcome_binary_lessthan0)
model_data$outcome_binary_lessthan0 = relevel(model_data$outcome_binary_lessthan0,ref = "equal to 0.00")
table(model_data$outcome_binary_lessthan0, useNA = "always")
# 1449/3727 = 38.9% (not a rare outcome)

# run the multi-level model with the binary outcome
model2 <- glmmTMB(outcome_binary_lessthan0~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(model2)
exp(confint(model2))

# run the multi-level model with the continuous outcome coding (more conservative)
model2 <- glmmTMB(p_te_all_csp~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(model2)
exp(confint(model2))

# pull out variance-covariance matrix
covariance_matrix = vcov(model2)
# confint(object, parm, level = 0.95,type = c("vcov"))



#### ---- calculate the proportion of participants infected each month in the full data set ----- ####

# filter final data to just the infections
final_data = final_data %>% filter(main_exposure_primary_case_def == "asymptomatic infection" | main_outcome_primary_case_def == "symptomatic infection")

# now create a variable for if it's an asymptomatic or symptomatic infection
final_data$aim2_exposure = ifelse(final_data$main_exposure_primary_case_def == "asymptomatic infection","asymptomatic infection","symptomatic infection")
final_data$aim2_exposure[which(is.na(final_data$aim2_exposure))] = "symptomatic infection"
table(final_data$aim2_exposure, useNA = "always")
table(final_data$aim2_exposure,final_data$main_exposure_primary_case_def, useNA = "always")
table(final_data$aim2_exposure,final_data$main_outcome_primary_case_def, useNA = "always")

# add a variable for month
final_data = final_data %>%
  mutate(month_human = lubridate::floor_date(sample_id_date, "month"))
table(final_data$month_human)

# calculate the proportion of people that were asymptomatic vs symptomatic each month
month_summary = final_data %>%
  group_by(month_human,aim2_exposure) %>%
  summarize(infection_n = n())
month_summary_2 = final_data %>%
  group_by(month_human) %>%
  summarize(total_n = n())
month_summary = data.frame(month_summary)
month_summary_2 = data.frame(month_summary_2)
month_summary = left_join(month_summary,month_summary_2,by="month_human")
month_summary = month_summary %>%
  filter(aim2_exposure == "asymptomatic infection") %>%
  mutate(prop_asymptomatic = infection_n/total_n)


#### ----- just calculate the pafs ------ ####

# calculate the population attributable fraction by hand at different prevalences for malaria
# PAF = p (RR − 1) / {p(RR-1) +1 }
# this currently uses the conservative continuous outcome coding estimate (makes the outcome more rare)
paf.6.17 = (month_summary$prop_asymptomatic[1]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[1]*(exp(0.402469)-1)+1)
paf.7.17 = (month_summary$prop_asymptomatic[2]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[2]*(exp(0.402469)-1)+1)
paf.8.17 = (month_summary$prop_asymptomatic[3]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[3]*(exp(0.402469)-1)+1)
paf.9.17 = (month_summary$prop_asymptomatic[4]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[4]*(exp(0.402469)-1)+1)
paf.10.17 = (month_summary$prop_asymptomatic[5]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[5]*(exp(0.402469)-1)+1)
paf.11.17 = (month_summary$prop_asymptomatic[6]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[6]*(exp(0.402469)-1)+1)
paf.1.18 = (month_summary$prop_asymptomatic[7]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[7]*(exp(0.402469)-1)+1)
paf.2.18 = (month_summary$prop_asymptomatic[8]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[8]*(exp(0.402469)-1)+1)
paf.3.18 = (month_summary$prop_asymptomatic[9]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[9]*(exp(0.402469)-1)+1)
paf.4.18 = (month_summary$prop_asymptomatic[10]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[10]*(exp(0.402469)-1)+1)
paf.5.18 = (month_summary$prop_asymptomatic[11]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[11]*(exp(0.402469)-1)+1)
paf.6.18 = (month_summary$prop_asymptomatic[12]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[12]*(exp(0.402469)-1)+1)
paf.7.18 = (month_summary$prop_asymptomatic[13]*(exp(0.402469)-1))/(month_summary$prop_asymptomatic[13]*(exp(0.402469)-1)+1)

# now calculate the lower bound of the 95% CI
paf.6.17_lower = (month_summary$prop_asymptomatic[1]*(1.06621274-1))/(month_summary$prop_asymptomatic[1]*(1.06621274-1)+1)
paf.7.17_lower = (month_summary$prop_asymptomatic[2]*(1.06621274-1))/(month_summary$prop_asymptomatic[2]*(1.06621274-1)+1)
paf.8.17_lower = (month_summary$prop_asymptomatic[3]*(1.06621274-1))/(month_summary$prop_asymptomatic[3]*(1.06621274-1)+1)
paf.9.17_lower = (month_summary$prop_asymptomatic[4]*(1.06621274-1))/(month_summary$prop_asymptomatic[4]*(1.06621274-1)+1)
paf.10.17_lower = (month_summary$prop_asymptomatic[5]*(1.06621274-1))/(month_summary$prop_asymptomatic[5]*(1.06621274-1)+1)
paf.11.17_lower = (month_summary$prop_asymptomatic[6]*(1.06621274-1))/(month_summary$prop_asymptomatic[6]*(1.06621274-1)+1)
paf.1.18_lower = (month_summary$prop_asymptomatic[7]*(1.06621274-1))/(month_summary$prop_asymptomatic[7]*(1.06621274-1)+1)
paf.2.18_lower = (month_summary$prop_asymptomatic[8]*(1.06621274-1))/(month_summary$prop_asymptomatic[8]*(1.06621274-1)+1)
paf.3.18_lower = (month_summary$prop_asymptomatic[9]*(1.06621274-1))/(month_summary$prop_asymptomatic[9]*(1.06621274-1)+1)
paf.4.18_lower = (month_summary$prop_asymptomatic[10]*(1.06621274-1))/(month_summary$prop_asymptomatic[10]*(1.06621274-1)+1)
paf.5.18_lower = (month_summary$prop_asymptomatic[11]*(1.06621274-1))/(month_summary$prop_asymptomatic[11]*(1.06621274-1)+1)
paf.6.18_lower = (month_summary$prop_asymptomatic[12]*(1.06621274-1))/(month_summary$prop_asymptomatic[12]*(1.06621274-1)+1)
paf.7.18_lower = (month_summary$prop_asymptomatic[13]*(1.06621274-1))/(month_summary$prop_asymptomatic[13]*(1.06621274-1)+1)

# now calculate the upper bound of the 95% CI
paf.6.17_upper = (month_summary$prop_asymptomatic[1]*(2.0976660-1))/(month_summary$prop_asymptomatic[1]*(2.0976660-1)+1)
paf.7.17_upper = (month_summary$prop_asymptomatic[2]*(2.0976660-1))/(month_summary$prop_asymptomatic[2]*(2.0976660-1)+1)
paf.8.17_upper = (month_summary$prop_asymptomatic[3]*(2.0976660-1))/(month_summary$prop_asymptomatic[3]*(2.0976660-1)+1)
paf.9.17_upper = (month_summary$prop_asymptomatic[4]*(2.0976660-1))/(month_summary$prop_asymptomatic[4]*(2.0976660-1)+1)
paf.10.17_upper = (month_summary$prop_asymptomatic[5]*(2.0976660-1))/(month_summary$prop_asymptomatic[5]*(2.0976660-1)+1)
paf.11.17_upper = (month_summary$prop_asymptomatic[6]*(2.0976660-1))/(month_summary$prop_asymptomatic[6]*(2.0976660-1)+1)
paf.1.18_upper = (month_summary$prop_asymptomatic[7]*(2.0976660-1))/(month_summary$prop_asymptomatic[7]*(2.0976660-1)+1)
paf.2.18_upper = (month_summary$prop_asymptomatic[8]*(2.0976660-1))/(month_summary$prop_asymptomatic[8]*(2.0976660-1)+1)
paf.3.18_upper = (month_summary$prop_asymptomatic[9]*(2.0976660-1))/(month_summary$prop_asymptomatic[9]*(2.0976660-1)+1)
paf.4.18_upper = (month_summary$prop_asymptomatic[10]*(2.0976660-1))/(month_summary$prop_asymptomatic[10]*(2.0976660-1)+1)
paf.5.18_upper = (month_summary$prop_asymptomatic[11]*(2.0976660-1))/(month_summary$prop_asymptomatic[11]*(2.0976660-1)+1)
paf.6.18_upper = (month_summary$prop_asymptomatic[12]*(2.0976660-1))/(month_summary$prop_asymptomatic[12]*(2.0976660-1)+1)
paf.7.18_upper = (month_summary$prop_asymptomatic[13]*(2.0976660-1))/(month_summary$prop_asymptomatic[13]*(2.0976660-1)+1)


# make a data frame
months = c("2017-06-01","2017-07-01","2017-08-01","2017-09-01","2017-10-01","2017-11-01","2017-12-01","2018-01-01","2018-02-01","2018-03-01","2018-04-01",
           "2018-05-01","2018-06-01","2018-07-01")
pafs = c(paf.6.17,paf.7.17,paf.8.17,paf.9.17,paf.10.17,paf.11.17,0,paf.1.18,paf.2.18,paf.3.18,paf.4.18,paf.5.18,paf.6.18,paf.7.18)
lower_ci = c(paf.6.17_lower,paf.7.17_lower,paf.8.17_lower,paf.9.17_lower,paf.10.17_lower,paf.11.17_lower,0,paf.1.18_lower,paf.2.18_lower,paf.3.18_lower,paf.4.18_lower,paf.5.18_lower,paf.6.18_lower,paf.7.18_lower)
upper_ci = c(paf.6.17_upper,paf.7.17_upper,paf.8.17_upper,paf.9.17_upper,paf.10.17_upper,paf.11.17_upper,0,paf.1.18_upper,paf.2.18_upper,paf.3.18_upper,paf.4.18_upper,paf.5.18_upper,paf.6.18_upper,paf.7.18_upper)
month_df = data.frame(months,pafs,lower_ci,upper_ci)
month_df$pafs = month_df$pafs*100
month_df$lower_ci = month_df$lower_ci*100
month_df$upper_ci = month_df$upper_ci*100

# make a line plot of the different paf values 
paf_plot = ggplot(data=month_df,aes(x=months,y=pafs)) +
  geom_line(size=1.5,group=1,colour="#a6bddb") +
  geom_ribbon(data=month_df,aes(x=1:length(months),ymin = lower_ci, ymax = upper_ci),alpha=0.3,fill="#a6bddb") +
  theme_bw() +
  scale_y_continuous(limits = c(0,100)) +
  xlab("Month") +
  ylab("Population attributable fraction (%)") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(text = element_text(size=12)) 
paf_plot
ggsave(paf_plot, filename="/Users/kelseysumner/Desktop/paf_plot.png", device="png",
       height=5, width=10, units="in", dpi=500)


#### ----- calculate contributions to the infectious reservoir ------ ####

# this is using the 2.66 OR

# OR value
exp(0.979918)

# Ia = 2.66/3.66
Ia = exp(0.979918)/(exp(0.979918)+1)
Is = 1-Ia

# Ca = PaIa/(PaIa + PsIs)
Ca_function = function(Pa,Ps){
  Ca = (Pa*Ia)/(Pa*Ia + Ps*Is)
  return(Ca)
}

# run the Ca formula for each month
ca_6_17 = Ca_function(month_summary$prop_asymptomatic[1],(1-month_summary$prop_asymptomatic[1]))
ca_7_17 = Ca_function(month_summary$prop_asymptomatic[2],(1-month_summary$prop_asymptomatic[2]))
ca_8_17 = Ca_function(month_summary$prop_asymptomatic[3],(1-month_summary$prop_asymptomatic[3]))
ca_9_17 = Ca_function(month_summary$prop_asymptomatic[4],(1-month_summary$prop_asymptomatic[4]))
ca_10_17 = Ca_function(month_summary$prop_asymptomatic[5],(1-month_summary$prop_asymptomatic[5]))
ca_11_17 = Ca_function(month_summary$prop_asymptomatic[6],(1-month_summary$prop_asymptomatic[6]))
ca_12_17 = Ca_function(month_summary$prop_asymptomatic[7],(1-month_summary$prop_asymptomatic[7]))
ca_1_18 = Ca_function(month_summary$prop_asymptomatic[8],(1-month_summary$prop_asymptomatic[8]))
ca_2_18 = Ca_function(month_summary$prop_asymptomatic[9],(1-month_summary$prop_asymptomatic[9]))
ca_3_18 = Ca_function(month_summary$prop_asymptomatic[10],(1-month_summary$prop_asymptomatic[10]))
ca_4_18 = Ca_function(month_summary$prop_asymptomatic[11],(1-month_summary$prop_asymptomatic[11]))
ca_5_18 = Ca_function(month_summary$prop_asymptomatic[12],(1-month_summary$prop_asymptomatic[12]))
ca_6_18 = Ca_function(month_summary$prop_asymptomatic[13],(1-month_summary$prop_asymptomatic[13]))
ca_7_18 = Ca_function(month_summary$prop_asymptomatic[14],(1-month_summary$prop_asymptomatic[14]))

# make a data frame
month_1 = c("2017-06-01","2017-07-01","2017-08-01","2017-09-01","2017-10-01","2017-11-01","2017-12-01","2018-01-01","2018-02-01","2018-03-01","2018-04-01","2018-05-01","2018-06-01","2018-07-01")
cas_1 = c(ca_6_17,ca_7_17,ca_8_17,ca_9_17,ca_10_17,ca_11_17,ca_12_17,ca_1_18,ca_2_18,ca_3_18,ca_4_18,ca_5_18,ca_6_18,ca_7_18)
month_df_1 = data.frame(month_1,cas_1,month_summary$prop_asymptomatic)
month_df_1$cas_1 = month_df_1$cas_1*100
month_df_1$month_summary.prop_asymptomatic = month_df_1$month_summary.prop_asymptomatic*100

# symptomatic (blue): #3B9AB2
# asymptomatic (yellow): #E1AF00
# mosquitoes (red): #F21A00
# no infection (light grey): #D3DDDC

# make a line plot of the different paf values 
ca_plot = ggplot() +
  geom_line(data=month_df_1,aes(x=month_1,y=cas_1),size=1.5,group=1,colour="black") +
  theme_bw() +
  xlab("Month") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(text = element_text(size=12)) +
  geom_area(data=month_df_1,aes(x=month_1,y=cas_1),group=1, fill="#E1AF00",alpha=0.8) +
  geom_ribbon(data=month_df_1,aes(ymin=pmin(cas_1,100), ymax=100,x=month_1), fill="#3B9AB2", alpha=0.8,group=1) +
  geom_line(data=month_df_1,aes(x=month_1,y=month_summary.prop_asymptomatic),group=1,colour="#081d58",size=1.5,alpha=0.7) +
  scale_y_continuous(limits = c(0,100),name="Contribution to infectious reservoir (%)",sec.axis = sec_axis(~.*1,name="Percent of asymptomatic infections (%)"))
ca_plot
ggsave(ca_plot, filename="/Users/kelseysumner/Desktop/ca_plot.png", device="png",
       height=5, width=10, units="in", dpi=500)


# make a line plot of the different paf values but with dissertation colors
ca_plot = ggplot() +
  geom_line(data=month_df_1,aes(x=month_1,y=cas_1),size=1.5,group=1,colour="black") +
  theme_bw() +
  xlab("Month") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(text = element_text(size=12)) +
  geom_area(data=month_df_1,aes(x=month_1,y=cas_1),group=1, fill="#ff7f00",alpha=0.8) +
  geom_ribbon(data=month_df_1,aes(ymin=pmin(cas_1,100), ymax=100,x=month_1), fill="#e41a1c", alpha=0.8,group=1) +
  geom_line(data=month_df_1,aes(x=month_1,y=month_summary.prop_asymptomatic),group=1,colour="#081d58",size=1.5,alpha=0.7) +
  scale_y_continuous(limits = c(0,100),name="Contribution to infectious reservoir (%)",sec.axis = sec_axis(~.*1,name="Percent of asymptomatic infections (%)"))
ca_plot
ggsave(ca_plot, filename="/Users/kelseysumner/Desktop/ca_plot_dis.png", device="png",
       height=5, width=10, units="in", dpi=500)





#### -------- calculate cas across all months ------- ####

# calculate the proportion of people that were asymptomatic vs symptomatic across the entire study follow-up
all_summary = final_data %>%
  group_by(aim2_exposure) %>%
  summarize(infection_n = n())
asymp_prop = 902/1039

# OR value - using the 2.66 OR
exp(0.979918)

# Ia = 2.66/3.66
Ia = exp(0.979918)/(exp(0.979918)+1)
Is = 1-Ia

# Ca = PaIa/(PaIa + PsIs)
Ca_function = function(Pa,Ps){
  Ca = (Pa*Ia)/(Pa*Ia + Ps*Is)
  return(Ca)
}

# calculate ca across all study follow-up
cas_all = Ca_function(asymp_prop,(1-asymp_prop))

# now calculate cas for the lower ci
Ia = 2.0479305/(2.0479305+1)
Is = 1-Ia
# Ca = PaIa/(PaIa + PsIs)
Ca_function = function(Pa,Ps){
  Ca = (Pa*Ia)/(Pa*Ia + Ps*Is)
  return(Ca)
}
cas_lower = Ca_function(asymp_prop,(1-asymp_prop))

# now calculate cas for the upper ci
Ia = 3.4660177/(3.4660177+1)
Is = 1-Ia
# Ca = PaIa/(PaIa + PsIs)
Ca_function = function(Pa,Ps){
  Ca = (Pa*Ia)/(Pa*Ia + Ps*Is)
  return(Ca)
}
cas_upper = Ca_function(asymp_prop,(1-asymp_prop))

# summarize the values
cas_all
cas_lower
cas_upper
