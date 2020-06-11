# -------------------------------------- #
#           Spat21/Mozzie Study          #
#     Run final computational model      #
#               AMA Target               # 
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#           February 21, 2020            #
# -------------------------------------- #



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
library(glmmTMB)


#### ----- read in the data sets ----- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA_and_CSP/final/model data/final_model_data/spat21_aim2_merged_data_with_weights_5MAR2020.rds")



#### ------ check covariate coding ------- ####

# rescale csp and ama moi
model_data$csp_moi_rescaled = scale(model_data$csp_moi)
model_data$ama_moi_rescaled = scale(model_data$ama_moi)

# subset the data set to samples that passed pfcsp sequencing only
model_data = model_data %>%
  filter(!(is.na(ama_haps_shared)))

# check the covariates
str(model_data$sample_id_human)
str(model_data$HH_ID_human)
str(model_data$unq_memID)
str(model_data$p_te_all_ama)
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
ggplot(model_data, aes(x = p_te_all_ama)) + geom_density() + facet_wrap(~aim2_exposure)
ggplot(model_data, aes(x = pfr364Q_std_combined_rescaled)) + geom_density() + facet_wrap(~aim2_exposure)
ggplot(model_data, aes(x = age_cat_baseline)) + geom_density() + facet_wrap(~aim2_exposure)
ggplot(model_data, aes(x = ama_moi_rescaled)) + geom_density() + facet_wrap(~aim2_exposure)
ggplot(model_data, aes(x = mosquito_week_count_cat)) + geom_density() + facet_wrap(~aim2_exposure)
ggplot(model_data, aes(x = village_name)) + geom_density() + facet_wrap(~aim2_exposure)
ggplot(model_data, aes(x = HH_ID_human)) + geom_density() + facet_wrap(~aim2_exposure)
table(model_data$HH_ID_human,model_data$aim2_exposure)
table(model_data$unq_memID,model_data$aim2_exposure)
table(model_data$sample_id_human,model_data$aim2_exposure)



#### ------ run the final models and do model selection ------- ####

# run model 2 chosen through the csp model selection process
model2 <- glmmTMB(p_te_all_ama~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(model2)
table1 = exp(confint(model2,method="Wald"))




#### ------ make a forest plot of the ama results --------- ####

estimates = c(table1[2,3],NA,table1[3,3],NA,NA,NA,table1[5,3],table1[4,3],NA,NA,NA,table1[6,3],NA,NA,NA,table1[7,3],table1[8,3])
lower_ci = c(table1[2,1],NA,table1[3,1],NA,NA,NA,table1[5,1],table1[4,1],NA,NA,NA,table1[6,1],NA,NA,NA,table1[7,1],table1[8,1])
upper_ci = c(table1[2,2],NA,table1[3,2],NA,NA,NA,table1[5,2],table1[4,2],NA,NA,NA,table1[6,2],NA,NA,NA,table1[7,2],table1[8,2])
names = c("Asymptomatic infection","","Parasite density (parasite/uL whole blood)"," ","Participant age           ","<5 years (REF)","5-15 years",">15 years","  ","Mosquito abundance        ","Low (REF)","High","   ","Village                ","Maruti (REF)","Kinesamo","Sitabicha")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","","Parasite density (parasite/uL whole blood)"," ","Participant age           ","<5 years (REF)","5-15 years",">15 years","  ","Mosquito abundance        ","Low (REF)","High","   ","Village                ","Maruti (REF)","Kinesamo","Sitabicha"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","","Parasite density (parasite/uL whole blood)"," ","Participant age           ","<5 years (REF)","5-15 years",">15 years","  ","Mosquito abundance        ","Low (REF)","High","   ","Village                ","Maruti (REF)","Kinesamo","Sitabicha"))

# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),colour=c("#E1AF00","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10") +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp

# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/forest_plot_aim2_model_continuous_outcome_ama_supplement.png", device="png",
       height=9, width=12.5, units="in", dpi=400)







