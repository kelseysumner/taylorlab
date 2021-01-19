# -------------------------------------- #
#           Spat21/Mozzie Study          #
#           Sensitivity analyses         #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#           October 13, 2020             #
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
library(glmmTMB)


#### ----- read in the data sets ----- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Aim 2/clean_ids_haplotype_results/AMA_and_CSP/final/model data/final_model_data/spat21_aim2_merged_data_with_weights_5MAR2020.rds")



#### ------ check covariate coding ------- ####

# rescale csp and ama moi
model_data$csp_moi_rescaled = scale(model_data$csp_moi)
model_data$ama_moi_rescaled = scale(model_data$ama_moi)

# # subset the data set to samples that passed pfcsp sequencing only
# model_data = model_data %>%
   # filter(!(is.na(csp_haps_shared)))

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


#### ----- create a new combined p_te_all variable with combined ama and csp targets for p_te_h ------ ####

# create a new combined p_te_a_c_combo variable
model_data$p_te_a_c_combo = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if ((is.na(model_data$ama_haps_shared[i]))){
    model_data$p_te_a_c_combo[i] = model_data$p_te_c_alt[i]
  } else if ((is.na(model_data$csp_haps_shared[i]))){
    model_data$p_te_a_c_combo[i] = model_data$p_te_a_alt[i]
  } else if (!(is.na(model_data$csp_haps_shared[i])) & !(is.na(model_data$ama_haps_shared[i]))){
    model_data$p_te_a_c_combo[i] = 1-((1-model_data$p_te_a_alt[i])*(1-model_data$p_te_c_alt[i]))
  }
}
summary(model_data$p_te_a_alt)
summary(model_data$p_te_c_alt)
summary(model_data$p_te_a_c_combo)

# rescale p_te_a_c_combo
model_data$rescaled_p_te_a_c_combo = (model_data$p_te_a_c_combo-min(model_data$p_te_a_c_combo))/(max(model_data$p_te_a_c_combo)-min(model_data$p_te_a_c_combo))

# rescale the variables to all have to be between 0 and 1
summary(model_data$p_te_d) 
summary(model_data$p_te_t)
summary(model_data$rescaled_p_te_a_c_combo)
# no longer need to be rescaled

# make a final variable that is P(TEall) for both gene targets combined
# have that variable conditioned so you only calculate the probability of transmission if p_te is non-zero for all 4 variables
p_te_all_combo = rep(NA,nrow(model_data))
for (i in 1:nrow(model_data)){
  if (model_data$p_te_t[i] != 0 & model_data$p_te_d[i] != 0 & model_data$rescaled_p_te_a_c_combo[i] != 0){
    p_te_all_combo[i] = model_data$p_te_t[i]*model_data$p_te_d[i]*model_data$rescaled_p_te_a_c_combo[i]
  } else {
    p_te_all_combo[i] = 0
  }
}
summary(p_te_all_combo)
model_data$p_te_all_combo = p_te_all_combo
length(which(p_te_all_combo == 0)) # 1336
length(which(p_te_all_combo > 0)) # 2634
length(which(model_data$p_te_t != 0 & model_data$p_te_d != 0 & model_data$rescaled_p_te_a_c_combo != 0)) # 2634



#### ------ run the final models and do model selection ------- ####

# run model 2 output without csp moi
model2 <- glmmTMB(p_te_all_combo~aim2_exposure+pfr364Q_std_combined_rescaled+age_cat_baseline+mosquito_week_count_cat+village_name+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = model_data)
summary(model2)
performance::icc(model2)
exp(confint(model2,method="Wald"))

table1 = exp(confint(model2,method="Wald"))
estimates = c(table1[2,3],NA,table1[3,3],NA,NA,NA,table1[5,3],table1[4,3],NA,table1[6,3],NA,NA,NA,table1[7,3],table1[8,3])
lower_ci = c(table1[2,1],NA,table1[3,1],NA,NA,NA,table1[5,1],table1[4,1],NA,table1[6,1],NA,NA,NA,table1[7,1],table1[8,1])
upper_ci = c(table1[2,2],NA,table1[3,2],NA,NA,NA,table1[5,2],table1[4,2],NA,table1[6,2],NA,NA,NA,table1[7,2],table1[8,2])
names = c("Asymptomatic infection","","Parasite density (parasite/uL whole blood)"," ","Participant age           ","<5 years (REF)","5-15 years",">15 years","  ","High mosquito abundance","   ","Village                ","Maruti (REF)","Kinesamo","Sitabicha")
forest_plot_df = data.frame(names,estimates,lower_ci,upper_ci)
forest_plot_df$names = factor(forest_plot_df$names, levels = c("Asymptomatic infection","","Parasite density (parasite/uL whole blood)"," ","Participant age           ","<5 years (REF)","5-15 years",">15 years","  ","High mosquito abundance","   ","Village                ","Maruti (REF)","Kinesamo","Sitabicha"))
forest_plot_df$names = ordered(forest_plot_df$names, levels = c("Asymptomatic infection","","Parasite density (parasite/uL whole blood)"," ","Participant age           ","<5 years (REF)","5-15 years",">15 years","  ","High mosquito abundance","   ","Village                ","Maruti (REF)","Kinesamo","Sitabicha"))

# create a forest plot
library(forcats)
fp <- ggplot(data=forest_plot_df, aes(x=fct_rev(names), y=estimates, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=c(3,1,1,1,1,1,1,1,1,1,1,1,1,1,1),colour=c("#E1AF00","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696","#969696")) + 
  geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("Odds ratio (95% CI)") +
  scale_y_continuous(trans="log10",breaks=c(0,0.4,0.6,0.8,1.0,1.2,1.4,1.6)) +
  theme_bw() +
  theme(text = element_text(size=25)) 
fp

# export the plot
ggsave(fp, filename="/Users/kelseysumner/Desktop/sensitivity_analysis_aim2_model_combined_csp_ama.png", device="png",
       height=10, width=12, units="in", dpi=400)


#### ---- change plot 4D to a histogram ------- ####

# first subset to just the infections with csp haplotypes
p_te_all_subset = model_data %>%
  filter(!(is.na(csp_haps_shared)))
# make a plot of p_te_all by symptomatic status
p_te_all_subset$aim2_exposure = relevel(p_te_all_subset$aim2_exposure, ref="asymptomatic infection")
p_te_all_plot = ggplot(data=p_te_all_subset,aes(x=p_te_all_csp,fill=aim2_exposure)) +
  geom_histogram(alpha=0.6,color="black") + 
  scale_fill_manual(values=c("#E1AF00","#3B9AB2")) + 
  labs(fill="Symptomatic status") +
  theme_bw() + 
  xlab("Probability of transmission across all variables") +
  ylab("Density") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5), text = element_text(size=25), legend.position = c(0.7, 0.77),legend.box.background = element_rect(colour = "black"), legend.text = element_text(size=40))
p_te_all_plot
ggsave(p_te_all_plot, filename="/Users/kelseysumner/Desktop/p_te_all_plot_histogram.png", device="png",
       height=8, width=14, units="in", dpi=500)



