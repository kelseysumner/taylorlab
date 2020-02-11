# -------------------------------------- #
#           Spat21/Mozzie Study          #
#  Covariate functional form assessment  #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#           January 14, 2020             #
# -------------------------------------- #

# really good resource for troubleshooting multi-level model convergence issues:
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)
library(betareg)
library(lme4)
library(ggplot2)
library(epiDisplay)
library(moments)



#### ----- read in the data sets ----- ####

# read in the combined ama and csp data set for mosquito abdomens
model_data = read_rds("Desktop/clean_ids_haplotype_results/AMA_and_CSP/final/model data/spat21_aim2_computational_model_subset_data_6FEB2020.rds")



#### ----- look at descriptive statistics for descriptive variables ------ ####

# aim2_exposure
table(model_data$aim2_exposure, useNA = "always")
# p_te_all
summary(model_data$p_te_all)
sd(model_data$p_te_all)
skewness(model_data$p_te_all)
kurtosis(model_data$p_te_all)
# pfr364Q_std_combined
summary(model_data$pfr364Q_std_combined)
sd(model_data$pfr364Q_std_combined)
skewness(model_data$pfr364Q_std_combined)
kurtosis(model_data$pfr364Q_std_combined)
# age_cat_baseline
table(model_data$age_cat_baseline,useNA = "always")
# mosquito_week_count
summary(model_data$mosquito_week_count)
sd(model_data$mosquito_week_count)
skewness(model_data$mosquito_week_count)
kurtosis(model_data$mosquito_week_count)
# mean_moi
summary(model_data$mean_moi)
sd(model_data$mean_moi)
skewness(model_data$mean_moi)
kurtosis(model_data$mean_moi)
# village_name
table(model_data$village_name, useNA = "always")


#### ----- do a functional form assessment of parasite density -------- ####

# first make a rescaled variable of parasite density (rescaled to a mean of 0)
model_data$pfr364Q_std_combined_rescaled = scale(model_data$pfr364Q_std_combined)
summary(model_data$pfr364Q_std_combined)
summary(model_data$pfr364Q_std_combined_rescaled)

# look at the natural form of pfr364Q_std_combined through the LOWESS curve
# plot the lowess graph 
plot_loess = ggplot(data=model_data) +
  geom_point(aes(x=pfr364Q_std_combined_rescaled,y=p_te_all)) +
  geom_smooth(aes(x=pfr364Q_std_combined_rescaled,y=p_te_all),method="loess")
plot_loess

# look at the linear coding
model1=glmer(p_te_all~pfr364Q_std_combined_rescaled + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model1) 
# exponentiate coefficient
exp(-0.30653)
# pull out log likelihood test value
logLik(model1) 
# plot the linear model fit
linear_plot = ggplot(model_data, aes(x=pfr364Q_std_combined_rescaled, y=p_te_all)) + 
  labs(title="LINEAR Model",
       x="Parasite density",
       y="P(TEall)",
       caption="NOTE: LOESS is blue. Linear is red. 95%CI is for the linear trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~x, color="red", linetype="dashed") +
  theme_bw()
linear_plot

# now look at the quadratic coding
# first make the quadratic term
model_data$pfr364Q_std_combined_rescaled_quad = model_data$pfr364Q_std_combined_rescaled*model_data$pfr364Q_std_combined_rescaled
# then run the model
model2=glmer(p_te_all~pfr364Q_std_combined_rescaled+pfr364Q_std_combined_rescaled_quad + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model2) 
model2=glm(p_te_all~pfr364Q_std_combined_rescaled+pfr364Q_std_combined_rescaled_quad,family=binomial(link = "log"),data=model_data) 
summary(model2) # did not converge with the multilevel model
# exponentiate coefficient

# now look at the cubic coding
# first make the cubic term
model_data$pfr364Q_std_combined_rescaled_cubic = model_data$pfr364Q_std_combined_rescaled*model_data$pfr364Q_std_combined_rescaled*model_data$pfr364Q_std_combined_rescaled
# then run the model
model3=glmer(p_te_all~pfr364Q_std_combined_rescaled+pfr364Q_std_combined_rescaled_quad+pfr364Q_std_combined_rescaled_cubic + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model3) 
# pull out log likelihood test value
logLik(model3) 

# now look at the binary coding
model4=glmer(p_te_all~pfr364Q_std_combined_cat + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model4)
# model failed to converge

# now look at categorical coding (quartiles)
summary(model_data$pfr364Q_std_combined)
model_data$pfr364_std_quartiles = ifelse(model_data$pfr364Q_std_combined < 1.93, "quartile 1",
                                         ifelse(model_data$pfr364Q_std_combined >= 1.93 & model_data$pfr364Q_std_combined < 51.64, "quartile 2",
                                                ifelse(model_data$pfr364Q_std_combined >= 51.64 & model_data$pfr364Q_std_combined < 773.53,"quartile 3","quartile 4")))
table(model_data$pfr364_std_quartiles, useNA = "always")
model5=glmer(p_te_all~pfr364_std_quartiles + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model5)
# model failed to converge

# look at the natural log coding
model_data$pfr364Q_std_combined_ln = log(model_data$pfr364Q_std_combined)
model6=glmer(p_te_all~pfr364_std_quartiles + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model6)
summary(model_data$pfr364Q_std_combined_ln)
# model failed to converge


#### ------- do a functional form assessment of age -------- ####

# first look at age
table(model_data$age_all_baseline, useNA = "always")
# change 9mos to 1 year because aged in
table(model_data$age_cat_baseline, useNA = "always")
is.numeric(model_data$age_all_baseline)
model_data$age_all_baseline = as.numeric(model_data$age_all_baseline)
model_data$age_all_baseline[which(is.na(model_data$age_all_baseline))] = 1
table(model_data$age_cat_baseline, useNA = "always")


# first make a rescaled variable of parasite density (rescaled to a mean of 0)
model_data$age_rescaled = scale(model_data$age_all_baseline)
summary(model_data$age_all_baseline)
summary(model_data$age_rescaled)

# plot the lowess graph 
plot_loess = ggplot(data=model_data) +
  geom_point(aes(x=age_all_baseline,y=p_te_all)) +
  geom_smooth(aes(x=age_all_baseline,y=p_te_all),method="loess")
plot_loess

# look at the linear coding
model1=glmer(p_te_all~age_rescaled + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model1) 
# exponentiate coefficient
exp(0.04460)
# pull out log likelihood test value
logLik(model1) 
# plot the linear model fit
linear_plot = ggplot(model_data, aes(x=age_rescaled, y=p_te_all)) + 
  labs(title="LINEAR Model",
       x="Age",
       y="P(TEall)",
       caption="NOTE: LOESS is blue. Linear is red. 95%CI is for the linear trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~x, color="red", linetype="dashed") +
  theme_bw()
linear_plot

# now look at the quadratic coding
model_data$age_quad = model_data$age_rescaled*model_data$age_rescaled
model2=glmer(p_te_all~age_rescaled+age_quad + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model2) 
# exponentiate coefficient
exp(-0.02421)
exp(0.04455)
# pull out log likelihood test value
logLik(model2) 
# plot the quadratic model fit
quadratic_plot = ggplot(model_data, aes(x=age_rescaled, y=p_te_all)) + 
  labs(title="QUADRATIC Model",
       x="Age",
       y="P(TEall)",
       caption="NOTE: LOESS is blue. quadratic is red. 95%CI is for the quadratic trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~x+x^2, color="red", linetype="dashed") +
  theme_bw()
quadratic_plot

# now look at the cubic coding
model_data$age_cubic = model_data$age_rescaled*model_data$age_rescaled*model_data$age_rescaled
model3=glmer(p_te_all~age_rescaled+age_quad+age_cubic + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model3) 
# exponentiate coefficient
exp(0.06282)
exp(0.38255)
exp(-0.12689)
# pull out log likelihood test value
logLik(model3) 
# plot the cubic model fit
cubic_plot = ggplot(model_data, aes(x=age_rescaled, y=p_te_all)) + 
  labs(title="CUBIC Model",
       x="Age",
       y="P(TEall)",
       caption="NOTE: LOESS is blue. cubic is red. 95%CI is for the cubic trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~x+x^2+x^3, color="red", linetype="dashed") +
  theme_bw()
cubic_plot

# now look at the categorical coding
model4=glmer(p_te_all~age_cat_baseline + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model4) 
# exponentiate coefficient
exp(-0.3192)
exp(-0.3933)
# pull out log likelihood test value
logLik(model4) 

# now look at the ln coding
model_data$age_ln = log(model_data$age_all_baseline)
model5=glmer(p_te_all~age_ln + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model5) 
# exponentiate coefficient
exp(-0.01179)
# pull out log likelihood test value
logLik(model5) 


#### ------- do the functional form assessment of mosquito_week_count ------- ####

# look at the variable
summary(model_data$mosquito_week_count)

# first make a rescaled variable of parasite density (rescaled to a mean of 0)
model_data$mosquito_week_count_rescaled = scale(model_data$mosquito_week_count)
summary(model_data$mosquito_week_count)
summary(model_data$mosquito_week_count_rescaled)

# plot the lowess graph 
plot_loess = ggplot(data=model_data) +
  geom_point(aes(x=mosquito_week_count,y=p_te_all)) +
  geom_smooth(aes(x=mosquito_week_count,y=p_te_all),method="loess")
plot_loess

# look at the linear coding
model1=glmer(p_te_all~mosquito_week_count_rescaled + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model1) 
# exponentiate coefficient
exp(-0.01211)
# pull out log likelihood test value
logLik(model1) 
# plot the linear model fit
linear_plot = ggplot(model_data, aes(x=mosquito_week_count_rescaled, y=p_te_all)) + 
  labs(title="LINEAR Model",
       x="Mosquito week count",
       y="P(TEall)",
       caption="NOTE: LOESS is blue. Linear is red. 95%CI is for the linear trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~x, color="red", linetype="dashed") +
  theme_bw()
linear_plot

# look at the quadratic coding
model_data$mosquito_week_count_rescaled_quad = model_data$mosquito_week_count_rescaled*model_data$mosquito_week_count_rescaled
model2=glmer(p_te_all~mosquito_week_count_rescaled+mosquito_week_count_rescaled_quad + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model2) 
# exponentiate coefficient
exp(-0.008373)
exp(0.020855)
# pull out log likelihood test value
logLik(model2) 

# look at the cubic coding
model_data$mosquito_week_count_rescaled_cubic = model_data$mosquito_week_count_rescaled*model_data$mosquito_week_count_rescaled*model_data$mosquio_week_count_rescaled_quad
model3=glmer(p_te_all~mosquito_week_count_rescaled+mosquito_week_count_rescaled_quad+mosquito_week_count_rescaled_cubic + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model3) 
# exponentiate coefficient
exp(0.06282)
exp(0.38255)
exp(-0.12689)
# pull out log likelihood test value
logLik(model3) 

# look at the categorical coding
summary(model_data$mosquito_week_count)
table(model_data$mosquito_week_count_cat, useNA = "always")
model4=glmer(p_te_all~mosquito_week_count_cat + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data,control = glmerControl(optimizer="bobyqa")) 
summary(model4) 
# this model doesn't converge so try a different categorical coding based on where the curve seems to change and that cuts data in half
model_data$mosquito_week_count_cat = ifelse(model_data$mosquito_week_count < 75, "<75 mosquitoes","75-147 mosquitoes")
table(model_data$mosquito_week_count_cat, useNA = "always")
model_data$mosquito_week_count_cat = as.factor(model_data$mosquito_week_count_cat)
# rerun the model
model4=glmer(p_te_all~mosquito_week_count_cat + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data,control = glmerControl(optimizer="bobyqa")) 
summary(model4) 
# exponentiate coefficient
exp(0.08478)
# pull out log likelihood test value
logLik(model4) 

# look at the ln coding 
model_data$mosquito_week_count_ln = log(model_data$mosquito_week_count)
model5=glmer(p_te_all~mosquito_week_count_ln + (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data) 
summary(model5) 
# exponentiate coefficient
exp(-0.04766)
exp(0.04939)
# pull out log likelihood test value
logLik(model5) 


#### ------- do the functional form assessment of mean_moi ------- ####

# look at the variable
summary(model_data$mean_moi)

# first make a rescaled variable of parasite density (rescaled to a mean of 0)
model_data$mean_moi_rescaled = scale(model_data$mean_moi)
summary(model_data$mean_moi)
summary(model_data$mean_moi_rescaled)

# plot the lowess graph 
plot_loess = ggplot(data=model_data) +
  geom_point(aes(x=mean_moi,y=p_te_all)) +
  geom_smooth(aes(x=mean_moi,y=p_te_all),method="loess")
plot_loess

# look at the linear coding
model1=glmer(p_te_all~mean_moi_rescaled+ (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data,control = glmerControl(optimizer="bobyqa")) 
summary(model1) 
# model did not converge

# look at the quadratic coding
model_data$mean_moi_quad = model_data$mean_moi_rescaled*model_data$mean_moi_rescaled
model2=glmer(p_te_all~mean_moi_rescaled+mean_moi_quad+(1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data,control = glmerControl(optimizer="bobyqa")) 
summary(model2) 
# model had singular fit

# look at the cubic coding
model_data$mean_moi_cubic = model_data$mean_moi_rescaled*model_data$mean_moi_rescaled*model_data$mean_moi_rescaled
model3=glmer(p_te_all~mean_moi_rescaled+mean_moi_quad+mean_moi_cubic+ (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data,control = glmerControl(optimizer="bobyqa")) 
summary(model3) 
# did not converge

# look at the binary coding
summary(model_data$mean_moi)
model_data$mean_moi_binary = ifelse(model_data$mean_moi > 2, ">2 mean MOI","1-2 mean MOI")
table(model_data$mean_moi_binary, useNA = "always")
table(model_data$mean_moi_binary,model_data$mean_moi)
model_data$mean_moi_binary = as.factor(model_data$mean_moi_binary)
model_data$mean_moi_binary = relevel(model_data$mean_moi_binary,ref="1-2 mean MOI")
model4=glmer(p_te_all~mean_moi_binary+ (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data,control = glmerControl(optimizer="bobyqa")) 
summary(model4) 
# exponentiate the coefficient
exp(2.1037)

# look at the categorical coding
summary(model_data$mean_moi)
model_data$mean_moi_category = ifelse(model_data$mean_moi <= 2, "1-2 mean MOI",ifelse(model_data$mean_moi > 2 & model_data$mean_moi <= 6,"2.5-6 mean MOI","6.5-15.5 mean MOI"))
table(model_data$mean_moi_category, useNA = "always")
table(model_data$mean_moi_category,model_data$mean_moi)
model_data$mean_moi_category = as.factor(model_data$mean_moi_category)
model_data$mean_moi_category = relevel(model_data$mean_moi_category,ref="1-2 mean MOI")
model5=glmer(p_te_all~mean_moi_category+ (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data,control = glmerControl(optimizer="bobyqa")) 
summary(model5) 
# exponentiate the coefficient
exp(1.7251)
exp(2.4965)

# look at the natural log
summary(model_data$mean_moi)
model_data$mean_moi_ln = log(model_data$mean_moi)
table(model_data$mean_moi_ln, useNA = "always")
model6=glmer(p_te_all~mean_moi_ln+ (1|HH_ID_human/unq_memID),family=binomial(link = "log"),data=model_data,control = glmerControl(optimizer="bobyqa")) 
summary(model6) 
# model did not converge



#### -------- clean up the model data set to just the variables of interest ------ ####

model_data = model_data %>%
  dplyr::select(-c(pfr364Q_std_combined_rescaled_quad,pfr364Q_std_combined_rescaled_cubic,pfr364Q_std_combined_ln,pfr364_std_quartiles,
            age_quad,age_rescaled,age_cubic,age_ln,mosquito_week_count_rescaled,mosquito_week_count_ln,mosquito_week_count_rescaled_quad
            ,mean_moi_quad,mean_moi_cubic,mean_moi_ln))
colnames(model_data)

# write out the data set
write_csv(model_data,"Desktop/spat21_final_model_data_set_11FEB2020.csv")
write_rds(model_data,"Desktop/spat21_final_model_data_set_11FEB2020.rds")






