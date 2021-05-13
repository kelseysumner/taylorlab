# -------------------------------------- #
#           Spat21/Mozzie Study          #
#          Risk factor analysis          #
#            Mozzie Phase 1              #
#           With haplotypes merged       #
#               K. Sumner                #
#              May 13, 2021              #
# -------------------------------------- #

# good resource for trouble shooting convergence problems
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html


#### -------- load packages ------------ ####

# load in the packages of interest
library(dplyr)
library(readr)
library(tidyr)
library(lme4)
library(ggplot2)
library(sjstats)
library(lmerTest)
library(glmmTMB)
library(tibble)


#### ----- read in the data sets ----- ####

# read in the model data with the haplotype categories and chronic/non-chronic infection categories merged in
ama_model_data = read_rds("Desktop/mozzie_rfa_data_with_chronic_categories_AMA_11MAY2021.rds")
csp_model_data = read_rds("Desktop/mozzie_rfa_data_with_chronic_categories_CSP_11MAY2021.rds")


#### ------ compare PTEall across covariates for csp ------- ####


# now create values for table 1 

# sex
# summaries
table(csp_model_data$sex,useNA = "always")
csp_model_data %>%
  group_by(sex) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
csp_model_data %>%
  group_by(sex,csp_transmission_median) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
sex_model <- glmer(csp_transmission_median~sex+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_model_data)
summary(sex_model)
performance::icc(sex_model)
exp(-0.127577)
exp(confint(sex_model,method="Wald"))


# age
# summaries
table(csp_model_data$age,useNA = "always")
csp_model_data %>%
  group_by(age) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
csp_model_data %>%
  group_by(age,csp_transmission_median) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
age_model <- glmer(csp_transmission_median~age+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_model_data)
summary(age_model)
performance::icc(age_model)
exp(-0.08115)
exp(0.09858)
exp(confint(age_model,method="Wald"))


# parasite density
# summaries
table(csp_model_data$parasite_density,useNA = "always")
csp_model_data %>%
  group_by(parasite_density) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
csp_model_data %>%
  group_by(parasite_density,csp_transmission_median) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
parasite_density_model <- glmer(csp_transmission_median~parasite_density+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_model_data)
summary(parasite_density_model)
performance::icc(parasite_density_model)
exp(-0.7266)
exp(-1.9409)
exp(confint(parasite_density_model,method="Wald"))


# current regular bed net usage
# summaries
table(csp_model_data$current_bed_net_usage,useNA = "always")
csp_model_data %>%
  group_by(current_bed_net_usage) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
csp_model_data %>%
  group_by(current_bed_net_usage,csp_transmission_median) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
current_bed_net_usage_model <- glmer(csp_transmission_median~current_bed_net_usage+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_model_data)
summary(current_bed_net_usage_model)
performance::icc(current_bed_net_usage_model)
exp(-0.6518)
exp(confint(current_bed_net_usage_model,method="Wald"))


# transmission season
# summaries
table(csp_model_data$transmission_season,useNA = "always")
csp_model_data %>%
  group_by(transmission_season) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
csp_model_data %>%
  group_by(transmission_season,csp_transmission_median) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
transmission_season_model <- glmer(csp_transmission_median~transmission_season+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_model_data)
summary(transmission_season_model)
performance::icc(transmission_season_model)
exp(0.01041)
exp(confint(transmission_season_model,method="Wald"))


# infection type
# summaries
table(csp_model_data$infection_type,useNA = "always")
csp_model_data %>%
  group_by(infection_type) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
csp_model_data %>%
  group_by(infection_type,csp_transmission_median) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
infection_type_model <- glmer(csp_transmission_median~infection_type+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_model_data)
summary(infection_type_model)
performance::icc(infection_type_model)
exp(0.8247)
exp(confint(infection_type_model,method="Wald"))


# infection type
# summaries
table(csp_model_data$chronic_infection_cat,useNA = "always")
csp_model_data %>%
  group_by(chronic_infection_cat) %>%
  summarise(x=list(enframe(quantile(p_te_all_csp, c(0.5,0.25,0.75)), "quantile", "p_te_all_csp"))) %>%
  unnest(x)
csp_model_data %>%
  group_by(chronic_infection_cat,csp_transmission_median) %>%
  summarise(n=n())
# crude model using binary outcome for csp transmission
chronic_infection_cat_model <- glmer(csp_transmission_median~chronic_infection_cat+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_model_data)
summary(chronic_infection_cat_model)
performance::icc(chronic_infection_cat_model)
exp(-0.21877)
exp(confint(chronic_infection_cat_model,method="Wald"))





#### -------- create a multivariable model with covariates with p < 0.02 -------- ####

# covariates to include in multivariate model:
#  parasite density, bed net usage, transmission season, and infection type

# run the multivariate model
multivariate_model <- glmer(csp_transmission_median~parasite_density+current_bed_net_usage+infection_type+chronic_infection_cat+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = csp_model_data)
summary(multivariate_model)
performance::icc(multivariate_model)
exp(-0.551339)
exp(-1.561315)
exp(-0.714120)
exp(-0.004712)
exp(-0.303541)
exp(confint(multivariate_model,method="Wald"))




#### -------- look at a model stratified by chronic/non-chronic infections -------- ####

# first create separate data sets for chronic/non-chronic infections
chronic_infections = csp_model_data %>% filter(chronic_infection_cat == "chronic infection")
non_chronic_infections = csp_model_data %>% filter(chronic_infection_cat == "non-chronic infection")

# now rerun the multivariate model for each data set
# for chronic infections
chronic_multivariate_model <- glmer(csp_transmission_median~parasite_density+current_bed_net_usage+infection_type+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = chronic_infections)
summary(chronic_multivariate_model)
performance::icc(chronic_multivariate_model)
# for non-chronic infections
non_chronic_multivariate_model <- glmer(csp_transmission_median~parasite_density+current_bed_net_usage+infection_type+(1|HH_ID_human/unq_memID),family=binomial(link = "logit"), data = non_chronic_infections)
summary(non_chronic_multivariate_model)
performance::icc(non_chronic_multivariate_model)




