# -------------------------------------- #
#           Spat21/Mozzie Study          #
#                 Aim 1A                 #
#       Survival Analysis Tutorial       #
#                K. Sumner               #
#             August 13, 2019            #
# -------------------------------------- #


#### -------- load packages ------------ ####

# load in the packages of interest
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)


#### -------- TUTORIAL ONE: ovarian tutorial data set -------- ####

# this tutorial was done through DataCamp's Survival Analysis in R for Beginners

# Import the ovarian cancer dataset and have a look at it
data(ovarian)
glimpse(ovarian)
## Observations: 26
## Variables: 6
## $ futime   <dbl> 59, 115, 156, 421, 431, 448, 464, 475, 477, 563, 638,...
## $ fustat   <dbl> 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,...
## $ age      <dbl> 72.3315, 74.4932, 66.4658, 53.3644, 50.3397, 56.4301,...
## $ resid.ds <dbl> 2, 2, 2, 2, 2, 1, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 2, 1,...
## $ rx       <dbl> 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 1, 2, 2, 2, 1, 1, 1, 1,...
## $ ecog.ps  <dbl> 1, 1, 2, 1, 1, 2, 2, 2, 1, 2, 2, 1, 2, 1, 1, 2, 2, 1,...
help(ovarian)

# Dichotomize age and change data labels
ovarian$rx <- factor(ovarian$rx, 
                     levels = c("1", "2"), 
                     labels = c("A", "B"))
ovarian$resid.ds <- factor(ovarian$resid.ds, 
                           levels = c("1", "2"), 
                           labels = c("no", "yes"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, 
                          levels = c("1", "2"), 
                          labels = c("good", "bad"))

# Data seems to be bimodal
hist(ovarian$age) 
ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)

# now you are prepared to create a survival object that is basically a compiled version of the futime and fustat columns that can be interpreted  by the survfit function
# a + behind survival times indicates censored data points
# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv_object 
##  [1]   59   115   156   421+  431   448+  464   475   477+  563   638 
## [12]  744+  769+  770+  803+  855+ 1040+ 1106+ 1129+ 1206+ 1227+  268 
## [23]  329   353   365   377+

# now fit a KM curve by passing the surv_object to the survfit function
# look at KM curve stratified by a variable (here rx)
fit1 <- survfit(surv_object ~ rx, data = ovarian)
summary(fit1)

# examine the corresponding survival curve by passing the survival object to the ggsurvplot
ggsurvplot(fit1, data = ovarian, pval = TRUE)

# Examine prdictive value of residual disease status
# look at KM curve stratified by resid.ds
fit2 <- survfit(surv_object ~ resid.ds, data = ovarian)
ggsurvplot(fit2, data = ovarian, pval = TRUE)

# fit a cox proportional hazards model
# a HR >1 indicates increased risk of death, HR < 1 indicates decreased risk
# output results in a forest plot
# outputs HRs for all covariates included in the formula for the model
fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, 
                   data = ovarian)
ggforest(fit.coxph, data = ovarian)



#### ---------- TUTORIAL 2: Survial Analysis in R by Emily C. Zabor ----------- ####

# link to the tutorial: https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html













