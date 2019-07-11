# ----------------------------------------- #
#       MESA Figure for Steve T             #
#              April 10, 2019               #
#                K. Sumner                  #
# ----------------------------------------- #

#### --------- load packages ----------------- ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(haven)
library(splines)
library(visreg)
library(broom)



#### --------- load in the data set ------------ ####

# read in clean mesa data set
mesa_data = read_csv("Desktop/mesa_figure_for_stevet/MESA cRDT sensitivity.csv")

# look at the data set
summary(mesa_data)
colnames(mesa_data)

# read in steve's full data set for mesa
stata_data = read_stata("Desktop/mesa_figure_for_stevet/MESA RACD.dta")

# look at that data set
summary(stata_data)
head(stata_data)


#### -------- old figure code ---------- ####

## step 1: compute by category (A), prob of detection (D/B) with upper and lower 95% CIs
# create a new variable (E) that is prob of detection (D/B)
mesa_data$prob_detection = mesa_data$crdtpos/mesa_data$npcrpos
# make the plot
plot_step1 = ggplot(data=mesa_data) +
  geom_point(aes(x=denscat88,y=prob_detection)) +
  geom_smooth(aes(x=denscat88,y=prob_detection)) +
  ylim(0,1)
plot_step1

## step 2: plot counts by category (A) of B and D, with D overlaying B
#  create a function for italicizing words
my_x_title <- expression(paste("Log of ", italic("P. falciparum"), " parasite density"))
my_y_title <- expression(paste("Number of ", italic("P. falciparum"), " positive samples"))
# make the plot
plot_step2 = ggplot(data=mesa_data) +
  geom_bar(aes(x=denscat88,y=npcrpos), stat = "identity", fill = "light blue", colour = "black") +
  geom_bar(aes(x=denscat88,y=crdtpos), stat = "identity", fill= "dark blue", colour = "black") +
  labs(x=my_x_title,y=my_y_title)
plot_step2


## step 3: plot by category (A) sensitivity and CIs from step 1. make continuous if possible.
# make the titles
my_x_title <- expression(paste("log10[2]",italic("P. falciparum"),"parasite density (p/uL)"))
my_y_title <- expression(paste("Number of parasite positive samples"))
# make the plot
plot_step3 = ggplot(data=mesa_data) +
  geom_bar(aes(x=denscat88,y=npcrpos), stat = "identity", fill = "light blue", colour = "black") +
  geom_bar(aes(x=denscat88,y=crdtpos), stat = "identity", fill= "dark blue", colour = "black") +
  geom_point(aes(x=denscat88,y=prob_detection*100),color = "dark grey") +
  geom_smooth(aes(x=denscat88,y=prob_detection*100),color = "pink",method = "loess") +
  theme_minimal() +
  scale_y_continuous(my_y_title, sec.axis=sec_axis(~ . * .01, name = "Probability parasite detection")) +
  labs(x=my_x_title,y=my_y_title)
plot_step3 
# make the plot with prettier colors 
plot_step3 = ggplot(data=mesa_data) +
  geom_bar(aes(x=denscat88,y=npcrpos), stat = "identity", fill = "#9ecae1", colour = "black") +
  geom_bar(aes(x=denscat88,y=crdtpos), stat = "identity", fill= "#045a8d", colour = "black") +
  geom_point(aes(x=denscat88,y=prob_detection*100),color = "#fec44f") +
  geom_smooth(aes(x=denscat88,y=prob_detection*100),color = "#cc4c02",method = "loess") +
  theme_minimal() +
  scale_y_continuous(my_y_title, sec.axis=sec_axis(~ . * .01, name = "Probability parasite detection"), limits = c(0,100)) +
  labs(x=my_x_title,y=my_y_title) 
plot_step3 
# export figure
#ggsave(plot_step3, filename="/Users/kelseysumner/Desktop/Meshnick Lab/mesa_figure_for_steve.png", device="png",
 #      height=4, width=5, units="in", dpi=500)

# make the plot with a logistic regression line instead of loess curve
plot_step3 = ggplot() +
  geom_bar(data=mesa_data,aes(x=denscat88,y=npcrpos), stat = "identity", fill = "#9ecae1", colour = "black") +
  geom_bar(data=mesa_data,aes(x=denscat88,y=crdtpos), stat = "identity", fill= "#045a8d", colour = "black") +
  geom_ribbon(data=stata_data,aes(x=logpfdens,ymin = pub*100, ymax = plb*100), fill = "grey70",alpha=0.5) +
  geom_line(data=stata_data,aes(x=logpfdens,y=phat*100),color = "#fec44f",lwd=2) +
  geom_line(data=stata_data,aes(x=logpfdens,y=plb*100),color = "black",lwd=1) +
  geom_line(data=stata_data,aes(x=logpfdens,y=pub*100),color = "black",lwd=1) +
  theme_minimal() +
  scale_y_continuous(my_y_title, sec.axis=sec_axis(~ . * .01, name = "Probability parasite detection"), limits = c(0,100)) +
  labs(x=my_x_title,y=my_y_title) 
plot_step3 
ggsave(plot_step3, filename="/Users/kelseysumner/Desktop/Meshnick Lab/mesa_figure_for_steve.png", device="png",
      height=4, width=5, units="in", dpi=500)

# make an additional plot using the full data set (Logistic regression model)
plot_step4 = ggplot(data=stata_data) +
  geom_point(aes(x=logpfdens,y=crdtresult),color = "#fec44f") +
  geom_smooth(aes(x=logpfdens,y=crdtresult),color = "#cc4c02",method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  theme_minimal() 
plot_step4 

# final plot
# make the plot with a logistic regression line instead of loess curve
plot_step3 = ggplot() +
  geom_bar(data=mesa_data,aes(x=denscat88,y=npcrpos), stat = "identity", fill = "#9ecae1", colour = "black") +
  geom_bar(data=mesa_data,aes(x=denscat88,y=crdtpos), stat = "identity", fill= "#045a8d", colour = "black") +
  geom_ribbon(data=stata_data,aes(x=logpfdens,ymin = pub*100, ymax = plb*100), fill = "#fec44f",alpha=0.8) +
  geom_line(data=stata_data,aes(x=logpfdens,y=phat*100),color = "#cc4c02",lwd=1.5) + 
  theme_minimal() +
  scale_y_continuous(my_y_title, sec.axis=sec_axis(~ . * .01, name = "Probability parasite detection"), limits = c(0,100)) +
  labs(x=my_x_title,y=my_y_title) 
plot_step3 
# ggsave(plot_step3, filename="/Users/kelseysumner/Desktop/Meshnick Lab/mesa_figure_for_steve.png", device="png",
       # height=4, width=5, units="in", dpi=500)


#### -------- new figures code ---------------- ####

# list the titles
my_x_title <- expression(paste("log10[2]",italic("P. falciparum"),"parasite density (p/uL)"))
my_y_title <- expression(paste("Number of parasite positive samples"))

## density plots
# make a density plots for the historgrams
plot_new_1 = ggplot() +
  geom_density(data=mesa_data,aes(x=denscat88,y=npcrpos), stat = "identity", fill = "#9ecae1", colour = "#9ecae1", alpha=0.7) +
  geom_density(data=mesa_data,aes(x=denscat88,y=crdtpos), stat = "identity", fill= "#045a8d", colour = "#045a8d", alpha=0.7) +
  theme_bw() +
  scale_x_discrete(limits = c(-2,-1,0,1,2,3,4,5,6,7)) +
  scale_y_discrete(limits = c(0,20,40,60,80,100,120)) +
  labs(x=my_x_title,y=my_y_title) 
plot_new_1
ggsave(plot_new_1, filename="/Users/kelseysumner/Desktop/density_plot.png", device="png",
  height=4, width=5, units="in", dpi=500)

# create new numeric variables for the number pcr and rdt positive at the individual level
# for the pcr data
stata_data$pcr_positive_numeric = rep(NA,nrow(stata_data))
stata_data$pcr_positive_numeric[which(stata_data$pf_pcr_infection_status == "positive")] = 1
stata_data$pcr_positive_numeric[which(stata_data$pf_pcr_infection_status == "negative")] = 0
stata_data$pcr_positive_numeric[which(is.na(stata_data$pf_pcr_infection_status))] = NA
table(stata_data$pcr_positive_numeric,stata_data$pf_pcr_infection_status, useNA = "always")
# for the rdt data
stata_data$rdt_positive_numeric = rep(NA,nrow(stata_data))
stata_data$rdt_positive_numeric[which(stata_data$rdt_positive == "yes")] = 1
stata_data$rdt_positive_numeric[which(stata_data$rdt_positive == "no")] = 0
stata_data$rdt_positive_numeric[which(is.na(stata_data$rdt_positive))] = NA
table(stata_data$rdt_positive_numeric,stata_data$rdt_positive, useNA = "always")

# try this with the raw data to see if smoother
plot_new_1 = ggplot() +
  geom_density(data=stata_data,aes(x=logpfdens,y=npcrpos), fill = "#9ecae1", colour = "#9ecae1", alpha=0.7) +
  geom_density(data=stata_data,aes(x=logpfdens,y=nrdtpos), fill= "#045a8d", colour = "#045a8d", alpha=0.7) +
  theme_minimal() +
  labs(x=my_x_title,y=my_y_title) 
plot_new_1
# not working well


## logistic regression plot
# first restrict the data set to just those with anpop=1 and includehh=1
restricted_stata_data = stata_data[which(stata_data$anpop==1 & stata_data$includehh==1),]
# then make the plot with the logistic regression curve
plot_new_2 = ggplot(data=restricted_stata_data) +
  geom_point(aes(x=logpfdens,y=crdtresult),color = "black",alpha=0.2) +
  geom_smooth(aes(x=logpfdens,y=crdtresult),color = "#045a8d",method = "glm", method.args = list(family = "binomial"), se = TRUE,lwd=1.1) +
  geom_ribbon(data=restricted_stata_data,aes(x=logpfdens,ymin = pub, ymax = plb), fill = "#045a8d",alpha=0.2) +
  theme_bw() +
  scale_y_continuous(breaks = c(0.00,0.20,0.40,0.60,0.80,1.00)) +
  scale_x_discrete(limits = c(-2,-1,0,1,2,3,4,5,6,7)) +
  labs(x=my_x_title,y="Probability of parasite detection")
plot_new_2 
ggsave(plot_new_2, filename="/Users/kelseysumner/Desktop/logistic_regression_plot.png", device="png",
       height=4, width=5, units="in", dpi=500)
# create a logistic regression model of this curve
log_model = glm(crdtresult~logpfdens,family=binomial("logit"),data=restricted_stata_data)
summary(log_model)
# model: logit(y) = -1.94259 + 1.17503(x)
# solve for x: (log10(0.95/(1-0.95))+1.94259)/1.17503 = 4.159067
10^(4.159067)
confint(log_model)
# export the model results
log_model_df = augment(log_model)
tidy(log_model)
# look for where upper CI is just barely still 0.95
lower95 = stata_data[which(stata_data$pub<0.95),]
max(lower95$pub)
newval = stata_data[which(stata_data$labid_new == "479"),]
newval$pub
newval$plb
newval$phat
newval$logpfdens
10^(3.867902)


## create a plot with age as the explanatory variable and number needed to screen as outcome variable

# first create data sets with restrictions: anpop==1,includehh==1,noninddex==1
r_stata_data = stata_data[which(stata_data$anpop==1 & stata_data$includehh==1 & stata_data$nonindex==1),]

# then subset the r_stata_data set into the case and control households
case_r_stata_data = r_stata_data[which(r_stata_data$person_type=="case household member"),]
control_r_stata_data = r_stata_data[which(r_stata_data$person_type=="control household member"),]
table(r_stata_data$person_type)

# make sure age is numeric
case_r_stata_data$mem_age = as.numeric(case_r_stata_data$mem_age)
control_r_stata_data$mem_age = as.numeric(control_r_stata_data$mem_age)

## look at functional form of age and probability of having a positive rdt result
# for the case households
plot_age = ggplot(data=case_r_stata_data) +
  geom_point(aes(x=mem_age,y=crdtresult)) +
  geom_smooth(aes(x=mem_age,y=crdtresult),method="loess")
plot_age
# for the control households
plot_age = ggplot(data=control_r_stata_data) +
  geom_point(aes(x=mem_age,y=crdtresult)) +
  geom_smooth(aes(x=mem_age,y=crdtresult),method="loess")
plot_age


## look at linear model fit for age
# for case households
model1=glm(crdtresult~mem_age,family=binomial("logit"),data=case_r_stata_data) 
summary(model1) # AIC: 1412.6
# pull out log likelihood test value
logLik(model1) # log-likelihood:-704.2954
# plot the linear model fit
case_plot1 = ggplot(case_r_stata_data, aes(x=mem_age, y=crdtresult)) + 
  labs(title="LINEAR Model",
       x="Household member's age (years)",
       y="Prevalence of malaria+ RDT results",
       caption="NOTE: LOESS is blue. Linear is red. 95%CI is for the linear trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~x, color="red", linetype="dashed") +
  theme_bw()
ggsave(case_plot1, filename="/Users/kelseysumner/Desktop/case_plot1.png", device="png",
       height=4, width=5, units="in", dpi=500)
# for control households
model1=glm(crdtresult~mem_age,family=binomial("logit"),data=control_r_stata_data) 
summary(model1) # AIC: 911.87
# pull out log likelihood test value
logLik(model1) # log-likelihood:-453.9362
# plot the linear model fit
control_plot1 = ggplot(control_r_stata_data, aes(x=mem_age, y=crdtresult)) + 
  labs(title="LINEAR Model",
       x="Household member's age (years)",
       y="Prevalence of malaria+ RDT results",
       caption="NOTE: LOESS is blue. Linear is red. 95%CI is for the linear trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~x, color="red", linetype="dashed") +
  theme_bw()
ggsave(control_plot1, filename="/Users/kelseysumner/Desktop/control_plot1.png", device="png",
       height=4, width=5, units="in", dpi=500)


## look at quadratic model fit
# create a quadratic variable
case_r_stata_data$mem_age_quad = case_r_stata_data$mem_age*case_r_stata_data$mem_age
control_r_stata_data$mem_age_quad = control_r_stata_data$mem_age*control_r_stata_data$mem_age
# for case households
model2=glm(crdtresult~mem_age+mem_age_quad,family=binomial("logit"),data=case_r_stata_data) 
summary(model2) # AIC: 1414.14
# pull out log likelihood test value
logLik(model2) # log-likelihood:-704.2007
case_r_stata_data$pred.quad = model2$fitted.values
# plot the quadratic model fit
case_plot2 = ggplot(case_r_stata_data, aes(x=mem_age, y=crdtresult)) + 
  labs(title="Quadratic Model",
       x="Household member's age (years)",
       y="Prevalence of malaria+ RDT results",
       caption="NOTE: LOESS is blue. Quadratic is red [y~ (x+x^2)]. 95%CI is for the quadratic trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~poly(x,2), color="red", linetype="dashed") +
  theme_bw()
ggsave(case_plot2, filename="/Users/kelseysumner/Desktop/case_plot2.png", device="png",
       height=4, width=5, units="in", dpi=500)
# for control households
model2=glm(crdtresult~mem_age+mem_age_quad,family=binomial("logit"),data=control_r_stata_data) 
summary(model2) # AIC: 910.81
# pull out log likelihood test value
logLik(model2) # log-likelihood:-452.4039
control_r_stata_data$pred.quad = model2$fitted.values
# plot the quadratic model fit
control_plot2 = ggplot(control_r_stata_data, aes(x=mem_age, y=crdtresult)) + 
  labs(title="Quadratic Model",
       x="Household member's age (years)",
       y="Prevalence of malaria+ RDT results",
       caption="NOTE: LOESS is blue. Quadratic is red [y~ (x+x^2)]. 95%CI is for the quadratic trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~poly(x,2), color="red", linetype="dashed") +
  theme_bw()
ggsave(control_plot2, filename="/Users/kelseysumner/Desktop/control_plot2.png", device="png",
       height=4, width=5, units="in", dpi=500)


## look at cubic model fit
# create a quadratic variable
case_r_stata_data$mem_age_cub = case_r_stata_data$mem_age*case_r_stata_data$mem_age*case_r_stata_data$mem_age
control_r_stata_data$mem_age_cub = control_r_stata_data$mem_age*control_r_stata_data$mem_age*control_r_stata_data$mem_age
# for case households
model3=glm(crdtresult~mem_age+mem_age_quad+mem_age_cub,family=binomial("logit"),data=case_r_stata_data) 
summary(model3) # AIC: 1389.4
# pull out log likelihood test value
logLik(model3) # log-likelihood:-690.6903
case_r_stata_data$pred.cub = model3$fitted.values
# plot the model fit
case_plot3 = ggplot(case_r_stata_data, aes(x=mem_age, y=crdtresult)) + 
  labs(title="Cubic Model",
       x="Household member's age (years)",
       y="Prevalence of malaria+ RDT results",
       caption="NOTE: LOESS is blue. Cubic is red [y~ (x+x^2+x^3)]. 95%CI is for the cubic trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~poly(x,3), color="red", linetype="dashed") +
  theme_bw()
ggsave(case_plot3, filename="/Users/kelseysumner/Desktop/case_plot3.png", device="png",
       height=4, width=5, units="in", dpi=500)
# for control households
model3=glm(crdtresult~mem_age+mem_age_quad+mem_age_cub,family=binomial("logit"),data=control_r_stata_data) 
summary(model3) # AIC: 904.28
# pull out log likelihood test value
logLik(model3) # log-likelihood:-448.1423
control_r_stata_data$pred.cub = model3$fitted.values
# plot the model fit
control_plot3 = ggplot(control_r_stata_data, aes(x=mem_age, y=crdtresult)) + 
  labs(title="Cubic Model",
       x="Household member's age (years)",
       y="Prevalence of malaria+ RDT results",
       caption="NOTE: LOESS is blue. Cubic is red [y~ (x+x^2+x^3)]. 95%CI is for the cubic trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~poly(x,3), color="red", linetype="dashed") +
  theme_bw()
ggsave(control_plot3, filename="/Users/kelseysumner/Desktop/control_plot3.png", device="png",
       height=4, width=5, units="in", dpi=500)

## look at categorical fit
# look at agecat
table(stata_data$agecat,stata_data$mem_age)
# make agecat a factor
case_r_stata_data$agecat_f = factor(case_r_stata_data$agecat,levels=c(0,1,2),labels=c("cat1","cat2","cat3"))
control_r_stata_data$agecat_f = factor(control_r_stata_data$agecat,levels=c(0,1,2),labels=c("cat1","cat2","cat3"))
# for case households
model4=glm(crdtresult~agecat_f,family=binomial("logit"),data=case_r_stata_data) 
summary(model4) # AIC: 1362
# pull out log likelihood test value
logLik(model4) # log-likelihood:-678.0209
case_r_stata_data$pred.cat = model4$fitted.values
# plot the model fit
case_plot4 = ggplot(case_r_stata_data, aes(x=mem_age, y=pred.cat)) + 
  labs(title="Categorical Model",
       x="Household member's age (years)",
       y="Prevalence of malaria+ RDT results",
       caption="NOTE: LOESS is blue. Categorical is red. 95%CI is for the categorical trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~x, color="red", linetype="dashed") +
  theme_bw()
ggsave(case_plot4, filename="/Users/kelseysumner/Desktop/case_plot4.png", device="png",
       height=4, width=5, units="in", dpi=500)
# for control households
model4=glm(crdtresult~agecat_f,family=binomial("logit"),data=control_r_stata_data) 
summary(model4) # AIC: 912.9
# pull out log likelihood test value
logLik(model4) # log-likelihood:-453.4517
control_r_stata_data$pred.cat = model4$fitted.values
# plot the model fit
control_plot4 = ggplot(control_r_stata_data, aes(x=mem_age, y=pred.cat)) + 
  labs(title="Categorical Model",
       x="Household member's age (years)",
       y="Prevalence of malaria+ RDT results",
       caption="NOTE: LOESS is blue. Categorical is red. 95%CI is for the categorical trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~x, color="red", linetype="dashed") +
  theme_bw()
ggsave(control_plot4, filename="/Users/kelseysumner/Desktop/control_plot4.png", device="png",
       height=4, width=5, units="in", dpi=500)


## look at fit with splines
library(rms)
# for case households
model5 = glm(crdtresult~bs(mem_age, degree=3),family=binomial("logit"),data=case_r_stata_data) 
summary(model5) # AIC: 1389.4
logLik(model5) # -690.6903
# plot the model fit
case_plot5 = ggplot(case_r_stata_data, aes(x=mem_age, y=pred.cat)) + 
  labs(title="Restricted Cubic Spline Model",
       x="Household member's age (years)",
       y="Prevalence of malaria+ RDT results",
       caption="NOTE: LOESS is blue. Restricted cubic spline is red. 95%CI is for the restricted cubic spline trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~ bs(x, degree=3), color="red", linetype="dashed") +
  theme_bw()
ggsave(case_plot5, filename="/Users/kelseysumner/Desktop/case_plot5.png", device="png",
       height=4, width=5, units="in", dpi=500)
# for control households
model5 = glm(crdtresult~bs(mem_age, degree=3),family=binomial("logit"),data=control_r_stata_data) 
summary(model5) # 904.28
logLik(model5) # -448.1423
# plot the model fit
control_plot5 = ggplot(control_r_stata_data, aes(x=mem_age, y=pred.cat)) + 
  labs(title="Restricted Cubic Spline Model",
       x="Household member's age (years)",
       y="Prevalence of malaria+ RDT results",
       caption="NOTE: LOESS is blue. Restricted cubic spline is red. 95%CI is for the restricted cubic spline trend alone.") +
  geom_smooth(method="loess", se = F) +                   
  geom_smooth(method="glm", formula = y ~ bs(x, degree=3), color="red", linetype="dashed") +
  theme_bw()
ggsave(control_plot5, filename="/Users/kelseysumner/Desktop/control_plot5.png", device="png",
       height=4, width=5, units="in", dpi=500)


##  log likelihood ratio tests comparing nested models
library(epiDisplay)
# for case households
# compare quadratic and linear models
lrtest(model1,model2) # linear better than quadratic
# compare cubic and linear models
lrtest(model1,model3) # cubic better than linear
# compare quadratic and cubic models
lrtest(model2,model3) # cubic better than quadratic
# compare cubic restricted spline and linear models 
lrtest(model1,model5) # spline better than linear
# between cubic model and cubic restricted spline
# for control households
# compare quadratic and linear models
lrtest(model1,model2) # linear better than quadratic
# compare cubic and linear models
lrtest(model1,model3) # cubic better than linear
# compare quadratic and cubic models
lrtest(model2,model3) # cubic better than quadratic
# compare cubic restricted spline and linear models 
lrtest(model1,model5) # spline better than linear
# between cubic model and cubic restricted spline
# chose cubic models because slightly more interpretable than cubic splines for both the case and control households


## make a graph of cubic age and RDT+ prevalence for both case and control households
# plot the model fit
case_control_age_plot = ggplot() + 
  labs(x="Household member's age (years)",
       y="Prevalence of malaria positive RDT results",
       caption="NOTE: Case households are light blue. Control households are dark blue.") +
  geom_smooth(data=case_r_stata_data, aes(x=mem_age, y=crdtresult),method="glm", formula = y ~poly(x,3), method.args = list(family = "binomial"), color="#9ecae1") +
  geom_smooth(data=control_r_stata_data, aes(x=mem_age, y=crdtresult),method="glm", formula = y ~poly(x,3), method.args = list(family = "binomial"), color="#045a8d") +
  scale_x_discrete(limits=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)) +
  scale_y_continuous(breaks=c(0,0.20,0.40,0.60,0.80,1.0),limits=c(0,1)) +
  theme_bw()
case_control_age_plot
ggsave(case_control_age_plot, filename="/Users/kelseysumner/Desktop/case_control_age_plot.png", device="png",
       height=4, width=5, units="in", dpi=500)


## make a plot that has outcome as 1/crdtresult (which is like the Number Needed to Screen or 1/prevalence of RDT results)
case_control_age_plot_NNS = ggplot() + 
  labs(x="Household member's age (years)",
       y="Number needed to screen",
       caption="NOTE: Case households are light blue. Control households are dark blue.") +
  geom_smooth(data=case_r_stata_data, aes(x=mem_age, y=1/pred.cub),method="glm", formula = y ~poly(x,3), color="#9ecae1") +
  geom_smooth(data=control_r_stata_data, aes(x=mem_age, y=1/pred.cub),method="glm", formula = y ~poly(x,3), color="#045a8d") +
  scale_x_discrete(limits=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)) +
  scale_y_continuous(limits=c(0,80)) +
  theme_bw()
case_control_age_plot_NNS
ggsave(case_control_age_plot_NNS, filename="/Users/kelseysumner/Desktop/case_control_age_plot_NNS.png", device="png",
       height=4, width=5, units="in", dpi=500)




#### -------- additional age figure code ---------------- ####


# list the titles
my_x_title <- expression(paste("Log"[10],italic(" P. falciparum")," parasite density (p/uL)"))
my_y_title_1 <- expression(paste("Number of", italic(" P. falciparum"), " parasite positive samples"))
my_y_title_2 <- expression(paste("Probability of cRDT positivity"))

# first restrict the data set to just those with anpop=1 and includehh=1
restricted_stata_data = stata_data[which(stata_data$anpop==1 & stata_data$includehh==1),]

test = restricted_stata_data[which(!(is.na(restricted_stata_data$denscat88))),]
## density plots
# make a density plots for the historgrams
plot_new_1 = ggplot() +
  geom_density(data=test,aes(x=denscat88,y=npcrpos), stat = "identity", fill = "#9ecae1", colour = "#9ecae1", alpha=0.7) +
  geom_density(data=test,aes(x=denscat88,y=nrdtpos), stat = "identity", fill= "#045a8d", colour = "#045a8d", alpha=0.7) +
  theme_bw() +
  scale_x_discrete(limits = c(-2,-1,0,1,2,3,4,5,6,7)) +
  scale_y_discrete(limits = c(0,20,40,60,80,100,120)) +
  labs(x=my_x_title,y=my_y_title_1) 
plot_new_1
ggsave(plot_new_1, filename="/Users/kelseysumner/Desktop/density_plot_all_participants.png", device="png",
       height=6, width=7, units="in", dpi=500)



## logistic regression plot stratified by age
# look at agecat
table(restricted_stata_data$agecat, useNA = "always")
# make a factor
restricted_stata_data$agecat=factor(restricted_stata_data$agecat,levels=c(0,1,2),labels=c("Less than 5 years","Inbetween 5 to 15 years","Greater than 15 years"))
# bind data frames
tempdataset = restricted_stata_data %>%
  mutate(agecat = as.factor("All ages"))
tempdataset2 = rbind(restricted_stata_data, tempdataset)
plot_new_2_age = tempdataset2 %>%
  ggplot(aes(x = logpfdens, y = crdtresult)) +
  geom_point(color = "black", alpha = 0.2) +
  geom_smooth(aes(color = agecat, linetype = agecat), method = "glm", method.args = list(family = "binomial"), se = TRUE,lwd=1) +
  theme_bw() +
  scale_y_continuous(breaks = c(0.00,0.20,0.40,0.60,0.80,1.00)) +
  scale_x_discrete(limits = c(-2,-1,0,1,2,3,4,5,6,7)) +
  labs(x=my_x_title,y=my_y_title_2,colour="Age categories",linetype="Age categories") +
  scale_color_manual(values=c("Less than 5 years"="#6baed6","Inbetween 5 to 15 years"="#2171b5","Greater than 15 years"="#08306b","All ages"="black")) +
  scale_linetype_manual(values=c("Less than 5 years"="twodash","Inbetween 5 to 15 years"="dotted","Greater than 15 years"="longdash","All ages"="solid")) +
  theme(legend.position = c(0.95, 0.10),legend.justification = c("right", "bottom"),legend.background = element_rect(fill = "light grey", colour = "black"),legend.key.size = grid::unit(2.5,"lines"),legend.text=element_text(size=8),legend.title = element_text(size = 10))
plot_new_2_age 
ggsave(plot_new_2_age, filename="/Users/kelseysumner/Desktop/logistic_regression_plot_stratified_by_age.png", device="png",
       height=6, width=7, units="in", dpi=500)
# old plot code
plot_new_2_age = ggplot() +
  geom_point(data=restricted_stata_data,aes(x=logpfdens,y=crdtresult),color = "black",alpha=0.2) +
  geom_smooth(data=restricted_stata_data,aes(x=logpfdens,y=crdtresult,color = "All ages",linetype="All ages"),method = "glm", method.args = list(family = "binomial"), se = TRUE,lwd=2) +
  geom_smooth(data=restricted_stata_data,aes(x=logpfdens,y=crdtresult,color=agecat,linetype=agecat),method = "glm", method.args = list(family = "binomial"), se = TRUE,lwd=1.5) +
  theme_bw() +
  scale_y_continuous(breaks = c(0.00,0.20,0.40,0.60,0.80,1.00)) +
  scale_x_discrete(limits = c(-2,-1,0,1,2,3,4,5,6,7)) +
  labs(x=my_x_title,y=my_y_title_2,colour="Age categories",linetype="Age categories") +
  scale_color_manual(values=c("Less than 5 years"="#6baed6","Inbetween 5 to 15 years"="#2171b5","Greater than 15 years"="#08306b","All ages"="black")) +
  scale_linetype_manual(values=c("Less than 5 years"="twodash","Inbetween 5 to 15 years"="dotted","Greater than 15 years"="longdash","All ages"="solid")) +
  theme(legend.position = c(0.95, 0.10),legend.justification = c("right", "bottom"),legend.background = element_rect(fill = "grey", colour = "black"),legend.key.size = grid::unit(2.5,"lines"),legend.text=element_text(size=8),legend.title = element_text(size = 10))
plot_new_2_age 

# now make a plot restricted to just the asymptomatic household members
table(restricted_stata_data$persontype,useNA = "always")
restricted_stata_data_2 = restricted_stata_data[which(restricted_stata_data$persontype==3 | restricted_stata_data$persontype==5),]
table(restricted_stata_data_2$agecat, useNA = "always")

# bind columns together
tempdataset_asymp = restricted_stata_data_2 %>%
  mutate(agecat = as.factor("All ages"))
tempdataset2_asymp = rbind(restricted_stata_data_2, tempdataset_asymp)
plot_new_2_age_asymp = tempdataset2_asymp %>%
  ggplot(aes(x = logpfdens, y = crdtresult)) +
  geom_point(color = "black", alpha = 0.2) +
  geom_smooth(aes(color = agecat, linetype = agecat), method = "glm", method.args = list(family = "binomial"), se = TRUE,lwd=1) +
  theme_bw() +
  scale_y_continuous(breaks = c(0.00,0.20,0.40,0.60,0.80,1.00)) +
  scale_x_discrete(limits = c(-2,-1,0,1,2,3,4,5,6,7)) +
  labs(x=my_x_title,y=my_y_title_2,colour="Age categories",linetype="Age categories") +
  scale_color_manual(values=c("Less than 5 years"="#66c2a4","Inbetween 5 to 15 years"="#238b45","Greater than 15 years"="#00441b","All ages"="black")) +
  scale_linetype_manual(values=c("Less than 5 years"="twodash","Inbetween 5 to 15 years"="dotted","Greater than 15 years"="longdash","All ages"="solid")) +
  theme(legend.position = c(0.98, 0.10),legend.justification = c("right", "bottom"),legend.background = element_rect(fill = "light grey", colour = "black"),legend.key.size = grid::unit(2.5,"lines"),legend.text=element_text(size=8),legend.title = element_text(size = 10))
ggsave(plot_new_2_age_asymp, filename="/Users/kelseysumner/Desktop/logistic_regression_plot_stratified_by_age_asymptomatic_hh_members.png", device="png",
       height=6, width=7, units="in", dpi=500)
# old plot code
plot_new_2_age_asymp = ggplot() +
  geom_point(data=restricted_stata_data_2,aes(x=logpfdens,y=crdtresult),color = "black",alpha=0.2) +
  geom_smooth(data=restricted_stata_data_2,aes(x=logpfdens,y=crdtresult,color = "All ages",linetype="All ages"),method = "glm", method.args = list(family = "binomial"), se = TRUE,lwd=2) +
  geom_smooth(data=restricted_stata_data_2,aes(x=logpfdens,y=crdtresult,color=agecat,linetype=agecat),method = "glm", method.args = list(family = "binomial"), se = TRUE,lwd=1.5) +
  theme_bw() +
  scale_y_continuous(breaks = c(0.00,0.20,0.40,0.60,0.80,1.00)) +
  scale_x_discrete(limits = c(-2,-1,0,1,2,3,4,5,6,7)) +
  labs(x=my_x_title,y=my_y_title_2,colour="Age categories",linetype="Age categories") +
  scale_color_manual(values=c("Less than 5 years"="#66c2a4","Inbetween 5 to 15 years"="#238b45","Greater than 15 years"="#00441b","All ages"="black")) +
  scale_linetype_manual(values=c("Less than 5 years"="twodash","Inbetween 5 to 15 years"="dotted","Greater than 15 years"="longdash","All ages"="solid")) +
  theme(legend.position = c(0.95, 0.10),legend.justification = c("right", "bottom"),legend.background = element_rect(fill = "grey", colour = "black"),legend.key.size = grid::unit(2,"lines"),legend.text=element_text(size=8),legend.title = element_text(size = 10))
plot_new_2_age_asymp 

test_asymp = restricted_stata_data_2[which(!(is.na(restricted_stata_data_2$denscat88))),]
# make a density plot of just the asymptomatic people
plot_new_4 = ggplot() +
  geom_density(data=restricted_stata_data_2,aes(x=denscat88,y=npcrpos), stat = "identity", fill = "#66c2a4", colour = "#66c2a4", alpha=0.7) +
  geom_density(data=restricted_stata_data_2,aes(x=denscat88,y=nrdtpos), stat = "identity", fill= "#00441b", colour = "#00441b", alpha=0.7) +
  theme_bw() +
  scale_x_discrete(limits = c(-2,-1,0,1,2,3,4,5,6,7)) +
  scale_y_discrete(limits = c(0,20,40,60,80,100,120)) +
  labs(x=my_x_title,y=my_y_title_1) 
plot_new_4
ggsave(plot_new_4, filename="/Users/kelseysumner/Desktop/density_plot_asymptomatic_participants.png", device="png",
       height=6, width=7, units="in", dpi=500)
# create a logistic regression model of this curve
log_model = glm(crdtresult~logpfdens,family=binomial("logit"),data=restricted_stata_data_2)
summary(log_model)
# model: logit(y) = -1.97810 + 0.96521(x) 
# solve for x: (log10(0.95/(1-0.95))+1.97810)/0.96521 = 3.374244
10^(3.374244) # 2367.249
confint(log_model)
# export the model results
log_model_df = augment(log_model)
tidy(log_model)
only_model_samples = restricted_stata_data_2[which(!(is.na(restricted_stata_data_2$logpfdens)) & !(is.na(restricted_stata_data_2$crdtresult))),]
only_model_samples$fitted_values = log_model$fitted.values
only_model_samples_new = data.frame(logpfdens = only_model_samples$logpfdens)
preds <- predict(log_model, newdata = only_model_samples_new, type = "link", se.fit = TRUE)
critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit
fit2 <- log_model$family$linkinv(fit)
upr2 <- log_model$family$linkinv(upr)
lwr2 <- log_model$family$linkinv(lwr)
only_model_samples_new$lwr <- lwr2 
only_model_samples_new$upr <- upr2 
# test the output by plotting
ggplot(data=restricted_stata_data_2, mapping=aes(x=logpfdens,y=crdtresult)) + geom_point() +         
  stat_smooth(method="glm", method.args=list(family=binomial)) + 
  geom_line(data=only_model_samples_new, mapping=aes(x=logpfdens, y=upr), col="red") + 
  geom_line(data=only_model_samples_new, mapping=aes(x=logpfdens, y=lwr), col="red")
# looks good
# now add fitted values to the data set and pull out where upper 95% CI bound is below 0.95
only_model_samples_new$fittedvals = only_model_samples$fitted_values
# look for where upper CI is just barely still 0.95
lower95 = only_model_samples_new[which(only_model_samples_new$upr<0.95),]
max(lower95$upr)
newval = only_model_samples_new[which(only_model_samples_new$upr<0.95 & only_model_samples_new$upr>0.945269),]
10^(4.56573)



# test run
restricted_stata_data_2$pfr364q_std_combined = as.numeric(restricted_stata_data_2$pfr364q_std_combined)
small_data_asymp = restricted_stata_data_2[which(!(is.na(restricted_stata_data_2$pfr364q_std_combined))),]
plot_new_4 = ggplot() +
  geom_density(data=small_data_asymp,aes(x=pfr364q_std_combined,y=pcrresult), stat = "identity", fill = "#66c2a4", colour = "#66c2a4", alpha=0.7) +
  geom_density(data=small_data_asymp,aes(x=pfr364q_std_combined,y=crdtresult), stat = "identity", fill= "#00441b", colour = "#00441b", alpha=0.7) +
  theme_bw() +
  scale_x_discrete(limits = c(-2,-1,0,1,2,3,4,5,6,7)) +
  scale_y_discrete(limits = c(0,20,40,60,80,100,120)) +
  labs(x=my_x_title,y=my_y_title_1) 
plot_new_4

# log10 p. falciparum parasite density (p/uL)
# probability of cRDT positivity
# also make a density plot of just the asymptomatic population
# change the x axis for all the plots
# make plots have dotted lines (first 4)

# check the values that were not asymptomatic
lost_obs = setdiff(test$labid_new,test_asymp$labid_new)
lost_obs_df = test[which(test$labid_new %in% lost_obs),]
summary(as.numeric(lost_obs_df$pfr364q_std_combined))
table(lost_obs_df$crdtresult, useNA = "always")
table(lost_obs_df$pcrresult, useNA = "always")
