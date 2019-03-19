# ----------------------------------------- #
#  Spat21 Power Calculations for Proposal   #
#            February 18, 2019              #
#                K. Sumner                  #
# ----------------------------------------- #

#### ------- load packages -------- ####
library(survival)
library(longpower)
library(ggplot2)
library(lme4)
library(pwr)
library(powerMediation)
library(powerSurvEpi)
library(ICCbin)


#### ------- estimate the intracluster correlation coefficient ------ ####

# read in the data set (preliminary)
prelim_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/human_merged_all_data_final_4MAR2019.rds")

# aim 1A
# create a new column for if the person had a symptomatic infection or not
symp_infection_only = ifelse(is.na(prelim_data$case_definition_1),NA,ifelse(prelim_data$case_definition_1=="symptomatic infection",1,0))
prelim_data$symp_infection_only = symp_infection_only
table(prelim_data$symp_infection_only,prelim_data$case_definition_1)
# calculate the ICC for case_definition_1 for HH_ID cluster-level
iccbin(cid=prelim_data$HH_ID,y=prelim_data$symp_infection_only,data=prelim_data)
# ICC by Monte Carlo Simulation: 0.00870821592075099
# calculate the ICC for case_definition_1 for participant cluster-level
iccbin(cid=prelim_data$memID,y=prelim_data$symp_infection_only,data=prelim_data)
# ICC by Monte Carlo Simulation: 0.003490622

# aim 1B
# create a new column for if the person had a symptomatic infection or not
symp_or_asymp_only = ifelse(prelim_data$case_definition_1=="asymptomatic infection",1,ifelse(prelim_data$case_definition_1=="symptomatic infection",0,NA))
prelim_data$symp_or_asymp_only = symp_or_asymp_only
table(prelim_data$symp_or_asymp_only,prelim_data$case_definition_1)
# calculate the ICC for case_definition_1 for HH_ID cluster-level
iccbin(cid=prelim_data$HH_ID,y=prelim_data$symp_or_asymp_only,data=prelim_data)
# ICC by Monte Carlo Simulation: 0.0185082195983387
# calculate the ICC for case_definition_1 for participant cluster-level
iccbin(cid=prelim_data$memID,y=prelim_data$symp_or_asymp_only,data=prelim_data)
# ICC by Monte Carlo Simulation: 0.00385528065651229


#### -------- aim 1A --------- ####

# model: cox proportional hazards

# simulator to approx power for two sample problem in survival library(survival)
set.seed(1)
n <- 2920 # total sample size
n0 <- n1 <- n/2 # balanced allocation
lambda0 <- -log(.5) # hazard in control arm
tau <- 1 # time of type I censoring 
nsims <- 10000 # total simulations
onesim <- function(){
  # randomly assign exponential failure times 
  t0 <- rexp(n0,lambda0)
  t1 <- rexp(n1,lambda0*hr)
  # censor if beyond tau
  x0 <- pmin(t0,tau); delta0 <- 1*(t0<tau) 
  x1 <- pmin(t1,tau); delta1 <- 1*(t1<tau)
  # combine data from two groups
  x <- c(x0,x1); delta <- c(delta0,delta1); group <- c(rep(0,n0),rep(1,n1))
  # return 1 if logrank test rejects, 0 o/w
  1*(survdiff(Surv(x,delta)~group)$chisq > qchisq(.95,1)) 
  }
getpower <- function(){
  power <- matrix(NA,1, nsims)
  for (ii in 1:nsims) power[ii] <- onesim()
  power <- sum(power)/nsims
  print(paste("Empirical power",power,"for hr",hr,"based on nsims=",nsims))}
hr <- 0.5; getpower()
hr <- 0.75; getpower()
hr <- 1; getpower() # check type I error hr <- 1.25; getpower()
hr <- 1.25; getpower()
hr <- 1.5; getpower()
hr <- 1.75; getpower()
hr <- 2; getpower()
hazardratios = c(1,1.25,1.5,1.75,2)
poweramounts = c(0.0503,0.2447,0.6643,0.9194,0.9905)
aim1a_df = data.frame(hazardratios,poweramounts)
hr <- 1.60; getpower()
# make the power curve
plotaim1a = ggplot(aim1a_df, aes(x=hazardratios,y=poweramounts)) +
 geom_line(size=2, color = "orange") + geom_hline(yintercept = 0.8, linetype = "dashed") +
  labs(x = "Effect Size (Hazard Ratio)", y = "Power")
plotaim1a


# another way to calculate cox proportional hazards power without simulations
# theta is the HR
# p is proportion who will be exposed
# psi is proportion with outcome
# rho2 is correlation between two covariates of interest
# alpha is type I error rate
# we are expecting a protective effect
# HR of 1
powerEpi.default(n=2920, theta=1, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.025
# HR of 0.9
powerEpi.default(n=2920, theta=.9, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.401
# HR of 0.8
powerEpi.default(n=2920, theta=.8, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.951
# HR of 0.75
powerEpi.default(n=2920, theta=.75, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.997
# HR of 0.7
powerEpi.default(n=2920, theta=.7, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.999
# HR of 0.6
powerEpi.default(n=2920, theta=.6, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 1
# HR of 0.5
powerEpi.default(n=2920, theta=.5, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 1
# HR of 0.4
powerEpi.default(n=2920, theta=.4, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 1
# HR of 0.3
powerEpi.default(n=2920, theta=.3, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 1
# HR of 0.25
powerEpi.default(n=2920, theta=.25, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 1
# HR of 0.2
powerEpi.default(n=2920, theta=.2, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 1
# HR of 0.1
powerEpi.default(n=2920, theta=.1, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 1
# HR of 0
powerEpi.default(n=2920, theta=0, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 1
# make the data frame
hazardratios = c(1,0.9,0.8,0.75,0.7,0.6,0.5,0.4,0.3,0.25,0.2,0.1,0)
poweramounts = c(0.025,0.401,0.951,0.997,0.999,1,1,1,1,1,1,1,1)
aim1a_df = data.frame(hazardratios,poweramounts)
# make the power curve
plotaim1a = ggplot(aim1a_df, aes(x=hazardratios,y=poweramounts)) +
  geom_line(size=2, color = "orange") + geom_hline(yintercept = 0.8, linetype = "dashed") +
  labs(x = "Effect Size (Hazard Ratio)", y = "Power")
plotaim1a






#### -------- aim 1B --------- ####

# model: logistic regression with binary outcome (longitudinal with binary outcome)
# the book recommends using an equation similar to GEE For sample size calculations
# there was an spass package in r that did this but has been taken off cran

# another way that runs a regular logistic regression model with binary predictor
## sample size for simple logistic regression
n <- 2920           # total sample size
# p1           # the event rate in unexposed (allow to vary)
# p2         # the event rate in the exposed (allow to vary)
B <- 0.5           # proportion that are exposed, assuming 50% of population becomes asymptomatic throughout study
# calculating an OR: 1
powerLogisticBin(n,p1=0.5,p2=0.5,B,alpha = 0.05) # power = 0.025
# calculating an OR of 1.25
powerLogisticBin(n,p1=0.5,p2=0.55,B,alpha = 0.05) # power = 0.772
# calculating an OR: 1.5 
powerLogisticBin(n,p1=0.5,p2=0.6,B,alpha = 0.05) # power = 0.999
# calculating an OR: 1.75
powerLogisticBin(n,p1=0.5,p2=0.64,B,alpha = 0.05) # power = 1
# calculating an OR: 2
powerLogisticBin(n,p1=0.5,p2=0.67,B,alpha = 0.05) # power = 1
# calculating an OR: 2.25
powerLogisticBin(n,p1=0.5,p2=0.7,B,alpha = 0.05) # power = 1
# calculating an OR: 2.5
powerLogisticBin(n,p1=0.5,p2=0.72,B,alpha = 0.05) # power = 1
# calculating an OR: 2.75
powerLogisticBin(n,p1=0.5,p2=0.735,B,alpha = 0.05) # power = 1
# calculating an OR: 3
powerLogisticBin(n,p1=0.5,p2=0.75,B,alpha = 0.05) # power = 1
# make a plot of the logistic regression results
# Plot sample size curves for detecting results of
# various sizes.
# set up the data frame
oddsratios = c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3)
poweramounts = c(0.025,0.772,0.999,1,1,1,1,1,1)
aim1b_df = data.frame(oddsratios,poweramounts)
# make the power curve
plotaim1b = ggplot(aim1b_df, aes(x=oddsratios,y=poweramounts)) +
  geom_line(size=2, color = "blue") + geom_hline(yintercept = 0.8, linetype = "dashed") +
  labs(x = "Effect Size (Odds Ratio)", y = "Power")
plotaim1b



#### -------- aim 2 --------- ####

# model: longitudinal continuous (proportion) outcome

# this way assumes that a linear model with a continuous outcome
# try using longpower package
Ra <- matrix(0.25, nrow = 14, ncol = 14)
diag(Ra) <- 1
ra <- c(1, 1, 0.96, 0.95, 0.91, 0.86, 0.79, 0.77, 0.75, 0.72, 0.67, 0.60, 0.50,0.32)
sigmaa <- 1
power.mmrm(Ra = Ra, ra = ra, sigmaa = sigmaa, delta = 0.5, power = 0.80)
power.mmrm(N = 243, Ra = Ra, ra = ra, sigmaa = sigmaa, delta = 0.5)
power.mmrm(N = 243, Ra = Ra, ra = ra, sigmaa = sigmaa, power = 0.80)

# calculate the power for the poisson model
# slope of 0 -> index ratio of 1
powerPoisson(beta0=0.1,beta1=0,mu.x1=0,sigma2.x1=1,mu.T = 1,phi = 1,alpha = 0.05,N = 243) # power 0.025
# slope of 0.1 -> 1.11
powerPoisson(beta0=0.1,beta1=0.1,mu.x1=0,sigma2.x1=1,mu.T = 1,phi = 1,alpha = 0.05,N = 243) # power 0.374
# slope of 0.2 -> 1.22
powerPoisson(beta0=0.1,beta1=0.2,mu.x1=0,sigma2.x1=1,mu.T = 1,phi = 1,alpha = 0.05,N = 243) # power 0.908
# slope of 0.3 -> 1.35
powerPoisson(beta0=0.1,beta1=0.3,mu.x1=0,sigma2.x1=1,mu.T = 1,phi = 1,alpha = 0.05,N = 243) # power 0.999
# slope of 0.4 -> 1.49
powerPoisson(beta0=0.1,beta1=0.4,mu.x1=0,sigma2.x1=1,mu.T = 1,phi = 1,alpha = 0.05,N = 243) # power 0.999
# slope of 0.5 -> 1.65
powerPoisson(beta0=0.1,beta1=0.5,mu.x1=0,sigma2.x1=1,mu.T = 1,phi = 1,alpha = 0.05,N = 243) # power 1
# set up the data frame
propratios = c(1,1.11,1.22,1.35,1.49,1.65)
poweramounts = c(0.025,0.374,0.908,0.999,0.999,1)
aim2_df = data.frame(propratios,poweramounts)
# make the power curve
plotaim2 = ggplot(aim2_df, aes(x=propratios,y=poweramounts)) +
  geom_line(size=2, color = "dark green") + geom_hline(yintercept = 0.8, linetype = "dashed") +
  labs(x = "Effect Size (Haplotype Sharing Index Ratio)", y = "Power")
plotaim2




#### ------ make a density plot of study retention ------- ####

# read in the merged human monthly and table data set
human_monthly_merged_data = read_rds("/Users/kelseysumner/Desktop/Dissertation Materials/SpatialR21 Grant/Final Dissertation Materials/Final Data Sets/Final Cohort data June 2017 to July 2018/Human data/spat21_clean_human_files/merged_files/hum_monthly_merged_with_table_data_4FEB2019.RDS")

# look at summaries of the people that will be excluded from longitudinal analyses
# create a household summary
household_summary = human_monthly_merged_data %>%
  group_by(HH_ID) %>%
  summarize(n_person_months = n(), n_households = n_distinct(HH_ID),
            n_participants = n_distinct(unq_memID))
# look at just a few households
m15 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="M15"),]
m16 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="M16"),]
k13 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="K13"),]
k14 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="K14"),]
s12 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="S12"),]
s13 = human_monthly_merged_data[which(human_monthly_merged_data$HH_ID=="S13"),]
# S13 and M16 entered study late
# look for how many people were <1
under1 = human_monthly_merged_data[which(!(is.na(human_monthly_merged_data$age_m))),]
length(unique(under1$unq_memID)) # 11
undernames = unique(under1$unq_memID)
# look for how many participants did not have monthly follow-up and weren't 1
nofolloup = human_monthly_merged_data[which(is.na(human_monthly_merged_data$gender_hum_monthly_data) & is.na(human_monthly_merged_data$age_m)),]
length(unique(nofolloup$unq_memID)) # 10
nofollowuptime = nofolloup$unq_memID
# look for how many participants had < 2 months follow-up
# create a new variable that has the village name for all data (not just those with monthly follow-up)
village_all_data = sapply(strsplit(human_monthly_merged_data$HH_ID,""),head,1)
table(village_all_data, useNA = "always")
# add variable to data set
human_monthly_merged_data$village_all_data = village_all_data
# calculate average person-months per participant
participant_data = human_monthly_merged_data %>%
  group_by(village_all_data,unq_memID) %>%
  summarize(n_count=n())
length(which(participant_data$n_count < 2))
lessthan2 = participant_data$unq_memID[which(participant_data$n_count < 2)]
# 25 participants had < 2 months follow-up
lessthan2
undernames
intersect(lessthan2,undernames)
length(intersect(lessthan2,undernames))
# 5 participants <1 and less than 2 months follow-up
lessthan2
nofollowuptime
intersect(lessthan2,nofollowuptime)
length(intersect(lessthan2,nofollowuptime))
# 10 participants declined monthly follow-up and had less than 2 months follow-up
# 25-15 = 10 participants had < 2 months follow-up
# 268 - 10 -5 - 10 = 237 participants in final data set

# now exclude those participants that fit those exclusion criteria
# create a new variable of unq_memIDs of everyone that fits exclusion criteria
firstcomparison = intersect(lessthan2,undernames)
exclude_ids_1 = union(firstcomparison,lessthan2)
exclude_ids_2 = union(exclude_ids_1,nofollowuptime)
length(exclude_ids_2)
length(which(human_monthly_merged_data$unq_memID %in% exclude_ids_2)) # 25 times
# remove all occurrences of those 25 unq_memIDs
test_data = human_monthly_merged_data[-which(human_monthly_merged_data$unq_memID %in% exclude_ids_2),]
# check the results
nrow(test_data)
nrow(human_monthly_merged_data)
2646-2621 # = 25
# looks good
human_monthly_merged_data = test_data


# calculate the maximum follow-up for each time 
participant_data = human_monthly_merged_data %>%
  group_by(village_all_data,unq_memID) %>%
  summarize(n_count=n())
median(participant_data$n_count)
month2 = length(which(participant_data$n_count == 2))/243
month3 = length(which(participant_data$n_count == 3))/243 + month2
month4 = length(which(participant_data$n_count == 4))/243 + month3
month5 = length(which(participant_data$n_count == 5))/243 + month4
month6 = length(which(participant_data$n_count == 6))/243 + month5
month7 = length(which(participant_data$n_count == 7))/243 + month6
month8 = length(which(participant_data$n_count == 8))/243 + month7
month9 = length(which(participant_data$n_count == 9))/243 + month8
month10 = length(which(participant_data$n_count == 10))/243 + month9
month11 = length(which(participant_data$n_count == 11))/243 + month10
month12 = length(which(participant_data$n_count == 12))/243 + month11
month13 = length(which(participant_data$n_count == 13))/243 + month12
month14 = length(which(participant_data$n_count == 14))/243 + month13

# set up the data frame
retention <- c(1, 1, 0.96, 0.95, 0.91, 0.86, 0.79, 0.77, 0.75, 0.72, 0.67, 0.60, 0.50,0.32)
retention_cumulative <- c(0.04,0.05,0.09,0.14,0.21,0.23,0.25,0.28,0.33,0.40,0.50,0.68,1)
months <- c(2,3,4,5,6,7,8,9,10,11,12,13,14)
retention_df=data.frame(retention_cumulative,months)



# cumulative distribution of retention plot
retentionplot = ggplot(retention_df, aes(x=months,y=retention_cumulative)) +
  geom_line(fill = "light blue",stat="identity") +
  labs(x = "Maximum months of follow-up for each person", y = "Cumulative Proportion of Study Population") + 
  geom_area(fill="light blue") +
  geom_vline(xintercept = 12, linetype = "dashed") +
  scale_x_discrete(name ="Total months of follow-up", limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14"))
retentionplot






