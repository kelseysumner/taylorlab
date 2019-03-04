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



#### -------- aim 1A --------- ####

# model: cox proportional hazards

# simulator to approx power for two sample problem in survival library(survival)
set.seed(1)
n <- 243 # total sample size
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
powerEpi.default(n=243, theta=1, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.025
# HR of 0.9
powerEpi.default(n=243, theta=.9, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.071
# HR of 0.8
powerEpi.default(n=243, theta=.8, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.179
# HR of 0.75
powerEpi.default(n=243, theta=.75, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.269
# HR of 0.7
powerEpi.default(n=243, theta=.7, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.385
# HR of 0.6
powerEpi.default(n=243, theta=.6, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.666
# HR of 0.5
powerEpi.default(n=243, theta=.5, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.900
# HR of 0.4
powerEpi.default(n=243, theta=.4, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.990
# HR of 0.3
powerEpi.default(n=243, theta=.3, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.999
# HR of 0.25
powerEpi.default(n=243, theta=.25, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 0.999
# HR of 0.2
powerEpi.default(n=243, theta=.2, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 1
# HR of 0.1
powerEpi.default(n=243, theta=.1, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 1
# HR of 0
powerEpi.default(n=243, theta=0, p=0.5, psi=0.4, rho2=0.1, alpha = 0.05) # power 1
# make the data frame
hazardratios = c(1,0.9,0.8,0.75,0.7,0.6,0.5,0.4,0.3,0.25,0.2,0.1,0)
poweramounts = c(0.025,0.071,0.179,0.269,0.385,0.666,0.900,0.990,0.999,0.999,1,1,1)
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
n <- 243           # total sample size
# p1           # the event rate in unexposed (allow to vary)
# p2         # the event rate in the exposed (allow to vary)
B <- 0.5           # proportion that are exposed, assuming 50% of population becomes asymptomatic throughout study
# calculating an OR: 1
powerLogisticBin(n,p1=0.5,p2=0.5,B,alpha = 0.05) # power = 0.025
# calculating an OR of 1.25
powerLogisticBin(n,p1=0.5,p2=0.55,B,alpha = 0.05) # power = 0.119
# calculating an OR: 1.5 
powerLogisticBin(n,p1=0.5,p2=0.6,B,alpha = 0.05) # power = 0.346
# calculating an OR: 1.75
powerLogisticBin(n,p1=0.5,p2=0.64,B,alpha = 0.05) # power = 0.597
# calculating an OR: 2
powerLogisticBin(n,p1=0.5,p2=0.67,B,alpha = 0.05) # power = 0.770
# calculating an OR: 2.25
powerLogisticBin(n,p1=0.5,p2=0.7,B,alpha = 0.05) # power = 0.894
# calculating an OR: 2.5
powerLogisticBin(n,p1=0.5,p2=0.72,B,alpha = 0.05) # power = 0.945
# calculating an OR: 2.75
powerLogisticBin(n,p1=0.5,p2=0.735,B,alpha = 0.05) # power = 0.969
# calculating an OR: 3
powerLogisticBin(n,p1=0.5,p2=0.75,B,alpha = 0.05) # power = 0.984
# make a plot of the logistic regression results
# Plot sample size curves for detecting results of
# various sizes.
# set up the data frame
oddsratios = c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3)
poweramounts = c(0.025,0.119,0.346,0.597,0.770,0.894,0.945,0.969,0.984)
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

# set up the data frame
retention <- c(1, 1, 0.96, 0.95, 0.91, 0.86, 0.79, 0.77, 0.75, 0.72, 0.67, 0.60, 0.50,0.32)
months <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
retention_df=data.frame(retention,months)

# bar plot option
retentionplot = ggplot(retention_df, aes(x=months,y=retention)) +
  geom_bar(fill = "light blue",stat="identity") +
  labs(x = "Total months of follow-up", y = "Proportion of Study Population") + 
  geom_vline(xintercept = 10.79, linetype = "dashed") +
  scale_x_discrete(name ="Total months of follow-up", limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14"))
retentionplot








