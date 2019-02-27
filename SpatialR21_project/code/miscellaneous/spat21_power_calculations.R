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


#### -------- aim 1B --------- ####

# model: logistic regression with binary outcome (longitudinal with binary outcome)
# the book recommends using an equation similar to GEE For sample size calculations
# there was an spass package in r that did this but has been taken off cran

# use the pwr package in r to run a regular linear model
# Cohen suggests that r values of 0.1, 0.3, and 0.5 represent small, medium, and large effect sizes respectively
pwr.r.test(n = 243, r=, sig.level = 0.05, power = 0.80)

# another way that runs a regular logistic regression model with binary predictor
## sample size for simple logistic regression
n <- 243           # total sample size
p1 <- 0.5          # the event rate in unexposed
p2 <- 0.3          # the event rate in the exposed
B <- 0.4           # proportion that are exposed
# for an increase of one unit in X.
# beta*=log(OR) is the effect size to be tested
powerLogisticBin(n,
                 p1,
                 p2,
                 B,
                 alpha = 0.05)



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


# attempt at simulating model: not work
# Power example from Longitudinal Data Analysis (Diggle, 2002) 
alpha <- 0.05
beta <-0.20
sigma2e <- cbind(100,200,300) # Range of variances
rho <- cbind(0.20, 0.50, 0.80) # Range of correlations
t <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14) # Time measurements
m <- length(t) # Number of observations
s2t <- var(t)*(m-1)/m # Variance of the t’s (covariates) 
delta <- 0.5 # Minimum detectable difference
samplesize <-function(alpha,beta,sigma2e,rho,m,s2t,delta){ 
  za<-qnorm(1-alpha) # Note: 1-sided alpha here 
  zb<-qnorm(1-beta)
  n <- (2*sigma2e*(1-rho)*(za+zb)^2)/(m*s2t*delta^2) 
  return(n)
}
NN<-matrix(0,3,3) 
for(i in 1:3){
  for(j in 1:3){ 
    NN[i,j]<-samplesize(alpha,beta,sigma2e[j],rho[i],m,s2t,delta)
  }
}
NN <- rbind(sigma2e,ceiling(NN))
NN <- cbind(rbind(0,t(rho)),NN)
NN # First row are the variances, first column are the rho values
# hmm don't think this is working properly

## try coding my own model, binomial logit regression
set.seed(1)
n <- 243 # total sample size 
sd <- 10 # standard deviation
nsims <- 10000  # total simulations
onesim <- function(){
  X <- rnorm(n,mu,sd) # randomly generate data from binomial distribution, 2 trials, equal probability of mosquito density
  E <- rbinom(n,sd,X)
  Y <- rbinom(n,sd,X+E)
  Fit <- glm(Y ~ X+E, family=binomial("logit"))
  p.value <- coef(fit)
  return(p.value<0.05)
}
getpower <- function(){
    power <- matrix(NA,1, nsims)
    for (ii in 1:nsims) power[ii] <- onesim()
    power <- sum(power)/nsims
    print(paste("Empirical power",power,"for mu",mu,"based on nsims=",nsims))}
  
mu <- 0; getpower() # Check type I error
mu <- 5; getpower() # Empirical power


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








