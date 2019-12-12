# -------------------------------------- #
#           Spat21/Mozzie Study          #
#       Make distribution plots          #
#                 Aim 2                  #
#            Mozzie Phase 1              #
#               K. Sumner                #
#          December 5, 2019              #
# -------------------------------------- #


#### -------- load packages ------------ ####
library(tidyverse)
require(pracma)


#### ------ make a normal distribution of the probability of transmission over time ------- ####

# create a normal, gaussian distribution between -18 and 0 centered at -9
x <- seq(-18,0, length=1000)
hx <- dnorm(x,-9,2.5)
df <- data.frame(x,hx)

# now plot that normal distribution
plot(x, hx, type="l", lty=2, xlab="Days between human infection and mosquito collection",
     ylab="P(TE)", main="Probability of transmission over time",xlim = c(-20,0.5))
abline(v=-9,col="dark red",lwd=1.5)

# calculate the area under the curve
AUC = trapz(df$x,df$hx) # 0.999681763749



# another way to do this
x<-seq(-18,8,length=1000)
s = 4
mu = -5
y <- (1/(s * sqrt(2*pi))) * exp(-((x-mu)^2)/(2*s^2)) # this is the guassian distribution formula
plot(x,y, type="l",lty=2, lwd=2, xlim = c(-20,0),xlab="Days between human infection and mosquito collection",
     ylab="P(TE)", main="Probability of transmission over time")
abline(v=-5,col="dark red",lwd=1.5)

# calculate the area under the curve
AUC = trapz(x,y) # 0.998845903386851



#### ----- make a exponentially decaying distribution for probability of transmission over distance -------- ####

# make an exponential decay function that is 1/(x^2) for the range 0-5km (0-5000m)
x <- seq(0,5, length=1000)
hx <- dexp(x,rate = 3.5) # set a rate of 3 and log = FALSE
df = data.frame(x,hx)

# now plot that normal distribution
plot(x, hx, type="l", lty=2, xlab="Distance (Km) between human infection and mosquito collection",
     ylab="P(TE)", main="Probability of transmission over distance")
abline(v=0.661,col="dark red",lwd=1.5)

# calculate the area under the curve
AUC = trapz(df$x,df$hx) # 1.00002554671025


