################################# R code ################################# 
###################### HW에 사용된 R code 입니다. ########################

################################## HW 2 ##################################

# Weibull distribution 
lambda = 3 ; k = 0.3
u = runif(10000)
x = lambda*(-log(1-u))^(1/k)
hist(x, nclass=500, xlim= c(0, 300),main="Histogram of 10,000 samples from Weibull(3, 0.3)")


# Polar Method
u1 = runif(10000)
u2 = runif(10000)
r = sqrt(-2*log(u1))
theta = 2*pi*u2
x = r*cos(theta)
y = r*sin(theta)


par(mfrow=c(1,3))
hist(x, nclass=100)
hist(y, nclass=100)
hist(c(x,y), nclass=100, main="Sampling result using polar method")


# ARMS
library(armspp)
dtmixture <- function(x) {
    parts <- log(c(0.3, 0.7)) + dt(x, df=c(1,4), log=TRUE)
    log(sum(exp(parts - max(parts)))) + max(parts)
}
curve(exp(Vectorize(dtmixture)(x)), -10, 10)
samples <- arms(1000, dtmixture, -1000, 1000)
hist(samples, freq=FALSE, nclass = 100, main="Histogram of mixture of t-distributions")
curve(exp(Vectorize(dtmixture)(x)),-10,10,col="red", add=TRUE)
