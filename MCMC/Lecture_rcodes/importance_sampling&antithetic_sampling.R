# Example 1

## h(x)
h <- function(x) {
    10 * exp(-2*abs(x-5))
}

## weight = f(x) / g(x)
w <- function(x) {
    dunif(x,0,10) / dnorm(x, 5, 1)
}

## 10만 개 sample 
x <- rnorm(1e6, 5, 1)
unstd.w <- w(x) 
std.w <- unstd.w / sum(unstd.w)
unstd.is <- mean(h(x) * unstd.w)
std.is <- sum(h(x) * std.w)




# Example 2 (HW 3)

## h(x) 
h <- function(x) x^2

## w(x) = f(x) / g(x)
w <- function(x) {
    (1/2 * exp(-abs(x))) / (1/2*exp(-x^2/2))
}

## Sample X from g(x)
x <- rnorm(1e6, 0, 1)

## Calculate weight
unstd.w <- w(x)
std.w <- unstd.w / sum(unstd.w)

## Calcualte E(h(x))
unstd.is <- mean(h(x) * unstd.w)
std.is <- sum(h(x) * std.w)



# Example 3

## Normal way to find P(Z > 4.5)
nsim = 1000
prob = rep(NA, nsim)
for(iter in 1:nsim){
    z = rnorm(10000,0,1)
    prob[iter] = length(z[z>4.5]) / 10000
}
table(prob)
mean(prob)

## Another way
x = rexp(1e6, 1) + 4.5
w = dnorm(x, 0, 1) / dexp(x-4.5,1)

## Monte Carlo estimates
mean(w)



# Example 4 (HW 3)
x = rexp(1e6, 1) + 20
w = dnorm(x,0,1) / dexp(x-20, 1)
mean(w)



# Antithetic sampler

# Example 1

## Setup
nrep = 100
nsamp = 10000
est.mc <- rep(NA, nrep)

for(i in 1:nrep){
    u <- runif(nsamp)
    x <- 1/(1+u)
    est.mc[i] <- mean(x)
}

est.as = rep(NA, nrep)
for(i in 1:nrep){
    u = runif(nsamp/2)
    u = c(u, 1-u)
    x = 1/(1+u)
    est.as[i] = mean(x)
}

mean(est.mc)
mean(est.as)
var(est.mc)
var(est.as) # var(est.as) < var(est.mc)



# Example 2

## Monte Carlo
x <- seq(0.1, 2.5, by=0.1)
n <- 10000
nrep <- 100
cdf <- matrix(NA, nrep, length(x))

for(i in 1:nrep){
    for(j in 1:length(x)){
        u = runif(n)
        theta = x[j] * exp(-.5*(u*x[j])^2)
        cdf[i,j] = 0.5 + mean(theta)/sqrt(2*pi)
    } 
}

est.mc <- apply(cdf,2,mean)
var.mc <- apply(cdf,2,var)

## Antithetic Sampler (HW 3)
cdf.as <- matrix(NA, nrep, length(x))

for(i in 1:nrep){
    for(j in 1:length(x)){
        u <- runif(n)
        v <- 1 - u
        u <- c(u, v)
        theta <- x[j] * exp(-(u*x[j])^2 / 2)
        cdf.as[i,j] <- 0.5 + mean(theta) / sqrt(2*pi)
    }
}        

est.as <- apply(cdf.as,2,mean)
var.as <- apply(cdf.as,2,var)
print(round(rbind(est.mc, est.as, var.mc, var.as)))
print(var.as - var.mc)
