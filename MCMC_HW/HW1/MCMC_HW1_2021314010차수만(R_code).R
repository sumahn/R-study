################################# R code ################################# 
###################### HW에 사용된 R code 입니다. ########################

################################## HW 1 ##################################

# Gamma(4,2)

alpha = 4 ; beta = 2
u = matrix(runif(10000*alpha), ncol=alpha)

# Using inverse cdf method for exponential distribution with parmater 2
x = -(1/beta)*log(1-u)

# Sum of 4 exp(2)
x_sum = rowSums(x)

# Histogram
hist(x_sum, nclass=100, main="Histogram of 10,000 samples from Gamma(4,2)")



# Gamma(6,3)

alpha=6 ; beta=3
u = matrix(runif(10000*alpha), ncol=alpha)

# Using inverse cdf method for exp(3)
x = -(1/beta)*log(1-u)

# Sum of 6 exp(3)
x_sum = rowSums(x)

# Histogram
hist(x_sum, nclass=100, main="Histogram of 10,000 samples from Gamma(6,3)")



# Cauchy(0,1)

u = runif(10000)
pi = 3.14
x = tan(pi*(u-1/2))
hist(x, nclass=100, main="Histogram of 10,000 samples from Cauchy(0,1)")




################################## HW 2 ##################################

# (a) Random number generator using inverse cdf F(x)

# sample size
n <- 10000

# inverse CDF
u <- runif(10000)
inverse.cdf <- function(u) {
    if (u >= 0 && u < 0.25)
        sqrt(u)/2
    else if (u >=0.25 && u <=1)
        1 - sqrt(3*(1-u))/2 }

# Generate random number 
x <- unlist(lapply(u, inverse.cdf))

# Show the 100 samples of generated random numbers
head(x, 100)

# Histogram 
hist(x, nclass=100, main="Histogram of 10,000 samples from F(x)")



# (b) Rejection sampling

## 1. Setup

# 1-0. desired sample size and sample space
desired_size <- 10000
samples <- c()

# 1-1. target density f(x)

target.x <- function(x) { 
    if ((x >=0) && (x < 0.25))
        8*x
    else if ((x >=0.25) && (x<=1))
        8/3 - 8/3*x
    else
        0
}

# 1-2. proposal density 
proposal.x <- function(x) {
    if (x>=0 && x<=1)
        1
    else 
        0
}

# 1-3. envelope density e(x) = 3g(x)
envelope.x <- function(x) {
    if (x>=0 && x<=1)
        3
    else
        0
}


## 2. Rejection sampling

# 2-1. Sample y from g(x)
# y <- runif(1)

# 2-2. Sample u from Unif(0,1)
#u <- runif(1) 

# 2-3. Calculation of rejection probability
# rej_prob <- target.x / envelope.x

### reject y if u > rej_prob

# This process should be repeated until we have samples of the desired size(10,000)
curr_size <- 0
while(curr_size < desired_size) {
    y <- runif(1)
    u <- runif(1)
    rej_prob <- target.x(y) / envelope.x(y)
    
    if (u <= rej_prob) {
        samples <- c(samples, y)
        curr_size <- curr_size + 1
    } 
}

# 3. Compare the target density and distribution of samples of rejection sampling.
value <- c()
res <- c()

for (i in sort(runif(10000))) 
{res <- c(res, target.x(i))
value <- c(value, i)}


hist(samples, nclass= 100, freq=FALSE, main="Histogram of rejection sampling samples")
lines(value ,res, type="l", col="red")
legend("topright",c("Target density"), col=("red"), lty=1)



# Example

# set values
x <- c(8,3,4,3,1,7,2,6,2,7)
n <- 10000
lambda.samp <- rep(NA, n)
xbar = mean(x)

# proposal density
iter = 1
total = 1

while(iter <= n){
    lambda = exp(rnorm(1, log(4), 0.5))
    u = runif(1,0,1)
    ratio = exp(sum(dpois(x, lambda, log=TRUE)) - sum(dpois(x, xbar, log=TRUE)))
    if (u <= ratio){
        lambda.samp[iter] = lambda
        iter = iter+1
    }
    total = total+1
}

hist(lambda.samp, nclass=100, main= "Histogram of samples")
n/total