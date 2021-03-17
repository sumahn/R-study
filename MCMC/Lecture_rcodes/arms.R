library(armspp)

# normal distribution
output <- arms(
    10000, function(x) -x ^ 2 / 2,
    -1000, 1000,
    metropolis = FALSE, include_n_evaluations = TRUE
)
print(str(output))

hist(output$samples, br = 50, freq = FALSE, main = 'Normal samples')


# mixture of normal distributions
dnormmixture <- function(x) {
    parts <- log(c(0.4, 0.6)) + dnorm(x, mean = c(-1, 4), log = TRUE)
    log(sum(exp(parts - max(parts)))) + max(parts)
}

curve(exp(Vectorize(dnormmixture)(x)), -7, 10)


## overflow / underflow
# Mixture: 0.4*Normal_a + 0.6*Normal_b
# log density: log(0.4*Normal*a + 0.6*Normal_b)
# (exp(component1 - max(component) + exp(component2 - max(component)) + exp(component3 - max(component))) * exp(max(component))
# log(exp(component1 - max(component) + exp(component2 - max(component)) + exp(component3 - max(component))) * exp(max(component)) 
# log(exp(component1 - max(component) + exp(component2 - max(component)) + exp(component3 - max(component)))) + max(component)

output2 <- arms(1000, dnormmixture, -1000, 1000, metropolis = TRUE)
hist(output2, freq = FALSE, nclass=100)
curve(exp(Vectorize(dnormmixture)(x)), -7, 10, col = 'red', add = TRUE)