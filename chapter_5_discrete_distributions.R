## Let x denote the number of defective computer parts in a shipment of 400. The
## following table gives the probability distribution of x.

x <- c(0, 1, 2, 3, 4, 5)
p <- c(0.02, 0.20, 0.30, 0.30, 0.10, 0.08)

## Compute the mean and standard deviation of x

mean_x <- sum(x * p)
var_x <- sum((x - mean_x)^2 * p)
sd_x <- sqrt(var_x)
mean_x
sd_x

## Binomial distribution, how to compute n choose x in R, and given pi, the
## probability of success, how to use this to compute p(x). Consider a simple
## example.

n <- 10
x <- 3

choose(n, x)

## Now let pi be the probability of success

pi <- 0.2

## Compute p(x) using the formula for the binomial distribution

p_x <- choose(n, x) * pi^x * (1 - pi)^(n - x)
p_x

## Use the R function dbinom to compute p(x) for a binomial distribution

p_x <- dbinom(x, size = n, prob = pi)
p_x

## Confirm with tables that will be used in the upcoming exams.