## Let X denote the number of breakdowns for this machine during a given week.
## Let x denote the possible values of X and P(x) the corresponding
## probabilities.

x <- c(0, 1, 2, 3)
P <- c(0.15, 0.20, 0.35, 0.30)

## Compute the mean of X by adding all possible outcomes weighted by their
## probabilities.

## First, note that x * P produces a vector of the products of the elements of x
## and P.

x * P

## Now compute the mean by summing these products.

sum(x * P)

## Let X denote the number of defective computer parts in a shipment of 400
## parts made by Baier’s Electronics. Let x denote the possible values of X and
## P(x) the corresponding probabilities.

x <- c(0, 1, 2, 3, 4, 5)
p <- c(0.02, 0.20, 0.30, 0.30, 0.10, 0.08)

## Compute the mean and standard deviation of X using the squared deviations
## about the mean formulas for the mean and standard deviation of a discrete
## random variable. In order to compute the standard deviation we first need to
## compute the variance.

mean_x <- sum(x * p)
var_x <- sum((x - mean_x)^2 * p)
sd_x <- sqrt(var_x)
mean_x
sd_x

## Note we can write the variance as E(X^2) - (E(X))^2. We can compute E(X^2) as
## follows.

E_x_sq <- sum(x^2 * p)
var_x <- E_x_sq - mean_x^2
sd_x <- sqrt(var_x)
var_x
sd_x

## Below we consider the Binomial distribution, how to compute "n choose x" in
## R, and given pi, the probability of success, how to use this to compute p(x).
## Consider a simple example where n=10 and x=3.

n <- 10
x <- 3

choose(n, x)

## Confirm that the formula for "n choose x" is correct by computing it using
## factorials.

factorial(n) / (factorial(x) * factorial(n - x))

## Now let pi be the probability of success.

pi <- 0.2

## Compute P(X=3) using the formula for the binomial distribution.

p_x <- choose(n, x) * pi^x * (1 - pi)^(n - x)
p_x

## Use the R function dbinom(x, n, pi) to compute P(x) for a binomial
## distribution.

p_x <- dbinom(x, size = n, prob = pi)
p_x

## Confirm that you can obtain identical results using the online tables that
## will be available during mid-term exam 2 and the final exam.