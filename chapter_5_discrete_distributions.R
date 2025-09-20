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