# Suppose 5000 female students are enrolled at a university, and X is the
# continuous random variable that represents the height in inches of a randomly
# selected female student. Note that these are exact probabilities because we
# are considering the population of all female students enrolled at the
# university.

# Height Classes
C <- c("60 to less than 61", "61 to less than 62", "62 to less than 63", 
       "63 to less than 64", "64 to less than 65", "65 to less than 66", 
       "66 to less than 67", "67 to less than 68", "68 to less than 69", 
       "69 to less than 70", "70 to less than 71")

# Frequencies
f <- c(90, 170, 460, 750, 970, 
       760, 640, 440, 320, 220, 180)

# Relative Frequencies
rf <- c(0.018, 0.034, 0.092, 0.150, 0.194, 
        0.152, 0.128, 0.088, 0.064, 0.044, 0.036)

# Create a data frame containing the height classes, frequencies, and relative
# frequencies.
height_data <- data.frame(Class = C, Frequency = f, Relative_Frequency = rf)
height_data

# Create a probability density histogram for these data and then superimpose a
# density estimate on the histogram. Use only the base R functions.
par(cex=.75, mar=c(8,4,4,2)+0.1)
hist(rep(1:11, times = f),
     breaks = seq(0.5, 11.5, by = 1),
     freq = FALSE,
     xlab = "Height Classes",
     ylab = "Density",
     main = "Probability Density Histogram of Female Student Heights",
     xaxt = "n")
axis(1, at = 1:11, labels = C, las = 2)
# Superimpose a density estimate
lines(density(rep(1:11, times = f),bw=0.5), col = "blue", lwd = 2)
# Add a legend
legend("topright", legend = "Density Estimate", col = "blue", lwd = 2,bty="n")

# For this data and using the density estimate, compute the probability that the
# height of a randomly selected female student from this university, X, lies in
# the interval 65 to 68 inches (i.e., P(65 ≤ X ≤ 68)). There are 11 classes, and
# the classes corresponding to 65 to 68 inches are 6 to 8. Here we could
# approximate the area by taking the sum of the relative frequencies for classes
# 6, 7, and 8, which represents the areas of the bars in the histogram for these
# classes.
prob_65_68 <- sum(rf[6:8])
prob_65_68
# Output: 0.368=0.152+0.128+0.088

# Plot the density estimate then shade the area corresponding to this probability.
# Create the histogram again
par(cex=.75, mar=c(8,4,4,2)+0.1)
hist(rep(1:11, times = f),
     breaks = seq(0.5, 11.5, by = 1),
     freq = FALSE,
     xlab = "Height Classes",
     ylab = "Density",
     main = "Probability Density Histogram of Female Student Heights with Shaded Area",
     xaxt = "n")
axis(1, at = 1:11, labels = C, las = 2)
# Superimpose the density estimate
density_est <- density(rep(1:11, times = f),bw=0.5)
lines(density_est, col = "blue", lwd = 2)
# Shade the area corresponding to P(65 ≤ X ≤ 68)
x_shade <- seq(5.5, 8.5, length.out = 100)
y_shade <- approx(density_est$x, density_est$y, xout = x_shade)$y
polygon(c(x_shade, rev(x_shade)), c(y_shade, rep(0, length(y_shade))), col = rgb(0, 0, 1, 0.5))
# Add a legend and add the probability value
legend("topright", legend = paste("P(65 ≤ X ≤ 68) =", round(prob_65_68, 3)), col = "blue", lwd = 2,bty="n")

# Now, use the density curve estimate to approximate the probability that the
# height of a randomly selected female student from this university, X, lies
# between 65 and 68 inches. The probability P(65 ≤ X ≤ 68) is approximated by
# the area under the density curve from 65 to 68 inches. Use numerican
# integration in R to compute this area. Define the density function based on
# the density estimate
density_function <- function(x) {
  approx(density_est$x, density_est$y, xout = x)$y
}
# Compute the area under the curve from 5.5 to 8.5 (corresponding to 65 to 68
# inches). This ought to be close to the previous probability value.
area_65_68 <- integrate(density_function, lower = 5.5, upper = 8.5)$value
area_65_68
# Output: 0.3707751

# Find the area under the standard normal curve to the left of z = 1.95. Plot
# the standard normal curve and shade the area to the left of z = 1.95.

# Compute the area to the left of z = 1.95 using numerical integration
area_left_1.95 <- integrate(dnorm, lower = -Inf, upper = 1.95)$value
area_left_1.95
# Output: 0.9744
# Plot the standard normal curve
z_values <- seq(-4, 4, length.out = 1000)
plot(z_values, dnorm(z_values), type = "l", lwd = 2,
     main = "Standard Normal Curve with Shaded Area to the Left of z = 1.95",
     xlab = "z", ylab = "Density")
# Shade the area to the left of z = 1.95
z_shade <- seq(-4, 1.95, length.out = 100)
y_shade <- dnorm(z_shade)
polygon(c(z_shade, rev(z_shade)), c(y_shade, rep(0, length(y_shade))), col = rgb(0, 0, 1, 0.5))
# Add a legend and the area value
legend("topright", legend = paste("Area to the left of z = 1.95 =", round(area_left_1.95, 4)), col = "blue", lwd = 2,bty="n")

# We could instead use the pnorm() function in R to find this area, which gives
# the cumulative area from the far left up to the specified z-value. This is
# what one would do in general. Or if you only had access to a standard normal
# table, you could look up the area corresponding to z = 1.95.
area_left_1.95_pnorm <- pnorm(1.95)
area_left_1.95_pnorm

# Find the area under the standard normal curve from z = -2.17 to z = 0 using pnorm().
area_neg2.17_to_0 <- pnorm(0) - pnorm(-2.17)
area_neg2.17_to_0
# Plot the standard normal curve and shade the area from z = -2.17 to z = 0.
# Plot the standard normal curve
plot(z_values, dnorm(z_values), type = "l", lwd = 2, 
     main = "Standard Normal Curve with Shaded Area from z = -2.17 to z = 0",
     xlab = "z", ylab = "Density")
# Shade the area from z = -2.17 to z = 0
z_shade <- seq(-2.17, 0, length.out = 100)
y_shade <- dnorm(z_shade)
polygon(c(z_shade, rev(z_shade)), c(y_shade, rep(0, length(y_shade))), col = rgb(0, 0, 1, 0.5))
# Add a legend and the area value
legend("topright", legend = paste("Area from z = -2.17 to z = 0 =", round(area_neg2.17_to_0, 4)), col = "blue", lwd = 2,bty="n")

# Let X be a continuous random variable that is normally distributed with a mean
# of 25 and a standard deviation of 4. Find the area between x = 18 and x = 34
# using pnorm(), and then standardize the values and recompute the area using
# pnorm().
mean_X <- 25
sd_X <- 4
# Compute the area between x = 18 and x = 34 using pnorm()
area_18_to_34 <- pnorm(34, mean = mean_X, sd = sd_X) - pnorm(18, mean = mean_X, sd = sd_X)
area_18_to_34
# Standardize the values
z_18 <- (18 - mean_X) / sd_X
z_34 <- (34 - mean_X) / sd_X
# Recompute the area using pnorm()
area_18_to_34_standardized <- pnorm(z_34) - pnorm(z_18)
area_18_to_34_standardized

# Find the value of z such that the area under the standard normal curve in the
# right tail is .0050. This is known as a quantile, and in R we can use the
# qnorm() function to find this value.
z_right_tail_0.005 <- qnorm(1 - 0.005)

# Find the value of z  such that the area under the standard normal curve in the
# left tail is .05
z_left_tail_0.05 <- qnorm(0.05)
z_left_tail_0.05

# It is known that the life of a calculator manufactured by Calculators
# Corporation has a normal distribution with a mean of 54 months and a standard
# deviation of 8 months. What should the warranty period be to replace a
# malfunctioning calculator if the company does not want to replace more than 1%
# of all the calculators sold?
mean_life <- 54
sd_life <- 8
x_left_tail_0.01 <- qnorm(0.01, mean = mean_life, sd = sd_life)
x_left_tail_0.01

