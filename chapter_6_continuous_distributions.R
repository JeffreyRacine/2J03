## Suppose 5000 female students are enrolled at a university, and X is the
## continuous random variable that represents the height in inches of a randomly
## selected female student. Note that these are exact probabilities because we
## are considering the population of all female students enrolled at the
## university.

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

## Create a probability density histogram for these data and then superimpose a
## density estimate on the histogram. Use only the base R functions.

# Create a data frame
height_data <- data.frame(Class = C, Frequency = f, Relative_Frequency = rf)
height_data
# Create the histogram, make plot less taller than normal
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

## For this data and using the density estimate, compute the probability that
#the height of a randomly selected female student from this university, X, lies
#in the interval 65 to 68 inches (i.e., P(65 ≤ X ≤ 68)). There are 11 classes,
#and the classes corresponding to 65 to 68 inches are 6 to 8. Here we could
#approximate the area by taking the sum of the relative frequencies for classes
#6, 7, and 8, which represents the areas of the bars in the histogram for these
#classes.
prob_65_68 <- sum(rf[6:8])
prob_65_68
# Output: 0.368=0.152+0.128+0.088
## Plot the density estimate then shade the area corresponding to this probability.
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
# inches)
area_65_68 <- integrate(density_function, lower = 5.5, upper = 8.5)$value
area_65_68
# Output: 0.368 approximately