## Suppose 5000 female students are enrolled at a university, and X is the
## continuous random variable that represents the height of a randomly selected
## female student. Note that these are exact probabilities because we are
## considering the population of all female students enrolled at the university.

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

## Create a probability density histogram for these data and then superimpose a density estimate on the histogram. Use only the base R functions.

# Create a data frame
height_data <- data.frame(Class = C, Frequency = f, Relative_Frequency = rf)
height_data
# Create the histogram
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
