## Example: At Canon Food Corporation, it used to take an average of 90 minutes
## for new workers to learn a food processing job. Recently the company
## installed a new food processing machine. The supervisor at the company wants
## to find if the mean time taken by new workers to learn the food processing
## procedure on this new machine is different from 90 minutes. A sample of 20
## workers showed that it took, on average, 85 minutes for them to learn the
## food processing procedure on the new machine. It is known that the learning
## times for all new workers are normally distributed with a population standard
## deviation of 7 minutes. Find the p–value for the test that the mean learning
## time for the food processing procedure on the new machine is different from
## 90 minutes. What will your conclusion be if the level of significance α =
## 0.01?

# Given data
n <- 20                     # Sample size
mu_0 <- 90                  # Hypothesized population mean
x_bar <- 85                 # Sample mean
sigma <- 7                  # Population standard deviation
alpha <- 0.01               # Significance level
# Calculate the z-test statistic
z <- (x_bar - mu_0) / (sigma / sqrt(n))
# Calculate the p-value for a two-tailed test
p_value <- 2 * (1 - pnorm(abs(z)))
# Print the z-test statistic and p-value
cat("Z-test statistic:", z, "\n")
cat("P-value:", p_value, "\n")
# Conclusion based on the p-value and significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis: The mean learning time is different from 90 minutes.\n")
} else {
  cat("Fail to reject the null hypothesis: The mean learning time is not significantly different from 90 minutes.\n")
}
# Output:
# Z-test statistic: -3.202564 
# P-value: 0.001366
# Reject the null hypothesis: The mean learning time is different from 90 minutes.
  