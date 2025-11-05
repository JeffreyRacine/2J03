# Example: A publishing company has just published a new college textbook.
# Before the company decides the price at which to sell this textbook, it wants
# to know the average price of all such textbooks in the market. The research
# department at the company took a sample of n = 25 comparable textbooks and
# collected information on their prices. This information produces a mean price
# of 𝑥 ‾= $145 for this sample. It is known that the standard deviation of the
# prices of all such textbooks is 𝜎= $35 and the population of such prices is
# normal. Construct a 90% confidence interval for the mean price of all such
# college textbooks.

# Given data
n <- 25                # Sample size
x_bar <- 145          # Sample mean
sigma <- 35           # Population standard deviation
confidence_level <- 0.90  # Confidence level
alpha <- 1 - confidence_level  # Significance level
z_alpha_over_2 <- qnorm(1 - alpha / 2)  # Z critical value for 90% confidence
# Standard error
standard_error <- sigma / sqrt(n)
# Margin of error
margin_of_error <- z_alpha_over_2 * standard_error
# Confidence interval
lower_bound <- x_bar - margin_of_error
upper_bound <- x_bar + margin_of_error
confidence_interval <- c(lower_bound, upper_bound)
# Output the confidence interval
confidence_interval

# Example: According to a 2013 study by Moebs Services Inc., an individual
# checking account at major U.S. banks costs the banks more than $380 per year.
# A recent random sample of 600 such checking accounts produced a mean annual
# cost of $500 to major U.S. banks. Assume that the standard deviation of
# annual costs to major U.S. banks of all such checking accounts is $40.
# Construct a 99% confidence interval for the current mean annual cost to major
# U.S. banks of all such checking accounts.

# Given data
n <- 600                # Sample size
x_bar <- 500           # Sample mean
sigma <- 40            # Population standard deviation
confidence_level <- 0.99  # Confidence level
alpha <- 1 - confidence_level  # Significance level
z_alpha_over_2 <- qnorm(1 - alpha / 2)  # Z
# Standard error
standard_error <- sigma / sqrt(n)
# Margin of error
margin_of_error <- z_alpha_over_2 * standard_error
# Confidence interval
lower_bound <- x_bar - margin_of_error
upper_bound <- x_bar + margin_of_error
confidence_interval <- c(lower_bound, upper_bound)
# Output the confidence interval
confidence_interval

# Example: The standard deviation for a population is 𝜎 = 14.8. A random sample
# of n = 25 observations selected from this population gave a mean equal to 𝑥 ‾
# = 143.72. The population is known to have a normal distribution. Construct a
# 99% confidence interval for 𝜇. Construct a 95% confidence interval for 𝜇.
# Construct a 90% confidence interval for 𝜇. Does the width of the confidence
# intervals constructed in parts a) through c) decrease as the confidence level
# decreases? Explain your answer.

# Given data
n <- 25                # Sample size
x_bar <- 143.72       # Sample mean
sigma <- 14.8         # Population standard deviation
confidence_levels <- c(0.99, 0.95, 0.90)  # Confidence levels
confidence_intervals <- list()  # To store confidence intervals
for (confidence_level in confidence_levels) {
  alpha <- 1 - confidence_level  # Significance level
  z_alpha_over_2 <- qnorm(1 - alpha / 2)  # Z critical value
  # Standard error
  standard_error <- sigma / sqrt(n)
  # Margin of error
  margin_of_error <- z_alpha_over_2 * standard_error
  # Confidence interval
  lower_bound <- x_bar - margin_of_error
  upper_bound <- x_bar + margin_of_error
  confidence_intervals[[as.character(confidence_level)]] <- c(lower_bound, upper_bound)
}
# Output the confidence intervals
confidence_intervals
# Yes, the width of the confidence intervals decreases as the confidence level decreases.


# Example: An alumni association wants to estimate the mean debt of this year’s
# graduates.  It is known that the population standard deviation of the debts of
# this year’s graduates is 𝜎 = $11,800. How large a sample should be selected
# so that the estimate 𝑥 ‾  with a 99% confidence level is within 𝐸= $800 of
# the population mean 𝜇?

# Given data
sigma <- 11800        # Population standard deviation
E <- 800              # Margin of error
confidence_level <- 0.99  # Confidence level
alpha <- 1 - confidence_level  # Significance level
z_alpha_over_2 <- qnorm(1 - alpha / 2)  # Z critical value
# Sample size calculation
n <- (z_alpha_over_2 * sigma / E)^2
n <- ceiling(n)  # Round up to the next whole number
# Output the required sample size
n

# Example: Sixty-four randomly selected adults who buy books for general reading
# were asked how much they usually spend on books per year. The sample produced
# a mean of $1,450 and a standard deviation of $300 for such annual expenses.
# Determine a 99% confidence interval for the corresponding population mean.

# Given data
n <- 64                # Sample size
x_bar <- 1450         # Sample mean
s <- 300              # Sample standard deviation
confidence_level <- 0.99  # Confidence level
alpha <- 1 - confidence_level  # Significance level
t_alpha_over_2 <- qt(1 - alpha / 2, df = n - 1)  # T critical value
# Standard error
standard_error <- s / sqrt(n)
# Margin of error
margin_of_error <- t_alpha_over_2 * standard_error
# Confidence interval
lower_bound <- x_bar - margin_of_error
upper_bound <- x_bar + margin_of_error
confidence_interval <- c(lower_bound, upper_bound)
# Output the confidence interval
confidence_interval