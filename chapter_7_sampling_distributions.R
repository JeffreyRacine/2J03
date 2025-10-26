# Suppose we assign the letters A, B, C, D, and E to the scores of the five
# students so that A = 70,   B = 78,   C = 80,   D = 80,   E = 95
scores <- c(A = 70, B = 78, C = 80, D = 80, E = 95)
# Consider the letters A, B, C, D, E, and find all combinations of these letters
# taken 3 at a time
combinations <- combn(c("A", "B", "C", "D", "E"), 3)
# Calculate the average score for each combination of letters
average_scores <- apply(combinations, 2, function(x) mean(scores[x]))
# Create a table listing all combinations, scores, and their average
result_table <- data.frame(
  Combination = apply(combinations, 2, paste, collapse = ", "),
  Score1 = scores[combinations[1, ]],
  Score2 = scores[combinations[2, ]],
  Score3 = scores[combinations[3, ]],
  Average_Score = average_scores
)
# Print the table with only 2 digits after the decimal for the average score
result_table$Average_Score <- format(round(result_table$Average_Score, 2), nsmall = 2)
print(result_table)
# Now create a table showing the unique scores, their frequency, and relative
# frequency
score_table <- as.data.frame(table(average_scores))
colnames(score_table) <- c("Average_Score", "Frequency")
score_table$Relative_Frequency <- score_table$Frequency / sum(score_table$Frequency)
# Print this table with only 2 digits after the decimal for the first column
score_table$Average_Score <- format(round(as.numeric(as.character(score_table$Average_Score)), 2), nsmall = 2)
print(score_table)

# Central limit theorem illustration

set.seed(42)
par(mfrow=c(4,2))
n <- 100
mean.vec <- numeric()

## X has a chi-square distribution with df = 3

df <- 3

n <- 4
for(i in 1:100000) mean.vec[i] <- mean(rchisq(n,df=df))
mean.vec <- sort(mean.vec)

## Generate a histogram density for the raw data X
hist(x <- sort(rchisq(100000,df=df)),breaks=50,prob=TRUE,main="Distribution of the data",xlab="X")
## Plot the true PDF
lines(x,dchisq(x,df=df),lwd=2)
## Now plot the histogram density and normal density of the 100,000 sample means
hist(mean.vec,prob=TRUE,breaks=50,main=paste("Distribution of the sample mean; n =",n),xlab="Sample Mean")
lines(mean.vec,dnorm(mean.vec,mean(mean.vec),sd(mean.vec)),lwd=2)
legend("topright",c("Sample Mean Distribution","Normal Distribution"),lwd=2,bty="n")

n <- 15
for(i in 1:100000) mean.vec[i] <- mean(rchisq(n,df=df))
mean.vec <- sort(mean.vec)

## Generate a histogram density for the raw data X
hist(x <- sort(rchisq(100000,df=df)),breaks=50,prob=TRUE,main="Distribution of the data",xlab="X")
## Plot the true PDF
lines(x,dchisq(x,df=df),lwd=2)
## Now plot the histogram density and normal density of the 100,000 sample means
hist(mean.vec,prob=TRUE,breaks=50,main=paste("Distribution of the sample mean; n =",n),xlab="Sample Mean")
lines(mean.vec,dnorm(mean.vec,mean(mean.vec),sd(mean.vec)),lwd=2)
legend("topright",c("Sample Mean Distribution","Normal Distribution"),lwd=2,bty="n")

n <- 30
for(i in 1:100000) mean.vec[i] <- mean(rchisq(n,df=df))
mean.vec <- sort(mean.vec)

## Generate a histogram density for the raw data X
hist(x <- sort(rchisq(100000,df=df)),breaks=50,prob=TRUE,main="Distribution of the data",xlab="X")
## Plot the true PDF
lines(x,dchisq(x,df=df),lwd=2)
## Now plot the histogram density and normal density of the 100,000 sample means
hist(mean.vec,prob=TRUE,breaks=50,main=paste("Distribution of the sample mean; n =",n),xlab="Sample Mean")
lines(mean.vec,dnorm(mean.vec,mean(mean.vec),sd(mean.vec)),lwd=2)
legend("topright",c("Sample Mean Distribution","Normal Distribution"),lwd=2,bty="n")

n <- 80
for(i in 1:100000) mean.vec[i] <- mean(rchisq(n,df=df))
mean.vec <- sort(mean.vec)

## Generate a histogram density for the raw data X
hist(x <- sort(rchisq(100000,df=df)),breaks=50,prob=TRUE,main="Distribution of the data",xlab="X")
## Plot the true PDF
lines(x,dchisq(x,df=df),lwd=2)
## Now plot the histogram density and normal density of the 100,000 sample means
hist(mean.vec,prob=TRUE,breaks=50,main=paste("Distribution of the sample mean; n =",n),xlab="Sample Mean")
lines(mean.vec,dnorm(mean.vec,mean(mean.vec),sd(mean.vec)),lwd=2)
legend("topright",c("Sample Mean Distribution","Normal Distribution"),lwd=2,bty="n")

# Boe Consultant Associates has five employees, only some of whom have knowledge
# of statistics. The names of these five employees and information concerning
# their knowledge of statistics are as follows:

# Names vector
Name <- c("Ally", "John", "Susan", "Lee", "Tom")
# Knows vector
Knows <- c("Yes", "No", "No", "Yes", "Yes")
# Now, suppose we draw all possible samples of three employees each and compute the proportion of employees, for each sample, who know statistics. The total number of samples is C(5,3) = 10. We can use the combn function in R to generate all possible combinations of three employees from the five employees.
# Generate all possible combinations of 3 employees from the 5 employees
combinations <- combn(Name, 3)
# Initialize a vector to store the proportion of employees who know statistics for each sample
proportions <- numeric(ncol(combinations))
# Loop through each combination and calculate the proportion of employees who know statistics
for (i in 1:ncol(combinations)) {
  sample <- combinations[, i]
  knows_count <- sum(Knows[match(sample, Name)] == "Yes")
  proportions[i] <- knows_count / length(sample)
}
# Create a data frame to display the results
result_df <- data.frame(
  Sample = apply(combinations, 2, paste, collapse = ", "),
  Proportion_Know_Statistics = proportions
)
# Print the result data frame using 2 digits for the proportion:
result_df$Proportion_Know_Statistics <- format(round(result_df$Proportion_Know_Statistics, 2), nsmall = 2)
print(result_df)
# Finally, we can create a table showing the unique proportions of employees who know statistics, along with their frequencies and relative frequencies.
# Create a table of unique proportions, their frequencies, and relative frequencies
proportion_table <- as.data.frame(table(proportions))
colnames(proportion_table) <- c("Proportion_Know_Statistics", "Frequency")
proportion_table$Relative_Frequency <- proportion_table$Frequency / sum(proportion_table$Frequency)
# Print the proportion table using 2 digits for the proportion:
proportion_table$Proportion_Know_Statistics <- format(round(as.numeric(as.character(proportion_table$Proportion_Know_Statistics)), 2), nsmall = 2)
print(proportion_table)
