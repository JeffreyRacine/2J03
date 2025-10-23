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
par(mfrow=c(3,2))
n <- 100
mean.vec <- numeric()

## X has a chi-square distribution with df = 3

df <- 3

n <- 4
for(i in 1:100000) mean.vec[i] <- mean(rchisq(n,df=df))
mean.vec <- sort(mean.vec)

## Generate a histogram density for the raw data X
hist(x <- sort(rchisq(100000,df=df)),breaks=50,prob=TRUE,main="Density of raw data",xlab="X")
## Plot the true PDF
lines(x,dchisq(x,df=df),lwd=2)
## Now plot the histogram density and normal density of the 100,000 sample means
hist(mean.vec,prob=TRUE,breaks=50,main="Density of the sample mean",xlab="Sample Mean",sub=paste("n =",n))
lines(mean.vec,dnorm(mean.vec,mean(mean.vec),sd(mean.vec)),lwd=2)
legend("topright",c("Sample Mean Density","Normal Density"),lwd=2,bty="n")

n <- 15
for(i in 1:100000) mean.vec[i] <- mean(rchisq(n,df=df))
mean.vec <- sort(mean.vec)

## Generate a histogram density for the raw data X
hist(x <- sort(rchisq(100000,df=df)),breaks=50,prob=TRUE,main="Density of raw data",xlab="X")
## Plot the true PDF
lines(x,dchisq(x,df=df),lwd=2)
## Now plot the histogram density and normal density of the 100,000 sample means
hist(mean.vec,prob=TRUE,breaks=50,main="Density of the sample mean",xlab="Sample Mean",sub=paste("n =",n))
lines(mean.vec,dnorm(mean.vec,mean(mean.vec),sd(mean.vec)),lwd=2)
legend("topright",c("Sample Mean Density","Normal Density"),lwd=2,bty="n")

n <- 80
for(i in 1:100000) mean.vec[i] <- mean(rchisq(n,df=df))
mean.vec <- sort(mean.vec)

## Generate a histogram density for the raw data X
hist(x <- sort(rchisq(100000,df=df)),breaks=50,prob=TRUE,main="Density of raw data",xlab="X")
## Plot the true PDF
lines(x,dchisq(x,df=df),lwd=2)
## Now plot the histogram density and normal density of the 100,000 sample means
hist(mean.vec,prob=TRUE,breaks=50,main="Density of the sample mean",xlab="Sample Mean",sub=paste("n =",n))
lines(mean.vec,dnorm(mean.vec,mean(mean.vec),sd(mean.vec)),lwd=2)
legend("topright",c("Sample Mean Density","Normal Density"),lwd=2,bty="n")
