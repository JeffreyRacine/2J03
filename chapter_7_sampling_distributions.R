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
