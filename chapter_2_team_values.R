## Consider data for the value (in million dollars) of each of the 30 baseball
## teams as estimated by Forbes magazine (source: Forbes Magazine, April 13,
## 2015).

## Corresponding vector of values (in millions of dollars) used in the analysis

values <- c(840, 1150, 1000, 2100, 1800, 975, 885, 825, 855, 1125, 800, 700,
            1300, 2400, 650, 875, 895, 1350, 3200, 725, 1250, 900, 890, 2000,
            1100, 1400, 605, 1220, 870, 1280)

## Following the example in class, create frequencies lying in six classes (so
## seven class boundaries are needed) taking 601 as the lowest boundary and 3300
## as the upper class boundary, where we construct the following "breaks".

breaks <- seq(601, 3300, length=7)

## Create an Absolute Frequency Table

table(cut(values,breaks=breaks,dig.lab=4,right=FALSE))

## Create a Relative Frequency Table

table(cut(values,breaks=breaks,include.lowest=TRUE,
          right=FALSE,dig.lab=4))/length(values)

## Create a Percentage Frequency Table

table(cut(values,breaks=breaks,include.lowest=TRUE,
          right=FALSE,dig.lab=4))/length(values)*100

## Create a table using all three frequencies

freq.table <- cbind(table(cut(values,breaks=breaks,dig.lab=4,right=FALSE)),
                    table(cut(values,breaks=breaks,include.lowest=TRUE,
                              right=FALSE,dig.lab=4))/length(values),
                    table(cut(values,breaks=breaks,include.lowest=TRUE,
                              right=FALSE,dig.lab=4))/length(values)*100)
colnames(freq.table) <- c("Frequency","Relative Frequency","Percentage Frequency")
knitr::kable(freq.table,digits=2)

## Construct a Frequency Histogram

hist(values, breaks=breaks, main="MLB Team Values", 
     xlab="Value (in millions of dollars)", 
     ylab="Frequency")

## Construct a Frequency Polygon on top of the histogram by extracting
## mid-points from the hist() function then plotting them using lines()

values.hist <- hist(values,breaks=breaks,plot=FALSE)
lines(values.hist$mids,values.hist$counts,type="b",pch=19,col="blue")
legend("topright",legend="Frequency Polygon",col="blue",pch=19,bty="n")

## To create a histogram in R using hist() with the vertical axis representing
## relative frequencies or percentages, you might calculate the density values
## and then plot the histogram with freq = FALSE.

## Calculate histogram information without plotting.

hist_info <- hist(values, breaks=breaks, plot = FALSE)

## Calculate relative frequencies for each bin. Divide the counts in each bin by
## the total number of observations.

hist_info$density <- hist_info$counts / sum(hist_info$counts)

## Plot the histogram with relative frequencies. Use plot() on the modified
## hist_info object and set freq = FALSE. This tells R to use the density values
## (which you've converted to percentages) for the y-axis. You can also
## customize the y-axis label.

plot(hist_info, freq = FALSE, main="MLB Team Values", ylab = "Relative Frequency")

## Calculate percentages for each bin. Divide the counts in each bin by the
## total number of observations and multiply by 100 to get percentages.

hist_info$density <- hist_info$counts / sum(hist_info$counts) * 100

## Plot the histogram with percentages. Use plot() on the modified hist_info
## object and set freq = FALSE. This tells R to use the density values (which
## you've converted to percentages) for the y-axis. You can also customize the
## y-axis label.

plot(hist_info, freq = FALSE, main="MLB Team Values", ylab = "Percentage")

## Construct a Cumulative Frequency Table

cumsum(table(cut(values,breaks=breaks,dig.lab=4,right=FALSE)))

## Construct a Cumulative Relative Frequency Table

cumsum(table(cut(values,breaks=breaks,include.lowest=TRUE,
                 right=FALSE,dig.lab=4))/length(values))

## Construct a Cumulative Percentage Frequency Table

cumsum(table(cut(values,breaks=breaks,include.lowest=TRUE,
                 right=FALSE,dig.lab=4))/length(values)*100)

## Construct Cumulative Frequencies

cum.abs <- cumsum(values.hist$counts)

## Construct the Relative and Cumulative Relative Frequencies

rel.freq <- values.hist$counts/length(values)
cum.rel <- cumsum(rel.freq)

## Create a table with class limits in column 1, the Frequencies in column 2,
## the Relative Frequencies in column 3, the Cumulative Frequencies in column 4,
## and the Cumulative Relative Frequencies in column 5. If you wanted
## percentages you multiply all relative frequencies by 100.

## Note that to create the classes for our table, we can use some R wizardry
## with the cut() function...

class.names <- names(table(cut(values,breaks,include.lowest=TRUE,
                                    right=FALSE,dig.lab=4)))

## Now we take all columns for our table and bind them together by columns

team.value.table <- cbind(class.names,values.hist$counts,rel.freq,cum.abs,cum.rel)

## We add column names and print the table using knitr::kable(), more R wizardry

colnames(team.value.table) <- c("Class Limits",
                                "Frequency",
                                "Relative Frequency",
                                "Cumulative Frequency",
                                "Cumulative Relative Frequency")

knitr::kable(team.value.table,digits=2)

