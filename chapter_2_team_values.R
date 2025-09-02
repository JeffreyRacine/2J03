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

## Construct a Frequency Histogram

hist(values, breaks=breaks, main="MLB Team Values", 
     xlab="Value (in millions of dollars)", 
     ylab="Frequency")

## Construct a Frequency Polygon on top of the histogram by extracting
## mid-points from the hist() function then plotting them using lines()

values.hist <- hist(values,breaks=breaks,plot=FALSE)
lines(values.hist$mids,values.hist$counts,type="b")

## Construct and plot the Cumulative Frequencies

cum.abs <- cumsum(values.hist$counts)
plot(values.hist$breaks[1:6],cum.abs,t="s",
     xlab="Value (in millions of dollars)",
     ylab="Cumulative Absolute Frequency (LE)",main="")

## Construct the Relative and Cumulative Relative Frequencies

rel.freq <- values.hist$counts/length(values)
cum.rel <- cumsum(rel.freq)

## Create a table with class limits in column 1, the Frequencies in column 2,
## the Relative Frequencies in column 3, the Cumulative Frequencies in column 4,
## and the Cumulative Relative Rrequencies in column 5. If you wanted
## percentages you mulitply all relative frequencies by 100.

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

