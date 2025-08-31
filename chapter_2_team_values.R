## Consider data for the value (in million dollars) of each of the 30 baseball
## teams as estimated by Forbes magazine (source: Forbes Magazine, April 13,
## 2015).

## Vector of team names (unused but you might find them useful)

teams <- c("Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles", "Boston Red Sox",
  "Chicago Cubs", "Chicago White Sox", "Cincinnati Reds", "Cleveland Indians",
  "Colorado Rockies", "Detroit Tigers", "Houston Astros", "Kansas City Royals",
  "Los Angeles Angels of Anaheim", "Los Angeles Dodgers", "Miami Marlins",
  "Milwaukee Brewers", "Minnesota Twins", "New York Mets", "New York Yankees",
  "Oakland Athletics", "Philadelphia Phillies", "Pittsburgh Pirates",
  "San Diego Padres", "San Francisco Giants", "Seattle Mariners",
  "St. Louis Cardinals", "Tampa Bay Rays", "Texas Rangers",
  "Toronto Blue Jays", "Washington Nationals")

## Corresponding vector of values (in millions of dollars) used in the analysis

values <- c(840, 1150, 1000, 2100, 1800, 975, 885, 825, 855, 1125, 800, 700,
  1300, 2400, 650, 875, 895, 1350, 3200, 725, 1250, 900, 890, 2000,
  1100, 1400, 605, 1220, 870, 1280)

## Following the example in class, create frequencies lying in six classes
## taking 601 as the lowest boundary and 3300 as the upper class boundary, where
## we construct the following "breaks".

breaks <- seq(601, 3300, length=7)

## We can create a Frequency Histogram

hist(values, breaks=breaks, main="MLB Team Values", 
     xlab="Value (in millions of dollars)", 
     ylab="Frequency")

## Construct a frequency polygon on top of the histogram by extracting
## mid-points from the hist() function

values.hist <- hist(values,breaks=breaks,plot=FALSE)
lines(values.hist$mids,values.hist$counts,type="b")

## Construct and plot the cumulative frequency

cum.abs <- cumsum(values.hist$counts)
plot(values.hist$breaks[1:6],cum.abs,t="s",
     xlab="Value (in millions of dollars)",
     ylab="Cumulative Absolute Frequency (LE)",main="")

## Construct the relative and cumulative relative frequencies

rel.freq <- values.hist$counts/length(values)
cum.rel <- cumsum(rel.freq)

## Create a table with class limits in column 1, the frequencies in column 2,
## the relative frequencies in column 3, the cumulative frequencies in column 4,
## and the cumulative relative frequencies in column 5. If you wanted
## percentages you mulitply the relative frequencies by 100.

team.value.table <- cbind(values.hist$breaks[1:6],values.hist$counts,rel.freq,cum.abs,cum.rel)
colnames(team.value.table) <- c("Class Limits",
                                "Frequency",
                                "Relative Frequency",
                                "Cumulative Frequency",
                                "Cumulative Relative Frequency")
knitr::kable(team.value.table,digits=2)

