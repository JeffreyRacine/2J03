## Total profits (in million dollars) of 10 U.S. companies for the year 2014
## (www.fortune.com).

profits <- c(37037, 18249, 11431, 32580, 5346, 13057, 5113, 5385, 16483, 16022)

## Compute the sample mean profit

mean(profits)

## The following are the ages (in years) of all eight employees of a 
# small company:

ages <- c(53, 32, 61, 27, 39, 44, 49, 57)

## Compute the population mean age (note that we have data on the entire
## population AND that the formula for the sample and population means are the
## same)

mean(ages)

## The following are the list prices of eight homes randomly selected from all
## homes for sale in a city. Note that the first seven prices lie between
## 175,000 and 400,000, while the last is an order of magnitude larger (10x).
## Suppose that this last price is an outlier due to a data entry error (it was
## supposed to be 387,448).
  
prices <- c(245670, 176200, 360280, 272440, 450394, 310160, 393610, 3874480)

## Compute the mean price with the outlier and with the corrected value.

mean(prices)

## Copy the prices vector and replace the outlier with the corrected value, then
## compute the mean, and note that the mean is sensitive to the outlier so is
## said to be non-robust.

prices.corrected <- prices
prices.corrected[8] <- 387448
mean(prices.corrected)

## Now compute the median price with and without the outlier, and note that
## since the median relies on ordering the data, it is not affected by the
## outlier, and is said to be robust.

median(prices)
median(prices.corrected)

## The following data give the speeds (in miles per hour) of eight cars that
## were stopped on I-95 for speeding violations. Find the mode.

speeds <- c(77, 82, 74, 81, 79, 84, 74, 78)

## Since there is no mode() function in R, we can use the table() function to 
## create a frequency table, and then use the which.max() function to find the
## value with the highest frequency.

table(speeds)
speeds.freq <- table(speeds)
speeds.freq
speeds.mode <- as.numeric(names(speeds.freq)[which.max(speeds.freq)])
speeds.mode

##   A small company has 12 employees. Their commuting times 
## (rounded to the nearest minute) from home to work are 23, 
## 36, 14, 23, 47, 32, 8, 14, 26, 31, 18, and 28, respectively. 

commute <- c(23, 36, 14, 23, 47, 32, 8, 14, 26, 31, 18, 28)

## Compute the mode

commute.freq <- table(commute)
commute.freq
commute.mode <- as.numeric(names(commute.freq)[which.max(commute.freq)])
commute.mode

## This is wrong! But we can write a function in R that computes the mode
## properly when there exists > 1 mode.

getmode <- function(v) {
  uniqv <- unique(v)
  tab <- tabulate(match(v, uniqv))
  modes <- uniqv[tab == max(tab)]
  return(modes)
}

commute.mode <- getmode(commute)
commute.mode

## Laura bought gas for her car four times during June 2018. She bought 10
## gallons at a price of $2.60 a gallon, 13 gallons at a price of $2.80 a
## gallon, 8 gallons at a price of $2.70 a gallon, and 15 gallons at a price of
## $2.75 a gallon. What is the average price that Laura paid for gas during June
## 2018? 

price <- c(2.60, 2.80, 2.70, 2.75)
gallons <- c(10, 13, 8, 15)

## Compute a weighted mean of the prices, using the gallons as weights.

weighted.mean(price, gallons)
