## Total profits (in million dollars) of 10 U.S. companies for the year 2014
## (www.fortune.com).

profits <- c(37037, 18249, 11431, 32580, 5346, 13057, 5113, 5385, 16483, 16022)

## Compute the sample mean of profits.

mean(profits)

## The following are the ages (in years) of all eight employees of a 
# small company:

ages <- c(53, 32, 61, 27, 39, 44, 49, 57)

## Compute the population mean age (note that we have data on the entire
## population AND that the formula for the sample and population means are the
## same)

mean(ages)

## Consider computing a weighted mean. Laura bought gas for her car four times
## during June 2018. She bought 10 gallons at a price of $2.60 a gallon, 13
## gallons at a price of $2.80 a gallon, 8 gallons at a price of $2.70 a gallon,
## and 15 gallons at a price of $2.75 a gallon. What is the average price that
## Laura paid for gas during June 2018?

price <- c(2.60, 2.80, 2.70, 2.75)
gallons <- c(10, 13, 8, 15)

## Compute a weighted mean of the prices, using the gallons as weights.

weighted.mean(price, gallons)

## The following are the list prices of eight homes randomly selected from all
## homes for sale in a city. Note that the first seven prices lie between
## 175,000 and 400,000, while the last is an order of magnitude larger (10x).
## Suppose that this last price is an outlier due to a data entry error (it was
## supposed to be 387,448).
  
prices <- c(245670, 176200, 360280, 272440, 450394, 310160, 393610, 3874480)

## Compute the mean price with the outlier and with the corrected value (here we
## use all the data for both computations).

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

## Since there is no mode() function in R, GitHub Copilot suggests that we can
## use the table() function to create a frequency table, and then use the
## which.max() function to find the value with the highest frequency.

table(speeds)
speeds.freq <- table(speeds)
speeds.freq
speeds.mode <- as.numeric(names(speeds.freq)[which.max(speeds.freq)])
speeds.mode

##   A small company has 12 employees. Their commuting times 
## (rounded to the nearest minute) from home to work are 23, 
## 36, 14, 23, 47, 32, 8, 14, 26, 31, 18, and 28, respectively. 

commute <- c(23, 36, 14, 23, 47, 32, 8, 14, 26, 31, 18, 28)

## Compute the mode using the suggestion from GitHub Copilot.

commute.freq <- table(commute)
commute.freq
commute.mode <- as.numeric(names(commute.freq)[which.max(commute.freq)])
commute.mode

## This is wrong! But we can write a function in R that computes the mode
## properly when there exists > 1 mode. We can then manually verify whether it
## works or not.

getmode <- function(v) {
  uniqv <- unique(v)
  tab <- tabulate(match(v, uniqv))
  modes <- uniqv[tab == max(tab)]
  return(modes)
}

commute.mode <- getmode(commute)
commute.mode

## Compute the mode for qualitative (yes, we can use the same function we
## wrote for both data types). The status of five students who are members of
## the student senate at a college is given below.

class <- c("senior", "sophomore", "senior", "junior", "senior")
class.mode <- getmode(class)
class.mode

## Consider the compensation of 11 female CEOs of US companies in millions of
## dollars for 2014.

compensation <- c(19.3, 16.2, 19.6, 19.3, 33.7, 21.0, 22.5, 16.9, 28.7, 42.1, 22.2)

## Find the (sample) variance and standard deviation for these data.

var(compensation)
sd(compensation)

## Use the formulas that, when done manually, can save some calculations

x <- compensation
n <- length(x)
## Sample variance
(sum(x^2)-sum(x)^2/n)/(n-1)
## Sample standard deviation
sqrt((sum(x^2)-sum(x)^2/n)/(n-1))

## Following are the 2015 earnings (in thousands of dollars) 
## before taxes for all six employees of a small company.

earnings <- c(88.50, 108.40, 65.50, 52.50, 79.80, 54.60)

## Find the (population) variance and standard deviation for these data. Since
## var() and sd() are the sample versions, we need to adjust them to get the
## population versions. To do this we multiply the sample variance by (n-1)/n,
## where n is the number of observations, and we multiply the sample standard
## deviation by sqrt((n-1)/n).

var(earnings) * (5/6)
sd(earnings) * sqrt(5/6)

## Use the formulas that, when done manually, can save some calculations

x <- earnings
N <- length(x)
## Population variance
(sum(x^2)-sum(x)^2/N)/N
## Population standard deviation
sqrt((sum(x^2)-sum(x)^2/N)/N)

## Table 3.8 gives the frequency distribution of the daily commuting times (in
## minutes) from home to work for all 25 employees of a company. Calculate the
## population mean of the daily commuting times for this grouped data.

f <- c(4, 9, 6, 4, 2)
m <- c(5, 15, 25, 35, 45)

N <- sum(f)
mu <- sum(f * m) / N
mu

## Compute the population variance and standard deviation for this grouped data.

var <- sum(f * (m - mu)^2) / N
var
sd <- sqrt(var)
sd

## Consider the frequency distribution of the number of orders received each day
## during the past 50 days at the office of a mail-order company. Compute the
## group sample means and standard deviations for the number of orders received
## each day.

orders <- c("10-12", "13-15", "16-18", "19-21")
m <- c(11, 14, 17, 20)
days <- c(4, 12, 20, 14)

n <- sum(days)
xbar <- sum(m * days) / n
xbar

var <- sum(days * (m - xbar)^2) / (n - 1)
var
sd <- sqrt(var)
sd

## Consider the frequency distribution of the daily commuting times (in minutes)
## from home to work for all 25 employees of a company. Compute the population
## group mean and standard deviation for this grouped data.


m <- c(5, 15, 25, 35, 45)
f <- c(4, 9, 6, 4, 2)

N <- sum(f)
mu <- sum(f * m) / N
mu
var <- sum(f * (m - mu)^2) / N
var
sd <- sqrt(var)
sd



