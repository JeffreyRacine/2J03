## The following are the scores of 30 college students on a statistics test:
## This is a simple R script containing the data and creating a stem-and-leaf
## plot from it. Note that the option atom=10 in the function stem() is used to
## specify the stem width of 10, which means that the stems will represent the
## tens place of the scores, and the leaves will represent the units. One
## advantage of a stem-and-leaf display is that we do not lose information on
## individual observations.

scores <- c(75,69,83,52,72,84,80,81,77,96,61,64,65,76,71,79,
            86,87,71,79,72,87,68,92,93,50,57,95,92,98)

## Print the raw data to the screen. 

print(scores)

## The stem and leaf plot. By default stem() is ranked in ascending order.

stem(scores,atom=10)
