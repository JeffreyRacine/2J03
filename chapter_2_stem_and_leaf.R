## This is a simple R script containing the data and creating a stem-and-leaf
## plot from it. Note that the option atom=10 in the function stem() is used to
## specify the stem width of 10, which means that the stems will represent the
## tens place of the ages, and the leaves will represent the units
Age <- c(21, 19, 24, 25, 29, 34, 26, 27, 37, 33, 18, 20, 19, 22, 19, 19, 25, 22, 25, 23, 25, 19, 31, 19, 23, 18, 23, 19, 23, 26, 22, 28, 21, 20, 22, 22, 21, 20, 19, 21, 25, 23, 18, 37, 27, 23, 21, 25, 21, 24)
## Print the raw data to the screen. 
print(Age)
## The stem and leaf plot. Option scale=1 is the default, you can play around to
## see its effect.
stem(Age,atom=10,scale=.3)
