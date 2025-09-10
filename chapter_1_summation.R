## In Chapter 1 we consider summation notation where we work things out with a
## pencil and paper. Here we use R to illustrate summation notation using the
## sum() function. Note that ^2 will square the object to its left, while the
## asterisk is for multiplication in R. Also, R uses the c() function to create
## a vector, and the <- operator to assign a value to an object, working right
## to left.

## Suppose a sample consists of five books, and the prices of these five books
## are $175, $80, $165, $97, and $88. Store these values in a vector named
## price, then apply the sum(), ^2, and * operators below for this and for the
## salary data. Try to become comfortable working back and forth between the
## notation on the slides and computation in R.

price <- c(175, 80, 165, 97, 88)

## Σx

sum(price)

## Annual salaries (in thousands of dollars) of four workers are 75, 90, 125,
## and 61, respectively.

salaries <- c(75, 90, 125, 61)

## (a) Σx

sum(salaries)

## (b) (Σx) all squared

(sum(salaries))^2

## (c) Σx-squared

sum(salaries^2)

## The following lists four pairs of m and f values:
## m1 = 12, m2 = 15, m3 = 20, m4 = 30
## f1 = 5, f2 = 9, f3 = 10, f4 = 16

m <- c(12, 15, 20, 30)
f <- c(5, 9, 10, 16)

## Compute the following: (a) Σm (b) Σf² (c) Σmf (d) Σm²f

## (a) Σm

sum(m)

## (b) Σf²

sum(f^2)

## (c) Σmf

sum(m * f)

## (d) Σm²f

sum((m^2) * f)

