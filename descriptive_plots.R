

## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# 1+3.3*log10(1000)


## ----height,echo=FALSE--------------------------------------------------------
nobs <- 5
set.seed(42)
hgt <- round(rnorm(nobs,160,20))
## Here we "simulate" data, if you use other data replace
## the variable name "hgt" with your variable name throughout
L <- min(hgt)
U <- max(hgt)
K <- round(1+3.3*log10(length(hgt)))
W <- (U-L)/K
B <- seq(L,U,W)
AF <- table(cut(sort(hgt), B, include.lowest=TRUE, right=FALSE))
RF <- AF/length(hgt)
CF <- cumsum(AF)
CR <- cumsum(RF)


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# hgt <- c(187, 149, 167, 173, 168)


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# K <- round(1+3.3*log10(5))


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# W <- (max(hgt)-min(hgt))/K


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# seq(min(hgt),max(hgt),by=W)


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
# AF <- c(1, 3, 1)
# RF <- AF/5
# CF <- cumsum(AF)
# CR <- CF/5
# cbind(AF,RF,CF,CR)


## ----ucbadmissions,echo=TRUE--------------------------------------------------
DF <- as.data.frame(UCBAdmissions)
xtabs(Freq ~ Gender + Admit, DF)
summary(xtabs(Freq ~ Gender + Admit, DF))


## -----------------------------------------------------------------------------
#| label:  histexample
#| echo: true
revenue <- scan("https://jeffreyracine.github.io/2J03/revenue.dat")
## Plot the absolute frequency histogram
hist(revenue,xlab="Net Revenue Classes",ylab="Absolute Frequency",main="",br=9)


## -----------------------------------------------------------------------------
#| label: histpolyexample
#| echo: true
## Plot the absolute frequency histogram and frequency polygon
revenue <- scan("https://jeffreyracine.github.io/2J03/revenue.dat")
hist(revenue,xlab="Net Revenue Classes",ylab="Absolute Frequency",main="",br=9)
revenue.hist <- hist(revenue,br=9,plot=FALSE)
lines(revenue.hist$mids,revenue.hist$counts,type="b")


## -----------------------------------------------------------------------------
#| label: histpolydensityexample
#| echo: true
revenue <- scan("https://jeffreyracine.github.io/2J03/revenue.dat")
## Plot the relative frequency histogram adjusted so that area equals 1 (prob=TRUE)
hist(revenue,xlab="Net Revenue Classes",ylim=c(0,.009),ylab="Density",main="",br=9,prob=TRUE)
lines(density(revenue))


## -----------------------------------------------------------------------------
#| label: cumsurvexample
#| echo: true
revenue <- scan("https://jeffreyracine.github.io/2J03/revenue.dat")
## Construct the cumulative and survivor functions
cum.abs <- cumsum(revenue.hist$counts)
## Plot the cumulative and survivor functions
par(mfrow=c(1,2))
plot(revenue.hist$breaks[1:8],cum.abs,t="s",xlab="Net Revenue Classes",ylab="Cumulative Absolute Frequency (LE)",main="")
plot(revenue.hist$breaks[1:8],100-cum.abs,t="s",xlab="Net Revenue Classes",ylab="Cumulative Absolute Frequency (ME)",main="")


## -----------------------------------------------------------------------------
#| label: growthexample
#| echo: true
growthdata <- read.table("https://jeffreyracine.github.io/2J03/growth.dat",header=TRUE)
attach(growthdata)
## Create a scatterplot
plot(age/12,height,xlab="Age (Years)",ylab="Height (cm)",type="p",cex=.25)


## ----deathdata----------------------------------------------------------------
## Read in the cause of death data
deathdat <- read.table("https://jeffreyracine.github.io/2J03/death.dat",header=TRUE)
attach(deathdat)
knitr::kable(deathdat,
             align = c("l","l","r","r","r"),
             col.names=c("Rank", "Causes of death", "Counts", "% of top 10", "% of total"))


## -----------------------------------------------------------------------------
#| label: deathbarplot
#| echo: true
## Create a barplot
deathdat <- read.table("https://jeffreyracine.github.io/2J03/death.dat",header=TRUE)
attach(deathdat)
barplot(counts/1000,names.arg=cause,main="Top 10 Causes of Death in the US, 2001",ylab="Counts (x1000)",cex.names=0.5,las=2)


## -----------------------------------------------------------------------------
#| label:  deathpieplot
#| echo: true
## Create a pie chart
deathdat <- read.table("https://jeffreyracine.github.io/2J03/death.dat",header=TRUE)
attach(deathdat)
pie(counts,labels=cause,cex=0.75)


## ----meanexample,echo=TRUE----------------------------------------------------
X <- c(112,114,98,91,112,64,113,94,94,82)
mean(X)


## ----medianexample,echo=TRUE--------------------------------------------------
X <- c(112,114,98,91,112,64,113,94,94,82)
median(X)


## ----modeexample,echo=TRUE----------------------------------------------------
mode <- function(x) {
  ux <- unique(x)
  tux <- tabulate(match(x, ux))
  ux[tux==max(tux)]
}
X <- c(112,114,98,91,112,64,113,94,94,82)
mode(X)


## ----varsdexample,echo=TRUE---------------------------------------------------
X <- c(112,114,98,91,112,64,113,94,94,82)
var(X)
sd(X)


## ----rangeexample,echo=TRUE---------------------------------------------------
range(X)


## ----iqrquantilexample,echo=TRUE----------------------------------------------
IQR(X)
## Compute quartiles
quantile(X,seq(0,1,by=0.25))


## ----skewexample--------------------------------------------------------------
## We simulate data for this example
## Properties of the chi-square distribution:
##
## Mean	v
## Median	 approximately  v - 2/3 for large v
## Mode	v -2 for v > 2
## Range	 0 to positive infinity
## Standard Deviation	sqrt(2v)
## Coefficient of Variation	 sqrt(2/v)
## Skewness	 2^{1.5}/sqrt(v)
## Kurtosis	3 + 12/v

rm(list=ls())

n <- 10000
df <- 5

x <- sort(rchisq(n,df=df))

plot(x,dchisq(x,df=5),
     xlim=c(0,4*df),
     xlab="X",
     ylab="Density",
     type="l",
     sub=paste("mode=",format(df-2,digits=2),
       ", median=",format(df-2/3,digits=2),
       ", mean=",format(df,digits=2),sep=""))

abline(v=(df-2),lwd=2,lty=1,col="red")
abline(v=(df-2/3),lwd=3,lty=2,col="black")
abline(v=df,lwd=3,lty=3,col="blue")


## ----momentsexample,echo=TRUE-------------------------------------------------
library(moments)
set.seed(12345)
x <- rnorm(10000)
skewness(x)
kurtosis(x)


## -----------------------------------------------------------------------------
set.seed(42)
x <- seq(-8,8,length=250)
df <- 2
plot(x,dnorm(x),
     xlab="X",
     ylab="Density",
     type="l",
     sub=paste("Gaussian kurtosis=",format(moments::kurtosis(rnorm(1000)),digits=4),", Student-t kurtosis=",format(moments::kurtosis(rt(1000,df=df)),digits=4),sep=""))
lines(x,dt(x,df=df),col=2,lty=2,lwd=2)
legend("topleft",c("Gaussian","Student-t"),lty=1:2,col=1:2,bty="n")


## ----fivenumexample,echo=TRUE-------------------------------------------------
## Create some data
options(digits=3)
set.seed(12345)
x <- rnorm(1000,mean=0,sd=1)
## Generate Tukey's five-number summary
fivenum(x)
## We could also generate this manually via the quantile() function
quantile(x,seq(0,1,by=.25),type=2)


## ----boxplotex,echo=TRUE,eval=FALSE-------------------------------------------
# par(mfrow=c(1,2))
# ## Data with no "outliers"
# X <- c(64,82,91,94,94,98,112,112,113,114)
# boxplot(X)
# ## Data with 2 "outliers"
# X[1] <- 50
# X[10] <- 150
# boxplot(X)
# Q1.X <- quantile(X,0.25,type=2)
# Q3.X <- quantile(X,0.75,type=2)
# abline(h=c(Q1.X,Q3.X),col="red",lty=2)
# abline(h=c(Q1.X-1.5*IQR(X),Q3.X+1.5*IQR(X)),col="blue",lty=2)
# legend("topleft",c("Q1","Q3","Suspected Outlier Limits"),lty=2,col=c("red","red","blue"),bty="n")


## ----boxplotcode,echo=TRUE----------------------------------------------------
## Read in the S&P100 data closing values, then generate a boxplot
Close <- read.table("https://jeffreyracine.github.io/2J03/sp100.dat",header=TRUE,sep=",")$Close
boxplot(Close,ylab="Standard and Poor's 100 Close (1982-2009)",notch=TRUE)


## -----------------------------------------------------------------------------
#| label: fig-gaussianexample
#| echo: true
#| fig-cap: "Standard Normal Probability Histogram and Density Function"
par(mfrow=c(1,2))
## Generate some data from a standard normal distribution, plot the histogram
## and the standard normal ("Gaussian") density
set.seed(42)
x <- rnorm(1e+05)
hist(x,prob=T,xlim=c(-4,4),xlab="X",ylab="Density",main="Standard Normal Probability Histogram")
x <- seq(-4,4,length=250)
plot(x,dnorm(x),xlim=c(-4,4),xlab="X",ylab="Density",main="Standard Normal Density Function",type="l")


## ----propexample,echo=TRUE----------------------------------------------------
## Simulate a random sample of categorical data
set.seed(42)
x <- factor(ifelse(rbinom(1000,1,0.2)==0,"Male","Female"))
table(x)
prop.table(table(x))

