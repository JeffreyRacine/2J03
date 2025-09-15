## Create a vector for the donut data set, then create frequency tables, bar
## graph, proportion tables, and a Pareto chart. We observe that the variable
## donut is a qualitative variable, and in R we will 'cast' it so that R
## recognizes that it is qualitative and not numeric using the factor()
## function.

donut <- c("glazed", "filled", "other", "plain", "glazed", "other", 
           "frosted", "filled", "filled", "glazed", "other", 
           "frosted", "glazed", "plain", "other", "glazed", 
           "glazed", "filled", "frosted", "plain", "other", 
           "other", "frosted", "filled", "filled", "other", 
           "frosted", "glazed", "glazed", "filled")

## Cast as a qualitative variable with levels in the order given in the class
## slides

donut <- factor(donut,levels=c("glazed","filled","frosted","plain","other"))

## Frequency Table

table(donut)

## Create a Relative Frequency Table

round(prop.table(table(donut)),3)

## Create a Percentage Table

round(100*prop.table(table(donut)),1)

## Combine the Frequency, Relative Frequency, and Percentage Tables

cbind(Frequency=table(donut),
      Relative_Frequency=round(prop.table(table(donut)),3),
      Percentage=round(100*prop.table(table(donut)),1))

## Bar graph 

barplot(table(donut),ylab="Frequency",xlab="Donut Variety",main="Bar Graph")

## Pareto chart - here we reorder the factor levels by frequency in decreasing
## order and store them in a new variable

donut.pareto <- reorder(donut,donut,FUN=length,decreasing=TRUE)

barplot(table(donut.pareto),ylab="Frequency",xlab="Donut Variety",main="Pareto Chart")
