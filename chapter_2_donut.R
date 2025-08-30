## Create a vector for the donut data set, then create frequency tables,
## bar graph, proportion tables, and a Pareto chart

donut <- c("glazed", "filled", "other", "plain", "glazed", "other", 
           "frosted", "filled", "filled", "glazed", "other", 
           "frosted", "glazed", "plain", "other", "glazed", 
           "glazed", "filled", "frosted", "plain", "other", 
           "other", "frosted", "filled", "filled", "other", 
           "frosted", "glazed", "glazed", "filled")

## Cast as a qualitative variable with levels in the order given in the class
## slides

donut <- factor(donut,levels=c("glazed","filled","frosted","plain","other"))

## Frequency table

table(donut)

## Bar graph 

barplot(table(donut),ylab="Frequency",xlab="Donut Variety",main="Bar Graph")

## Proportion table

prop.table(table(donut))

## Pareto chart - here we reorder the factor levels by frequency in decreasing
## order and store them in a new variable

donut.pareto <- reorder(donut,donut,FUN=length,decreasing=TRUE)

barplot(table(donut.pareto),ylab="Frequency",xlab="Donut Variety",main="Pareto Chart")
