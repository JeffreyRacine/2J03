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

## Create a table with frequencies, relative frequencies, and percentages for the donut data

donut.freq <- table(donut)
donut.rel.freq <- prop.table(donut.freq)
donut.perc <- round(donut.rel.freq*100,2)
donut.table <- cbind(donut.freq,donut.rel.freq,donut.perc)
colnames(donut.table) <- c("Frequency","Relative Frequency","Percentage")
donut.table

## Create a pie chart for the donut data showing percentages for each slice

pie(donut.freq,labels=paste(names(donut.freq),"\n",donut.perc,"%"),main="Pie Chart")
