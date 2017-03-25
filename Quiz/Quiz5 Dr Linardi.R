library(ggplot2) 
summary(mpg)

ismanual<-function(x) {
  tmp<-grep("manual", x)
  return(length(tmp))
}

lenunique <- function(x){
  length(unique(x))
}

## Solutions in base R 
count<-table(as.factor(mpg$manufacturer))
modelcount<-tapply(mpg$model, mpg$manufacturer, lenunique)
manualcount<-tapply(mpg$trans, mpg$manufacturer, ismanual)
dt<-data.frame(count, modelcount, manualcount) 

# plotting
fraction<-manualcount*100/count
barplot(fraction[fraction>0], las=2, ylab="% manual transmission", main="Car manufacturers with manual transmission vehicles")