PlotByTime <- function(time, count){
  # bar plot or histogram
  library(ggplot2)
  theme_set(theme_bw())
  
  dat <- data.frame(time, count)
  ggplot(data=dat, aes(x=time, y=count)) + 
    geom_bar(stat="identity") +
    theme(axis.text.x=element_text(size=5, angle=90))
}