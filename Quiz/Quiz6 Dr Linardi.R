#airquality is a built in data set in R. Type airquality in the Console.
# see https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/airquality.html
# for explanation

# 3 pts. The code below produces the following data frame. 
# Rewrite it using dplyr. Note that windClass is 1 for wind speed of <5mph, 2 for <10mph, etc

#windClass avgWindMph  N
#1         1       3.05  6
#2         2       7.31 64
#3         3      11.49 65
#4         4      15.62 16
#5         5      20.40  2

#Extra credit, 1 pt: add another column, quality.
#If N<10, there are too few observations. 
#If N>=10, report the % of observation that is missing informatione about ozone.

#windClass avgWindMph  N                  dataQuality
#1         1       3.05  6        Too few observations
#2         2       7.31 64    Ozone info: 25 % missing
#3         3      11.49 65 Ozone info: 23.08 % missing
#4         4      15.62 16 Ozone info: 31.25 % missing
#5         5      20.40  2        Too few observations

windcategory<-round((airquality$Wind+3)/5, 0)
windcat2<-sort(unique(windcategory))
dat<-data.frame(windClass=integer(), avgWindMph=double(), N=integer(), dataQuality=character(), stringsAsFactors=FALSE)
for (i in 1:length(windcat2)) {
  idx<-which(windcategory==windcat2[i])
  dat[i,]$windClass<-windcat2[i]
  dat[i,]$avgWindMph<-round(mean(airquality[idx,]$Wind),2)
  dat[i,]$N<-length(idx)
  ## for extra credit
  # Usual way: add this to bottom of loop
  if (length(idx)<10) {
    dat[i,]$dataQuality<-"Too few observations"
  } else {
    percentMissing<-round(length(which(is.na(airquality[idx,]$Ozone)))*100/(length(idx)),0)
    dat[i,]$dataQuality<-paste("Ozone info:", percentMissing, "% missing")
  }
}


# Solutions # dplyr way:
library(dplyr)

percentMissing<-function(x) {
    return(round(length(which(is.na(x)))*100/length(x),0))
}
# works without mutate as well
aq<-mutate(airquality, windClass=round((airquality$Wind+3)/5, 0))
aq_group<-group_by(aq, windClass)
aq2<-summarise(aq_group, avgWindMph=round(mean(Wind),2), N=n(),
dataQuality=ifelse(N<10,"Too few observations", paste("Ozone info:", percentMissing(Ozone), "% missing") ))




