library(dplyr)
library(ggplot2)
library(lubridate)

#Continuing Quiz 6, we will use the built-in airquality dataset. 
#1. Use dplyr to calculate the number of ozone observation per month.  What month has the fewest ozone observation? 
# A tibble: 5 ? 3
#      Month     N NOzoneComplete
#<int> <int>          <int>
# ....
#4     8    31             26
#5     9    30             29

#2. Use dplyr and lubridate to create the data set below -- you will need it for task 3. 
#Ozone Solar.R Wind Temp Month Day missingOz      time
#1      41     190  7.4   67     5   1     FALSE 1973-05-01
#2      36     118  8.0   72     5   2     FALSE 1973-05-02
#3      12     149 12.6   74     5   3     FALSE 1973-05-03
#4      18     313 11.5   62     5   4     FALSE 1973-05-04


#3. Use ggplot and the data set you created in 2 to create a time series of temperature,
# colored by solar radiation level and using markers that indicate if the ozone data is missing. 


#time=Day+Month*30, 
aq<-mutate(airquality, 
           missingOz=is.na(Ozone), 
           time2=mdy(paste(Month,Day,1973, sep="-")))

# in which month is Ozone information most often missing?
aq2<-group_by(aq, Month)
aq3<-summarise(aq2, 
               N=length(Ozone), 
               NOzoneComplete=N-sum(missingOz))

ggplot(aq, aes(x=time2, y=Temp, g=Solar.R )) +   
  geom_line(size=1.1, color="gray") +   
  geom_point(alpha=0.5, size=4, aes(color=Solar.R, shape = factor(missingOz)))+
  xlab("Date")+ 
  ylab("Temperature (F)") +  
  ggtitle("Weather in New York, May-Sept 1973") +
labs(colour="Solar radiation", shape="Missing ozone data")
