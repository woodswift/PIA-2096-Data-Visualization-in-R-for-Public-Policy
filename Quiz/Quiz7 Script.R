dat <- airquality

str(dat)
summary(dat)
dat[1:6,]

#Q1
library(dplyr)
month_class<-group_by(dat, Month)
ozone_month_class<-summarise(month_class, 
                             count=n(), 
                             NOzoneComplete=length(which(is.na(Ozone)=="FALSE")))

ozone_month_class

#Q2
dat1<-mutate(dat,
             missingOz=ifelse(is.na(Ozone),"TRUE","FALSE"),
             time=paste("1973", Month, Day, sep="-"))
dat1$missingOz<-as.factor(dat1$missingOz)

str(dat1)
dat1[1:6,]

library(lubridate)
dat1$time <- as.POSIXct(dat1$time)

#Q3
library(ggplot2)
ggplot(dat1, aes(x=time, y=Temp)) +
  geom_point(aes(colour=Solar.R, shape = factor(missingOz), size = 3)) + 
  scale_colour_gradient(low = "blue") +
  geom_line(color="grey") +
  labs(title="Weather in New York, May-Spet 1973") +
  labs(colour="Solar Radiation", shape="Missing Ozone Data") + 
  labs(x="Date") +
  labs(y="Temperature (F)")