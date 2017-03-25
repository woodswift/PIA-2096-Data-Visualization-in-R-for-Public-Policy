library(ggplot2)

dim(mpg) # 234 11
str(mpg)
summary(mpg)
head(mpg)

# manufacturer, model, trans
summary(as.factor(mpg$manufacturer))
summary(as.factor(mpg$model))
summary(as.factor(mpg$trans))

# note: each row represents vehicle type

# the number of vehicle types
numVehicleTypes <- tapply(mpg$manufacturer, mpg$manufacturer, length) # 15
numVehicleTypes

# the number of models
lenunique <- function(x){
  length(unique(x))
}
numModels <- tapply(mpg$model, mpg$manufacturer, lenunique) # 15
numModels

# the number of vehicle types that utilize manual transmission
# extract index of manual transmission
manualIdx <- grep("manual", mpg$trans) # 77=58+19
numManualVehicleTypes <- tapply(mpg$manufacturer[manualIdx], 
                                mpg$manufacturer[manualIdx], 
                                length) # 10 only!
numManualVehicleTypes
# initial a new one with 15 slots
numManualVehicleTypes1 <- rep(0,15)
names(numManualVehicleTypes1) <- names(numVehicleTypes)
numManualVehicleTypes1[names(numManualVehicleTypes)] <- numManualVehicleTypes
numManualVehicleTypes1 # 15

dat <- data.frame(Var1=names(numVehicleTypes),
                  Freq=as.vector(numVehicleTypes),
                  modelcount=as.vector(numModels),
                  manualcount=as.vector(numManualVehicleTypes1))

dat

# ------ Extra credit ------
idx <- which(dat$manualcount>0)
dat1 <- data.frame(Var1=dat$Var1[idx],
                   manualpercent=dat$manualcount[idx]/dat$Freq[idx])
dat1
ggplot(dat1, aes(x=as.factor(dat1$Var1), y=dat1$manualpercent)) + 
  geom_bar(stat="identity")